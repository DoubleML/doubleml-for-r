library('R6')
library('data.table')
library('mlr3')
library("mlr3learners")

DoubleML <- R6Class("DoubleML", public = list(
  n_folds = NULL,
  ml_learners = NULL,
  params = NULL,
  dml_procedure = NULL,
  inf_model = NULL,
  n_rep_cross_fit = 1,
  n_nuisance = NULL,
  coef = NULL,
  se = NULL,
  se_reestimate = FALSE,
  boot_coef = NULL,
  param_set = NULL, 
  tune_settings = NULL,
  param_tuning = NULL,
  initialize = function(...) {
    stop("DoubleML is an abstract class that can't be initialized.")
  },
  fit = function(data, y, d, z=NULL) {
    
    # TBD: insert check for tuned params
    
    n_obs = dim(data)[1]
    n_treat = length(d)
    
    private$initialize_arrays(n_obs, n_treat, self$n_rep_cross_fit)
    
    if (is.null(private$smpls)) {
      private$split_samples(data)
    }

    for (i_rep in 1:self$n_rep_cross_fit) {
      private$i_rep = i_rep
      
      for (i_treat in 1:private$n_treat) {
        private$i_treat = i_treat
        
        # ml estimation of nuisance models and computation of score elements
        scores = private$ml_nuisance_and_score_elements(data,
                                                        private$get__smpls(),
                                                        y, d[i_treat], z, 
                                                        params = private$get__params())
        private$set__score_a(scores$score_a)
        private$set__score_b(scores$score_b)
        
        # estimate the causal parameter(s)
        coef <- private$est_causal_pars()
        private$set__all_coef(coef)
        
        # compute score (depends on estimated causal parameter)
        private$compute_score()
        
        # compute standard errors for causal parameter
        se <- private$se_causal_pars()
        private$set__all_se(se)
      }
    }
    
    private$agg_cross_fit()
    
    invisible(self)
  },
  bootstrap = function(method='normal', n_rep = 500) {
    private$initialize_boot_arrays(n_rep, self$n_rep_cross_fit)
    
    for (i_rep in 1:self$n_rep_cross_fit) {
      private$i_rep = i_rep
      
      for (i_treat in 1:private$n_treat) {
        private$i_treat = i_treat
        
        boot_coef = private$compute_bootstrap(method, n_rep)
        private$set__boot_coef(boot_coef)
      }
    }
    
    invisible(self)
  },
  set_samples = function(smpls) {
    # TODO place some checks for the externally provided sample splits
    # see also python
    private$smpls <- smpls
    
    invisible(self)
  }, 
  tune = function(data, y, d, z=NULL) {
    n_obs = dim(data)[1]
    n_treat = length(d)
    
    # TBD: prepare output of parameter tuning (dimensions: n_folds x n_rep_cross_fit)
    private$initialize_list(n_treat, self$n_rep_cross_fit, self$n_nuisance) 
    
    if (is.null(private$smpls)) {
      private$split_samples(data)
    }
    
    for (i_rep in 1:self$n_rep_cross_fit) {
      private$i_rep = i_rep
      
      for (i_treat in 1:private$n_treat) {
        private$i_treat = i_treat
        
        # TBD: insert setter/getter function -> correct indices and names, repeated crossfitting & multitreatment
        #  ! important ! tuned params must exactly correspond to training samples
        # TBD: user friendly way to pass (pre-)trained parameters
        # TBD: advanced usage passing original mlr3training objects like terminator, smpl, 
        #      e.g., in seperate function (tune_mlr3)...
        # TBD: Pass through instances (requires prespecified tasks)
        # TBD: Handling different measures for classification and regression (logit???)
        self$param_tuning = private$tune_params(data, private$get__smpls(),
                                                y, d[i_treat], z, param_set = self$param_set, 
                                                tune_settings = self$tune_settings)
        
        # here: set__params()
        self$params = self$param_tuning$params

      }
    }

    invisible(self)
  }
),
private = list(
  smpls = NULL,
  score = NULL,
  score_a = NULL,
  score_b = NULL,
  all_coef = NULL,
  all_se = NULL,
  n_obs = NULL,
  n_treat = NULL,
  n_rep_boot = NULL,
  i_rep = NA,
  i_treat = NA,
  initialize_double_ml = function(n_folds,
                        ml_learners,
                        params,
                        dml_procedure,
                        inf_model,
                        se_reestimate,
                        n_rep_cross_fit,
                        n_nuisance,
                        param_set,
                        tune_settings, 
                        param_tuning) {
    stopifnot(is.numeric(n_folds), length(n_folds) == 1)
    # TODO add input checks for ml_learners
    stopifnot(is.character(dml_procedure), length(dml_procedure) == 1)
    stopifnot(is.logical(se_reestimate), length(se_reestimate) == 1)
    stopifnot(is.character(inf_model), length(inf_model) == 1)
    stopifnot(is.numeric(n_rep_cross_fit), length(n_rep_cross_fit) == 1)
    stopifnot(is.list(tune_settings))
    
    self$n_folds <- n_folds
    self$ml_learners <- ml_learners
    self$params <- params
    self$dml_procedure <- dml_procedure
    self$se_reestimate <- se_reestimate
    self$inf_model <- inf_model
    self$n_rep_cross_fit <- n_rep_cross_fit
    self$n_nuisance <- n_nuisance
    self$param_set <- param_set
    self$tune_settings <- tune_settings
    self$param_tuning <- param_tuning
    
    invisible(self)
  },
  initialize_arrays = function(n_obs,
                               n_treat,
                               n_rep_cross_fit) {
    # set dimensions as private properties before initializing arrays
    private$n_obs = n_obs
    private$n_treat = n_treat
    
    private$score = array(NA, dim=c(n_obs, n_rep_cross_fit, n_treat))
    private$score_a = array(NA, dim=c(n_obs, n_rep_cross_fit, n_treat))
    private$score_b = array(NA, dim=c(n_obs, n_rep_cross_fit, n_treat))
    
    self$coef = array(NA, dim=c(n_treat))
    self$se = array(NA, dim=c(n_treat))
    
    private$all_coef = array(NA, dim=c(n_treat, n_rep_cross_fit))
    private$all_se = array(NA, dim=c(n_treat, n_rep_cross_fit))
    
  },
  initialize_boot_arrays = function(n_rep, n_rep_cross_fit) {
    private$n_rep_boot = n_rep
    self$boot_coef = array(NA, dim=c(private$n_treat, n_rep * n_rep_cross_fit))
  },
  initialize_lists = function(n_treat,
                              n_rep_cross_fit, 
                              n_nuisance) {
    # set dimensions as private properties before initializing arrays
    private$n_treat = n_treat
    
    private$params = rep(list(rep(list(vector("list", n_nuisance)), n_treat)), n_rep_cross_fit) 
  },
  # Comment from python: The private properties with __ always deliver the single treatment, single (cross-fitting) sample subselection
  # The slicing is based on the two properties self._i_treat, the index of the treatment variable, and
  # self._i_rep, the index of the cross-fitting sample.
  get__smpls = function() private$smpls[[private$i_rep]],
  get__score_a = function() private$score_a[, private$i_rep, private$i_treat],
  set__score_a = function(value) private$score_a[, private$i_rep, private$i_treat] <- value,
  get__score_b = function() private$score_b[, private$i_rep, private$i_treat],
  set__score_b = function(value) private$score_b[, private$i_rep, private$i_treat] <- value,
  get__score = function() private$score[, private$i_rep, private$i_treat],
  set__score = function(value) private$score[, private$i_rep, private$i_treat] <- value,
  get__all_coef = function() private$all_coef[private$i_treat, private$i_rep],
  set__all_coef = function(value) private$all_coef[private$i_treat, private$i_rep] <- value,
  get__all_se = function() private$all_se[private$i_treat, private$i_rep],
  set__all_se = function(value) private$all_se[private$i_treat, private$i_rep] <- value,
  get__boot_coef = function() {
    ind_start = (private$i_rep-1) * private$n_rep_boot + 1
    ind_end = private$i_rep * private$n_rep_boot
    self$boot_coef[private$i_treat, ind_start:ind_end]
    },
  set__boot_coef = function(value) {
    ind_start = (private$i_rep-1) * private$n_rep_boot + 1
    ind_end = private$i_rep * private$n_rep_boot
    self$boot_coef[private$i_treat, ind_start:ind_end] <- value
    },
  get__params = function(){
    
    if (is.null(self$params)){
      params = list()
    }
    else if (length(self$params) == self$n_nuisance){
      params = self$params
    }
    else {
      params = self$params[[private$i_rep]][[private$i_treat]]
    }
    return(params)
    },
  split_samples = function(data) {
    dummy_task = Task$new('dummy_resampling', 'regr', data)
    dummy_resampling_scheme <- rsmp("repeated_cv",
                                    folds = self$n_folds,
                                    repeats = self$n_rep_cross_fit)$instantiate(dummy_task)
    
    train_ids <- lapply(1:(self$n_folds * self$n_rep_cross_fit),
                        function(x) dummy_resampling_scheme$train_set(x))
    test_ids <- lapply(1:(self$n_folds * self$n_rep_cross_fit),
                       function(x) dummy_resampling_scheme$test_set(x))
    smpls <- lapply(1:self$n_rep_cross_fit, function(i_repeat) list(
      train_ids = train_ids[((i_repeat-1)*self$n_folds + 1):(i_repeat*self$n_folds)],
      test_ids = test_ids[((i_repeat-1)*self$n_folds + 1):(i_repeat*self$n_folds)]))
    
    private$smpls <- smpls
    invisible(self)
  },
  est_causal_pars = function() {
    dml_procedure = self$dml_procedure
    se_reestimate = self$se_reestimate
    n_folds = self$n_folds
    smpls = private$get__smpls()
    test_ids = smpls$test_ids
    
    if (dml_procedure == "dml1") {
      thetas <- rep(NA, n_folds)
      for (i_fold in 1:n_folds) {
        test_index <- test_ids[[i_fold]]
        thetas[i_fold] <- private$orth_est(inds=test_index)
      }
      coef <- mean(thetas)
    }
    else if (dml_procedure == "dml2") {
      coef <- private$orth_est()
    }
    
    return(coef)
  },
  se_causal_pars = function() {
    dml_procedure = self$dml_procedure
    se_reestimate = self$se_reestimate
    n_folds = self$n_folds
    smpls = private$get__smpls()
    test_ids = smpls$test_ids
    
    if (dml_procedure == "dml1") {
      vars <-  rep(NA, n_folds)
      if (se_reestimate == FALSE) {
        for (i_fold in 1:n_folds) {
          test_index <- test_ids[[i_fold]]
          vars[i_fold] <- private$var_est(inds=test_index)
        }
        se = sqrt(mean(vars)) 
      }
      if (se_reestimate == TRUE) {
        se = sqrt(private$var_est())
      }
      
    }
    else if (dml_procedure == "dml2") {
      se = sqrt(private$var_est())
    }
    
    return(se)
  },
  agg_cross_fit = function() {
    # aggregate parameters from the repeated cross-fitting
    # don't use the getter (always for one treatment variable and one sample), but the private variable
    self$coef = apply(private$all_coef, 1, stats::median)
    self$se = sqrt(apply(private$all_se^2  - (private$all_coef - self$coef)^2, 1, stats::median))
    
    invisible(self)
  },
  compute_bootstrap = function(method, n_rep) {
    dml_procedure = self$dml_procedure
    smpls = private$get__smpls()
    test_ids = smpls$test_ids
    
    if (method == "Bayes") {
      weights <- stats::rexp(n_rep * private$n_obs, rate = 1) - 1
    } else if (method == "normal") {
      weights <- stats::rnorm(n_rep * private$n_obs)
    } else if (method == "wild") {
      weights <- stats::rnorm(n_rep * private$n_obs)/sqrt(2) + (stats::rnorm(n_rep * private$n_obs)^2 - 1)/2
    }
    
    # for alignment with the functional (loop-wise) implementation we fill by row
    weights <- matrix(weights, nrow = n_rep, ncol = private$n_obs, byrow=TRUE)
    
    if (dml_procedure == "dml1") {
      boot_coefs <- matrix(NA, nrow = n_rep, ncol = self$n_folds)
      ii = 0
      for (i_fold in 1:self$n_folds) {
        test_index <- test_ids[[i_fold]]
        n_obs_in_fold = length(test_index)
        
        J = mean(private$get__score_a()[test_index])
        boot_coefs[,i_fold] <- weights[,(ii+1):(ii+n_obs_in_fold)] %*% private$get__score()[test_index] / (
          n_obs_in_fold * private$get__all_se() * J)
        ii = ii + n_obs_in_fold
      }
      boot_coef = rowMeans(boot_coefs)
    }
    else if (dml_procedure == "dml2") {
      
      J = mean(private$get__score_a())
      boot_coef = weights %*% private$get__score() / (private$n_obs * private$get__all_se() * J)
    }
    
    return(boot_coef)
  },
  var_est = function(inds=NULL) {
    score_a = private$get__score_a()
    score = private$get__score()
    if(!is.null(inds)) {
      score_a = score_a[inds]
      score = score[inds]
    }
    
    J = mean(score_a)
    sigma2_hat = 1/private$n_obs * mean(score^2) / (J^2)
  },
  orth_est = function(inds=NULL) {
    score_a = private$get__score_a()
    score_b = private$get__score_b()
    if(!is.null(inds)) {
      score_a = score_a[inds]
      score_b = score_b[inds]
    }
    theta = -mean(score_b) / mean(score_a)
  },
  compute_score = function() {
    score = private$get__score_a() * private$get__all_coef() + private$get__score_b()
    private$set__score(score)
    invisible(self)
  }
)
)

