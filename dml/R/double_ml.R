library('R6')
library('data.table')

DoubleML <- R6Class("DoubleML", public = list(
  n_folds = NULL,
  ml_learners = NULL,
  params = NULL,
  dml_procedure = NULL,
  inf_model = NULL,
  n_rep_cross_fit = 1,
  coef = NULL,
  se = NULL,
  se_reestimate = FALSE,
  boot_coef = NULL,
  initialize = function(...) {
    stop("DoubleML is an abstract class that can't be initialized.")
  },
  fit = function(data, y, d, z=NULL) {
    
    n_obs = dim(data)[1]
    n_treat = length(d)
    
    private$initialize_arrays(n_obs, n_treat, self$n_rep_cross_fit)
    
    if ((self$n_rep_cross_fit > 1) & (!is.null(private$smpls))) {
      stop("Externally transferred samples not supported for repeated cross-fitting.")
    }
    
    for (i_rep in 1:self$n_rep_cross_fit) {
      private$i_rep = i_rep
      if(is.null(private$smpls) | (self$n_rep_cross_fit>1)) {
        # perform sample splitting based on a dummy task with the whole data set
        private$split_samples(data)
      }
      
      for (i_treat in 1:n_treat) {
        private$i_treat = i_treat
        
        # ml estimation of nuisance models and computation of score elements
        scores = private$ml_nuisance_and_score_elements(data, y, d[i_treat], z)
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
    
    self$coef = apply(private$all_coef, 1, stats::median)
    self$se = sqrt(apply(private$all_se^2  - (private$all_coef - self$coef)^2, 1, stats::median))
    
    invisible(self)
  },
  bootstrap = function(method='normal', n_rep = 500) {
    dml_procedure = self$dml_procedure
    n_obs = dim(private$score_a)[1]
    n_folds = self$n_folds
    test_ids = private$smpls$test_ids
    
    if (method == "Bayes") {
      weights <- stats::rexp(n_rep * n_obs, rate = 1) - 1
    } else if (method == "normal") {
      weights <- stats::rnorm(n_rep * n_obs)
    } else if (method == "wild") {
      weights <- stats::rnorm(n_rep * n_obs)/sqrt(2) + (stats::rnorm(n_rep * n_obs)^2 - 1)/2
    }
    
    # for alignment with the functional (loop-wise) implementation we fill by row
    weights <- matrix(weights, nrow = n_rep, ncol = n_obs, byrow=TRUE)
    
    if (dml_procedure == "dml1") {
      boot_coefs <- matrix(NA, nrow = n_rep, ncol = n_folds)
      ii = 0
      for (i_fold in 1:n_folds) {
        test_index <- test_ids[[i_fold]]
        n_obs_in_fold = length(test_index)
        #print(private$score[test_index])
        
        J = mean(private$get__score_a()[test_index])
        boot_coefs[,i_fold] <- weights[,(ii+1):(ii+n_obs_in_fold)] %*% private$get__score()[test_index] / (n_obs_in_fold * self$se * J)
        ii = ii + n_obs_in_fold
      }
      self$boot_coef = rowMeans(boot_coefs)
    }
    else if (dml_procedure == "dml2") {
      
      J = mean(private$score_a)
      self$boot_coef = weights %*% private$get__score() / (n_obs * self$se * J)
    }
    invisible(self)
  },
  set_samples = function(train_ids, test_ids) {
    # TODO place some checks for the externally provided sample splits
    private$smpls <- list(train_ids = train_ids,
                          test_ids = test_ids)
    
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
  i_rep = NA,
  i_treat = NA,
  initialize_double_ml = function(n_folds,
                        ml_learners,
                        params,
                        dml_procedure,
                        inf_model,
                        se_reestimate,
                        n_rep_cross_fit) {
    stopifnot(is.numeric(n_folds), length(n_folds) == 1)
    # TODO add input checks for ml_learners
    stopifnot(is.character(dml_procedure), length(dml_procedure) == 1)
    stopifnot(is.logical(se_reestimate), length(se_reestimate) == 1)
    stopifnot(is.character(inf_model), length(inf_model) == 1)
    stopifnot(is.numeric(n_rep_cross_fit), length(n_rep_cross_fit) == 1)
    
    self$n_folds <- n_folds
    self$ml_learners <- ml_learners
    self$params <- params
    self$dml_procedure <- dml_procedure
    self$se_reestimate <- se_reestimate
    self$inf_model <- inf_model
    self$n_rep_cross_fit <- n_rep_cross_fit
    
    invisible(self)
  },
  initialize_arrays = function(n_obs,
                               n_treat,
                               n_rep_cross_fit) {
    private$score = array(NA, dim=c(n_obs, n_rep_cross_fit, n_treat))
    private$score_a = array(NA, dim=c(n_obs, n_rep_cross_fit, n_treat))
    private$score_b = array(NA, dim=c(n_obs, n_rep_cross_fit, n_treat))
    
    self$coef = array(NA, dim=c(n_treat))
    self$se = array(NA, dim=c(n_treat))
    
    private$all_coef = array(NA, dim=c(n_treat, n_rep_cross_fit))
    private$all_se = array(NA, dim=c(n_treat, n_rep_cross_fit))
    
  },
  # Comment from python: The private properties with __ always deliver the single treatment, single (cross-fitting) sample subselection
  # The slicing is based on the two properties self._i_treat, the index of the treatment variable, and
  # self._i_rep, the index of the cross-fitting sample.
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
  split_samples = function(data) {
    dummy_task = Task$new('dummy_resampling', 'regr', data)
    dummy_resampling_scheme <- rsmp("cv", folds = self$n_folds)$instantiate(dummy_task)
    train_ids <- lapply(1:self$n_folds, function(x) dummy_resampling_scheme$train_set(x))
    test_ids <- lapply(1:self$n_folds, function(x) dummy_resampling_scheme$test_set(x))
    private$smpls <- list(train_ids = train_ids,
                          test_ids = test_ids)
    
    invisible(self)
  },
  instantiate_resampling = function(task, external_train_ids=NULL) {
    # instantiate custom resampling with already sampled train and test ids
    resampling <- mlr3::rsmp("custom")
    if(!is.null(external_train_ids)) {
      train_ids <- external_train_ids
    } else {
      train_ids<- private$smpls$train_ids
    }
    test_ids <- private$smpls$test_ids
    
    resampling$instantiate(task, train_ids, test_ids)
    return(resampling)
  },
  est_causal_pars = function() {
    dml_procedure = self$dml_procedure
    se_reestimate = self$se_reestimate
    n_folds = self$n_folds
    test_ids = private$smpls$test_ids
    
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
    test_ids = private$smpls$test_ids
    
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
  var_est = function(inds=NULL) {
    score_a = private$get__score_a()
    score = private$get__score()
    # n_obs must be determined before subsetting
    n_obs = length(score)
    if(!is.null(inds)) {
      score_a = score_a[inds]
      score = score[inds]
    }
    
    J = mean(score_a)
    sigma2_hat = 1/n_obs * mean(score^2) / (J^2)
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

