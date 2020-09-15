#' @title DoubleML
#'
#' @description 
#' Abstract base class for DoubleML models. 
#' 
#' * Methods `$tune()` and `$fit()` which call internal methods for parameter tuning and estimation of the causal effects. 
#' @param data
#' @param n_folds
#' @param ml_learners
#' @param params
#' @param dml_procedure
#' @param score
#' @param subgroups
#' @param n_rep_cross_fit
#' @param coef
#' @param se
#' @param t
#' @param pval
#' @param se_reestimate
#' @param boot_coef
#' @param param_set
#' @param tune_settings
#' @param param_tuning
#' @export

DoubleML <- R6Class("DoubleML", public = list(
  #' @field data (`DoubleMLData`)\cr 
  #' Data object.
  data = NULL,
  #' @field n_folds (`integer(1)`)\cr
  #' Number of folds used for cross-fitting.
  n_folds = NULL,
  #' @field ml_learners (`list()`)\cr
  #' List of learners as named in mlr3 and mlr3learners packages.
  ml_learners = NULL,
  #' @field params (`list()`)\cr
  #' List of parameters passed to learners.
  params = NULL,
  #' @field dml_procedure (`character(1)`)\cr
  #' Double Machine Learning algorithm used (either "dml1" or "dml2").
  dml_procedure = NULL,
  #' @field score (`character(1)`)\cr
  #' psi to be used for estimation. 
  score = NULL,
  #' @field subgroups (`list()`)\cr
  #' Subgroups.
  subgroups = NULL,
  #' @field n_rep_cross_fit (`integer(1)`)\cr
  #' Number of repetitions for cross-fitting.
  n_rep_cross_fit = 1,
  #' @return coef (`numeric(1)`)\cr
  #' Final coefficient estimate.
  coef = NULL,
  #' @return se (`numeric(1)`)\cr
  #' Standard error.
  se = NULL,
  #' @return t (`numeric(1)`)\cr
  #' t statistic.
  t = NULL,  
  #' @return pval (`numeric(1)`)\cr
  #' p-value.
  pval = NULL,
  #' @field se_reestimate (`logical(1)`)\cr
  #' Flag for reestimation of standard error after cross-fitting. 
  se_reestimate = FALSE,
  #' @return boot_coef (`numeric()`)\cr
  #' Coefficients from multiplier bootstrap. 
  boot_coef = NULL,
  #' @field param_set (`list()`)\cr
  #' List with parameter set from mlr3tuning of class `ParamSet`. 
  param_set = NULL, 
  #' @field tune_settings (`list()`)\cr
  #' List with settings for parameter tuning passed to mlr3tuning.
  tune_settings = NULL,
  #' @return param_tuning (`list()`)\cr
  #' List with results from parameter tuning with mlr3tuning.
  param_tuning = NULL,
  #' @description 
  #' Initialize instance. `DoubleML` is an abstract class and initialization is typically performed for inherited classes. 
  initialize = function(...) {
    stop("DoubleML is an abstract class that can't be initialized.")
  },
  
  
  fit = function(se_reestimate = FALSE) {
    
    # TBD: insert check for tuned params
    
    private$initialize_arrays()
    
    if (is.null(private$smpls)) {
      private$split_samples()
    }

    for (i_rep in 1:self$n_rep_cross_fit) {
      private$i_rep = i_rep
      
      for (i_treat in 1:private$n_treat) {
        private$i_treat = i_treat
        
        if (private$n_treat > 1){
          self$data$set__data_model(self$data$d_cols[i_treat], self$data$use_other_treat_as_covariate)
        }
        
        # ml estimation of nuisance models and computation of psi elements
        psis = private$ml_nuisance_and_score_elements(self$data,
                                                        private$get__smpls(),
                                                        params = private$get__params())
        private$set__psi_a(psis$psi_a)
        private$set__psi_b(psis$psi_b)
        
        # estimate the causal parameter(s)
        coef <- private$est_causal_pars()
        private$set__all_coef(coef)
        
        # compute psi (depends on estimated causal parameter)
        private$compute_psi()
        
        # compute standard errors for causal parameter
        se <- private$se_causal_pars()
        private$set__all_se(se)
      }
    }
    
    private$agg_cross_fit()
    
    self$t = self$coef/self$se
    self$pval = 2 * stats::pnorm(-abs(self$t))
    names(self$coef) = names(self$se) = names(self$t) = names(self$pval) = self$data$d_cols

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
  tune = function() {
    n_obs = nrow(self$data$data_model)

    # TBD: prepare output of parameter tuning (list[[n_rep_cross_fit]][[n_treat]][[n_folds]])
    private$initialize_lists(private$n_treat, self$n_rep_cross_fit, private$n_nuisance) 
    
    if (is.null(private$smpls)) {
      private$split_samples()
    }
    
    for (i_rep in 1:self$n_rep_cross_fit) {
      private$i_rep = i_rep
      
      for (i_treat in 1:private$n_treat) {
        private$i_treat = i_treat
        
        if (private$n_treat > 1){
          self$data$set__data_model(self$data$d_cols[i_treat], self$data$use_other_treat_as_covariate)
        }
        
        # TBD: insert setter/getter function -> correct indices and names, repeated crossfitting & multitreatment
        #  ! important ! tuned params must exactly correspond to training samples
        # TBD: user friendly way to pass (pre-)trained parameters
        # TBD: advanced usage passing original mlr3training objects like terminator, smpl, 
        #      e.g., in seperate function (tune_mlr3)...
        # TBD: Pass through instances (requires prespecified tasks)
        # TBD: Handling different measures for classification and regression (logit???)
        param_tuning = private$tune_params(self$data, private$get__smpls(),
                                                param_set = self$param_set, 
                                                tune_settings = self$tune_settings)
        
        # here: set__params()
        private$set__params(param_tuning)
        
        #self$params = self$param_tuning$params

      }
    }

    invisible(self)
  },
  summary = function(digits = max(3L, getOption("digits") - 
                                                          3L)) {
    ans <- NULL
    k <- length(self$coef)
    table <- matrix(NA, ncol = 4, nrow = k)
    rownames(table) <- names(self$coef)
    colnames(table) <- c("Estimate.", "Std. Error", "t value", "Pr(>|t|)")
    table[, 1] <- self$coef
    table[, 2] <- self$se
    table[, 3] <- self$t
    table[, 4] <- self$pval
#    ans$coefficients <- table
#    ans$object <- object
    
    if (length(k)) {
      print("Estimates and significance testing of the effect of target variables")
      res <- as.matrix(stats::printCoefmat(table, digits = digits, P.values = TRUE, has.Pvalue = TRUE))
      cat("\n")
    } 
    else {
      cat("No coefficients\n")
    }
    cat("\n")
    invisible(res)
  },
  confint = function(parm, level = 0.95, joint = FALSE){
    if (missing(parm)) {
      parm <- names(self$coef)
      }
    else if (is.numeric(parm)) {
      parm <- names(self$coef)[parm]
    }

    if (joint == FALSE) {
      a <- (1 - level)/2
      a <- c(a, 1 - a)
      pct <- format.perc(a, 3)
      fac <- stats::qnorm(a)
      ci <- array(NA_real_, dim = c(length(parm), 2L), dimnames = list(parm,
                                                                     pct))
      ci[] <- self$coef[parm] + self$se %o% fac
    }
  
    if (joint == TRUE) {
      
      a <- (1 - level) 
      ab <- c(a/2, 1 - a/2)      
      pct <- format.perc(ab, 3)
      ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
      
      if (is.null(self$boot_coef)){
        stop("Multiplier bootstrap has not yet been performed. First call bootstrap() and then try confint() again.")
      }
      
      sim <- apply(abs(self$boot_coef), 2, max)
      hatc <- stats::quantile(sim, probs = 1 - a)
      
      ci[, 1] <- self$coef[parm] - hatc * self$se
      ci[, 2] <- self$coef[parm] + hatc * self$se
     }
    return(ci)
  } # , 
  # print = function(digits = max(3L, getOption("digits") -
  #                                                 3L)) {
  # 
  #   if (length(self$coef)) {
  #     cat("Coefficients:\n")
  #     print.default(format(self$coef, digits = digits), print.gap = 2L,
  #                   quote = FALSE)
  #   }
  #   else {
  #     cat("No coefficients\n")
  #   }
  #   cat("\n")
  #   invisible(self$coef)
  # }
),
private = list(
  smpls = NULL,
  psi = NULL,
  psi_a = NULL,
  psi_b = NULL,
  all_coef = NULL,
  all_se = NULL,
  n_obs = NULL,
  n_treat = NULL,
  n_rep_boot = NULL,
  i_rep = NA,
  i_treat = NA,
  initialize_double_ml = function(data, 
                        n_folds,
                        ml_learners,
                        params,
                        dml_procedure,
                        score,
                        subgroups, 
                        se_reestimate,
                        n_rep_cross_fit,
                        param_set,
                        tune_settings, 
                        param_tuning) {
    
    checkmate::check_class(data, "DoubleMLData")
    stopifnot(is.numeric(n_folds), length(n_folds) == 1)
    # TODO add input checks for ml_learners
    stopifnot(is.character(dml_procedure), length(dml_procedure) == 1)
    stopifnot(is.logical(se_reestimate), length(se_reestimate) == 1)
    stopifnot(is.character(score), length(score) == 1)
    stopifnot(is.numeric(n_rep_cross_fit), length(n_rep_cross_fit) == 1)
    stopifnot(is.list(tune_settings))
    
    self$data <- data
    self$n_folds <- n_folds
    self$ml_learners <- ml_learners
    self$params <- params
    self$dml_procedure <- dml_procedure
    self$se_reestimate <- se_reestimate
    self$score <- score
    self$subgroups <- subgroups
    self$n_rep_cross_fit <- n_rep_cross_fit
    self$param_set <- param_set
    self$tune_settings <- tune_settings
    self$param_tuning <- param_tuning
    
    private$n_obs = data$n_obs()
    private$n_treat = data$n_treat()
    
    invisible(self)
  },
  initialize_arrays = function() {
    
    private$psi = array(NA, dim=c(private$n_obs, self$n_rep_cross_fit, private$n_treat))
    private$psi_a = array(NA, dim=c(private$n_obs, self$n_rep_cross_fit, private$n_treat))
    private$psi_b = array(NA, dim=c(private$n_obs, self$n_rep_cross_fit, private$n_treat))
    
    self$coef = array(NA, dim=c(private$n_treat))
    self$se = array(NA, dim=c(private$n_treat))
    
    private$all_coef = array(NA, dim=c(private$n_treat, self$n_rep_cross_fit))
    private$all_se = array(NA, dim=c(private$n_treat, self$n_rep_cross_fit))
    
  },
  initialize_boot_arrays = function(n_rep, n_rep_cross_fit) {
    private$n_rep_boot = n_rep
    self$boot_coef = array(NA, dim=c(private$n_treat, n_rep * n_rep_cross_fit))
  },
  initialize_lists = function(n_treat,
                              n_rep_cross_fit, 
                              n_nuisance) {
    self$params = rep(list(rep(list(vector("list", n_nuisance)), n_treat)), n_rep_cross_fit) 
    self$param_tuning = rep(list(rep(list(vector("list", n_nuisance)), n_treat)), n_rep_cross_fit) 
  },
  # Comment from python: The private properties with __ always deliver the single treatment, single (cross-fitting) sample subselection
  # The slicing is based on the two properties self._i_treat, the index of the treatment variable, and
  # self._i_rep, the index of the cross-fitting sample.
  get__smpls = function() private$smpls[[private$i_rep]],
  get__psi_a = function() private$psi_a[, private$i_rep, private$i_treat],
  set__psi_a = function(value) private$psi_a[, private$i_rep, private$i_treat] <- value,
  get__psi_b = function() private$psi_b[, private$i_rep, private$i_treat],
  set__psi_b = function(value) private$psi_b[, private$i_rep, private$i_treat] <- value,
  get__psi = function() private$psi[, private$i_rep, private$i_treat],
  set__psi = function(value) private$psi[, private$i_rep, private$i_treat] <- value,
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
    
    if (is.null(self$params) | all(lapply(self$params, length)==0)){
      params = list()
    }
    else {
      params = self$params[[private$i_rep]][[private$i_treat]]
    }
    return(params)
    },
  set__params = function(tuning_params){
    self$params[[private$i_rep]][[private$i_treat]] <- tuning_params$params
    self$param_tuning[[private$i_rep]][[private$i_treat]] <- tuning_params$tuning_result
  },
  split_samples = function() {
    dummy_task = Task$new('dummy_resampling', 'regr', self$data$data)
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
      coef <- mean(thetas, na.rm = TRUE)
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
    self$coef = apply(private$all_coef, 1, function(x) stats::median(x, na.rm = TRUE))
    self$se = sqrt(apply(private$all_se^2  - (private$all_coef - self$coef)^2, 1, 
                                           function(x) stats::median(x, na.rm = TRUE)))
    
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
        
        J = mean(private$get__psi_a()[test_index])
        boot_coefs[,i_fold] <- weights[,(ii+1):(ii+n_obs_in_fold)] %*% private$get__psi()[test_index] / (
          n_obs_in_fold * private$get__all_se() * J)
        ii = ii + n_obs_in_fold
      }
      boot_coef = rowMeans(boot_coefs)
    }
    else if (dml_procedure == "dml2") {
      
      J = mean(private$get__psi_a())
      boot_coef = weights %*% private$get__psi() / (private$n_obs * private$get__all_se() * J)
    }
    
    return(boot_coef)
  },
  var_est = function(inds=NULL) {
    psi_a = private$get__psi_a()
    psi = private$get__psi()
    if(!is.null(inds)) {
      psi_a = psi_a[inds]
      psi = psi[inds]
    }
    
    J = mean(psi_a)
    sigma2_hat = 1/private$n_obs * mean(psi^2) / (J^2)
  },
  orth_est = function(inds=NULL) {
    psi_a = private$get__psi_a()
    psi_b = private$get__psi_b()
    if(!is.null(inds)) {
      psi_a = psi_a[inds]
      psi_b = psi_b[inds]
    }
    theta = -mean(psi_b) / mean(psi_a)
  },
  compute_psi = function() {
    psi = private$get__psi_a() * private$get__all_coef() + private$get__psi_b()
    private$set__psi(psi)
    invisible(self)
  }
)
)

