
DoubleML <- R6Class("DoubleML", public = list(
  data = NULL,
  n_folds = NULL,
  n_rep = NULL,
  score = NULL,
  dml_procedure = NULL,
  draw_sample_splitting = NULL,
  apply_cross_fitting = NULL,
  coef = NULL, 
  se = NULL, 
  t = NULL, 
  pval = NULL,
  boot_coef = NULL,
  ml_nuisance_params = NULL,
  param_tuning = NULL,
  smpls = NULL,
  
  initialize = function(...) {
    stop("DoubleML is an abstract class that can't be initialized.")
  },
  
  fit = function(se_reestimate = FALSE) {
    
    if (!self$apply_cross_fitting) {
      if (se_reestimate) {
        # redirect to se_reestimate = False; se_reestimate is of no relevance without cross-fitting
        se_reestimate = FALSE
      }
    }
    
    # TBD: insert check for tuned params
    
    for (i_rep in 1:self$n_rep) {
      private$i_rep = i_rep
      
      for (i_treat in 1:private$n_treat) {
        private$i_treat = i_treat
        
        if (!is.null(self$ml_nuisance_params)){
          self$set__ml_nuisance_params(nuisance_part = NULL, treat_var = NULL, self$ml_nuisance_params[[private$i_treat]][[private$i_rep]])
        }
        
        if (private$n_treat > 1){
          self$data$set__data_model(self$data$d_cols[i_treat], self$data$use_other_treat_as_covariate)
        }
        
        # ml estimation of nuisance models and computation of psi elements
        psis = private$ml_nuisance_and_score_elements(self$data,
                                                        private$get__smpls())
        private$set__psi_a(psis$psi_a)
        private$set__psi_b(psis$psi_b)
        
        # estimate the causal parameter(s)
        coef <- private$est_causal_pars()
        private$set__all_coef(coef)
        # compute psi (depends on estimated causal parameter)
        private$compute_score()
        
        # compute standard errors for causal parameter
        se <- private$se_causal_pars(se_reestimate)
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
    
    if (all(is.na(private$psi))) {
      stop("Apply fit() before bootstrap().")      
    }
    
    private$initialize_boot_arrays(n_rep)
    
    for (i_rep in 1:self$n_rep) {
      private$i_rep = i_rep
      
      for (i_treat in 1:private$n_treat) {
        private$i_treat = i_treat
        
        boot_coef = private$compute_bootstrap(method, n_rep)
        private$set__boot_coef(boot_coef)
      }
    }
    
    invisible(self)
  },
  split_samples = function() {

    dummy_task = Task$new('dummy_resampling', 'regr', self$data$data)
    
     if (self$n_folds == 1 & self$apply_cross_fitting) {
      message("apply_cross_fitting is set to FALSE. Cross-fitting is not supported for n_folds = 1.")
      self$apply_cross_fitting = FALSE
    }

    if (self$apply_cross_fitting) {
      
      dummy_resampling_scheme = rsmp("repeated_cv",
                                      folds = self$n_folds,
                                      repeats = self$n_rep)$instantiate(dummy_task)
      train_ids = lapply(1:(self$n_folds * self$n_rep),
                          function(x) dummy_resampling_scheme$train_set(x))
      test_ids = lapply(1:(self$n_folds * self$n_rep),
                         function(x) dummy_resampling_scheme$test_set(x))
      
      smpls <- lapply(1:self$n_rep, function(i_repeat) list(
                        train_ids = train_ids[((i_repeat-1)*self$n_folds + 1):(i_repeat*self$n_folds)],
                        test_ids = test_ids[((i_repeat-1)*self$n_folds + 1):(i_repeat*self$n_folds)]))
                              
    }  else {
      
      if (self$n_folds > 2) {
        stop("Estimation without cross-fitting not supported for n_folds > 2.")
      }
      
      if (self$n_folds == 2) {

        if (self$n_rep != 1) { 
          stop("Repeated sample splitting without cross-fitting not implemented.")
        }
        
        dummy_resampling_scheme = rsmp("holdout", ratio = 0.5)$instantiate(dummy_task)
        train_ids = list("train_ids" = dummy_resampling_scheme$train_set(1))
        test_ids = list("test_ids" = dummy_resampling_scheme$test_set(1))
        
        smpls = list(list(train_ids = train_ids, test_ids = test_ids))
        
      } else if (self$n_folds == 1) {
        dummy_resampling_scheme = rsmp("insample")$instantiate(dummy_task)
      
        train_ids = lapply(1:(self$n_folds * self$n_rep),
                            function(x) dummy_resampling_scheme$train_set(x))
        test_ids = lapply(1:(self$n_folds * self$n_rep),
                           function(x) dummy_resampling_scheme$test_set(x))
      
        smpls <- lapply(1:self$n_rep, function(i_repeat) list(
                          train_ids = train_ids[((i_repeat-1)*self$n_folds + 1):(i_repeat*self$n_folds)],
                          test_ids = test_ids[((i_repeat-1)*self$n_folds + 1):(i_repeat*self$n_folds)]))
      }
    }

    self$smpls <- smpls
    invisible(self)
  },
  set_samples = function(smpls) {
    self$n_rep = length(smpls)
    n_folds_each_train_smpl = vapply(smpls, function(x) length(x$train_ids), integer(1L))
    n_folds_each_test_smpl = vapply(smpls, function(x) length(x$test_ids), integer(1L))
    
    if (!all(n_folds_each_train_smpl == n_folds_each_test_smpl)) {
      stop("Number of folds for train and test samples do not match.")
    }
    
    if (!all(n_folds_each_train_smpl == n_folds_each_train_smpl[1])) {
      stop("Different number of folds for repeated cross-fitting.")
    }
    
    self$n_folds = n_folds_each_train_smpl[1]
    
     if (self$n_folds == 1 & self$apply_cross_fitting) {
      message("apply_cross_fitting is set to FALSE. Cross-fitting is not supported for n_folds = 1.")
      self$apply_cross_fitting = FALSE
    }
    
    self$smpls = smpls
    
    private$initialize_arrays()
    
    invisible(self)
  }, 
  tune = function(param_set, tune_on_folds = FALSE, tune_settings = list(
                                        n_folds_tune = 5,
                                        rsmp_tune = "cv", 
                                        measure_g = NULL, 
                                        measure_m = NULL,
                                        measure_r = NULL,
                                        measure_p = NULL,
                                        measure_mu = NULL,
                                        terminator = mlr3tuning::trm("evals", n_evals = 20), 
                                        algorithm = "grid_search",
                                        tuner = "grid_search",
                                        resolution = 5)) {
    
    if (!self$apply_cross_fitting){
      stop("Parameter tuning for no-cross-fitting case not implemented.")
    }
    
    n_obs = nrow(self$data$data_model)
    self$ml_nuisance_params = rep(list(rep(list(vector("list", private$n_nuisance)), self$n_rep)), private$n_treat) 
    names(self$ml_nuisance_params) = self$data$d_cols
    self$param_tuning = rep(list(rep(list(vector("list", private$n_nuisance)), self$n_rep)), private$n_treat) 
  
      for (i_treat in 1:private$n_treat) {
        private$i_treat = i_treat
          
        if (private$n_treat > 1){
            self$data$set__data_model(self$data$d_cols[i_treat], self$data$use_other_treat_as_covariate)
        }
          
        for (i_rep in 1:self$n_rep) {
          private$i_rep = i_rep
        
          # TBD: insert setter/getter function -> correct indices and names, repeated crossfitting & multitreatment
          #  ! important ! tuned params must exactly correspond to training samples
          # TBD: user friendly way to pass (pre-)trained parameters
          # TBD: advanced usage passing original mlr3training objects like terminator, smpl, 
          #      e.g., in seperate function (tune_mlr3)...
          # TBD: Pass through instances (requires prespecified tasks)
          # TBD: Handling different measures for classification and regression (logit???)
          
          if (tune_on_folds) {
            param_tuning = private$ml_nuisance_tuning(self$data, private$get__smpls(),
                                                   param_set, tune_on_folds, 
                                                   tune_settings)
          } else {
            
            if (private$i_rep == 1) {
              param_tuning = private$ml_nuisance_tuning(self$data, private$get__smpls(),
                                                     param_set, tune_on_folds, 
                                                     tune_settings)
            } 
          }

          # here: set__params()
          private$set__params(param_tuning, tune_on_folds)
          
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
  psi = NULL,
  psi_a = NULL,
  psi_b = NULL,
  all_coef = NULL,
  all_dml1_coef = NULL,
  all_se = NULL,
  n_obs = NULL,
  n_treat = NULL,
  n_rep_boot = NULL,
  i_rep = NA,
  i_treat = NA,
  initialize_double_ml = function(data, 
                        n_folds,
                        n_rep,
                        score,
                        dml_procedure,
                        draw_sample_splitting,
                        apply_cross_fitting) {
    
    checkmate::check_class(data, "DoubleMLData")
    stopifnot(is.numeric(n_folds), length(n_folds) == 1)
    stopifnot(is.character(dml_procedure), length(dml_procedure) == 1)
    stopifnot(is.character(score), length(score) == 1)
    stopifnot(is.numeric(n_rep), length(n_rep) == 1)

    self$data <- data
    self$n_folds <- n_folds
    self$dml_procedure <- dml_procedure
    self$score <- score
    self$n_rep <- n_rep
    
    self$draw_sample_splitting = draw_sample_splitting
    self$apply_cross_fitting = apply_cross_fitting
    
    if (self$n_folds == 1 & self$apply_cross_fitting) {
      message("apply_cross_fitting is set to FALSE. Cross-fitting is not supported for n_folds = 1.")
      self$apply_cross_fitting = FALSE
    }
    
    if (!self$apply_cross_fitting) {
      stopifnot(self$n_folds <= 2)
      
      if (self$dml_procedure == "dml2") {
         # redirect to dml1 which works out-of-the-box; dml_procedure is of no relevance without cross-fitting
        self$dml_procedure = "dml1"
      }
    }
    
    if (draw_sample_splitting) {
      self$split_samples()
    } else {
      self$smpls = NULL
    }

    private$n_obs = data$n_obs()
    private$n_treat = data$n_treat()
    
    private$initialize_arrays()
    
    invisible(self)
  },
  initialize_arrays = function() {
    
    private$psi = array(NA, dim=c(self$data$n_obs(), self$n_rep, private$n_treat))
    private$psi_a = array(NA, dim=c(self$data$n_obs(), self$n_rep, private$n_treat))
    private$psi_b = array(NA, dim=c(self$data$n_obs(), self$n_rep, private$n_treat))
    
    self$coef = array(NA, dim=c(private$n_treat))
    self$se = array(NA, dim=c(private$n_treat))
    
    private$all_coef = array(NA, dim=c(private$n_treat, self$n_rep))
    private$all_se = array(NA, dim=c(private$n_treat, self$n_rep))
    
    if (self$dml_procedure == "dml1") {
      if (self$apply_cross_fitting) {
        private$all_dml1_coef = array(NA, dim=c(private$n_treat, self$n_rep, self$n_folds))
      } else {
        private$all_dml1_coef = array(NA, dim=c(private$n_treat, self$n_rep, 1))
      }
    }
    
  },
  initialize_boot_arrays = function(n_rep) {
    private$n_rep_boot = n_rep
    self$boot_coef = array(NA, dim=c(private$n_treat, n_rep * self$n_rep))
  },
  # Comment from python: The private properties with __ always deliver the single treatment, single (cross-fitting) sample subselection
  # The slicing is based on the two properties self._i_treat, the index of the treatment variable, and
  # self._i_rep, the index of the cross-fitting sample.
  get__smpls = function() self$smpls[[private$i_rep]],
  get__psi_a = function() private$psi_a[, private$i_rep, private$i_treat],
  set__psi_a = function(value) private$psi_a[, private$i_rep, private$i_treat] <- value,
  get__psi_b = function() private$psi_b[, private$i_rep, private$i_treat],
  set__psi_b = function(value) private$psi_b[, private$i_rep, private$i_treat] <- value,
  get__psi = function() private$psi[, private$i_rep, private$i_treat],
  set__psi = function(value) private$psi[, private$i_rep, private$i_treat] <- value,
  get__all_coef = function() private$all_coef[private$i_treat, private$i_rep],
  set__all_coef = function(value) private$all_coef[private$i_treat, private$i_rep] <- value,
  set__all_dml1_coef = function(value) private$all_dml1_coef[private$i_treat, private$i_rep, ] <- value,
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
  set__params = function(tuning_params, tune_on_folds){
    
    if (tune_on_folds) {
      self$ml_nuisance_params[[private$i_treat]][[private$i_rep]] <- tuning_params$params
      self$param_tuning[[private$i_treat]][[private$i_rep]] <- tuning_params$tuning_result
    } else {
      
      export_params = lapply(1:private$n_nuisance, function(x) rep(tuning_params$params[[x]], self$n_folds))
      names(export_params) = names(tuning_params$params)
      self$ml_nuisance_params[[private$i_treat]][[private$i_rep]] <- export_params
      self$param_tuning[[private$i_treat]][[private$i_rep]] <- tuning_params$tuning_result
      
      # self$set__ml_nuisance_params(tuning_params$params)
      # self$ml_nuisance_params[[private$i_rep]][[private$i_treat]] = NULL
      #       self$param_tuning[[private$i_rep]][[private$i_treat]] <- tuning_params$tuning_result

      
      }
  },
  est_causal_pars = function() {
    dml_procedure = self$dml_procedure
    n_folds = self$n_folds
    smpls = private$get__smpls()
    test_ids = smpls$test_ids
    
    if (dml_procedure == "dml1") {
      # Note that length(test_ids) is only not equal to self.n_folds if self$apply_cross_fitting ==False
      thetas <- rep(NA, length(test_ids))
      for (i_fold in 1:length(test_ids)) {
        test_index <- test_ids[[i_fold]]
        thetas[i_fold] <- private$orth_est(inds=test_index)
      }
      coef <- mean(thetas, na.rm = TRUE)
      private$set__all_dml1_coef(thetas)
    }
    
    else if (dml_procedure == "dml2") {
      coef <- private$orth_est()
    }
    
    return(coef)
  },
  se_causal_pars = function(se_reestimate) {
    dml_procedure = self$dml_procedure
    n_folds = self$n_folds
    smpls = private$get__smpls()
    test_ids = smpls$test_ids
    
    if (dml_procedure == "dml1") {
      vars <-  rep(NA, length(test_ids))
      if (se_reestimate == FALSE) {
        for (i_fold in 1:length(test_ids)) {
          # Note that length(test_ids) is only not equal to self.n_folds if self$apply_cross_fitting == False
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
    
    if (apply_cross_fitting) {
      n_obs = private$n_obs
    } else {
      test_index = test_ids[[1]]
      n_obs = length(test_index)
    }
    
    if (method == "Bayes") {
      weights <- stats::rexp(n_rep * n_obs, rate = 1) - 1
    } else if (method == "normal") {
      weights <- stats::rnorm(n_rep * n_obs)
    } else if (method == "wild") {
      weights <- stats::rnorm(n_rep * n_obs)/sqrt(2) + (stats::rnorm(n_rep * n_obs)^2 - 1)/2
    }
    
    # for alignment with the functional (loop-wise) implementation we fill by row
    weights <- matrix(weights, nrow = n_rep, ncol = n_obs, byrow=TRUE)
    
    if (self$apply_cross_fitting) {
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
        boot_coef = weights %*% private$get__psi() / (n_obs * private$get__all_se() * J)
      }
      
    } else {
      J =  mean(private$get__psi_a()[test_index])
       boot_coef = weights %*% private$get__psi()[test_index] / (n_obs * private$get__all_se() * J)
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
    if (self$apply_cross_fitting) {
      n_obs = private$n_obs
    } else {
      smpls = private$get__smpls()
      test_index = smpls$test_ids[[1]]
      n_obs = length(test_index)
    }
    J = mean(psi_a)
    sigma2_hat = 1/n_obs * mean(psi^2) / (J^2)
    return(sigma2_hat)
  },
  orth_est = function(inds=NULL) {
    psi_a = private$get__psi_a()
    psi_b = private$get__psi_b()
    if(!is.null(inds)) {
      psi_a = psi_a[inds]
      psi_b = psi_b[inds]
    }
    theta = -mean(psi_b) / mean(psi_a)
    return(theta)
  },
  compute_score = function() {
    psi = private$get__psi_a() * private$get__all_coef() + private$get__psi_b()
    private$set__psi(psi)
    invisible(self)
  }
)
)

