
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
  t_stat = NULL, 
  pval = NULL,
  boot_coef = NULL,
  boot_t_stats = NULL,
  ml_nuisance_params = NULL,
  tuning_res = NULL,
  smpls = NULL,
  learner = NULL, 
  params = NULL,
  
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
    
    # TODO: insert check for tuned params
    
    for (i_rep in 1:self$n_rep) {
      private$i_rep = i_rep
      
      for (i_treat in 1:private$n_treat) {
        private$i_treat = i_treat

        if (private$n_treat > 1){
          self$data$set__data_model(self$data$d_cols[i_treat], self$data$use_other_treat_as_covariate)
        }
        
        # ml estimation of nuisance models and computation of psi elements
        psis = private$ml_nuisance_and_score_elements(private$get__smpls())
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
    
    self$t_stat = self$coef/self$se
    self$pval = 2 * stats::pnorm(-abs(self$t_stat))
    names(self$coef) = names(self$se) = names(self$t_stat) = names(self$pval) = self$data$d_cols

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
        
        boot_res = private$compute_bootstrap(method, n_rep)
        boot_coef = boot_res$boot_coef
        boot_t_stats = boot_res$boot_t_stats
        private$set__boot_coef(boot_coef)
        private$set__boot_t_stats(boot_t_stats)
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
                                        measure = list(measure_g = NULL, 
                                                       measure_m = NULL,
                                                       measure_r = NULL,
                                                       measure_p = NULL,
                                                       measure_mu = NULL),
                                        terminator = mlr3tuning::trm("evals", n_evals = 20), 
                                        algorithm = "grid_search",
                                        tuner = "grid_search",
                                        resolution = 5)) {
    
    if (!self$apply_cross_fitting){
      stop("Parameter tuning for no-cross-fitting case not implemented.")
    }
    
    if (tune_on_folds) {
      # TODO: initiate params for tune_on_folds
      params_rep = vector("list", self$n_rep)
      self$tuning_res = rep(list(params_rep), self$data$n_treat())
      names(self$tuning_res) = self$data$d_cols
      private$fold_specific_params = TRUE
      } else {
      self$tuning_res = vector("list", self$data$n_treat())
      names(self$tuning_res) = self$data$d_cols
    }
    
      for (i_treat in 1:private$n_treat) {
        private$i_treat = i_treat
          
        if (private$n_treat > 1){
            self$data$set__data_model(self$data$d_cols[i_treat], self$data$use_other_treat_as_covariate)
        }
          
        if (tune_on_folds) {
          for (i_rep in 1:self$n_rep) {
            private$i_rep = i_rep
            # TODO: user friendly way to pass (pre-)trained learners
            # TODO: advanced usage passing original mlr3training objects like terminator, smpl, 
            #      e.g., in seperate function (tune_mlr3)...
            param_tuning = private$ml_nuisance_tuning(private$get__smpls(),
                                                      param_set, tune_on_folds, tune_settings)
            self$tuning_res[[i_treat]][[i_rep]] = param_tuning
            
            for (nuisance_model in names(param_tuning)) {
              if(!is.null(param_tuning[[nuisance_model]][[1]])) {
                self$set__ml_nuisance_params(learner = nuisance_model,
                                             treat_var = self$data$treat_col, 
                                             params = param_tuning[[nuisance_model]]$params)
              } else {
                next
              }
            }
          }
        } else {
          private$i_rep = 1
          param_tuning = private$ml_nuisance_tuning(private$get__smpls(),
                                                     param_set, tune_on_folds, tune_settings)
          self$tuning_res[[i_treat]] = param_tuning
          
          for (nuisance_model in self$params_names()) {
            if(!is.null(param_tuning[[nuisance_model]][[1]])) {
              self$set__ml_nuisance_params(learner = nuisance_model, 
                                           treat_var = self$data$treat_col, 
                                           params = param_tuning[[nuisance_model]]$params[[1]])
            } else {
              next
            }
          }
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
    table[, 3] <- self$t_stat
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
      
      sim <- apply(abs(self$boot_t_stats), 2, max)
      hatc <- stats::quantile(sim, probs = 1 - a)
      
      ci[, 1] <- self$coef[parm] - hatc * self$se
      ci[, 2] <- self$coef[parm] + hatc * self$se
     }
    return(ci)
  }, 
  params_names = function() {
    return(names(self$params))
  }, 
  set__ml_nuisance_params = function(learner = NULL, treat_var = NULL, params) {
    valid_learner = self$params_names()
    if (!learner %in% valid_learner) {
      stop(paste("invalid nuisance learner", learner, "\n",
                 "valid nuisance learner", paste(valid_learner, collapse = " or ")))
    }
    if (!treat_var %in% self$data$d_cols) {
      stop(paste("invalid treatment variable", treat_var, "\n",
                 "valid treatment variable", paste(data$self$d_cols, collapse = " or ")))
    }
    
    if (private$fold_specific_params) {
          self$params[[learner]][[treat_var]][[private$i_rep]] = params
     } else {
           self$params[[learner]][[treat_var]] = params
    }
  }, 
  p_adjust = function(method = "romano-wolf", return_matrix = TRUE) {
    if (all(is.na(self$coef))) {
      stop("apply fit() before p_adust().")
    }
   
    if (tolower(method) %in% c("rw", "romano-wolf")) {
      if (is.null(self$boot_t_stats) | all(is.na(self$coef))){
        stop("apply fit() & bootstrap() before p_adust().")
      }
      k = self$data$n_treat()
      pinit = p_val_corrected = vector(mode = "numeric", length = k)
      
      boot_t_stats = self$boot_t_stats
      t_stat = self$t_stat
      stepdown_ind = order(abs(t_stat), decreasing = TRUE)
      ro = order(stepdown_ind)
      
      for (i_d in 1:k) {
        if (i_d == 1) {
         sim = apply(abs(boot_t_stats), 2, max)
         pinit[i_d] = pmin(1, mean(sim > abs(t_stat[stepdown_ind][i_d])))
        } else {
         sim = apply(abs(boot_t_stats[ -stepdown_ind[1:(i_d - 1)], , drop = FALSE]), 2, max )
         pinit[i_d] = pmin(1, mean(sim > abs(t_stat[stepdown_ind][i_d])))
        }
      }
      # ensure monotonicity
      for (i_d in 1:k){
        if (i_d == 1){
          p_val_corrected[i_d] = pinit[i_d] 
        } else {
          p_val_corrected[i_d] = max(pinit[i_d], p_val_corrected[i_d-1])
        }
      }
      p_val = p_val_corrected[ro]
    } else { 
       if (is.element(method, stats::p.adjust.methods)) {
        p_val = stats::p.adjust(self$pval, method = method, n = self$data$n_treat())
       } else {
        stop(paste("Invalid method", method, "argument specified in p_adjust()."))
      }
    }
    
   if (return_matrix) {
    res <- as.matrix(cbind(self$coef, p_val))
    colnames(res) <- c("Estimate.", "pval")
    return(res)
   } else {
     return(p_val)
   }
    
  }
  # TODO: implement print() method for DoubleML objects
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
  fold_specific_params = NULL,
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
    
    # initialize learners and parameters which are set model specific
    self$learner = NULL
    self$params = NULL
    
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
    
    # Set fold_specific_params = FALSE at instantiation
    private$fold_specific_params = FALSE
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
    self$boot_t_stats = array(NA, dim=c(private$n_treat, n_rep * self$n_rep))
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
  get__boot_t_stats = function() {
    ind_start = (private$i_rep-1) * private$n_rep_boot + 1
    ind_end = private$i_rep * private$n_rep_boot
    self$boot_t_stats[private$i_treat, ind_start:ind_end]
    },
  set__boot_coef = function(value) {
    ind_start = (private$i_rep-1) * private$n_rep_boot + 1
    ind_end = private$i_rep * private$n_rep_boot
    self$boot_coef[private$i_treat, ind_start:ind_end] <- value
    },
  set__boot_t_stats = function(value) {
    ind_start = (private$i_rep-1) * private$n_rep_boot + 1
    ind_end = private$i_rep * private$n_rep_boot
    self$boot_t_stats[private$i_treat, ind_start:ind_end] <- value
    },
  get__params = function(learner){
    if (private$fold_specific_params) {
      params = self$params[[learner]][[self$data$treat_col]][[private$i_rep]]
    } else {
      params = self$params[[learner]][[self$data$treat_col]]
    }
    return(params)
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
    self$se = sqrt(apply(private$all_se^2  + (private$all_coef - self$coef)^2, 1, 
                                           function(x) stats::median(x, na.rm = TRUE)))
    
    invisible(self)
  },
  compute_bootstrap = function(method, n_rep) {
    dml_procedure = self$dml_procedure
    smpls = private$get__smpls()
    test_ids = smpls$test_ids
    
    if (self$apply_cross_fitting) {
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
        boot_coefs <- boot_t_stats <- matrix(NA, nrow = n_rep, ncol = self$n_folds)
        ii = 0
        for (i_fold in 1:self$n_folds) {
          test_index <- test_ids[[i_fold]]
          n_obs_in_fold = length(test_index)
          
          J = mean(private$get__psi_a()[test_index])
          boot_coefs[,i_fold] <- weights[,(ii+1):(ii+n_obs_in_fold)] %*% private$get__psi()[test_index] / (
            n_obs_in_fold * J)
          boot_t_stats[,i_fold] <- weights[,(ii+1):(ii+n_obs_in_fold)] %*% private$get__psi()[test_index] / (
            n_obs_in_fold * private$get__all_se() * J)
          ii = ii + n_obs_in_fold
        }
        boot_coef = rowMeans(boot_coefs)
        boot_t_stats = rowMeans(boot_t_stats)
      }
      else if (dml_procedure == "dml2") {
        J = mean(private$get__psi_a())
        boot_coef = weights %*% private$get__psi() / (n_obs * J)
        boot_t_stats = weights %*% private$get__psi() / (n_obs * private$get__all_se() * J)
      }
      
    } else {
      J =  mean(private$get__psi_a()[test_index])
      boot_coef = weights %*% private$get__psi()[test_index] / (n_obs * private$get__all_se() * J)
      boot_t_stats = weights %*% private$get__psi()[test_index] / (n_obs * J)
    }
    
    res = list(boot_coef = boot_coef, boot_t_stats = boot_t_stats)
    return(res)
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

