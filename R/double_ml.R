#' @title Abstract class DoubleML
#' 
#' @description
#' Abstract base class that can't be initialized. 
#' 
#' 
#' @format [R6::R6Class] object.
#' 
#' @family DoubleML
DoubleML = R6Class("DoubleML", public = list(
  #' @field all_coef (`matrix()`) \cr 
  #' Estimates of the causal parameter(s) for the `n_rep` different sample splits after calling `fit()`. 
  all_coef = NULL, 
  
  #' @field all_dml1_coef (`array()`) \cr
  #' Estimates of the causal parameter(s) for the `n_rep` different sample splits after calling `fit()` with `dml_procedure = "dml1"`.  
  all_dml1_coef = NULL, 
  
  #' @field all_se (`matrix()`) \cr 
  #' Standard errors of the causal parameter(s) for the `n_rep` different sample splits after calling `fit()`. 
  all_se = NULL, 
  
  #' @field apply_cross_fitting (`logical(1)`) \cr
  #' Indicates whether cross-fitting should be applied. Default is `TRUE`.
  apply_cross_fitting = NULL,
  
  #' @field boot_coef (`matrix()`) \cr
  #' Bootstrapped coefficients for the causal parameter(s) after calling `fit()` and `bootstrap()`. 
  boot_coef = NULL, 
  
  #' @field boot_t_stat (`matrix()`) \cr
  #' Bootstrapped t-statistics for the causal parameter(s) after calling `fit()` and `bootstrap()`. 
  boot_t_stat = NULL, 
  
  #' @field coef (`numeric()`) \cr
  #' Estimates for the causal parameter(s) after calling `fit()`. 
  coef = NULL, 
  
  #' @field data ([`data.table`][data.table::data.table()])\cr 
  #' Data object.
  data = NULL, 
  
  #' @field dml_procedure (`character(1)`) \cr
  #' A `character()` (`"dml1"` or `"dml2"`) specifying the double machine learning algorithm. Default is `"dml2"`. 
  dml_procedure = NULL, 
  
  #' @field draw_sample_splitting (`logical(1)`) \cr
  #' Indicates whether the sample splitting should be drawn during initialization of the object. Default is `TRUE`.
  draw_sample_splitting = NULL, 
  
  #' @field learner (named `list()`) \cr
  #' The machine learners for the nuisance functions. 
  learner = NULL, 
  
  #' @field n_folds (`integer(1)`) \cr
  #' Number of folds. Default is `5`. 
  n_folds = NULL,
  
  #' @field n_rep (`integer(1)`) \cr
  #' Number of repetitions for the sample splitting. Default is `1`. 
  n_rep = NULL, 
  
  #' @field params (named `list()`) \cr
  #' The hyperparameters of the learners. 
  params = NULL, 
  
  #' @field psi (`array()`) \cr
  #' Value of the score function
  #' \eqn{\psi(W;\theta, \eta)=\psi_a(W;\eta) \theta + \psi_b (W; \eta)} after calling `fit()`. 
  psi = NULL, 
  
  #' @field psi_a (`array()`) \cr
  #' Value of the score function component \eqn{\psi_a(W;\eta)} after calling `fit()`. 
  psi_a = NULL,
  
  #' @field psi_b (`array()`) \cr
  #' Value of the score function component \eqn{\psi_b(W;\eta)} after calling `fit()`. 
  psi_b = NULL, 
  
  #' @field pval (`numeric()`) \cr
  #' p-values for the causal parameter(s) after calling `fit()`. 
  pval = NULL, 
  
  #' @field score (`character(1)`, `function()`) \cr
  #' A `character(1)` or `function()` specifying the score function. 
  score = NULL, 
  
  #' @field se (`numeric()`) \cr
  #' Standard errors for the causal parameter(s) after calling `fit()`.
  se = NULL, 
  
  #' @field smpls (`list()`) \cr
  #' The partition used for cross-fitting. 
  smpls = NULL, 
  
  #' @field t_stat (`numeric()`) \cr
  #' t-statistics for the causal parameter(s) after calling `fit()`. 
  t_stat = NULL, 
  
  #' @field tuning_res (named `list()`) \cr
  #' Results from hyperparameter tuning. 
  tuning_res = NULL, 

  #' @description 
  #' DoubleML is an abstract class that can't be initialized. 
  initialize = function() {
    stop("DoubleML is an abstract class that can't be initialized.")
  },
  #' @description 
  #' Print DoubleML objects. 
  print = function() {
    class_name = class(self)[1]
    header = paste0("================= ", class_name, " Object ==================\n")
    data_info = paste0("Outcome variable: ", self$data$y_col, "\n", 
                    "Treatment variable(s): ", paste0(self$data$d_cols, collapse = ", "), "\n", 
                    "Covariates: ",  paste0(self$data$x_cols, collapse = ", "), "\n", 
                    "Instrument(s): ", paste0(self$data$z_cols, collapse = ", "), "\n", 
                    "No. Observations: ", self$data$n_obs, "\n")
    
    if (is.character(self$score)) {
      score_info = paste0("Score function: ", self$score, "\n", 
                         "DML algorithm: ", self$dml_procedure, "\n")
    } else if (is.function(self$score)) {
      score_info =  paste0("Score function: User specified score function \n", 
                           "DML algorithm: ", self$dml_procedure, "\n")
    }
    learner_info = character(length(self$learner))
    for (i_lrn in 1:length(self$learner)) {
      if (any(class(self$learner[[i_lrn]]) == "Learner")) {
        learner_info[i_lrn] = paste0(self$learner_names()[[i_lrn]], ": ", self$learner[[i_lrn]]$id, "\n")
      } else {
        learner_info[i_lrn] = paste0(self$learner_names()[[i_lrn]], ": ", self$learner[i_lrn], "\n")
      }
    }
    resampling_info = paste0("No. folds: ", self$n_folds, "\n", 
                          "No. repeated sample splits: ", self$n_rep, "\n", 
                          "Apply cross-fitting: ", self$apply_cross_fitting, "\n")
    res = cat(header, "\n", 
              "\n------------------ Data summary      ------------------\n", data_info, 
              "\n------------------ Score & algorithm ------------------\n",  score_info, 
              "\n------------------ Machine learner   ------------------\n", learner_info, 
              "\n------------------ Resampling        ------------------\n", resampling_info, 
              "\n------------------ Fit summary       ------------------\n ", sep="")
    self$summary()
    
    return(res)
  },
  
  #' @description 
  #' Estimate DoubleML models. 
  #' 
  #' @return self
  fit = function() {
    
    # TODO: insert check for tuned params
    for (i_rep in 1:self$n_rep) {
      private$i_rep = i_rep
      
      for (i_treat in 1:self$data$n_treat) {
        private$i_treat = i_treat

        if (self$data$n_treat > 1){
          self$data$set_data_model(self$data$d_cols[i_treat])
        }
        
        # ml estimation of nuisance models and computation of psi elements
        psis = private$ml_nuisance_and_score_elements(private$get__smpls())
        private$set__psi_a(psis$psi_a)
        private$set__psi_b(psis$psi_b)
        
        # estimate the causal parameter
        coef = private$est_causal_pars()
        private$set__all_coef(coef)
        # compute score (depends on estimated causal parameter)
        private$compute_score()
        
        # compute standard errors for causal parameter
        se = private$se_causal_pars()
        private$set__all_se(se)
      }
    }
    
    private$agg_cross_fit()
    
    self$t_stat = self$coef/self$se
    self$pval = 2 * stats::pnorm(-abs(self$t_stat))
    names(self$coef) = names(self$se) = names(self$t_stat) = names(self$pval) = self$data$d_cols
    
    invisible(self)
  },
  
  #' @description 
  #' Multiplier bootstrap for DoubleML models. 
  #' 
  #' @param method (`character(1)`) \cr
  #' A `character(1)` (`"Bayes"`, `"normal"` or `"wild"`) specifying the multiplier bootstrap method. 
  #' 
  #' @param n_rep_boot (`integer(1)`) \cr
  #' The number of bootstrap replications. 
  #' 
  #' @return self
  bootstrap = function(method='normal', n_rep_boot = 500) {
    
    if (all(is.na(self$psi))) {
      stop("Apply fit() before bootstrap().")      
    }
    checkmate::assert_choice(method, c("normal", "Bayes", "wild"))
    checkmate::assert_count(n_rep_boot, positive = TRUE)
    
    private$initialize_boot_arrays(n_rep_boot)

    for (i_rep in 1:self$n_rep) {
      private$i_rep = i_rep
      
      if (self$apply_cross_fitting) {
        n_obs = self$data$n_obs
      } else {
        smpls = private$get__smpls()
        test_ids = smpls$test_ids
        test_index = test_ids[[1]]
        n_obs = length(test_index)
      }
      weights = draw_weights(method, n_rep_boot, n_obs)  
    
      for (i_treat in 1:self$data$n_treat) {
        private$i_treat = i_treat
  
        boot_res = private$compute_bootstrap(weights, n_rep_boot)
        boot_coef = boot_res$boot_coef
        boot_t_stat = boot_res$boot_t_stat
        private$set__boot_coef(boot_coef)
        private$set__boot_t_stat(boot_t_stat)
      }
    }
    invisible(self)
  },
  #' @description 
  #' Draw sample splitting for DoubleML models. 
  #' 
  #' The samples are drawn according to the attributes `n_folds`, `n_rep` and `apply_cross_fitting`. 
  #' 
  #' @return self
  split_samples = function() {

    dummy_task = mlr3::Task$new('dummy_resampling', 'regr', self$data$data)
    
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
      
      smpls = lapply(1:self$n_rep, function(i_repeat) list(
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
      
        smpls = lapply(1:self$n_rep, function(i_repeat) list(
                          train_ids = train_ids[((i_repeat-1)*self$n_folds + 1):(i_repeat*self$n_folds)],
                          test_ids = test_ids[((i_repeat-1)*self$n_folds + 1):(i_repeat*self$n_folds)]))
      }
    }
    self$smpls = smpls
    invisible(self)
  },
  #' @description
  #' Set the sample splitting for DoubleML models. 
  #' 
  #' The attributes `n_folds` and `n_rep` are derived from the provided partition. 
  #' 
  #' @param smpls (`list()`) \cr
  #' A nested `list()`. The outer lists needs to provide an entry per repeated sample splitting (length of the list is set as `n_rep`). The inner list is a named `list()` with names `train_ids` and `test_ids`. The entries in `train_ids` and `test_ids` must be partitions per fold (length of `train_ids` and `test_ids` is set as `n_folds`).
  #' 
  #' @return self
  set_sample_splitting = function(smpls) {
    checkmate::assert_list(smpls)
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
  #' @description 
  #' Hyperparameter-tuning for DoubleML models. 
  #' 
  #' The hyperparameter-tuning is performed using the tuning methods provided in the [mlr3tuning](https://mlr3tuning.mlr-org.com/) package. For more information on tuning in [mlr3](https://mlr3.mlr-org.com/), we refer to the section on parameter tuning in the [mlr3 book](https://mlr3book.mlr-org.com/tuning.html).
  #' 
  #' @param param_set (named `list()`) \cr
  #' A named `list` with a parameter grid for each nuisance model/learner (see method `learner_names()`). The parameter grid must be an object of class [ParamSet][paradox::ParamSet].   
  #'
  #' @param tune_settings (named `list()`) \cr
  #' A named `list()` with argument passed to the hyperparameter-tuning with [mlr3tuning](https://mlr3tuning.mlr-org.com/) to set up [TuningInstance][mlr3tuning::TuningInstanceSingleCrit] objects. `tune_settings` has entries 
  #' * `rsmp_tune` \cr [Resampling][mlr3::Resampling] or option passed to [rsmpl()][mlr3::mlr_sugar] to initialize a [Resampling][mlr3::Resampling] for parameter tuning in `mlr3`. Default is `"cv"` (cross-validation). 
  #' * `n_folds_tune` (`integer(1)`) \cr If `rsmp_tune = "cv"`, number of folds used for cross-validation. Default is `5`. 
  #' * `measure` (`NULL`, named `list()`) \cr `NULL` or named `list()` with options passed to [msr()][mlr3::msr()]. Names of entries are set to names of learners (see method `learner_names()`). If `NULL`, default measures as provided by [default_measures()][mlr3::default_measures()] are used. Default is `NULL`. 
  #' * `terminator` \cr A [Terminator][bbotk::Terminator] object. Default is `mlr3tuning::trm("evals", n_evals = 20)`.
  #' * `algorithm` (`character(1)`) \cr The key passed to the respective dictionary to specify the tuning algorithm used in [tnr()][mlr3tuning::tnr()]. `algorithm` is passed as an argument to [tnr()][mlr3tuning::tnr()]. Default is `grid_search`. 
  #' * `resolution` (`character(1)`) \cr The key passed to the respective dictionary to specify  the tuning algorithm used in [tnr()][mlr3tuning::tnr()]. `resolution` is passed as an argument to [tnr()][mlr3tuning::tnr()]. Default is `5`. 
  #' 
  #' @param tune_on_folds (`logical(1)`) \cr
  #' Indicates whether the tuning should be done fold-specific or globally. Default is `FALSE`. 
  #' 
  #' 
  #' @return self
  tune = function(param_set, tune_settings = list(
                                        n_folds_tune = 5,
                                        rsmp_tune = "cv", 
                                        measure = list(ml_g = NULL, 
                                                       ml_m = NULL,
                                                       ml_r = NULL),
                                        terminator = mlr3tuning::trm("evals", n_evals = 20), 
                                        algorithm = "grid_search",
                                        resolution = 5), 
                             tune_on_folds = FALSE) {
    checkmate::assert_list(param_set)
    valid_learner = self$learner_names()
    if (! (all(names(param_set) %in% valid_learner))) {
      stop(paste("invalid param_set", paste0(names(param_set), collapse = ", "),
                 "\n param_grids must be a named list with elements named", 
                  paste0(valid_learner, collapse = ", ")))
    }
    for (i_grid in seq_len(length(param_set))){
      checkmate::assert_class(param_set[[i_grid]], "ParamSet")
    }
    
    required_settings = c("n_folds_tune", "rsmp_tune", "measure", "terminator", "algorithm", "resolution")
    if (! all(required_settings %in% names(tune_settings))) {
      missing_setting = required_settings[which(! (required_settings %in% names(tune_settings)))]
      stop(paste("Invalid tune_settings\n", 
                  paste0(missing_setting, collapse = ", "), "is missing.\n",
                  "Tune settngs require specification of", toString(required_settings), "."))
    }
    
    checkmate::assert_count(tune_settings$n_folds_tune, positive = TRUE)
    checkmate::assert(checkmate::check_character(tune_settings$rsmp_tune),
                      checkmate::check_class(tune_settings$rsmp_tune, "Resampling"))
    checkmate::assert_list(tune_settings$measure)
    checkmate::assert(checkmate::check_character(tune_settings$terminator),
                      checkmate::check_class(tune_settings$terminator, "Terminator"))
    checkmate::assert_character(tune_settings$algorithm, len = 1)
    checkmate::assert_count(tune_settings$resolution, positive = TRUE)
  
    checkmate::assert_logical(tune_on_folds, len = 1)
    
    if (!self$apply_cross_fitting){
      stop("Parameter tuning for no-cross-fitting case not implemented.")
    }
    
    if (tune_on_folds) {
      params_rep = vector("list", self$n_rep)
      self$tuning_res = rep(list(params_rep), self$data$n_treat)
      names(self$tuning_res) = self$data$d_cols
      private$fold_specific_params = TRUE
      } else {
      self$tuning_res = vector("list", self$data$n_treat)
      names(self$tuning_res) = self$data$d_cols
    }
    
      for (i_treat in 1:self$data$n_treat) {
        private$i_treat = i_treat
          
        if (self$data$n_treat > 1){
            self$data$set_data_model(self$data$d_cols[i_treat])
        }
          
        if (tune_on_folds) {
          for (i_rep in 1:self$n_rep) {
            private$i_rep = i_rep
            # TODO: advanced usage passing original mlr3training objects like terminator, smpl, 
            #      e.g., in seperate function (tune_mlr3)...
            param_tuning = private$ml_nuisance_tuning(private$get__smpls(),
                                                      param_set, tune_settings,  tune_on_folds)
            self$tuning_res[[i_treat]][[i_rep]] = param_tuning
            
            for (nuisance_model in names(param_tuning)) {
              if(!is.null(param_tuning[[nuisance_model]][[1]])) {
                self$set_ml_nuisance_params(learner = nuisance_model,
                                             treat_var = self$data$treat_col, 
                                             params = param_tuning[[nuisance_model]]$params, 
                                             set_fold_specific = FALSE)
              } else {
                next
              }
            }
          }
        } else {
          private$i_rep = 1
          param_tuning = private$ml_nuisance_tuning(private$get__smpls(),
                                                     param_set, tune_settings,  tune_on_folds)
          self$tuning_res[[i_treat]] = param_tuning
          
          for (nuisance_model in self$params_names()) {
            if(!is.null(param_tuning[[nuisance_model]][[1]])) {
              self$set_ml_nuisance_params(learner = nuisance_model, 
                                           treat_var = self$data$treat_col, 
                                           params = param_tuning[[nuisance_model]]$params[[1]], 
                                           set_fold_specific = FALSE)
            } else {
              next
            }
          }
        }
    }
  invisible(self)
  },
  #' @description 
  #' Summary for DoubleML models after calling `fit()`. 
  #' 
  #' @param digits (`integer(1)`) \cr
  #' The number of significant digits to use when printing. 
  summary = function(digits = max(3L, getOption("digits") - 
                                                          3L)) {
    
    if (all(is.na(self$coef))) {
      print("fit() not yet called.")
    } else {
      ans = NULL
      k = length(self$coef)
      table = matrix(NA, ncol = 4, nrow = k)
      rownames(table) = names(self$coef)
      colnames(table) = c("Estimate.", "Std. Error", "t value", "Pr(>|t|)")
      table[, 1] = self$coef
      table[, 2] = self$se
      table[, 3] = self$t_stat
      table[, 4] = self$pval
  #    ans$coefficients = table
  #    ans$object = object
      private$summary_table = table
      
      if (length(k)) {
        print("Estimates and significance testing of the effect of target variables")
        res = as.matrix(stats::printCoefmat(private$summary_table, digits = digits, P.values = TRUE, has.Pvalue = TRUE))
        cat("\n")
      } 
      else {
        cat("No coefficients\n")
      }
      cat("\n")
      invisible(res)
    }
  },
  #' @description 
  #' Confidence intervals for DoubleML models. 
  #' 
  #' @param joint (`logical(1)`) \cr
  #' Indicates whether joint confidence intervals are computed. Default is `FALSE`. 
  #'  
  #' @param level (`numeric(1)`) \cr
  #' The confidence level. Default is `0.95`.
  #' 
  #' @param parm (`numeric()`) \cr
  #' A specification of which parameters are to be given confidence intervals among the variables for which inference was done, either a vector of numbers or a vector of names. If missing, all parameters are considered (default).
  #' @return A `matrix()` with the confidence interval(s). 
  confint = function(parm, joint = FALSE, level = 0.95){
    if (missing(parm)) {
      parm = names(self$coef)
      }
    else if (is.numeric(parm)) {
      parm = names(self$coef)[parm]
    }

    if (joint == FALSE) {
      a = (1 - level)/2
      a = c(a, 1 - a)
      pct = format.perc(a, 3)
      fac = stats::qnorm(a)
      ci = array(NA_real_, dim = c(length(parm), 2L), dimnames = list(parm,
                                                                     pct))
      ci[] = self$coef[parm] + self$se %o% fac
    }
  
    if (joint == TRUE) {
      
      a = (1 - level) 
      ab = c(a/2, 1 - a/2)      
      pct = format.perc(ab, 3)
      ci = array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
      
      if (is.null(self$boot_coef)){
        stop("Multiplier bootstrap has not yet been performed. First call bootstrap() and then try confint() again.")
      }
      
      sim = apply(abs(self$boot_t_stat), 2, max)
      hatc = stats::quantile(sim, probs = 1 - a)
      
      ci[, 1] = self$coef[parm] - hatc * self$se
      ci[, 2] = self$coef[parm] + hatc * self$se
     }
    return(ci)
  }, 
  #' @description 
  #' Returns the names of the learners. 
  #' 
  #' @return `character()` with names of learners. 
  learner_names = function() {
    return(names(self$learner))
  },
  #' @description 
  #' Returns the names of the nuisance models with hyperparameters. 
  #' 
  #' @return `character()` with names of nuisance models with hyperparameters. 
  params_names = function() {
    return(names(self$params))
  }, 
  #' @description 
  #' Set hyperparameters for the nuisance models of DoubleML models. 
  #' 
  #' Note that in the current implementation, either all parameters have to be set globally or all parameters have to be provided fold-specific. 
  #' 
  #' @param learner (`character(1)`) \cr
  #' The nuisance model/learner (see method `params_names`)
  #' 
  #' @param treat_var (`character(1)`) \cr
  #' The treatment varaible (hyperparameters can be set treatment-variable specific).
  #' 
  #' @param params (named `list()`) \cr
  #' A named `list()` with estimator parameters. Parameters are used for all folds by default. Alternatively, parameters can be passed in a fold-specific way if option  `fold_specific`is `TRUE`. In this case, the outer list needs to be of length `n_rep` and the inner list of length `n_folds`.
  #' 
  #' @param set_fold_specific (`logical(1)`) \cr
  #' Indicates if the parameters passed in `params` should be passed in fold-specific way. Default is `FALSE`. If `TRUE`, the outer list needs to be of length `n_rep` and the inner list of length `n_folds`. Note that in the current implementation, either all parameters have to be set globally or all parameters have to be provided fold-specific. 
  #'
  #' @return self
  set_ml_nuisance_params = function(learner = NULL, treat_var = NULL, params, set_fold_specific = FALSE) {
    valid_learner = self$params_names()
    checkmate::assert_character(learner, len = 1)
    checkmate::assert_choice(learner, valid_learner) 
  
    checkmate::assert_choice(treat_var, self$data$d_cols)
    checkmate::assert_list(params)
    checkmate::assert_logical(set_fold_specific, len = 1)
    
    if (!set_fold_specific) {
      if (private$fold_specific_params) {
            self$params[[learner]][[treat_var]][[private$i_rep]] = params
       } else {
             self$params[[learner]][[treat_var]] = params
       }
    } else {
      if (length(params) != self$n_rep) {
        stop("Length of (outer) parameter list does not match n_rep.")
      } 
      if (!all(lapply(params, length) == self$n_folds)) {
        stop("Length of (inner) parameter list does not match n_folds.")
      } 
      
      private$fold_specific_params = set_fold_specific
      self$params[[learner]][[treat_var]] = params
    }
  }, 
  #' @description 
  #' Multiple testing adjustment for DoubleML models. 
  #' 
  #' @param method (`character(1)`) \cr
  #' A `character(1)`(`"romano-wolf"`, `"bonferroni"`, `"holm"`, etc) specifying the adjustment method. In addition to `"romano-wolf"`, all methods implemented in [p.adjust()][stats::p.adjust()] can be applied. Default is `"romano-wolf"`. 
  #' @param return_matrix (`logical(1)`) \cr
  #' Indicates if the output is returned as a matrix with corresponding coefficient names. 
  #' 
  #' @return `numeric()` with adjusted p-values. If `return_matrix = TRUE`, a `matrix()` with adjusted p_values.  
  p_adjust = function(method = "romano-wolf", return_matrix = TRUE) {
    if (all(is.na(self$coef))) {
      stop("apply fit() before p_adust().")
    }
   
    if (tolower(method) %in% c("rw", "romano-wolf")) {
      if (is.null(self$boot_t_stat) | all(is.na(self$coef))){
        stop("apply fit() & bootstrap() before p_adust().")
      }
      k = self$data$n_treat
      pinit = p_val_corrected = vector(mode = "numeric", length = k)
      
      boot_t_stat = self$boot_t_stat
      t_stat = self$t_stat
      stepdown_ind = order(abs(t_stat), decreasing = TRUE)
      ro = order(stepdown_ind)
      
      for (i_d in 1:k) {
        if (i_d == 1) {
         sim = apply(abs(boot_t_stat), 2, max)
         pinit[i_d] = pmin(1, mean(sim > abs(t_stat[stepdown_ind][i_d])))
        } else {
         sim = apply(abs(boot_t_stat[ -stepdown_ind[1:(i_d - 1)], , drop = FALSE]), 2, max )
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
        p_val = stats::p.adjust(self$pval, method = method, n = self$data$n_treat)
       } else {
        stop(paste("Invalid method", method, "argument specified in p_adjust()."))
      }
    }
    
   if (return_matrix) {
    res = as.matrix(cbind(self$coef, p_val))
    colnames(res) = c("Estimate.", "pval")
    return(res)
   } else {
     return(p_val)
   }
  },
  #' @description 
  #' Get hyperparameters for the nuisance model of DoubleML models. 
  #' 
  #' @param learner (`character(1)`) \cr
  #' The nuisance model/learner (see method `params_names()`)
  #' 
  #' @return named `list()`with paramers for the nuisance model/learner.
  get_params = function(learner){
    valid_learner = self$params_names()
    checkmate::assert_character(learner, len = 1)
    checkmate::assert_choice(learner, valid_learner) 
    
    if (private$fold_specific_params) {
      params = self$params[[learner]][[self$data$treat_col]][[private$i_rep]]
    } else {
      params = self$params[[learner]][[self$data$treat_col]]
    }
    return(params)
    }
),
private = list(
  n_rep_boot = NULL,
  i_rep = NA,
  i_treat = NA,
  fold_specific_params = NULL,
  summary_table = NULL,
  initialize_double_ml = function(data, 
                        n_folds,
                        n_rep,
                        score,
                        dml_procedure,
                        draw_sample_splitting,
                        apply_cross_fitting) {
    # check and pick up obj_dml_data
    checkmate::assert_class(data, "DoubleMLData")
    private$check_data(data)
    self$data = data
    
    # initialize learners and parameters which are set model specific
    self$learner = NULL
    self$params = NULL
    # Set fold_specific_params = FALSE at instantiation
    private$fold_specific_params = FALSE
    
    # check resampling specifications
    checkmate::assert_count(n_folds)
    checkmate::assert_count(n_rep)
    checkmate::assert_logical(apply_cross_fitting, len = 1)
    checkmate::assert_logical(draw_sample_splitting, len = 1)
    
    # set resampling specifications
    self$n_folds = n_folds
    self$n_rep = n_rep
    self$apply_cross_fitting = apply_cross_fitting
    self$draw_sample_splitting = draw_sample_splitting

    # check and set dml_procedure and score
    checkmate::assert_choice(dml_procedure, c("dml1", "dml2"))
    self$dml_procedure = dml_procedure
    self$score = private$check_score(score)
  
    if (self$n_folds == 1 & self$apply_cross_fitting) {
      message("apply_cross_fitting is set to FALSE. Cross-fitting is not supported for n_folds = 1.")
      self$apply_cross_fitting = FALSE
    }
    
    if (!self$apply_cross_fitting) {
      if(self$n_folds > 2) {
        stop("Estimation without cross-fitting not supported for n_folds > 2.")
      }
      if (self$dml_procedure == "dml2") {
         # redirect to dml1 which works out-of-the-box; dml_procedure is of no relevance without cross-fitting
        self$dml_procedure = "dml1"
      }
    }
    
    # perform sample splitting
    if (self$draw_sample_splitting) {
      self$split_samples()
    } else {
      self$smpls = NULL
    }
    
    # initialize arrays according to obj_dml_data and the resampling settings
    private$initialize_arrays()
    
    # also initialize bootstrap arrays with the default number of bootstrap replications
    private$initialize_boot_arrays(n_rep_boot = 500)
    
     # initialize instance attributes which are later used for iterating
    invisible(self)
  },
  initialize_arrays = function() {
    
    self$psi = array(NA, dim=c(self$data$n_obs, self$n_rep, self$data$n_treat))
    self$psi_a = array(NA, dim=c(self$data$n_obs, self$n_rep, self$data$n_treat))
    self$psi_b = array(NA, dim=c(self$data$n_obs, self$n_rep, self$data$n_treat))
    
    self$coef = array(NA, dim=c(self$data$n_treat))
    self$se = array(NA, dim=c(self$data$n_treat))
    
    self$all_coef = array(NA, dim=c(self$data$n_treat, self$n_rep))
    self$all_se = array(NA, dim=c(self$data$n_treat, self$n_rep))
    
    if (self$dml_procedure == "dml1") {
      if (self$apply_cross_fitting) {
        self$all_dml1_coef = array(NA, dim=c(self$data$n_treat, self$n_rep, self$n_folds))
      } else {
        self$all_dml1_coef = array(NA, dim=c(self$data$n_treat, self$n_rep, 1))
      }
    }
    
  },
  initialize_boot_arrays = function(n_rep_boot) {
    private$n_rep_boot = n_rep_boot
    self$boot_coef = array(NA, dim=c(self$data$n_treat, n_rep_boot * self$n_rep))
    self$boot_t_stat = array(NA, dim=c(self$data$n_treat, n_rep_boot * self$n_rep))
  },
  # Comment from python: The private properties with __ always deliver the single treatment, single (cross-fitting) sample subselection
  # The slicing is based on the two properties self._i_treat, the index of the treatment variable, and
  # self._i_rep, the index of the cross-fitting sample.
  get__smpls = function() self$smpls[[private$i_rep]],
  get__psi_a = function() self$psi_a[, private$i_rep, private$i_treat],
  set__psi_a = function(value) self$psi_a[, private$i_rep, private$i_treat] = value,
  get__psi_b = function() self$psi_b[, private$i_rep, private$i_treat],
  set__psi_b = function(value) self$psi_b[, private$i_rep, private$i_treat] = value,
  get__psi = function() self$psi[, private$i_rep, private$i_treat],
  set__psi = function(value) self$psi[, private$i_rep, private$i_treat] = value,
  get__all_coef = function() self$all_coef[private$i_treat, private$i_rep],
  set__all_dml1_coef = function(value) self$all_dml1_coef[private$i_treat, private$i_rep, ] = value,
  set__all_coef = function(value) self$all_coef[private$i_treat, private$i_rep] = value,
  get__all_se = function() self$all_se[private$i_treat, private$i_rep],
  set__all_se = function(value) self$all_se[private$i_treat, private$i_rep] = value,
  get__boot_coef = function() {
    ind_start = (private$i_rep-1) * private$n_rep_boot + 1
    ind_end = private$i_rep * private$n_rep_boot
    self$boot_coef[private$i_treat, ind_start:ind_end]
    },
  get__boot_t_stat = function() {
    ind_start = (private$i_rep-1) * private$n_rep_boot + 1
    ind_end = private$i_rep * private$n_rep_boot
    self$boot_t_stat[private$i_treat, ind_start:ind_end]
    },
  set__boot_coef = function(value) {
    ind_start = (private$i_rep-1) * private$n_rep_boot + 1
    ind_end = private$i_rep * private$n_rep_boot
    self$boot_coef[private$i_treat, ind_start:ind_end] = value
    },
  set__boot_t_stat = function(value) {
    ind_start = (private$i_rep-1) * private$n_rep_boot + 1
    ind_end = private$i_rep * private$n_rep_boot
    self$boot_t_stat[private$i_treat, ind_start:ind_end] = value
    },
  est_causal_pars = function() {
    dml_procedure = self$dml_procedure
    n_folds = self$n_folds
    smpls = private$get__smpls()
    test_ids = smpls$test_ids
    
    if (dml_procedure == "dml1") {
      # Note that length(test_ids) is only not equal to self.n_folds if self$apply_cross_fitting ==False
      thetas = rep(NA, length(test_ids))
      for (i_fold in 1:length(test_ids)) {
        test_index = test_ids[[i_fold]]
        thetas[i_fold] = private$orth_est(inds=test_index)
      }
      coef = mean(thetas, na.rm = TRUE)
      private$set__all_dml1_coef(thetas)
    }
    
    else if (dml_procedure == "dml2") {
      coef = private$orth_est()
    }
    
    return(coef)
  },
  se_causal_pars = function() {
    if (self$apply_cross_fitting) {
      se = sqrt(private$var_est())
    } else {
      smpls = private$get__smpls()
      test_ids = smpls$test_ids
      test_index = test_ids[[1]]
      se = sqrt(private$var_est(test_index))
    }
    return(se)
  },
  agg_cross_fit = function() {
    # aggregate parameters from the repeated cross-fitting
    # don't use the getter (always for one treatment variable and one sample), but the private variable
    self$coef = apply(self$all_coef, 1, function(x) stats::median(x, na.rm = TRUE))
    self$se = sqrt(apply(self$all_se^2  + (self$all_coef - self$coef)^2, 1, 
                                           function(x) stats::median(x, na.rm = TRUE)))
    
    invisible(self)
  },
  compute_bootstrap = function(weights, n_rep_boot) {
    dml_procedure = self$dml_procedure
    smpls = private$get__smpls()
    test_ids = smpls$test_ids
    
    if (self$apply_cross_fitting) {
      n_obs = self$data$n_obs
    } else {
      test_index = test_ids[[1]]
      n_obs = length(test_index)
    }
    
    if (self$apply_cross_fitting) {
      if (dml_procedure == "dml1") {
        boot_coefs = boot_t_stat = matrix(NA, nrow = n_rep_boot, ncol = self$n_folds)
        ii = 0
        for (i_fold in 1:self$n_folds) {
          test_index = test_ids[[i_fold]]
          n_obs_in_fold = length(test_index)
          
          J = mean(private$get__psi_a()[test_index])
          boot_coefs[,i_fold] = weights[,(ii+1):(ii+n_obs_in_fold)] %*% private$get__psi()[test_index] / (
            n_obs_in_fold * J)
          boot_t_stat[,i_fold] = weights[,(ii+1):(ii+n_obs_in_fold)] %*% private$get__psi()[test_index] / (
            n_obs_in_fold * private$get__all_se() * J)
          ii = ii + n_obs_in_fold
        }
        boot_coef = rowMeans(boot_coefs)
        boot_t_stat = rowMeans(boot_t_stat)
      }
      else if (dml_procedure == "dml2") {
        J = mean(private$get__psi_a())
        boot_coef = weights %*% private$get__psi() / (n_obs * J)
        boot_t_stat = weights %*% private$get__psi() / (n_obs * private$get__all_se() * J)
      }
      
    } else {
      J =  mean(private$get__psi_a()[test_index])
      boot_coef = weights %*% private$get__psi()[test_index] / (n_obs * private$get__all_se() * J)
      boot_t_stat = weights %*% private$get__psi()[test_index] / (n_obs * J)
    }
    
    res = list(boot_coef = boot_coef, boot_t_stat = boot_t_stat)
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
      n_obs = self$data$n_obs
    } else {
      n_obs = length(inds)
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

