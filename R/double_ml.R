#' @title Abstract class DoubleML
#'
#' @description
#' Abstract base class that can't be initialized.
#'
#'
#' @format [R6::R6Class] object.
#'
#' @family DoubleML
DoubleML = R6Class("DoubleML",
    active = list(
    #' @field all_coef (`matrix()`) \cr
    #' Estimates of the causal parameter(s) for the `n_rep` different sample
    #' splits after calling `fit()`.
    all_coef = function(value) {
      if (missing(value)) return(private$all_coef_)
      else stop("can't set field all_coef")
    },

    #' @field all_dml1_coef (`array()`) \cr
    #' Estimates of the causal parameter(s) for the `n_rep` different sample
    #' splits after calling `fit()` with `dml_procedure = "dml1"`.
    all_dml1_coef = function(value) {
      if (missing(value)) return(private$all_dml1_coef_)
      else stop("can't set field all_dml1_coef")
    },

    #' @field all_se (`matrix()`) \cr
    #' Standard errors of the causal parameter(s) for the `n_rep` different
    #' sample splits after calling `fit()`.
    all_se = function(value) {
      if (missing(value)) return(private$all_se_)
      else stop("can't set field all_se")
    },

    #' @field apply_cross_fitting (`logical(1)`) \cr
    #' Indicates whether cross-fitting should be applied. Default is `TRUE`.
    apply_cross_fitting = function(value) {
      if (missing(value)) return(private$apply_cross_fitting_)
      else stop("can't set field apply_cross_fitting")
    },

    #' @field boot_coef (`matrix()`) \cr
    #' Bootstrapped coefficients for the causal parameter(s) after calling
    #' `fit()` and `bootstrap()`.
    boot_coef = function(value) {
      if (missing(value)) return(private$boot_coef_)
      else stop("can't set field boot_coef")
    },

    #' @field boot_t_stat (`matrix()`) \cr
    #' Bootstrapped t-statistics for the causal parameter(s) after calling
    #' `fit()` and `bootstrap()`.
    boot_t_stat = function(value) {
      if (missing(value)) return(private$boot_t_stat_)
      else stop("can't set field boot_t_stat")
    },

    #' @field coef (`numeric()`) \cr
    #' Estimates for the causal parameter(s) after calling `fit()`.
    coef = function(value) {
      if (missing(value)) return(private$coef_)
      else stop("can't set field coef")
    },

    #' @field data ([`data.table`][data.table::data.table()])\cr
    #' Data object.
    data = function(value) {
      if (missing(value)) return(private$data_)
      else stop("can't set field data")
    },

    #' @field dml_procedure (`character(1)`) \cr
    #' A `character()` (`"dml1"` or `"dml2"`) specifying the double machine
    #' learning algorithm. Default is `"dml2"`.
    dml_procedure = function(value) {
      if (missing(value)) return(private$dml_procedure_)
      else stop("can't set field dml_procedure")
    },

    #' @field draw_sample_splitting (`logical(1)`) \cr
    #' Indicates whether the sample splitting should be drawn during
    #' initialization of the object. Default is `TRUE`.
    draw_sample_splitting = function(value) {
      if (missing(value)) return(private$draw_sample_splitting_)
      else stop("can't set field draw_sample_splitting")
    },

    #' @field learner (named `list()`) \cr
    #' The machine learners for the nuisance functions.
    learner = function(value) {
      if (missing(value)) return(private$learner_)
      else stop("can't set field learner")
    },

    #' @field n_folds (`integer(1)`) \cr
    #' Number of folds. Default is `5`.
    n_folds = function(value) {
      if (missing(value)) return(private$n_folds_)
      else stop("can't set field n_folds")
    },

    #' @field n_rep (`integer(1)`) \cr
    #' Number of repetitions for the sample splitting. Default is `1`.
    n_rep = function(value) {
      if (missing(value)) return(private$n_rep_)
      else stop("can't set field n_rep")
    },

    #' @field params (named `list()`) \cr
    #' The hyperparameters of the learners.
    params = function(value) {
      if (missing(value)) return(private$params_)
      else stop("can't set field params")
    },

    #' @field psi (`array()`) \cr
    #' Value of the score function
    #' \eqn{\psi(W;\theta, \eta)=\psi_a(W;\eta) \theta + \psi_b (W; \eta)}
    #' after calling `fit()`.
    psi  = function(value) {
      if (missing(value)) return(private$psi_)
      else stop("can't set field psi")
    },

    #' @field psi_a (`array()`) \cr
    #' Value of the score function component \eqn{\psi_a(W;\eta)} after
    #' calling `fit()`.
    psi_a  = function(value) {
      if (missing(value)) return(private$psi_a_)
      else stop("can't set field psi_a")
    },

    #' @field psi_b (`array()`) \cr
    #' Value of the score function component \eqn{\psi_b(W;\eta)} after
    #' calling `fit()`.
    psi_b  = function(value) {
      if (missing(value)) return(private$psi_b_)
      else stop("can't set field psi_b")
    },

    #' @field predictions (`array()`) \cr
    #' Predictions of the nuisance models after calling
    #' `fit(store_predictions=TRUE)`.
    predictions  = function(value) {
      if (missing(value)) return(private$predictions_)
      else stop("can't set field predictions")
    },

    #' @field pval (`numeric()`) \cr
    #' p-values for the causal parameter(s) after calling `fit()`.
    pval  = function(value) {
      if (missing(value)) return(private$pval_)
      else stop("can't set field pval")
    },

    #' @field score (`character(1)`, `function()`) \cr
    #' A `character(1)` or `function()` specifying the score function.
    score  = function(value) {
      if (missing(value)) return(private$score_)
      else stop("can't set field score")
    },

    #' @field se (`numeric()`) \cr
    #' Standard errors for the causal parameter(s) after calling `fit()`.
    se  = function(value) {
      if (missing(value)) return(private$se_)
      else stop("can't set field se")
    },

    #' @field smpls (`list()`) \cr
    #' The partition used for cross-fitting.
    smpls  = function(value) {
      if (missing(value)) return(private$smpls_)
      else stop("can't set field smpls")
    },

    #' @field t_stat (`numeric()`) \cr
    #' t-statistics for the causal parameter(s) after calling `fit()`.
    t_stat  = function(value) {
      if (missing(value)) return(private$t_stat_)
      else stop("can't set field t_stat")
    },

    #' @field tuning_res (named `list()`) \cr
    #' Results from hyperparameter tuning.
    tuning_res  = function(value) {
      if (missing(value)) return(private$tuning_res_)
      else stop("can't set field tuning_res")
    }),

    public = list(
    #' @description
    #' DoubleML is an abstract class that can't be initialized.
    initialize = function() {
      stop("DoubleML is an abstract class that can't be initialized.")
    },
    #' @description
    #' Print DoubleML objects.
    print = function() {

      class_name = class(self)[1]
      header = paste0(
        "================= ", class_name,
        " Object ==================\n")
      data_info = paste0(
        "Outcome variable: ", self$data$y_col, "\n",
        "Treatment variable(s): ", paste0(self$data$d_cols, collapse = ", "),
        "\n",
        "Covariates: ", paste0(self$data$x_cols, collapse = ", "), "\n",
        "Instrument(s): ", paste0(self$data$z_cols, collapse = ", "), "\n",
        "No. Observations: ", self$data$n_obs, "\n")

      if (is.character(self$score)) {
        score_info = paste0(
          "Score function: ", self$score, "\n",
          "DML algorithm: ", self$dml_procedure, "\n")
      } else if (is.function(self$score)) {
        score_info = paste0(
          "Score function: User specified score function \n",
          "DML algorithm: ", self$dml_procedure, "\n")
      }
      learner_info = character(length(self$learner))
      for (i_lrn in seq_len(length(self$learner))) {
        if (any(class(self$learner[[i_lrn]]) == "Learner")) {
          learner_info[i_lrn] = paste0(
            self$learner_names()[[i_lrn]], ": ",
            self$learner[[i_lrn]]$id, "\n")
        } else {
          learner_info[i_lrn] = paste0(
            self$learner_names()[[i_lrn]], ": ",
            self$learner[i_lrn], "\n")
        }
      }
      resampling_info = paste0(
        "No. folds: ", self$n_folds, "\n",
        "No. repeated sample splits: ", self$n_rep, "\n",
        "Apply cross-fitting: ", self$apply_cross_fitting, "\n")
      cat(header, "\n",
          "\n------------------ Data summary      ------------------\n",
          data_info,
          "\n------------------ Score & algorithm ------------------\n",
          score_info,
          "\n------------------ Machine learner   ------------------\n",
          learner_info,
          "\n------------------ Resampling        ------------------\n",
          resampling_info,
          "\n------------------ Fit summary       ------------------\n ",
          sep = "")
      self$summary()
      
      invisible(self)
    },

    #' @description
    #' Estimate DoubleML models.
    #'
    #' @param store_predictions (`logical(1)`) \cr
    #' Indicates whether the predictions for the nuisance functions should be
    #' stored in field `predictions`. Default is `FALSE`.
    #'
    #' @return self
    fit = function(store_predictions = FALSE) {

      if (store_predictions) {
        private$initialize_predictions()
      }

      # TODO: insert check for tuned params
      for (i_rep in 1:self$n_rep) {
        private$i_rep = i_rep

        for (i_treat in 1:self$data$n_treat) {
          private$i_treat = i_treat

          if (self$data$n_treat > 1) {
            self$data$set_data_model(self$data$d_cols[i_treat])
          }

          # ml estimation of nuisance models and computation of psi elements
          res = private$ml_nuisance_and_score_elements(private$get__smpls())
          private$psi_a_[, private$i_rep, private$i_treat] = res$psi_a
          private$psi_b_[, private$i_rep, private$i_treat] = res$psi_b
          if (store_predictions) {
            private$store_predictions(res$preds)
          }

          # estimate the causal parameter
          private$all_coef_[private$i_treat, private$i_rep] = private$est_causal_pars()
          # compute score (depends on estimated causal parameter)
          private$psi_[, private$i_rep, private$i_treat] = private$compute_score()

          # compute standard errors for causal parameter
          private$all_se_[private$i_treat, private$i_rep] = private$se_causal_pars()
        }
      }

      private$agg_cross_fit()

      private$t_stat_ = self$coef / self$se
      private$pval_ = 2 * pnorm(-abs(self$t_stat))
      names(private$coef_) = names(private$se_) = names(private$t_stat_) =
        names(private$pval_) = self$data$d_cols

      invisible(self)
    },

    #' @description
    #' Multiplier bootstrap for DoubleML models.
    #'
    #' @param method (`character(1)`) \cr
    #' A `character(1)` (`"Bayes"`, `"normal"` or `"wild"`) specifying the
    #' multiplier bootstrap method.
    #'
    #' @param n_rep_boot (`integer(1)`) \cr
    #' The number of bootstrap replications.
    #'
    #' @return self
    bootstrap = function(method = "normal", n_rep_boot = 500) {
      if (all(is.na(self$psi))) {
        stop("Apply fit() before bootstrap().")
      }
      assert_choice(method, c("normal", "Bayes", "wild"))
      assert_count(n_rep_boot, positive = TRUE)

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
          i_start = (private$i_rep - 1) * private$n_rep_boot + 1
          i_end = private$i_rep * private$n_rep_boot
          private$boot_coef_[private$i_treat, i_start:i_end] = boot_res$boot_coef
          private$boot_t_stat_[private$i_treat, i_start:i_end] = boot_res$boot_t_stat
        }
      }
      invisible(self)
    },
    #' @description
    #' Draw sample splitting for DoubleML models.
    #'
    #' The samples are drawn according to the attributes `n_folds`, `n_rep`
    #' and `apply_cross_fitting`.
    #'
    #' @return self
    split_samples = function() {
      dummy_task = Task$new("dummy_resampling", "regr", self$data$data)

      if (self$apply_cross_fitting) {

        dummy_resampling_scheme = rsmp("repeated_cv",
          folds = self$n_folds,
          repeats = self$n_rep)$instantiate(dummy_task)
        train_ids = lapply(
          1:(self$n_folds * self$n_rep),
          function(x) dummy_resampling_scheme$train_set(x))
        test_ids = lapply(
          1:(self$n_folds * self$n_rep),
          function(x) dummy_resampling_scheme$test_set(x))

        smpls = lapply(1:self$n_rep, function(i_repeat) {
          list(
            train_ids = train_ids[((i_repeat - 1) * self$n_folds + 1):
            (i_repeat * self$n_folds)],
            test_ids = test_ids[((i_repeat - 1) * self$n_folds + 1):
            (i_repeat * self$n_folds)])
        })
      } else {
        if (self$n_folds == 2) {
          dummy_resampling_scheme = rsmp("holdout", ratio = 0.5)$instantiate(dummy_task)
          train_ids = list(dummy_resampling_scheme$train_set(1))
          test_ids = list(dummy_resampling_scheme$test_set(1))

          smpls = list(list(train_ids = train_ids, test_ids = test_ids))

        } else if (self$n_folds == 1) {
          dummy_resampling_scheme = rsmp("insample")$instantiate(dummy_task)

          train_ids = lapply(
            1:(self$n_folds * self$n_rep),
            function(x) dummy_resampling_scheme$train_set(x))
          test_ids = lapply(
            1:(self$n_folds * self$n_rep),
            function(x) dummy_resampling_scheme$test_set(x))

          smpls = lapply(1:self$n_rep, function(i_repeat) {
            list(
              train_ids = train_ids[((i_repeat - 1) * self$n_folds + 1):
              (i_repeat * self$n_folds)],
              test_ids = test_ids[((i_repeat - 1) * self$n_folds + 1):
              (i_repeat * self$n_folds)])
          })
        }
      }
      private$smpls_ = smpls
      invisible(self)
    },
    #' @description
    #' Set the sample splitting for DoubleML models.
    #'
    #' The attributes `n_folds` and `n_rep` are derived from the provided
    #' partition.
    #'
    #' @param smpls (`list()`) \cr
    #' A nested `list()`. The outer lists needs to provide an entry per
    #' repeated sample splitting (length of the list is set as `n_rep`).
    #' The inner list is a named `list()` with names `train_ids` and `test_ids`.
    #' The entries in `train_ids` and `test_ids` must be partitions per fold
    #' (length of `train_ids` and `test_ids` is set as `n_folds`).
    #'
    #' @return self
    #' 
    #' @examples
    #' library(DoubleML)
    #' library(mlr3)
    #' set.seed(2)
    #' obj_dml_data = make_plr_CCDDHNR2018(n_obs=10)
    #' dml_plr_obj = DoubleMLPLR$new(obj_dml_data,
    #'                               lrn("regr.rpart"), lrn("regr.rpart"))
    #' 
    #' # simple sample splitting with two folds and without cross-fitting
    #' smpls = list(list(train_ids = list(c(1, 2, 3, 4, 5)),
    #'                   test_ids = list(c(6, 7, 8, 9, 10))))
    #' dml_plr_obj$set_sample_splitting(smpls)
    #' 
    #' # sample splitting with two folds and cross-fitting but no repeated cross-fitting
    #' smpls = list(list(train_ids = list(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10)),
    #'                   test_ids = list(c(6, 7, 8, 9, 10), c(1, 2, 3, 4, 5))))
    #' dml_plr_obj$set_sample_splitting(smpls)
    #' 
    #' # sample splitting with two folds and repeated cross-fitting with n_rep = 2
    #' smpls = list(list(train_ids = list(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10)),
    #'                   test_ids = list(c(6, 7, 8, 9, 10), c(1, 2, 3, 4, 5))),
    #'              list(train_ids = list(c(1, 3, 5, 7, 9), c(2, 4, 6, 8, 10)),
    #'                   test_ids = list(c(2, 4, 6, 8, 10), c(1, 3, 5, 7, 9))))
    #' dml_plr_obj$set_sample_splitting(smpls)
    set_sample_splitting = function(smpls) {

      if (test_list(smpls, names = "unnamed")) {
        lapply(smpls, function(x) check_smpl_split(x, self$data$n_obs))
        
        n_folds_each_train_smpl = vapply(
          smpls, function(x) length(x$train_ids),
          integer(1L))
        n_folds_each_test_smpl = vapply(
          smpls, function(x) length(x$test_ids),
          integer(1L))
        
        if (!all(n_folds_each_train_smpl == n_folds_each_train_smpl[1])) {
          stop("Different number of folds for repeated cross-fitting.")
        }
        
        smpls_are_partitions = vapply(
          smpls,
          function(x) check_is_partition(x$test_ids, self$data$n_obs),
          FUN.VALUE=TRUE)
        
        if (all(smpls_are_partitions)) {
          if (length(smpls) == 1 &
              n_folds_each_train_smpl[1] == 1 &
              check_is_partition(smpls[[1]]$train_ids, self$data$n_obs)) {
            private$n_rep_ = 1
            private$n_folds_ = 1
            private$apply_cross_fitting_ = FALSE
            private$smpls_ = smpls
          } else {
            private$n_rep_ = length(smpls)
            private$n_folds_ = n_folds_each_train_smpl[1]
            private$apply_cross_fitting_ = TRUE
            lapply(smpls,
                   function(x) check_smpl_split(x, self$data$n_obs,
                                                check_intersect = TRUE))
            private$smpls_ = smpls
          }
        } else {
          if (n_folds_each_train_smpl[1] != 1) {
            stop(paste("Invalid partition provided.",
                       "Tuples (train_ids, test_ids) for more than one fold",
                       "provided that don't form a partition."))
          }
          if (length(smpls) != 1) {
            stop(paste("Repeated sample splitting without cross-fitting not",
                       "implemented."))
          }
          private$n_rep_ = length(smpls)
          private$n_folds_ = 2
          private$apply_cross_fitting_ = FALSE
          lapply(smpls,
                 function(x) check_smpl_split(x, self$data$n_obs,
                                              check_intersect = TRUE))
          private$smpls_ = smpls
        }
      } else {
        check_smpl_split(smpls, self$data$n_obs)
        private$n_rep_ = 1
        n_folds = length(smpls$train_ids)
        if (check_is_partition(smpls$test_ids, self$data$n_obs)) {
          if (n_folds == 1 & check_is_partition(smpls$train_ids, self$data$n_obs)) {
            private$n_folds_ = 1
            private$apply_cross_fitting_ = FALSE
            private$smpls_ = list(smpls)
          } else {
            private$n_folds_ = n_folds
            private$apply_cross_fitting_ = TRUE
            check_smpl_split(smpls, self$data$n_obs,
                             check_intersect = TRUE)
            private$smpls_ = list(smpls)
          }
        } else {
          if (n_folds != 1) {
            stop(paste("Invalid partition provided.",
                       "Tuples (train_ids, test_ids) for more than one fold",
                       "provided that don't form a partition."))
          }
          private$n_folds_ = 2
          private$apply_cross_fitting_ = FALSE
          check_smpl_split(smpls, self$data$n_obs,
                           check_intersect = TRUE)
          private$smpls_ = list(smpls)
        }
      }

      private$initialize_arrays()

      invisible(self)
    },
    #' @description
    #' Hyperparameter-tuning for DoubleML models.
    #'
    #' The hyperparameter-tuning is performed using the tuning methods provided
    #' in the [mlr3tuning](https://mlr3tuning.mlr-org.com/) package. For more
    #' information on tuning in [mlr3](https://mlr3.mlr-org.com/), we refer to
    #' the section on parameter tuning in the
    #' [mlr3 book](https://mlr3book.mlr-org.com/tuning.html).
    #'
    #' @param param_set (named `list()`) \cr
    #' A named `list` with a parameter grid for each nuisance model/learner
    #' (see method `learner_names()`). The parameter grid must be an object of
    #' class [ParamSet][paradox::ParamSet].
    #'
    #' @param tune_settings (named `list()`) \cr
    #' A named `list()` with arguments passed to the hyperparameter-tuning with
    #' [mlr3tuning](https://mlr3tuning.mlr-org.com/) to set up
    #' [TuningInstance][mlr3tuning::TuningInstanceSingleCrit] objects.
    #' `tune_settings` has entries
    #' * `terminator` ([Terminator][bbotk::Terminator]) \cr
    #' A [Terminator][bbotk::Terminator] object. Specification of `terminator`
    #' is required to perform tuning.
    #' * `algorithm` ([Tuner][mlr3tuning::Tuner] or `character(1)`) \cr
    #' A [Tuner][mlr3tuning::Tuner] object (recommended) or key passed to the
    #' respective dictionary to specify the tuning algorithm used in
    #' [tnr()][mlr3tuning::tnr()]. `algorithm` is passed as an argument to
    #' [tnr()][mlr3tuning::tnr()]. If `algorithm` is not specified by the users,
    #' default is set to `"grid_search"`. If set to `"grid_search"`, then
    #' additional argument `"resolution"` is required.
    #' * `rsmp_tune` ([Resampling][mlr3::Resampling] or `character(1)`)\cr
    #' A [Resampling][mlr3::Resampling] object (recommended) or option passed
    #' to [rsmp()][mlr3::mlr_sugar] to initialize a
    #' [Resampling][mlr3::Resampling] for parameter tuning in `mlr3`.
    #' If not specified by the user, default is set to `"cv"`
    #' (cross-validation).
    #' * `n_folds_tune` (`integer(1)`, optional) \cr
    #' If `rsmp_tune = "cv"`, number of folds used for cross-validation.
    #' If not specified by the user, default is set to `5`.
    #' * `measure` (`NULL`, named `list()`, optional) \cr
    #' Named list containing the measures used for parameter tuning. Entries in
    #' list must either be [Measure][mlr3::Measure] objects or keys to be
    #' passed to passed to [msr()][mlr3::msr()]. The names of the entries must
    #' match the learner names (see method `learner_names()`). If set to `NULL`,
    #' default measures are used, i.e., `"regr.mse"` for continuous outcome
    #' variables and `"classif.ce"` for binary outcomes.
    #' * `resolution` (`character(1)`) \cr The key passed to the respective
    #' dictionary to specify  the tuning algorithm used in
    #' [tnr()][mlr3tuning::tnr()]. `resolution` is passed as an argument to
    #' [tnr()][mlr3tuning::tnr()].
    #'
    #' @param tune_on_folds (`logical(1)`) \cr
    #' Indicates whether the tuning should be done fold-specific or globally.
    #' Default is `FALSE`.
    #'
    #' @return self
    tune = function(param_set, tune_settings = list(
      n_folds_tune = 5,
      rsmp_tune = mlr3::rsmp("cv", folds = 5),
      measure = NULL,
      terminator = mlr3tunin::trm("evals", n_evals = 20),
      algorithm = mlr3tuning::tnr("grid_search"),
      resolution = 5),
    tune_on_folds = FALSE) {

      assert_list(param_set)
      valid_learner = self$learner_names()
      if (!test_names(names(param_set), subset.of = valid_learner)) {
        stop(paste(
          "Invalid param_set", paste0(names(param_set), collapse = ", "),
          "\n param_grids must be a named list with elements named",
          paste0(valid_learner, collapse = ", ")))
      }
      for (i_grid in seq_len(length(param_set))) {
        assert_class(param_set[[i_grid]], "ParamSet")
      }
      assert_logical(tune_on_folds, len = 1)
      tune_settings = private$assert_tune_settings(tune_settings)

      if (!self$apply_cross_fitting) {
        stop("Parameter tuning for no-cross-fitting case not implemented.")
      }

      if (tune_on_folds) {
        params_rep = vector("list", self$n_rep)
        private$tuning_res_ = rep(list(params_rep), self$data$n_treat)
        names(private$tuning_res_) = self$data$d_cols
        private$fold_specific_params = TRUE
      } else {
        private$tuning_res_ = vector("list", self$data$n_treat)
        names(private$tuning_res_) = self$data$d_cols
      }

      for (i_treat in 1:self$data$n_treat) {
        private$i_treat = i_treat

        if (self$data$n_treat > 1) {
          self$data$set_data_model(self$data$d_cols[i_treat])
        }

        if (tune_on_folds) {
          for (i_rep in 1:self$n_rep) {
            private$i_rep = i_rep
            param_tuning = private$ml_nuisance_tuning(
              private$get__smpls(),
              param_set, tune_settings, tune_on_folds)
            private$tuning_res_[[i_treat]][[i_rep]] = param_tuning

            for (nuisance_model in names(param_tuning)) {
              if (!is.null(param_tuning[[nuisance_model]][[1]])) {
                self$set_ml_nuisance_params(
                  learner = nuisance_model,
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
          param_tuning = private$ml_nuisance_tuning(
            private$get__smpls(),
            param_set, tune_settings, tune_on_folds)
          private$tuning_res_[[i_treat]] = param_tuning

          for (nuisance_model in self$params_names()) {
            if (!is.null(param_tuning[[nuisance_model]][[1]])) {
              self$set_ml_nuisance_params(
                learner = nuisance_model,
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
        message("fit() not yet called.")
      } else {
        k = length(self$coef)
        table = matrix(NA, ncol = 4, nrow = k)
        rownames(table) = names(self$coef)
        colnames(table) = c("Estimate.", "Std. Error", "t value", "Pr(>|t|)")
        table[, 1] = self$coef
        table[, 2] = self$se
        table[, 3] = self$t_stat
        table[, 4] = self$pval
        private$summary_table = table

        if (length(k)) {
          print(paste(
            "Estimates and significance testing of the",
            "effect of target variables"))
          res = as.matrix(printCoefmat(private$summary_table,
            digits = digits,
            P.values = TRUE,
            has.Pvalue = TRUE))
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
    #' Indicates whether joint confidence intervals are computed.
    #' Default is `FALSE`.
    #'
    #' @param level (`numeric(1)`) \cr
    #' The confidence level. Default is `0.95`.
    #'
    #' @param parm (`numeric()` or `character()`) \cr
    #' A specification of which parameters are to be given confidence intervals
    #' among the variables for which inference was done, either a vector of
    #' numbers or a vector of names. If missing, all parameters are considered
    #' (default).
    #' @return A `matrix()` with the confidence interval(s).
    confint = function(parm, joint = FALSE, level = 0.95) {
      assert_logical(joint, len = 1)
      assert_numeric(level, len = 1)
      if (level <= 0 | level >= 1) {
        stop("'level' must be > 0 and < 1.")
      }
      if (missing(parm)) {
        parm = names(self$coef)
      }
      else {
        assert(
          check_character(parm, max.len = self$data$n_treat),
          check_numeric(parm, max.len = self$data$n_treat))
        if (is.numeric(parm)) {
          parm = names(self$coef)[parm]
        }
      }

      if (joint == FALSE) {
        a = (1 - level) / 2
        a = c(a, 1 - a)
        pct = format.perc(a, 3)
        fac = qnorm(a)
        ci = array(NA_real_, dim = c(length(parm), 2L), dimnames = list(
          parm,
          pct))
        ci[] = self$coef[parm] + self$se[parm] %o% fac
      }

      if (joint == TRUE) {

        a = (1 - level)
        ab = c(a / 2, 1 - a / 2)
        pct = format.perc(ab, 3)
        ci = array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))

        if (all(is.na(self$boot_coef))) {
          stop(paste(
            "Multiplier bootstrap has not yet been performed.",
            "First call bootstrap() and then try confint() again."))
        }

        sim = apply(abs(self$boot_t_stat), 2, max)
        hatc = quantile(sim, probs = 1 - a)

        ci[, 1] = self$coef[parm] - hatc * self$se[parm]
        ci[, 2] = self$coef[parm] + hatc * self$se[parm]
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
    #' Note that in the current implementation, either all parameters have to
    #' be set globally or all parameters have to be provided fold-specific.
    #'
    #' @param learner (`character(1)`) \cr
    #' The nuisance model/learner (see method `params_names`).
    #'
    #' @param treat_var (`character(1)`) \cr
    #' The treatment varaible (hyperparameters can be set treatment-variable
    #' specific).
    #'
    #' @param params (named `list()`) \cr
    #' A named `list()` with estimator parameters. Parameters are used for all
    #' folds by default. Alternatively, parameters can be passed in a
    #' fold-specific way if option  `fold_specific`is `TRUE`. In this case, the
    #' outer list needs to be of length `n_rep` and the inner list of length
    #' `n_folds`.
    #'
    #' @param set_fold_specific (`logical(1)`) \cr
    #' Indicates if the parameters passed in `params` should be passed in
    #' fold-specific way. Default is `FALSE`. If `TRUE`, the outer list needs
    #' to be of length `n_rep` and the inner list of length `n_folds`.
    #' Note that in the current implementation, either all parameters have to
    #' be set globally or all parameters have to be provided fold-specific.
    #'
    #' @return self
    set_ml_nuisance_params = function(learner = NULL, treat_var = NULL, params,
      set_fold_specific = FALSE) {

      valid_learner = self$params_names()
      assert_character(learner, len = 1)
      assert_choice(learner, valid_learner)

      assert_choice(treat_var, self$data$d_cols)
      assert_list(params)
      assert_logical(set_fold_specific, len = 1)

      if (!set_fold_specific) {
        if (private$fold_specific_params) {
          private$params_[[learner]][[treat_var]][[private$i_rep]] = params
        } else {
          private$params_[[learner]][[treat_var]] = params
        }
      } else {
        if (length(params) != self$n_rep) {
          stop("Length of (outer) parameter list does not match n_rep.")
        }
        if (!all(lapply(params, length) == self$n_folds)) {
          stop("Length of (inner) parameter list does not match n_folds.")
        }

        private$fold_specific_params = set_fold_specific
        private$params_[[learner]][[treat_var]] = params
      }
    },
    #' @description
    #' Multiple testing adjustment for DoubleML models.
    #'
    #' @param method (`character(1)`) \cr
    #' A `character(1)`(`"romano-wolf"`, `"bonferroni"`, `"holm"`, etc)
    #' specifying the adjustment method. In addition to `"romano-wolf"`,
    #' all methods implemented in [p.adjust()][stats::p.adjust()] can be
    #' applied. Default is `"romano-wolf"`.
    #' @param return_matrix (`logical(1)`) \cr
    #' Indicates if the output is returned as a matrix with corresponding
    #' coefficient names.
    #'
    #' @return `numeric()` with adjusted p-values. If `return_matrix = TRUE`,
    #' a `matrix()` with adjusted p_values.
    p_adjust = function(method = "romano-wolf", return_matrix = TRUE) {
      if (all(is.na(self$coef))) {
        stop("apply fit() before p_adjust().")
      }

      if (tolower(method) %in% c("rw", "romano-wolf")) {
        if (is.null(self$boot_t_stat) | all(is.na(self$coef))) {
          stop("apply fit() & bootstrap() before p_adjust().")
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
            sim = apply(
              abs(boot_t_stat[-stepdown_ind[1:(i_d - 1)], , drop = FALSE]), 2,
              max)
            pinit[i_d] = pmin(1, mean(sim > abs(t_stat[stepdown_ind][i_d])))
          }
        }
        # ensure monotonicity
        for (i_d in 1:k) {
          if (i_d == 1) {
            p_val_corrected[i_d] = pinit[i_d]
          } else {
            p_val_corrected[i_d] = max(pinit[i_d], p_val_corrected[i_d - 1])
          }
        }
        p_val = p_val_corrected[ro]
      } else {
        if (is.element(method, p.adjust.methods)) {
          p_val = p.adjust(self$pval,
            method = method,
            n = self$data$n_treat)
        } else {
          stop(paste(
            "Invalid method", method,
            "argument specified in p_adjust()."))
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
    get_params = function(learner) {
      valid_learner = self$params_names()
      assert_character(learner, len = 1)
      assert_choice(learner, valid_learner)

      if (private$fold_specific_params) {
        params = self$params[[learner]][[self$data$treat_col]][[private$i_rep]]
      } else {
        params = self$params[[learner]][[self$data$treat_col]]
      }
      return(params)
    }
  ),
  private = list(
    all_coef_ = NULL,
    all_dml1_coef_ = NULL,
    all_se_ = NULL,
    apply_cross_fitting_ = NULL,
    boot_coef_ = NULL,
    boot_t_stat_ = NULL,
    coef_ = NULL,
    data_ = NULL,
    dml_procedure_ = NULL,
    draw_sample_splitting_ = NULL,
    learner_ = NULL,
    n_folds_ = NULL,
    n_rep_ = NULL,
    params_ = NULL,
    psi_ = NULL,
    psi_a_ = NULL,
    psi_b_ = NULL,
    predictions_ = NULL,
    pval_ = NULL,
    score_ = NULL,
    se_ = NULL,
    smpls_ = NULL,
    t_stat_ = NULL,
    tuning_res_ = NULL,
    n_rep_boot = NULL,
    i_rep = NA,
    i_treat = NA,
    fold_specific_params = NULL,
    summary_table = NULL,
    learner_class = list(),
    initialize_double_ml = function(data,
      n_folds,
      n_rep,
      score,
      dml_procedure,
      draw_sample_splitting,
      apply_cross_fitting) {
      # check and pick up obj_dml_data

      assert_class(data, "DoubleMLData")
      private$data_ = data

      # initialize learners and parameters which are set model specific
      private$learner_ = NULL
      private$params_ = NULL
      # Set fold_specific_params = FALSE at instantiation
      private$fold_specific_params = FALSE

      # check resampling specifications
      assert_count(n_folds)
      assert_count(n_rep)
      assert_logical(apply_cross_fitting, len = 1)
      assert_logical(draw_sample_splitting, len = 1)

      # set resampling specifications
      private$n_folds_ = n_folds
      private$n_rep_ = n_rep
      private$apply_cross_fitting_ = apply_cross_fitting
      private$draw_sample_splitting_ = draw_sample_splitting

      # check and set dml_procedure and score
      assert_choice(dml_procedure, c("dml1", "dml2"))
      private$dml_procedure_ = dml_procedure
      private$score_ = score

      if (self$n_folds == 1 & self$apply_cross_fitting) {
        message(paste(
          "apply_cross_fitting is set to FALSE.",
          "Cross-fitting is not supported for n_folds = 1."))
        private$apply_cross_fitting_ = FALSE
      }

      if (!self$apply_cross_fitting) {
        if (self$n_folds > 2) {
          stop(paste(
            "Estimation without cross-fitting not supported for",
            "n_folds > 2."))
        }
        if (self$dml_procedure == "dml2") {
          # redirect to dml1 which works out-of-the-box; dml_procedure is of no
          # relevance without cross-fitting
          private$dml_procedure_ = "dml1"
        }
      }

      # perform sample splitting
      if (self$draw_sample_splitting) {
        self$split_samples()
      } else {
        private$smpls_ = NULL
      }

      # initialize arrays according to obj_dml_data and the resampling settings
      private$initialize_arrays()

      # also initialize bootstrap arrays with the default number of
      # bootstrap replications
      private$initialize_boot_arrays(n_rep_boot = 500)

      # initialize instance attributes which are later used for iterating
      invisible(self)
    },
    assert_learner = function(learner, learner_name, Regr, Classif) {

      assert(
        check_character(learner, max.len = 1),
        check_class(learner, "Learner"))

      if (is.character(learner)) {
        # warning("Learner provision by character() will be deprecated in the
        # future.")
        learner = lrn(learner)
      }

      if (Regr & test_class(learner, "LearnerRegr")) {
        private$learner_class[learner_name] = "LearnerRegr"
      }
      if (Classif & test_class(learner, "LearnerClassif")) {
        private$learner_class[learner_name] = "LearnerClassif"
      }

      if ((Regr & !Classif & !test_class(learner, "LearnerRegr"))) {
        stop(paste0(
          "Invalid learner provided for ", learner_name,
          ": must be of class 'LearnerRegr'"))
      }
      if ((Classif & !Regr & !test_class(learner, "LearnerClassif"))) {
        stop(paste0(
          "Invalid learner provided for ", learner_name,
          ": must be of class 'LearnerClassif'"))
      }
      invisible(learner)
    },
    assert_tune_settings = function(tune_settings) {

      valid_learner = self$learner_names()

      if (!test_names(names(tune_settings), must.include = "terminator")) {
        stop(paste(
          "Invalid tune_settings\n",
          "object 'terminator' is missing."))
      }
      assert_class(tune_settings$terminator, "Terminator")

      if (test_names(names(tune_settings), must.include = "n_folds_tune")) {
        assert_integerish(tune_settings$n_folds_tune, len = 1, lower = 2)
      } else {
        tune_settings$n_folds_tune = 5
      }

      if (test_names(names(tune_settings), must.include = "rsmp_tune")) {
        assert(
          check_character(tune_settings$rsmp_tune),
          check_class(tune_settings$rsmp_tune, "Resampling"))
        if (!test_class(tune_settings$rsmp_tune, "Resampling")) {
          if (tune_settings$rsmp_tune == "cv") {
            tune_settings$rsmp_tune = rsmp(tune_settings$rsmp_tune,
              folds = tune_settings$n_folds_tune)
          } else {
            tune_settings$rsmp_tune = rsmp(tune_settings$rsmp_tune)
          }
        }
      } else {
        tune_settings$rsmp_tune = rsmp("cv", folds = tune_settings$n_folds_tune)
      }

      if (test_names(names(tune_settings), must.include = "measure")) {
        assert_list(tune_settings$measure)
        if (!test_names(names(tune_settings$measure),
          subset.of = valid_learner)) {
          stop(paste(
            "Invalid name of measure", paste0(names(tune_settings$measure),
              collapse = ", "),
            "\n measure must be a named list with elements named",
            paste0(valid_learner, collapse = ", ")))
        }
        for (i_msr in seq_len(length(tune_settings$measure))) {
          assert(
            check_character(tune_settings$measure[[i_msr]]),
            check_class(tune_settings$measure[[i_msr]], "Measure"))
        }
      } else {
        tune_settings$measure = rep(list(NA), length(valid_learner))
        names(tune_settings$measure) = valid_learner
      }

      for (i_msr in seq_len(length(tune_settings$measure))) {
        if (!test_class(tune_settings$measure[[i_msr]], "Measure")) {
          this_learner = names(tune_settings$measure)[i_msr]
          tune_settings$measure[[this_learner]] = set_default_measure(
            tune_settings$measure[[this_learner]],
            private$learner_class[[this_learner]])
        }
      }

      if (!test_names(names(tune_settings), must.include = "algorithm")) {
        tune_settings$algorithm = "grid_search"
      } else {
        assert(
          check_character(tune_settings$algorithm, len = 1),
          check_class(tune_settings$algorithm, "Tuner"))
      }

      if (test_character(tune_settings$algorithm)) {
        if (tune_settings$algorithm == "grid_search") {
          if (is.null(tune_settings$resolution)) {
            stop(paste(
              "Invalid tune_settings\n",
              "object 'resolution' is missing."))
          } else {
            assert_count(tune_settings$resolution, positive = TRUE)
          }
          tune_settings$tuner = tnr(tune_settings$algorithm,
            resolution = tune_settings$resolution)
        }
      } else {
        tune_settings$tuner = tune_settings$algorithm
      }

      return(tune_settings)
    },
    initialize_arrays = function() {

      private$psi_ = array(NA, dim = c(
        self$data$n_obs, self$n_rep,
        self$data$n_treat))
      private$psi_a_ = array(NA, dim = c(
        self$data$n_obs, self$n_rep,
        self$data$n_treat))
      private$psi_b_ = array(NA, dim = c(
        self$data$n_obs, self$n_rep,
        self$data$n_treat))

      private$coef_ = array(NA, dim = c(self$data$n_treat))
      private$se_ = array(NA, dim = c(self$data$n_treat))

      private$all_coef_ = array(NA, dim = c(self$data$n_treat, self$n_rep))
      private$all_se_ = array(NA, dim = c(self$data$n_treat, self$n_rep))

      if (self$dml_procedure == "dml1") {
        if (self$apply_cross_fitting) {
          private$all_dml1_coef_ = array(NA, dim = c(
            self$data$n_treat, self$n_rep,
            self$n_folds))
        } else {
          private$all_dml1_coef_ = array(NA, dim = c(
            self$data$n_treat, self$n_rep,
            1))
        }
      }
    },
    initialize_boot_arrays = function(n_rep_boot) {
      private$n_rep_boot = n_rep_boot
      private$boot_coef_ = array(NA, dim = c(
        self$data$n_treat,
        n_rep_boot * self$n_rep))
      private$boot_t_stat_ = array(NA, dim = c(
        self$data$n_treat,
        n_rep_boot * self$n_rep))
    },
    initialize_predictions = function() {
      private$predictions_ = sapply(self$params_names(),
        function(key) {
          array(NA, dim = c(
            self$data$n_obs, self$n_rep,
            self$data$n_treat))
        },
        simplify = F)
    },
    store_predictions = function(preds) {
      for (learner in self$params_names()) {
        if (!is.null(preds[[learner]])) {
          private$predictions_[[learner]][
            , private$i_rep,
            private$i_treat] = preds[[learner]]
        }
      }
    },
    # Comment from python: The private properties with __ always deliver the
    # single treatment, single (cross-fitting) sample subselection
    # The slicing is based on the two properties self._i_treat,
    # the index of the treatment variable, and
    # self._i_rep, the index of the cross-fitting sample.
    get__smpls = function() self$smpls[[private$i_rep]],
    get__psi_a = function() self$psi_a[, private$i_rep, private$i_treat],
    get__psi_b = function() self$psi_b[, private$i_rep, private$i_treat],
    get__psi = function() self$psi[, private$i_rep, private$i_treat],
    get__all_coef = function() self$all_coef[private$i_treat, private$i_rep],
    get__all_se = function() self$all_se[private$i_treat, private$i_rep],
    est_causal_pars = function() {
      dml_procedure = self$dml_procedure
      n_folds = self$n_folds
      smpls = private$get__smpls()
      test_ids = smpls$test_ids

      if (dml_procedure == "dml1") {
        # Note that length(test_ids) is only not equal to self.n_folds
        # if self$apply_cross_fitting ==False
        thetas = rep(NA, length(test_ids))
        for (i_fold in seq_len(length(test_ids))) {
          test_index = test_ids[[i_fold]]
          thetas[i_fold] = private$orth_est(inds = test_index)
        }
        coef = mean(thetas, na.rm = TRUE)
        private$all_dml1_coef_[private$i_treat, private$i_rep, ] = thetas
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
      # don't use the getter (always for one treatment variable and one sample),
      # but the private variable
      private$coef_ = apply(
        self$all_coef, 1,
        function(x) median(x, na.rm = TRUE))
      if (self$apply_cross_fitting) {
        n_obs = self$data$n_obs
      } else {
        smpls = private$get__smpls()
        test_ids = smpls$test_ids
        test_index = test_ids[[1]]
        n_obs = length(test_index)
      }
      private$se_ = sqrt(apply(
        n_obs * self$all_se^2 + (self$all_coef - self$coef)^2, 1,
        function(x) median(x, na.rm = TRUE))/n_obs)

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
          J = mean(private$get__psi_a())
          boot_coef = weights %*% private$get__psi() / (n_obs * J)
          boot_t_stat = weights %*% private$get__psi() /
            (n_obs * private$get__all_se() * J)
      } else {
        J = mean(private$get__psi_a()[test_index])
        boot_coef = weights %*% private$get__psi()[test_index] /
          (n_obs * private$get__all_se() * J)
        boot_t_stat = weights %*% private$get__psi()[test_index] /
          (n_obs * J)
      }

      res = list(boot_coef = boot_coef, boot_t_stat = boot_t_stat)
      return(res)
    },
    var_est = function(inds = NULL) {
      psi_a = private$get__psi_a()
      psi = private$get__psi()
      if (!is.null(inds)) {
        psi_a = psi_a[inds]
        psi = psi[inds]
      }
      if (self$apply_cross_fitting) {
        n_obs = self$data$n_obs
      } else {
        n_obs = length(inds)
      }
      J = mean(psi_a)
      sigma2_hat = 1 / n_obs * mean(psi^2) / (J^2)
      return(sigma2_hat)
    },
    orth_est = function(inds = NULL) {
      psi_a = private$get__psi_a()
      psi_b = private$get__psi_b()
      if (!is.null(inds)) {
        psi_a = psi_a[inds]
        psi_b = psi_b[inds]
      }
      theta = -mean(psi_b) / mean(psi_a)
      return(theta)
    },
    compute_score = function() {
      psi = private$get__psi_a() * private$get__all_coef() + private$get__psi_b()
      return(psi)
    }
  )
)
