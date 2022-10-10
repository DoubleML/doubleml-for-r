#' @title Double machine learning for partially linear regression models
#'
#' @description
#' Double machine learning for partially linear regression models.
#'
#' @format [R6::R6Class] object inheriting from [DoubleML].
#'
#' @family DoubleML
#' @details
#' Partially linear regression (PLR) models take the form
#'
#' \eqn{Y = D\theta_0 + g_0(X) + \zeta,}
#'
#' \eqn{D = m_0(X) + V,}
#'
#' with \eqn{E[\zeta|D,X]=0} and \eqn{E[V|X] = 0}. \eqn{Y} is the outcome
#' variable variable and \eqn{D} is the policy variable of interest.
#' The high-dimensional vector \eqn{X = (X_1, \ldots, X_p)} consists of other
#' confounding covariates, and \eqn{\zeta} and \eqn{V} are stochastic errors.
#'
#' @usage NULL
#'
#' @examples
#' \donttest{
#' library(DoubleML)
#' library(mlr3)
#' library(mlr3learners)
#' library(data.table)
#' set.seed(2)
#' ml_g = lrn("regr.ranger", num.trees = 10, max.depth = 2)
#' ml_m = ml_g$clone()
#' obj_dml_data = make_plr_CCDDHNR2018(alpha = 0.5)
#' dml_plr_obj = DoubleMLPLR$new(obj_dml_data, ml_g, ml_m)
#' dml_plr_obj$fit()
#' dml_plr_obj$summary()
#' }
#'
#' \dontrun{
#' library(DoubleML)
#' library(mlr3)
#' library(mlr3learners)
#' library(mlr3tuning)
#' library(data.table)
#' set.seed(2)
#' ml_l = lrn("regr.rpart")
#' ml_m = ml_l$clone()
#' obj_dml_data = make_plr_CCDDHNR2018(alpha = 0.5)
#' dml_plr_obj = DoubleMLPLR$new(obj_dml_data, ml_l, ml_m)
#'
#' param_grid = list(
#'   "ml_l" = paradox::ParamSet$new(list(
#'     paradox::ParamDbl$new("cp", lower = 0.01, upper = 0.02),
#'     paradox::ParamInt$new("minsplit", lower = 1, upper = 2))),
#'   "ml_m" = paradox::ParamSet$new(list(
#'     paradox::ParamDbl$new("cp", lower = 0.01, upper = 0.02),
#'     paradox::ParamInt$new("minsplit", lower = 1, upper = 2))))
#'
#' # minimum requirements for tune_settings
#' tune_settings = list(
#'   terminator = mlr3tuning::trm("evals", n_evals = 5),
#'   algorithm = mlr3tuning::tnr("grid_search", resolution = 5))
#' dml_plr_obj$tune(param_set = param_grid, tune_settings = tune_settings)
#' dml_plr_obj$fit()
#' dml_plr_obj$summary()
#' }
#' @export
DoubleMLPLR = R6Class("DoubleMLPLR",
  inherit = DoubleML, public = list(
    #' @description
    #' Creates a new instance of this R6 class.
    #'
    #' @param data (`DoubleMLData`) \cr
    #' The `DoubleMLData` object providing the data and specifying the
    #' variables of the causal model.
    #'
    #' @param ml_l ([`LearnerRegr`][mlr3::LearnerRegr],
    #' [`Learner`][mlr3::Learner], `character(1)`) \cr
    #' A learner of the class [`LearnerRegr`][mlr3::LearnerRegr], which is
    #' available from [mlr3](https://mlr3.mlr-org.com/index.html) or its
    #' extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or
    #' [mlr3extralearners](https://mlr3extralearners.mlr-org.com/).
    #' Alternatively, a [`Learner`][mlr3::Learner] object with public field
    #' `task_type = "regr"` can be passed, for example of class
    #' [`GraphLearner`][mlr3pipelines::GraphLearner]. The learner can possibly
    #' be passed with specified parameters, for example
    #' `lrn("regr.cv_glmnet", s = "lambda.min")`. \cr
    #' `ml_l` refers to the nuisance function \eqn{l_0(X) = E[Y|X]}.
    #'
    #' @param ml_m ([`LearnerRegr`][mlr3::LearnerRegr],
    #' [`LearnerClassif`][mlr3::LearnerClassif], [`Learner`][mlr3::Learner],
    #' `character(1)`) \cr
    #' A learner of the class [`LearnerRegr`][mlr3::LearnerRegr], which is
    #' available from [mlr3](https://mlr3.mlr-org.com/index.html) or its
    #' extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or
    #' [mlr3extralearners](https://mlr3extralearners.mlr-org.com/).
    #' For binary treatment variables, an object of the class
    #' [`LearnerClassif`][mlr3::LearnerClassif] can be passed, for example
    #' `lrn("classif.cv_glmnet", s = "lambda.min")`.
    #' Alternatively, a [`Learner`][mlr3::Learner] object with public field
    #' `task_type = "regr"` or `task_type = "classif"` can be passed,
    #' respectively, for example of class
    #' [`GraphLearner`][mlr3pipelines::GraphLearner]. \cr
    #' `ml_m` refers to the nuisance function \eqn{m_0(X) = E[D|X]}.
    #'
    #' @param ml_g ([`LearnerRegr`][mlr3::LearnerRegr],
    #' [`Learner`][mlr3::Learner], `character(1)`) \cr
    #' A learner of the class [`LearnerRegr`][mlr3::LearnerRegr], which is
    #' available from [mlr3](https://mlr3.mlr-org.com/index.html) or its
    #' extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or
    #' [mlr3extralearners](https://mlr3extralearners.mlr-org.com/).
    #' Alternatively, a [`Learner`][mlr3::Learner] object with public field
    #' `task_type = "regr"` can be passed, for example of class
    #' [`GraphLearner`][mlr3pipelines::GraphLearner]. The learner can possibly
    #' be passed with specified parameters, for example
    #' `lrn("regr.cv_glmnet", s = "lambda.min")`. \cr
    #' `ml_g` refers to the nuisance function \eqn{g_0(X) = E[Y - D\theta_0|X]}.
    #' Note: The learner `ml_g` is only required for the score `'IV-type'`.
    #' Optionally, it can be specified and estimated for callable scores.
    #'
    #' @param n_folds (`integer(1)`)\cr
    #' Number of folds. Default is `5`.
    #'
    #' @param n_rep (`integer(1)`) \cr
    #' Number of repetitions for the sample splitting. Default is `1`.
    #'
    #' @param score (`character(1)`, `function()`) \cr
    #' A `character(1)` (`"partialling out"` or `"IV-type"`) or a `function()`
    #' specifying the score function.
    #' If a `function()` is provided, it must be of the form
    #' `function(y, d, l_hat, m_hat, g_hat, smpls)` and
    #' the returned output must be a named `list()` with elements `psi_a` and
    #' `psi_b`. Default is `"partialling out"`.
    #'
    #' @param dml_procedure (`character(1)`) \cr
    #' A `character(1)` (`"dml1"` or `"dml2"`) specifying the double machine
    #' learning algorithm. Default is `"dml2"`.
    #'
    #' @param draw_sample_splitting (`logical(1)`) \cr
    #' Indicates whether the sample splitting should be drawn during
    #' initialization of the object. Default is `TRUE`.
    #'
    #' @param apply_cross_fitting (`logical(1)`) \cr
    #' Indicates whether cross-fitting should be applied. Default is `TRUE`.
    initialize = function(data,
      ml_l,
      ml_m,
      ml_g = NULL,
      n_folds = 5,
      n_rep = 1,
      score = "partialling out",
      dml_procedure = "dml2",
      draw_sample_splitting = TRUE,
      apply_cross_fitting = TRUE) {

      if (missing(ml_l)) {
        if (!missing(ml_g)) {
          warning(paste0(
            "The argument ml_g was renamed to ml_l. ",
            "Please adapt the argument name accordingly. ",
            "ml_g is redirected to ml_l.\n",
            "The redirection will be removed in a future version."),
          call. = FALSE)
          ml_l = ml_g
          ml_g = NULL
        }
      }

      super$initialize_double_ml(
        data,
        n_folds,
        n_rep,
        score,
        dml_procedure,
        draw_sample_splitting,
        apply_cross_fitting)

      private$check_data(self$data)
      private$check_score(self$score)
      ml_l = private$assert_learner(ml_l, "ml_l", Regr = TRUE, Classif = FALSE)
      ml_m = private$assert_learner(ml_m, "ml_m", Regr = TRUE, Classif = TRUE)

      private$learner_ = list(
        "ml_l" = ml_l,
        "ml_m" = ml_m)

      if (!is.null(ml_g)) {
        assert(
          check_character(ml_g, max.len = 1),
          check_class(ml_g, "Learner"))
        if ((is.character(self$score) && (self$score == "IV-type")) ||
          is.function(self$score)) {
          ml_g = private$assert_learner(ml_g, "ml_g",
            Regr = TRUE, Classif = FALSE)
          private$learner_[["ml_g"]] = ml_g
        } else if (is.character(self$score) &&
          (self$score == "partialling out")) {
          warning(paste0(
            "A learner ml_g has been provided for ",
            "score = 'partialling out' but will be ignored. ",
            "A learner ml_g is not required for estimation."))
        }
      } else if (is.character(self$score) && (self$score == "IV-type")) {
        warning(paste0(
          "For score = 'IV-type', learners ml_l and ml_g ",
          "should be specified. ",
          "Set ml_g = ml_l$clone()."),
        call. = FALSE)
        ml_g = private$assert_learner(ml_l$clone(), "ml_g",
          Regr = TRUE, Classif = FALSE)
        private$learner_[["ml_g"]] = ml_g
      }

      private$initialize_ml_nuisance_params()
    },
    # To be removed in version 0.6.0
    #
    # Note: Ideally the following duplicate roxygen / docu parts should be taken
    # from the base class DoubleML. However, this is an open issue in pkg
    # roxygen2, see https://github.com/r-lib/roxygen2/issues/996 &
    # https://github.com/r-lib/roxygen2/issues/1043
    #
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
      assert_character(learner, len = 1)
      if (is.character(self$score) && (self$score == "partialling out") &&
        (learner == "ml_g")) {
        warning(paste0(
          "Learner ml_g was renamed to ml_l. ",
          "Please adapt the argument learner accordingly. ",
          "The provided parameters are set for ml_l. ",
          "The redirection will be removed in a future version."),
        call. = FALSE)
        learner = "ml_l"
      }
      super$set_ml_nuisance_params(
        learner, treat_var, params,
        set_fold_specific)
    },
    # To be removed in version 0.6.0
    #
    # Note: Ideally the following duplicate roxygen / docu parts should be taken
    # from the base class DoubleML. However, this is an open issue in pkg
    # roxygen2, see https://github.com/r-lib/roxygen2/issues/996 &
    # https://github.com/r-lib/roxygen2/issues/1043
    #
    #' @description
    #' Hyperparameter-tuning for DoubleML models.
    #'
    #' The hyperparameter-tuning is performed using the tuning methods provided
    #' in the [mlr3tuning](https://mlr3tuning.mlr-org.com/) package. For more
    #' information on tuning in [mlr3](https://mlr3.mlr-org.com/), we refer to
    #' the section on parameter tuning in the
    #' [mlr3 book](https://mlr3book.mlr-org.com/optimization.html#tuning).
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
      terminator = mlr3tuning::trm("evals", n_evals = 20),
      algorithm = mlr3tuning::tnr("grid_search"),
      resolution = 5),
    tune_on_folds = FALSE) {

      assert_list(param_set)
      if (is.character(self$score) && (self$score == "partialling out")) {
        if (exists("ml_g", where = param_set) && !exists("ml_l", where = param_set)) {
          warning(paste0(
            "Learner ml_g was renamed to ml_l. ",
            "Please adapt the name in param_set accordingly. ",
            "The provided param_set for ml_g is used for ml_l. ",
            "The redirection will be removed in a future version."),
          call. = FALSE)
          names(param_set)[names(param_set) == "ml_g"] = "ml_l"
        }
      }

      assert_list(tune_settings)
      if (test_names(names(tune_settings), must.include = "measure") && !is.null(tune_settings$measure)) {
        assert_list(tune_settings$measure)
        if (exists("ml_g", where = tune_settings$measure) && !exists("ml_l", where = tune_settings$measure)) {
          warning(paste0(
            "Learner ml_g was renamed to ml_l. ",
            "Please adapt the name in tune_settings$measure accordingly. ",
            "The provided tune_settings$measure for ml_g is used for ml_l. ",
            "The redirection will be removed in a future version."),
          call. = FALSE)
          names(tune_settings$measure)[names(tune_settings$measure) == "ml_g"] = "ml_l"
        }
      }

      super$tune(param_set, tune_settings, tune_on_folds)
    }
  ),
  private = list(
    n_nuisance = 2,
    initialize_ml_nuisance_params = function() {
      nuisance = vector("list", self$data$n_treat)
      names(nuisance) = self$data$d_cols
      private$params_ = list(
        "ml_l" = nuisance,
        "ml_m" = nuisance)
      if (exists("ml_g", where = private$learner_)) {
        private$params_[["ml_g"]] = nuisance
      }
      invisible(self)
    },

    nuisance_est = function(smpls, ...) {

      l_hat = dml_cv_predict(self$learner$ml_l,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$y_col,
        self$data$data_model,
        nuisance_id = "nuis_l",
        smpls = smpls,
        est_params = self$get_params("ml_l"),
        return_train_preds = FALSE,
        task_type = private$task_type$ml_l,
        fold_specific_params = private$fold_specific_params)

      m_hat = dml_cv_predict(self$learner$ml_m,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$treat_col,
        self$data$data_model,
        nuisance_id = "nuis_m",
        smpls = smpls,
        est_params = self$get_params("ml_m"),
        return_train_preds = FALSE,
        task_type = private$task_type$ml_m,
        fold_specific_params = private$fold_specific_params)

      d = self$data$data_model[[self$data$treat_col]]
      y = self$data$data_model[[self$data$y_col]]

      g_hat = list(preds = NULL, models = NULL)
      if (exists("ml_g", where = private$learner_)) {
        # get an initial estimate for theta using the partialling out score
        psi_a = -(d - m_hat$preds) * (d - m_hat$preds)
        psi_b = (d - m_hat$preds) * (y - l_hat$preds)
        theta_initial = -mean(psi_b, na.rm = TRUE) / mean(psi_a, na.rm = TRUE)

        data_aux = data.table(self$data$data_model,
          "y_minus_theta_d" = y - theta_initial * d)

        g_hat = dml_cv_predict(self$learner$ml_g,
          c(self$data$x_cols, self$data$other_treat_cols),
          "y_minus_theta_d",
          data_aux,
          nuisance_id = "nuis_g",
          smpls = smpls,
          est_params = self$get_params("ml_g"),
          return_train_preds = FALSE,
          task_type = private$task_type$ml_g,
          fold_specific_params = private$fold_specific_params)
      }

      res = private$score_elements(
        y, d, l_hat$preds, m_hat$preds, g_hat$preds,
        smpls)
      res$preds = list(
        "ml_l" = l_hat$preds,
        "ml_m" = m_hat$preds,
        "ml_g" = g_hat$preds)
      res$models = list(
        "ml_l" = l_hat$models,
        "ml_m" = m_hat$models,
        "ml_g" = g_hat$models)
      return(res)
    },
    score_elements = function(y, d, l_hat, m_hat, g_hat, smpls) {
      v_hat = d - m_hat
      u_hat = y - l_hat
      v_hatd = v_hat * d

      if (is.character(self$score)) {
        if (self$score == "IV-type") {
          psi_a = -v_hatd
          psi_b = v_hat * (y - g_hat)
        } else if (self$score == "partialling out") {
          psi_a = -v_hat * v_hat
          psi_b = v_hat * u_hat
        }
        psis = list(
          psi_a = psi_a,
          psi_b = psi_b)
      } else if (is.function(self$score)) {
        psis = self$score(
          y = y, d = d,
          l_hat = l_hat, m_hat = m_hat, g_hat = g_hat,
          smpls = smpls)
      }
      return(psis)
    },
    nuisance_tuning = function(smpls, param_set, tune_settings,
      tune_on_folds, ...) {

      if (!tune_on_folds) {
        data_tune_list = list(self$data$data_model)
      } else {
        data_tune_list = lapply(smpls$train_ids, function(x) {
          extract_training_data(self$data$data_model, x)
        })
      }

      tuning_result_l = dml_tune(self$learner$ml_l,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$y_col, data_tune_list,
        nuisance_id = "nuis_l",
        param_set$ml_l, tune_settings,
        tune_settings$measure$ml_l,
        private$task_type$ml_l)

      tuning_result_m = dml_tune(self$learner$ml_m,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$treat_col, data_tune_list,
        nuisance_id = "nuis_m",
        param_set$ml_m, tune_settings,
        tune_settings$measure$ml_m,
        private$task_type$ml_m)

      if (exists("ml_g", where = private$learner_)) {
        if (tune_on_folds) {
          params_l = tuning_result_l$params
          params_m = tuning_result_m$params
        } else {
          params_l = tuning_result_l$params[[1]]
          params_m = tuning_result_m$params[[1]]
        }
        l_hat = dml_cv_predict(self$learner$ml_l,
          c(self$data$x_cols, self$data$other_treat_cols),
          self$data$y_col,
          self$data$data_model,
          nuisance_id = "nuis_l",
          smpls = smpls,
          est_params = params_l,
          return_train_preds = FALSE,
          task_type = private$task_type$ml_l,
          fold_specific_params = private$fold_specific_params)

        m_hat = dml_cv_predict(self$learner$ml_m,
          c(self$data$x_cols, self$data$other_treat_cols),
          self$data$treat_col,
          self$data$data_model,
          nuisance_id = "nuis_m",
          smpls = smpls,
          est_params = params_m,
          return_train_preds = FALSE,
          task_type = private$task_type$ml_m,
          fold_specific_params = private$fold_specific_params)

        d = self$data$data_model[[self$data$treat_col]]
        y = self$data$data_model[[self$data$y_col]]

        psi_a = -(d - m_hat$preds) * (d - m_hat$preds)
        psi_b = (d - m_hat$preds) * (y - l_hat$preds)
        theta_initial = -mean(psi_b, na.rm = TRUE) / mean(psi_a, na.rm = TRUE)

        data_aux = data.table(self$data$data_model,
          "y_minus_theta_d" = y - theta_initial * d)

        if (!tune_on_folds) {
          data_aux_tune_list = list(data_aux)
        } else {
          data_aux_tune_list = lapply(smpls$train_ids, function(x) {
            extract_training_data(data_aux, x)
          })
        }

        tuning_result_g = dml_tune(self$learner$ml_g,
          c(self$data$x_cols, self$data$other_treat_cols),
          "y_minus_theta_d", data_aux_tune_list,
          nuisance_id = "nuis_g",
          param_set$ml_g, tune_settings,
          tune_settings$measure$ml_g,
          private$task_type$ml_g)
        tuning_result = list(
          "ml_l" = list(tuning_result_l, params = tuning_result_l$params),
          "ml_m" = list(tuning_result_m, params = tuning_result_m$params),
          "ml_g" = list(tuning_result_g, params = tuning_result_g$params))
      } else {
        tuning_result = list(
          "ml_l" = list(tuning_result_l, params = tuning_result_l$params),
          "ml_m" = list(tuning_result_m, params = tuning_result_m$params))
      }

      return(tuning_result)
    },
    check_score = function(score) {
      assert(
        check_character(score),
        check_class(score, "function"))
      if (is.character(score)) {
        valid_score = c("IV-type", "partialling out")
        assertChoice(score, valid_score)
      }
      return()
    },
    check_data = function(obj_dml_data) {
      if (!is.null(obj_dml_data$z_cols)) {
        stop(paste(
          "Incompatible data.\n", paste(obj_dml_data$z_cols, collapse = ", "),
          "has been set as instrumental variable(s).\n",
          "To fit a partially linear IV regression model use",
          "DoubleMLPLIV instead of DoubleMLPLR."))
      }
      return()
    }
  )
)
