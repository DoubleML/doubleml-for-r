#' @title Double machine learning for sample selection models
#'
#' @description
#' Double machine learning for sample selection models.
#'
#' @format [R6::R6Class] object inheriting from [DoubleML].
#'
#' @family DoubleML
#'
#' @usage NULL
#' @examples
#' \donttest{
#' library(DoubleML)
#' library(mlr3)
#' library(mlr3learners)
#' library(data.table)
#' set.seed(2)
#' ml_g = lrn("regr.ranger",
#'   num.trees = 100, mtry = 20,
#'   min.node.size = 2, max.depth = 5)
#' ml_m = lrn("classif.ranger",
#'   num.trees = 100, mtry = 20,
#'   min.node.size = 2, max.depth = 5)
#' ml_pi = lrn("classif.ranger",
#'   num.trees = 100, mtry = 20,
#'   min.node.size = 2, max.depth = 5)
#'
#' n_obs = 2000
#' df = make_ssm_data(n_obs = n_obs, mar = TRUE, return_type = "data.table")
#' dml_data = DoubleMLData$new(df, y_col = "y", d_cols = "d", s_col = "s")
#' dml_ssm = DoubleMLSSM$new(dml_data, ml_g, ml_m, ml_pi, score = "missing-at-random")
#' dml_ssm$fit()
#' print(dml_ssm)
#' }
#' \dontrun{
#' library(DoubleML)
#' library(mlr3)
#' library(mlr3learners)
#' library(mlr3tuning)
#' library(data.table)
#' set.seed(2)
#' ml_g = lrn("regr.rpart")
#' ml_m = lrn("classif.rpart")
#' ml_pi = lrn("classif.rpart")
#' dml_data = make_ssm_data(n_obs = n_obs, mar = TRUE)
#' dml_ssm = DoubleMLSSM$new(dml_data, ml_g = ml_g, ml_m = ml_m, ml_pi = ml_pi,
#'   score = "missing-at-random")
#'
#' param_grid = list(
#'   "ml_g" = paradox::ps(
#'     cp = paradox::p_dbl(lower = 0.01, upper = 0.02),
#'     minsplit = paradox::p_int(lower = 1, upper = 2)),
#'   "ml_m" = paradox::ps(
#'     cp = paradox::p_dbl(lower = 0.01, upper = 0.02),
#'     minsplit = paradox::p_int(lower = 1, upper = 2)),
#'   "ml_pi" = paradox::ps(
#'     cp = paradox::p_dbl(lower = 0.01, upper = 0.02),
#'     minsplit = paradox::p_int(lower = 1, upper = 2)))
#'
#' # minimum requirements for tune_settings
#' tune_settings = list(
#'   terminator = mlr3tuning::trm("evals", n_evals = 5),
#'   algorithm = mlr3tuning::tnr("grid_search", resolution = 5))
#'
#' dml_ssm$tune(param_set = param_grid, tune_settings = tune_settings)
#' dml_ssm$fit()
#' dml_ssm$summary()
#' }
#' @export
DoubleMLSSM = R6Class("DoubleMLSSM",
  inherit = DoubleML,

  active = list(
    #' @field trimming_rule (`character(1)`) \cr
    #' A `character(1)` specifying the trimming approach.
    trimming_rule = function(value) {
      if (missing(value)) {
        return(private$trimming_rule_)
      } else {
        stop("can't set field trimming_rule")
      }
    },
    #' @field trimming_threshold (`numeric(1)`) \cr
    #' The threshold used for timming.
    trimming_threshold = function(value) {
      if (missing(value)) {
        return(private$trimming_threshold_)
      } else {
        stop("can't set field trimming_threshold")
      }
    }
  ),

  public = list(
    #' @description
    #' Creates a new instance of this R6 class.
    #'
    #' @param data (`DoubleMLData`) \cr
    #' The `DoubleMLData` object providing the data and specifying the
    #' variables of the causal model.
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
    #' `ml_g` refers to the nuisance function \eqn{g_0(S,D,X) = E[Y|S,D,X]}.
    #'
    #' @param ml_m ([`LearnerRegr`][mlr3::LearnerRegr],
    #' [`LearnerClassif`][mlr3::LearnerClassif], [`Learner`][mlr3::Learner],
    #' `character(1)`) \cr
    #'  A learner of the class [`LearnerClassif`][mlr3::LearnerClassif], which is
    #' available from [mlr3](https://mlr3.mlr-org.com/index.html) or its
    #' extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or
    #' [mlr3extralearners](https://mlr3extralearners.mlr-org.com/).
    #' Alternatively, a [`Learner`][mlr3::Learner] object with public field
    #' `task_type = "classif"` can be passed, for example of class
    #' [`GraphLearner`][mlr3pipelines::GraphLearner]. The learner can possibly
    #' be passed with specified parameters, for example
    #' `lrn("classif.cv_glmnet", s = "lambda.min")`. \cr
    #' `ml_m` refers to the nuisance function \eqn{m_0(X) = Pr[D=1|X]}.
    #'
    #' @param ml_pi ([`LearnerClassif`][mlr3::LearnerClassif],
    #' [`Learner`][mlr3::Learner], `character(1)`) \cr
    #' A learner of the class [`LearnerClassif`][mlr3::LearnerClassif], which is
    #' available from [mlr3](https://mlr3.mlr-org.com/index.html) or its
    #' extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or
    #' [mlr3extralearners](https://mlr3extralearners.mlr-org.com/).
    #' Alternatively, a [`Learner`][mlr3::Learner] object with public field
    #' `task_type = "classif"` can be passed, for example of class
    #' [`GraphLearner`][mlr3pipelines::GraphLearner]. The learner can possibly
    #' be passed with specified parameters, for example
    #' `lrn("classif.cv_glmnet", s = "lambda.min")`. \cr
    #' `ml_pi` refers to the nuisance function \eqn{pi_0(D,X) = Pr[S=1|D,X]}.
    #'
    #' @param n_folds (`integer(1)`)\cr
    #' Number of folds. Default is `5`.
    #'
    #' @param n_rep (`integer(1)`) \cr
    #' Number of repetitions for the sample splitting. Default is `1`.
    #'
    #' @param score (`character(1)`, `function()`) \cr
    #' A `character(1)` (`"missing-at-random"` or `"nonignorable"`) specifying
    #' the score function. Default is `"missing-at-random"`.
    #'
    #' @param normalize_ipw (`logical(1)`) \cr
    #' Indicates whether the inverse probability weights are normalized. Default is `FALSE`.
    #'
    #' @param trimming_rule (`character(1)`) \cr
    #' A `character(1)` (`"truncate"` is the only choice) specifying the
    #' trimming approach. Default is `"truncate"`.
    #'
    #' @param trimming_threshold (`numeric(1)`) \cr
    #' The threshold used for timming. Default is `1e-12`.
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
      ml_g,
      ml_pi,
      ml_m,
      n_folds = 5,
      n_rep = 1,
      score = "missing-at-random",
      normalize_ipw = FALSE,
      trimming_rule = "truncate",
      trimming_threshold = 1e-12,
      dml_procedure = "dml2",
      draw_sample_splitting = TRUE,
      apply_cross_fitting = TRUE) {

      super$initialize_double_ml(
        data,
        n_folds,
        n_rep,
        score,
        dml_procedure,
        draw_sample_splitting,
        apply_cross_fitting)

      private$normalize_ipw = normalize_ipw

      private$check_data(self$data)
      private$check_score(self$score)
      ml_g = private$assert_learner(ml_g, "ml_g", Regr = TRUE, Classif = TRUE)
      ml_pi = private$assert_learner(ml_pi, "ml_pi", Regr = FALSE, Classif = TRUE)
      ml_m = private$assert_learner(ml_m, "ml_m", Regr = FALSE, Classif = TRUE)

      private$learner_ = list(
        "ml_g" = ml_g,
        "ml_pi" = ml_pi,
        "ml_m" = ml_m)

      private$initialize_ml_nuisance_params()

      private$trimming_rule_ = trimming_rule
      private$trimming_threshold_ = trimming_threshold
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
    #' [mlr3 book](https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html).
    #'
    #' @param param_set (named `list()`) \cr
    #' A named `list` with a parameter grid for each nuisance model/learner
    #' (see method `learner_names()`). The parameter grid must be an object of
    #' class [ParamSet][paradox::ParamSet].
    #'
    #' @param tune_settings (named `list()`) \cr
    #' A named `list()` with arguments passed to the hyperparameter-tuning with
    #' [mlr3tuning](https://mlr3tuning.mlr-org.com/) to set up
    #' [TuningInstance][mlr3tuning::TuningInstanceBatchSingleCrit] objects.
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
      assert_list(tune_settings)
      if (test_names(names(tune_settings), must.include = "measure") && !is.null(tune_settings$measure)) {
        assert_list(tune_settings$measure)
      }

      super$tune(param_set, tune_settings, tune_on_folds)
    }
  ),


  private = list(
    n_nuisance = 4,
    normalize_ipw = FALSE,
    trimming_rule_ = NULL,
    trimming_threshold_ = NULL,
    initialize_ml_nuisance_params = function() {
      nuisance = vector("list", self$data$n_treat)
      names(nuisance) = self$data$d_cols
      private$params_ = list(
        "ml_g_d0" = nuisance,
        "ml_g_d1" = nuisance,
        "ml_pi" = nuisance,
        "ml_m" = nuisance)
      invisible(self)
    },

    nuisance_est = function(smpls, ...) {

      if (self$score == "missing-at-random") {

        smpls_d_s = get_cond_samples_2d(smpls, self$data$data_model[[self$data$treat_col]],
          self$data$data_model[[self$data$s_col]])
        smpls_d0_s1 = smpls_d_s$smpls_01
        smpls_d1_s1 = smpls_d_s$smpls_11

        pi_hat = dml_cv_predict(self$learner$ml_pi,
          c(self$data$x_cols, self$data$other_treat_cols, self$data$d_cols),
          self$data$s_col,
          self$data$data_model,
          nuisance_id = "nuis_pi",
          smpls = smpls,
          est_params = self$get_params("ml_pi"),
          return_train_preds = FALSE,
          task_type = private$task_type$ml_pi,
          fold_specific_params = private$fold_specific_params)

        m_hat = dml_cv_predict(self$learner$ml_m,
          c(self$data$x_cols, self$data$other_treat_cols),
          self$data$d_cols,
          self$data$data_model,
          nuisance_id = "nuis_m",
          smpls = smpls,
          est_params = self$get_params("ml_m"),
          return_train_preds = FALSE,
          task_type = private$task_type$ml_m,
          fold_specific_params = private$fold_specific_params)

        g_hat_d0 = dml_cv_predict(self$learner$ml_g,
          c(self$data$x_cols, self$data$other_treat_cols),
          self$data$y_col,
          self$data$data_model,
          nuisance_id = "nuis_g_d0",
          smpls = smpls_d0_s1,
          est_params = self$get_params("ml_g_d0"),
          return_train_preds = FALSE,
          task_type = private$task_type$ml_g,
          fold_specific_params = private$fold_specific_params)

        g_hat_d1 = dml_cv_predict(self$learner$ml_g,
          c(self$data$x_cols, self$data$other_treat_cols),
          self$data$y_col,
          self$data$data_model,
          nuisance_id = "nuis_g_d1",
          smpls = smpls_d1_s1,
          est_params = self$get_params("ml_g_d1"),
          return_train_preds = FALSE,
          task_type = private$task_type$ml_g,
          fold_specific_params = private$fold_specific_params)

      } else { # nonignorable

        pi_hat = list(preds = NULL, models = NULL)
        m_hat = list(preds = NULL, models = NULL)
        g_hat_d0 = list(preds = NULL, models = NULL)
        g_hat_d1 = list(preds = NULL, models = NULL)

        preds_pi_hat = numeric(nrow(self$data$data))
        preds_m_hat = numeric(nrow(self$data$data))
        preds_g_hat_d0 = numeric(nrow(self$data$data))
        preds_g_hat_d1 = numeric(nrow(self$data$data))

        strata = self$data$data$d + 2 * self$data$data$s
        self$data$data[, strata := strata]


        for (i_fold in 1:(self$n_folds)) {

          train_inds = smpls$train_ids[[i_fold]]
          test_inds = smpls$test_ids[[i_fold]]


          # split train_inds into 2 sets
          dummy_train_task = Task$new("dummy", "regr", self$data$data)
          dummy_train_task$set_col_roles("strata", c("target", "stratum"))
          dummy_train_resampling = rsmp("holdout", ratio = 0.5)$instantiate(dummy_train_task$filter(train_inds))
          train1 = dummy_train_resampling$train_set(1)
          train2 = dummy_train_resampling$test_set(1)

          # pi_hat_prelim and pi_hat
          task_pred_pi_hat = initiate_task(
            id = "nuis_pi",
            data = self$data$data_model,
            target = self$data$s_col,
            select_cols = c(self$data$x_cols, self$data$other_treat_cols, self$data$d_cols, self$data$z_cols),
            task_type = private$task_type$ml_pi)

          ml_learner_pi_hat = initiate_learner(
            learner = self$learner$ml_pi,
            task_type = private$task_type$ml_pi,
            params = self$get_params("ml_pi"),
            return_train_preds = FALSE)

          resampling_smpls_pi_hat = rsmp("custom")$instantiate(
            task_pred_pi_hat, list(train1), list(1:nrow(self$data$data)))

          resampling_pred_pi_hat = resample(task_pred_pi_hat, ml_learner_pi_hat, resampling_smpls_pi_hat, store_models = TRUE)

          pi_hat$models[[i_fold]] = resampling_pred_pi_hat$score()$learner

          preds_pi_hat_prelim = extract_prediction(resampling_pred_pi_hat, private$task_type$ml_pi, n_obs = nrow(self$data$data))

          preds_pi_hat[test_inds] = preds_pi_hat_prelim[test_inds]

          # add pi_hat_prelim
          self$data$data_model[, pi_hat_prelim := preds_pi_hat_prelim]

          # m_hat
          task_pred_m_hat = initiate_task(
            id = "nuis_m",
            data = self$data$data_model,
            target = self$data$d_cols,
            select_cols = c(self$data$x_cols, self$data$other_treat_cols, "pi_hat_prelim"),
            task_type = private$task_type$ml_m)

          ml_learner_m_hat = initiate_learner(
            learner = self$learner$ml_m,
            task_type = private$task_type$ml_pi,
            params = self$get_params("ml_m"),
            return_train_preds = FALSE)

          resampling_smpls_m_hat = rsmp("custom")$instantiate(
            task_pred_m_hat, list(train2), list(test_inds))

          resampling_pred_m_hat = resample(task_pred_m_hat, ml_learner_m_hat, resampling_smpls_m_hat, store_models = TRUE)

          m_hat$models[[i_fold]] = resampling_pred_m_hat$score()$learner

          preds_m_hat[test_inds] = extract_prediction(resampling_pred_m_hat, private$task_type$ml_m, n_obs = nrow(self$data$data))[test_inds]


          # g_hat_d0
          d = self$data$data_model[[self$data$treat_col]]
          s = self$data$data_model[[self$data$s_col]]
          train2_d0_s1 = train2[d[train2] == 0 & s[train2] == 1]

          task_pred_g_hat_d0 = initiate_task(
            id = "nuis_g_d0",
            data = self$data$data_model,
            target = self$data$y_col,
            select_cols = c(self$data$x_cols, self$data$other_treat_cols, "pi_hat_prelim"),
            task_type = private$task_type$ml_g)

          ml_learner_g_hat_d0 = initiate_learner(
            learner = self$learner$ml_g,
            task_type = private$task_type$ml_g,
            params = self$get_params("ml_g_d0"),
            return_train_preds = FALSE)

          resampling_smpls_g_hat_d0 = rsmp("custom")$instantiate(
            task_pred_g_hat_d0, list(train2_d0_s1), list(test_inds))

          resampling_pred_g_hat_d0 = resample(task_pred_g_hat_d0, ml_learner_g_hat_d0, resampling_smpls_g_hat_d0, store_models = TRUE)

          g_hat_d0$models[[i_fold]] = resampling_pred_g_hat_d0$score()$learner

          preds_g_hat_d0[test_inds] = extract_prediction(resampling_pred_g_hat_d0, private$task_type$ml_g, n_obs = nrow(self$data$data))[test_inds]

          # g_hat_d1
          train2_d1_s1 = train2[d[train2] == 1 & s[train2] == 1]

          task_pred_g_hat_d1 = initiate_task(
            id = "nuis_g_d1",
            data = self$data$data_model,
            target = self$data$y_col,
            select_cols = c(self$data$x_cols, self$data$other_treat_cols, "pi_hat_prelim"),
            task_type = private$task_type$ml_g)

          ml_learner_g_hat_d1 = initiate_learner(
            learner = self$learner$ml_g,
            task_type = private$task_type$ml_g,
            params = self$get_params("ml_g_d1"),
            return_train_preds = FALSE)

          resampling_smpls_g_hat_d1 = rsmp("custom")$instantiate(
            task_pred_g_hat_d1, list(train2_d1_s1), list(test_inds))

          resampling_pred_g_hat_d1 = resample(task_pred_g_hat_d1, ml_learner_g_hat_d1, resampling_smpls_g_hat_d1, store_models = TRUE)

          g_hat_d1$models[[i_fold]] = resampling_pred_g_hat_d1$score()$learner

          preds_g_hat_d1[test_inds] = extract_prediction(resampling_pred_g_hat_d1, private$task_type$ml_g, n_obs = nrow(self$data$data))[test_inds]

        }

        pi_hat$preds = preds_pi_hat
        m_hat$preds = preds_m_hat
        g_hat_d0$preds = preds_g_hat_d0
        g_hat_d1$preds = preds_g_hat_d1

        self$data$data[, strata := NULL]
        self$data$data_model[, pi_hat_prelim := NULL]

      }


      d = self$data$data_model[[self$data$treat_col]]
      y = self$data$data_model[[self$data$y_col]]
      s = self$data$data_model[[self$data$s_col]]

      res = private$score_elements(
        y, d, s, pi_hat$preds, m_hat$preds, g_hat_d0$preds, g_hat_d1$preds,
        smpls)
      res$preds = list(
        "ml_pi" = pi_hat$preds,
        "ml_m" = m_hat$preds,
        "ml_g_d0" = g_hat_d0$preds,
        "ml_g_d1" = g_hat_d1$preds)
      res$models = list(
        "ml_pi" = pi_hat$models,
        "ml_m" = m_hat$models,
        "ml_g_d0" = g_hat_d0$models,
        "ml_g_d1" = g_hat_d1$models)
      return(res)
    },


    score_elements = function(y, d, s, pi_hat, m_hat, g_hat_d0, g_hat_d1, smpls) {

      dtreat = (d == 1)
      dcontrol = (d == 0)

      if (self$trimming_rule == "truncate" & self$trimming_threshold > 0) {
        m_hat[m_hat < self$trimming_threshold] = self$trimming_threshold
        m_hat[m_hat > 1 - self$trimming_threshold] = 1 - self$trimming_threshold
      }

      psi_a = -1

      if (private$normalize_ipw == TRUE) {
        weight_treat = sum(dtreat) / sum((dtreat * s) / (pi_hat * m_hat))
        weight_control = sum(dcontrol) / sum((dcontrol * s) / (pi_hat * (1 - m_hat)))

        psi_b1 = weight_treat * ((dtreat * s * (y - g_hat_d1)) / (m_hat * pi_hat)) + g_hat_d1
        psi_b0 = weight_control * ((dcontrol * s * (y - g_hat_d0)) / ((1 - m_hat) * pi_hat)) + g_hat_d0

      } else {
        psi_b1 = (dtreat * s * (y - g_hat_d1)) / (m_hat * pi_hat) + g_hat_d1
        psi_b0 = (dcontrol * s * (y - g_hat_d0)) / ((1 - m_hat) * pi_hat) + g_hat_d0

      }

      psi_b = psi_b1 - psi_b0

      psis = list(
        psi_a = psi_a,
        psi_b = psi_b)

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

      indx_d0_s1 = lapply(data_tune_list, function(x) x[[self$data$d_cols]] == 0 & x[[self$data$s_col]] == 1)
      indx_d1_s1 = lapply(data_tune_list, function(x) x[[self$data$d_cols]] == 1 & x[[self$data$s_col]] == 1)
      data_tune_list_d0_s1 = lapply(
        seq_len(length(data_tune_list)),
        function(x) data_tune_list[[x]][indx_d0_s1[[x]], ])
      data_tune_list_d1_s1 = lapply(
        seq_len(length(data_tune_list)),
        function(x) data_tune_list[[x]][indx_d1_s1[[x]], ])

      tuning_result_pi = dml_tune(self$learner$ml_pi,
        c(self$data$x_cols, self$data$other_treat_cols, self$data$d_cols, self$data$z_cols),
        self$data$s_col, data_tune_list,
        nuisance_id = "nuis_pi",
        param_set$ml_pi, tune_settings,
        tune_settings$measure$ml_pi,
        private$task_type$ml_pi)

      tuning_result_m = dml_tune(self$learner$ml_m,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$d_cols, data_tune_list,
        nuisance_id = "nuis_m",
        param_set$ml_m, tune_settings,
        tune_settings$measure$ml_m,
        private$task_type$ml_m)

      tuning_result_g_d0 = dml_tune(self$learner$ml_g,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$y_col, data_tune_list_d0_s1,
        nuisance_id = "nuis_g_d0",
        param_set$ml_g, tune_settings,
        tune_settings$measure$ml_g,
        private$task_type$ml_g)

      tuning_result_g_d1 = dml_tune(self$learner$ml_g,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$y_col, data_tune_list_d1_s1,
        nuisance_id = "nuis_g_d1",
        param_set$ml_g, tune_settings,
        tune_settings$measure$ml_g,
        private$task_type$ml_g)


      tuning_result = list(
        "ml_pi" = list(tuning_result_pi, params = tuning_result_pi$params),
        "ml_m" = list(tuning_result_m, params = tuning_result_m$params),
        "ml_g_d0" = list(tuning_result_g_d0, params = tuning_result_g_d0$params),
        "ml_g_d1" = list(tuning_result_g_d1, params = tuning_result_g_d1$params))

      return(tuning_result)
    },

    check_score = function(score) {
      assert(
        check_character(score),
        check_class(score, "function"))
      if (is.character(score)) {
        valid_score = c("missing-at-random", "nonignorable")
        assertChoice(score, valid_score)
      }
      return()
    },

    check_data = function(obj_dml_data) {
      if (!is.null(obj_dml_data$z_cols) && self$score == "missing-at-random") {
        warn_msg = paste(
          "A variable has been set as instrumental variable(s).\n",
          "You are estimating the effect under the assumption of data missing at random.",
          "Instrumental variables will not be used in estimation."
        )
        warning(warn_msg)
      }
      if (is.null(obj_dml_data$z_cols) && self$score == "nonignorable") {
        err_msg = paste(
          "Sample selection by nonignorable nonresponse was set but instrumental variable is NULL.\n",
          "To estimate treatment effect under nonignorable nonresponse,",
          "specify an instrument for the selection variable."
        )
        stop(err_msg)
      }
      return()
    }
  )
)
