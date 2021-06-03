#' @title Double machine learning for interactive regression models
#'
#' @description
#' Double machine learning for interactive regression models.
#'
#' @format [R6::R6Class] object inheriting from [DoubleML].
#'
#' @family DoubleML
#'
#' @details
#' Interactive regression (IRM) models take the form
#'
#' \eqn{Y = g_0(D,X) + U},
#'
#' \eqn{D = m_0(X) + V},
#'
#' with \eqn{E[U|X,D]=0} and \eqn{E[V|X] = 0}. \eqn{Y} is the outcome variable
#' and \eqn{D \in \{0,1\}} is the binary treatment variable. We consider
#' estimation of the average treamtent effects when treatment effects are
#' fully heterogeneous. Target parameters of interest in this model are the
#' average treatment effect (ATE),
#'
#' \eqn{\theta_0 = E[g_0(1,X) - g_0(0,X)]}
#'
#' and the average treament effect on the treated (ATTE),
#'
#' \eqn{\theta_0 = E[g_0(1,X) - g_0(0,X)|D=1]}.
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
#' ml_g = lrn("regr.ranger",
#'   num.trees = 100, mtry = 20,
#'   min.node.size = 2, max.depth = 5)
#' ml_m = lrn("classif.ranger",
#'   num.trees = 100, mtry = 20,
#'   min.node.size = 2, max.depth = 5)
#' obj_dml_data = make_irm_data(theta = 0.5)
#' dml_irm_obj = DoubleMLIRM$new(obj_dml_data, ml_g, ml_m)
#' dml_irm_obj$fit()
#' dml_irm_obj$summary()
#' }
#' \dontrun{
#' library(DoubleML)
#' library(mlr3)
#' library(mlr3learners)
#' library(mlr3uning)
#' library(data.table)
#' set.seed(2)
#' ml_g = lrn("regr.rpart")
#' ml_m = lrn("classif.rpart")
#' obj_dml_data = make_irm_data(theta = 0.5)
#' dml_irm_obj = DoubleMLIRM$new(obj_dml_data, ml_g, ml_m)
#'
#' param_grid = list(
#'   "ml_g" = paradox::ParamSet$new(list(
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
#' dml_irm_obj$tune(param_set = param_grid, tune_settings = tune_settings)
#' dml_irm_obj$fit()
#' dml_irm_obj$summary()
#' }
#'
#' @export
DoubleMLIRM = R6Class("DoubleMLIRM",
  inherit = DoubleML, 
  active = list(
    #' @field trimming_rule (`character(1)`) \cr
    #' A `character(1)` specifying the trimming approach.
    trimming_rule = function(value) {
      if (missing(value)) return(private$trimming_rule_)
      else stop("can't set field trimming_rule")
    },

    #' @field trimming_threshold (`numeric(1)`) \cr
    #' The threshold used for timming.
    trimming_threshold = function(value) {
      if (missing(value)) return(private$trimming_threshold_)
      else stop("can't set field trimming_threshold")
    }),

  public = list(
    #' @description
    #' Creates a new instance of this R6 class.
    #'
    #' @param data (`DoubleMLData`) \cr
    #' The `DoubleMLData` object providing the data and specifying the variables
    #' of the causal model.
    #'
    #' @param ml_g ([`LearnerRegr`][mlr3::LearnerRegr], `character(1)`) \cr
    #' An object of the class [mlr3 regression learner][mlr3::LearnerRegr] to
    #' pass a learner, possibly with specified parameters, for example
    #' `lrn("regr.cv_glmnet", s = "lambda.min")`.
    #' Alternatively, a `character(1)` specifying the name of a
    #' [mlr3 regression learner][mlr3::LearnerRegr] that is available in
    #' [mlr3](https://mlr3.mlr-org.com/index.html) or its extension packages
    #' [mlr3learners](https://mlr3learners.mlr-org.com/) or
    #' [mlr3extralearners](https://mlr3extralearners.mlr-org.com/), for example
    #' `"regr.cv_glmnet"`.  \cr
    #' `ml_g` refers to the nuisance function \eqn{g_0(X) = E[Y|X,D]}.
    #'
    #' @param ml_m ([`LearnerClassif`][mlr3::LearnerClassif], `character(1)`) \cr
    #' An object of the class
    #' [mlr3 classification learner][mlr3::LearnerClassif] to pass a learner,
    #' possibly with specified parameters, for example
    #' `lrn("classif.cv_glmnet", s = "lambda.min")`.
    #' Alternatively, a `character(1)` specifying the name of a
    #' [mlr3 classification learner][mlr3::LearnerClassif] that is available
    #' in [mlr3](https://mlr3.mlr-org.com/index.html) or its extension packages
    #' [mlr3learners](https://mlr3learners.mlr-org.com/) or
    #' [mlr3extralearners](https://mlr3extralearners.mlr-org.com/),
    #' for example `"classif.cv_glmnet"`. \cr
    #' `ml_m` refers to the nuisance function \eqn{m_0(X) = E[D|X]}.
    #'
    #' @param n_folds (`integer(1)`)\cr
    #' Number of folds. Default is `5`.
    #'
    #' @param n_rep (`integer(1)`) \cr
    #' Number of repetitions for the sample splitting. Default is `1`.
    #'
    #' @param score (`character(1)`, `function()`) \cr
    #' A `character(1)` (`"ATE"` or `ATTE`) or a `function()` specifying the
    #' score function. If a `function()`
    #' is provided, it must be of the form
    #' `function(y, d, g0_hat, g1_hat, m_hat, smpls)` and the returned output
    #' must be a named `list()` with elements `psi_a` and `psi_b`.
    #' Default is `"ATE"`.
    #'
    #' @param trimming_rule (`character(1)`) \cr
    #' A `character(1)` (`"truncate"` is the only choice) specifying the
    #' trimming approach. Default is `"truncate"`.
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
      ml_m,
      n_folds = 5,
      n_rep = 1,
      score = "ATE",
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

      private$check_data(self$data)
      private$check_score(self$score)
      private$learner_class = list(
        "ml_g" = NULL,
        "ml_m" = NULL)
      ml_g = private$assert_learner(ml_g, "ml_g", Regr = TRUE, Classif = FALSE)
      ml_m = private$assert_learner(ml_m, "ml_m", Regr = FALSE, Classif = TRUE)

      private$learner_ = list(
        "ml_g" = ml_g,
        "ml_m" = ml_m)
      private$initialize_ml_nuisance_params()

      private$trimming_rule_ = trimming_rule
      private$trimming_threshold_ = trimming_threshold
    }
  ),
  private = list(
    trimming_rule_ = NULL,
    trimming_threshold_ = NULL,
    n_nuisance = 2,
    initialize_ml_nuisance_params = function() {
      nuisance = vector("list", self$data$n_treat)
      names(nuisance) = self$data$d_cols
      private$params_ = list(
        "ml_g0" = nuisance,
        "ml_g1" = nuisance,
        "ml_m" = nuisance)
      invisible(self)
    },
    ml_nuisance_and_score_elements = function(smpls, ...) {

      cond_smpls = get_cond_samples(
        smpls,
        self$data$data_model[[self$data$treat_col]])

      m_hat = dml_cv_predict(self$learner$ml_m,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$treat_col,
        self$data$data_model,
        nuisance_id = "nuis_m",
        smpls = smpls,
        est_params = self$get_params("ml_m"),
        return_train_preds = FALSE,
        learner_class = private$learner_class$ml_m,
        fold_specific_params = private$fold_specific_params)

      g0_hat = dml_cv_predict(self$learner$ml_g,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$y_col,
        self$data$data_model,
        nuisance_id = "nuis_g0",
        smpls = cond_smpls$smpls_0,
        est_params = self$get_params("ml_g0"),
        return_train_preds = FALSE,
        learner_class = private$learner_class$ml_g,
        fold_specific_params = private$fold_specific_params)

      g1_hat = NULL
      if ((is.character(self$score) && self$score == "ATE") | is.function(self$score)) {
        g1_hat = dml_cv_predict(self$learner$ml_g,
          c(self$data$x_cols, self$data$other_treat_cols),
          self$data$y_col,
          self$data$data_model,
          nuisance_id = "nuis_g1",
          smpls = cond_smpls$smpls_1,
          est_params = self$get_params("ml_g1"),
          return_train_preds = FALSE,
          learner_class = private$learner_class$ml_g,
          fold_specific_params = private$fold_specific_params)
      }

      d = self$data$data_model[[self$data$treat_col]]
      y = self$data$data_model[[self$data$y_col]]

      res = private$score_elements(y, d, g0_hat, g1_hat, m_hat, smpls)
      res$preds = list(
        "ml_g0" = g0_hat,
        "ml_g1" = g1_hat,
        "ml_m" = m_hat)
      return(res)
    },
    score_elements = function(y, d, g0_hat, g1_hat, m_hat, smpls) {
      if (is.character(self$score) && self$score == "ATTE") {
        # fraction of treated for ATTE
        p_hat = vector("numeric", length = self$data$n_obs)
        for (i_fold in seq_len(length(smpls$test_ids))) {
          p_hat[smpls$test_ids[[i_fold]]] = mean(
            self$data$data_model[[self$data$treat_col]][smpls$test_ids[[i_fold]]])
        }
      }

      if (self$trimming_rule == "truncate" & self$trimming_threshold > 0) {
        m_hat[m_hat < self$trimming_threshold] = self$trimming_threshold
        m_hat[m_hat > 1 - self$trimming_threshold] = 1 - self$trimming_threshold
      }

      if (is.character(self$score)) {
        # compute residuals
        u0_hat = y - g0_hat
        if (self$score == "ATE") {
          u1_hat = y - g1_hat
          psi_b = g1_hat - g0_hat + d * (u1_hat) / m_hat -
            (1 - d) * u0_hat / (1 - m_hat)
          psi_a = rep(-1, self$data$n_obs)
        } else if (self$score == "ATTE") {
          psi_b = d * u0_hat / p_hat -
            m_hat * (1 - d) * u0_hat / (p_hat * (1 - m_hat))
          psi_a = -d / p_hat
        }
        psis = list(psi_a = psi_a, psi_b = psi_b)
      } else if (is.function(self$score)) {
        psis = self$score(y, d, g0_hat, g1_hat, m_hat, smpls)
      }
      return(psis)
    },
    ml_nuisance_tuning = function(smpls, param_set, tune_settings,
      tune_on_folds, ...) {

      if (!tune_on_folds) {
        data_tune_list = list(self$data$data_model)
      } else {
        data_tune_list = lapply(smpls$train_ids, function(x) {
          extract_training_data(self$data$data_model, x)
        })
      }
      # TODO: Use wrapper here
      indx_g0 = lapply(
        data_tune_list,
        function(x) x[[self$data$treat_col]] == 0)
      indx_g1 = lapply(
        data_tune_list,
        function(x) x[[self$data$treat_col]] == 1)
      data_tune_list_d0 = lapply(
        seq_len(length(data_tune_list)),
        function(x) data_tune_list[[x]][indx_g0[[x]], ])
      data_tune_list_d1 = lapply(
        seq_len(length(data_tune_list)),
        function(x) data_tune_list[[x]][indx_g1[[x]], ])

      tuning_result_m = dml_tune(self$learner$ml_m,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$treat_col,
        data_tune_list,
        nuisance_id = "nuis_m",
        param_set$ml_m, tune_settings,
        tune_settings$measure$ml_m,
        private$learner_class$ml_m)

      tuning_result_g0 = dml_tune(self$learner$ml_g,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$y_col,
        data_tune_list_d0,
        nuisance_id = "nuis_g0",
        param_set$ml_g, tune_settings,
        tune_settings$measure$ml_g,
        private$learner_class$ml_g)

      if (self$score == "ATE" | is.function(self$score)) {
        tuning_result_g1 = dml_tune(self$learner$ml_g,
          c(self$data$x_cols, self$data$other_treat_cols),
          self$data$y_col,
          data_tune_list_d1,
          nuisance_id = "nuis_g1",
          param_set$ml_g, tune_settings,
          tune_settings$measure$ml_g,
          private$learner_class$ml_g)
      } else {
        tuning_result_g1 = list(list(), "params" = list(list()))
      }

      tuning_result = list(
        "ml_g0" = list(tuning_result_g0, params = tuning_result_g0$params),
        "ml_g1" = list(tuning_result_g1, params = tuning_result_g1$params),
        "ml_m" = list(tuning_result_m, params = tuning_result_m$params))

      return(tuning_result)
    },
    check_score = function(score) {
      assert(
        check_character(score),
        check_class(score, "function"))
      if (is.character(score)) {
        valid_score = c("ATE", "ATTE")
        assertChoice(score, valid_score)
      }
      return()
    },
    check_data = function(obj_dml_data) {
      if (!is.null(obj_dml_data$z_cols)) {
        stop(paste(
          "Incompatible data.\n", paste(obj_dml_data$z_cols, collapse = ", "),
          "has been set as instrumental variable(s).\n",
          "To fit an interactive IV regression model use DoubleMLIIVM",
          "instead of DoubleMLIRM."))
      }
      one_treat = (obj_dml_data$n_treat == 1)
      err_msg = paste(
        "Incompatible data.\n",
        "To fit an IRM model with DoubleML",
        "exactly one binary variable with values 0 and 1",
        "needs to be specified as treatment variable.")
      if (one_treat) {
        binary_treat = test_integerish(obj_dml_data$data[[obj_dml_data$d_cols]],
                                       lower = 0, upper = 1)
        if (!(one_treat & binary_treat)) {
          stop(err_msg)
        }
      } else {
        stop(err_msg)
      }
      return()
    }
  )
)
