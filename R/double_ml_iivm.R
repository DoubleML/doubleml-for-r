#' @title Double machine learning for interactive IV regression models
#'
#' @description
#' Double machine learning for interactive IV regression models.
#'
#' @format [R6::R6Class] object inheriting from [DoubleML].
#'
#' @family DoubleML
#'
#' @details
#' Interactive IV regression (IIVM) models take the form
#'
#' \eqn{Y = \ell_0(D,X) + \zeta},
#'
#' \eqn{Z = m_0(X) + V},
#'
#' with \eqn{E[\zeta|X,Z]=0} and \eqn{E[V|X] = 0}. \eqn{Y} is the outcome
#' variable, \eqn{D \in \{0,1\}} is the binary treatment variable and
#' \eqn{Z \in \{0,1\}} is a binary instrumental variable. Consider the functions
#' \eqn{g_0}, \eqn{r_0} and \eqn{m_0}, where \eqn{g_0} maps the support of
#' \eqn{(Z,X)} to \eqn{R} and \eqn{r_0} and \eqn{m_0}, respectively, map the
#' support of \eqn{(Z,X)} and \eqn{X} to \eqn{(\epsilon, 1-\epsilon)} for some
#' \eqn{\epsilon \in (1, 1/2)}, such that
#'
#' \eqn{Y = g_0(Z,X) + \nu,}
#'
#' \eqn{D = r_0(Z,X) + U,}
#'
#' \eqn{Z = m_0(X) + V,}
#'
#' with \eqn{E[\nu|Z,X]=0}, \eqn{E[U|Z,X]=0} and \eqn{E[V|X]=0}. The target
#' parameter of interest in this model is the local average treatment effect
#' (LATE),
#'
#' \eqn{\theta_0 = \frac{E[g_0(1,X)] - E[g_0(0,X)]}{E[r_0(1,X)] - E[r_0(0,X)]}.}
#'
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
#' ml_r = ml_m$clone()
#' obj_dml_data = make_iivm_data(
#'   theta = 0.5, n_obs = 1000,
#'   alpha_x = 1, dim_x = 20)
#' dml_iivm_obj = DoubleMLIIVM$new(obj_dml_data, ml_g, ml_m, ml_r)
#' dml_iivm_obj$fit()
#' dml_iivm_obj$summary()
#' }
#'
#' \dontrun{
#' library(DoubleML)
#' library(mlr3)
#' library(mlr3learners)
#' library(mlr3tuning)
#' library(data.table)
#' set.seed(2)
#' ml_g = lrn("regr.rpart")
#' ml_m = lrn("classif.rpart")
#' ml_r = ml_m$clone()
#' obj_dml_data = make_iivm_data(
#'   theta = 0.5, n_obs = 1000,
#'   alpha_x = 1, dim_x = 20)
#' dml_iivm_obj = DoubleMLIIVM$new(obj_dml_data, ml_g, ml_m, ml_r)
#' param_grid = list(
#'   "ml_g" = paradox::ParamSet$new(list(
#'     paradox::ParamDbl$new("cp", lower = 0.01, upper = 0.02),
#'     paradox::ParamInt$new("minsplit", lower = 1, upper = 2))),
#'   "ml_m" = paradox::ParamSet$new(list(
#'     paradox::ParamDbl$new("cp", lower = 0.01, upper = 0.02),
#'     paradox::ParamInt$new("minsplit", lower = 1, upper = 2))),
#'   "ml_r" = paradox::ParamSet$new(list(
#'     paradox::ParamDbl$new("cp", lower = 0.01, upper = 0.02),
#'     paradox::ParamInt$new("minsplit", lower = 1, upper = 2))))
#' # minimum requirements for tune_settings
#' tune_settings = list(
#'   terminator = mlr3tuning::trm("evals", n_evals = 5),
#'   algorithm = mlr3tuning::tnr("grid_search", resolution = 5))
#' dml_iivm_obj$tune(param_set = param_grid, tune_settings = tune_settings)
#' dml_iivm_obj$fit()
#' dml_iivm_obj$summary()
#' }
#'
#' @export
DoubleMLIIVM = R6Class("DoubleMLIIVM",
  inherit = DoubleML,
  active = list(
    #' @field subgroups (named `list(2)`) \cr
    #' Named `list(2)` with options to adapt to cases with and without the
    #' subgroups of always-takers and never-takes.
    #' The entry `always_takers`(`logical(1)`) speficies whether there are
    #' always takers in the sample. The entry `never_takers` (`logical(1)`)
    #' speficies whether there are never takers in the sample.
    subgroups = function(value) {
      if (missing(value)) return(private$subgroups_)
      else stop("can't set field subgroups")
    },

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
    #' [mlr3extralearners](https://mlr3extralearners.mlr-org.com/),
    #' for example `"regr.cv_glmnet"`. \cr
    #' `ml_g` refers to the nuisance function \eqn{g_0(Z,X) = E[Y|X,Z]}.
    #'
    #' @param ml_m ([`LearnerClassif`][mlr3::LearnerClassif], `character(1)`) \cr
    #' An object of the class
    #' [mlr3 classification learner][mlr3::LearnerClassif] to pass a learner,
    #' possibly with specified parameters, for example
    #' `lrn("classif.cv_glmnet", s = "lambda.min")`.
    #' Alternatively, a `character(1)` specifying the name of
    #' a [mlr3 classification learner][mlr3::LearnerClassif] that is available
    #' in [mlr3](https://mlr3.mlr-org.com/index.html) or its extension packages
    #' [mlr3learners](https://mlr3learners.mlr-org.com/) or
    #' [mlr3extralearners](https://mlr3extralearners.mlr-org.com/),
    #' for example `"classif.cv_glmnet"`. \cr
    #' `ml_m` refers to the nuisance function \eqn{m_0(X) = E[Z|X]}.
    #'
    #' @param ml_r ([`LearnerClassif`][mlr3::LearnerClassif], `character(1)`) \cr
    #' An object of the class
    #' [mlr3 classification learner][mlr3::LearnerClassif] to pass a learner,
    #' possibly with specified parameters, for example
    #' `lrn("classif.cv_glmnet", s = "lambda.min")`.
    #' Alternatively, a `character(1)` specifying the name of a
    #' [mlr3 classification learner][mlr3::LearnerClassif] that is available in
    #' [mlr3](https://mlr3.mlr-org.com/index.html) or its extension packages
    #' [mlr3learners](https://mlr3learners.mlr-org.com/) or
    #' [mlr3extralearners](https://mlr3extralearners.mlr-org.com/),
    #' for example `"classif.cv_glmnet"`. \cr
    #' `ml_r` refers to the nuisance function \eqn{r_0(Z,X) = E[D|X,Z]}.
    #'
    #' @param n_folds (`integer(1)`)\cr
    #' Number of folds. Default is `5`.
    #'
    #' @param n_rep (`integer(1)`) \cr
    #' Number of repetitions for the sample splitting. Default is `1`.
    #'
    #' @param score (`character(1)`, `function()`) \cr
    #' A `character(1)` (`"LATE"` is the only choice) specifying the score
    #' function.
    #' If a `function()` is provided, it must be of the form
    #' `function(y, z, d, g0_hat, g1_hat, m_hat, r0_hat, r1_hat, smpls)` and
    #' the returned output must be a named `list()` with elements `psi_a` and
    #' `psi_b`. Default is `"LATE"`.
    #'
    #' @param subgroups (named `list(2)`) \cr
    #' Named `list(2)` with options to adapt to cases with and without the
    #' subgroups of always-takers and never-takes. The entry
    #' `always_takers`(`logical(1)`) speficies whether there are always takers
    #' in the sample. The entry `never_takers` (`logical(1)`) speficies whether
    #' there are never takers in the sample. Default is
    #' `list(always_takers = TRUE, never_takers = TRUE)`.
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
      ml_r,
      n_folds = 5,
      n_rep = 1,
      score = "LATE",
      subgroups = list(
        always_takers = TRUE,
        never_takers = TRUE),
      dml_procedure = "dml2",
      trimming_rule = "truncate",
      trimming_threshold = 1e-12,
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
        "ml_m" = NULL,
        "ml_r" = NULL)
      ml_g = private$assert_learner(ml_g, "ml_g", Regr = TRUE, Classif = FALSE)
      ml_m = private$assert_learner(ml_m, "ml_m", Regr = FALSE, Classif = TRUE)
      ml_r = private$assert_learner(ml_r, "ml_r", Regr = FALSE, Classif = TRUE)

      private$learner_ = list(
        "ml_g" = ml_g,
        "ml_m" = ml_m,
        "ml_r" = ml_r)
      private$initialize_ml_nuisance_params()

      private$subgroups_ = subgroups
      private$trimming_rule_ = trimming_rule
      private$trimming_threshold_ = trimming_threshold
    }
  ),
  private = list(
    subgroups_ = NULL,
    trimming_rule_ = NULL,
    trimming_threshold_ = NULL,
    n_nuisance = 3,
    initialize_ml_nuisance_params = function() {
      nuisance = vector("list", self$data$n_treat)
      names(nuisance) = self$data$d_cols
      private$params_ = list(
        "ml_g0" = nuisance,
        "ml_g1" = nuisance,
        "ml_m" = nuisance,
        "ml_r0" = nuisance,
        "ml_r1" = nuisance)
      invisible(self)
    },
    ml_nuisance_and_score_elements = function(smpls, ...) {

      if (self$subgroups$always_takers == FALSE &
        self$subgroups$never_takers == FALSE) {
        message("If there are no always-takers and no never-takers,
                ATE is estimated")
      }
      cond_smpls = get_cond_samples(
        smpls,
        self$data$data_model[[self$data$z_cols]])

      m_hat = dml_cv_predict(self$learner$ml_m,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$z_cols,
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

      if (self$subgroups$always_takers == FALSE) {
        r0_hat = rep(0, self$data$n_obs)
      } else {
        r0_hat = dml_cv_predict(self$learner$ml_r,
          c(self$data$x_cols, self$data$other_treat_cols),
          self$data$treat_col,
          self$data$data_model,
          nuisance_id = "nuis_r0",
          smpls = cond_smpls$smpls_0,
          est_params = self$get_params("ml_r0"),
          return_train_preds = FALSE,
          learner_class = private$learner_class$ml_r,
          fold_specific_params = private$fold_specific_params)
      }

      if (self$subgroups$never_takers == FALSE) {
        r1_hat = rep(1, self$data$n_obs)
      } else {
        r1_hat = dml_cv_predict(self$learner$ml_r,
          c(self$data$x_cols, self$data$other_treat_cols),
          self$data$treat_col,
          self$data$data_model,
          nuisance_id = "nuis_r1",
          smpls = cond_smpls$smpls_1,
          est_params = self$get_params("ml_r1"),
          return_train_preds = FALSE,
          learner_class = private$learner_class$ml_r,
          fold_specific_params = private$fold_specific_params)
      }

      # compute residuals
      z = self$data$data_model[[self$data$z_cols]]
      d = self$data$data_model[[self$data$treat_col]]
      y = self$data$data_model[[self$data$y_col]]

      res = private$score_elements(
        y, z, d, g0_hat, g1_hat, m_hat, r0_hat,
        r1_hat, smpls)
      res$preds = list(
        "ml_g0" = g0_hat,
        "ml_g1" = g1_hat,
        "ml_m" = m_hat,
        "ml_r0" = r0_hat,
        "ml_r1" = r1_hat)
      return(res)
    },
    score_elements = function(y, z, d, g0_hat, g1_hat, m_hat, r0_hat,
      r1_hat, smpls) {

      u0_hat = y - g0_hat
      u1_hat = y - g1_hat
      w0_hat = d - r0_hat
      w1_hat = d - r1_hat

      if (self$trimming_rule == "truncate" & self$trimming_threshold > 0) {
        m_hat[m_hat < self$trimming_threshold] = self$trimming_threshold
        m_hat[m_hat > 1 - self$trimming_threshold] = 1 - self$trimming_threshold
      }

      if (is.character(self$score)) {
        if (self$score == "LATE") {
          psi_b = g1_hat - g0_hat + z * (u1_hat) / m_hat -
            (1 - z) * u0_hat / (1 - m_hat)
          psi_a = -1 * (r1_hat - r0_hat + z * (w1_hat) / m_hat -
            (1 - z) * w0_hat / (1 - m_hat))
        }
        psis = list(psi_a = psi_a, psi_b = psi_b)
      } else if (is.function(self$score)) {
        psis = self$score(
          y, z, d, g0_hat, g1_hat, m_hat, r0_hat,
          r1_hat, smpls)
      }
      return(psis)
    },
    ml_nuisance_tuning = function(smpls, param_set, tune_settings,
      tune_on_folds, ...) {

      if (!tune_on_folds) {
        data_tune_list = list(self$data$data_model)
      } else {
        data_tune_list = lapply(
          smpls$train_ids,
          function(x) extract_training_data(self$data$data_model, x))
      }

      indx_g0 = lapply(data_tune_list, function(x) x[[self$data$z_cols]] == 0)
      indx_g1 = lapply(data_tune_list, function(x) x[[self$data$z_cols]] == 1)
      data_tune_list_z0 = lapply(
        seq_len(length(data_tune_list)),
        function(x) data_tune_list[[x]][indx_g0[[x]], ])
      data_tune_list_z1 = lapply(
        seq_len(length(data_tune_list)),
        function(x) data_tune_list[[x]][indx_g1[[x]], ])

      tuning_result_m = dml_tune(self$learner$ml_m,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$z_cols,
        data_tune_list,
        nuisance_id = "nuis_m",
        param_set$ml_m, tune_settings,
        tune_settings$measure$ml_m,
        private$learner_class$ml_m)

      tuning_result_g0 = dml_tune(self$learner$ml_g,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$y_col,
        data_tune_list_z0,
        nuisance_id = "nuis_g0",
        param_set$ml_g, tune_settings,
        tune_settings$measure$ml_g,
        private$learner_class$ml_g)

      tuning_result_g1 = dml_tune(self$learner$ml_g,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$y_col,
        data_tune_list_z1,
        nuisance_id = "nuis_g1",
        param_set$ml_g, tune_settings,
        tune_settings$measure$ml_g,
        private$learner_class$ml_g)

      if (self$subgroups$always_takers == TRUE) {
        tuning_result_r0 = dml_tune(self$learner$ml_r,
          c(self$data$x_cols, self$data$other_treat_cols),
          self$data$treat_col,
          data_tune_list_z0,
          nuisance_id = "nuis_r0",
          param_set$ml_r, tune_settings,
          tune_settings$measure$ml_r,
          private$learner_class$ml_r)
      } else {
        tuning_result_r0 = list(list(), "params" = list(list()))
      }

      if (self$subgroups$never_takers == TRUE) {
        tuning_result_r1 = dml_tune(self$learner$ml_r,
          c(self$data$x_cols, self$data$other_treat_cols),
          self$data$treat_col,
          data_tune_list_z1,
          nuisance_id = "nuis_r1",
          param_set$ml_r, tune_settings,
          tune_settings$measure$ml_r,
          private$learner_class$ml_r)
      } else {
        tuning_result_r1 = list(list(), "params" = list(list()))
      }
      tuning_result = list(
        "ml_m" = list(tuning_result_m, params = tuning_result_m$params),
        "ml_g0" = list(tuning_result_g0, params = tuning_result_g0$params),
        "ml_g1" = list(tuning_result_g1, params = tuning_result_g1$params),
        "ml_r0" = list(tuning_result_r0, params = tuning_result_r0$params),
        "ml_r1" = list(tuning_result_r1, params = tuning_result_r1$params))
      return(tuning_result)
    },
    check_score = function(score) {
      assert(
        check_character(score),
        check_class(score, "function"))
      if (is.character(score)) {
        valid_score = c("LATE")
        assertChoice(score, valid_score)
      }
      return()
    },
    check_data = function(obj_dml_data) {
      one_treat = (obj_dml_data$n_treat == 1)
      err_msg = paste(
        "Incompatible data.\n",
        "To fit an IIVM model with DoubleML",
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

      one_instr = (obj_dml_data$n_instr == 1)
      err_msg = paste(
        "Incompatible data.\n",
        "To fit an IIVM model with DoubleML",
        "exactly one binary variable with values 0 and 1",
        "needs to be specified as instrumental variable.")
      if (one_instr) {
        binary_instr = test_integerish(obj_dml_data$data[[obj_dml_data$z_cols]],
                                       lower = 0, upper = 1)
        if (!(one_instr & binary_instr)) {
          stop(err_msg)
        }
      } else {
        stop(err_msg)
      }
      return()
    }
  )
)
