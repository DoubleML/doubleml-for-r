#' @title Double machine learning for average potential outcomes
#'
#' @description
#' Estimates the average potential outcome at one nominal treatment level.
#'
#' @format [R6::R6Class] object inheriting from [DoubleML].
#' @family DoubleML
#' @details
#' For the indicator \eqn{T_d = 1\{D = d\}}, the nuisance functions are
#' \eqn{g_d(X) = E[Y|D=d,X]} and \eqn{p_d(X) = P(D=d|X)}.
#' The orthogonal score is \eqn{g_d(X) + T_d (Y-g_d(X))/p_d(X) - \theta_d}.
#' Identification requires conditional exchangeability and positive treatment
#' probabilities. Numeric treatment values are labels, without a dose ordering.
#' This initial implementation fits supplied nuisance predictions only;
#' learner estimation, normalized weights and propensity diagnostics are
#' introduced separately.
#' @usage NULL
#' @export
DoubleMLAPO = R6Class("DoubleMLAPO",
  inherit = DoubleML,
  active = list(
    #' @field treatment_level (`factor(1)`, `character(1)`, `numeric(1)`) \cr
    #' The nominal treatment label whose average potential outcome is estimated.
    treatment_level = function(value) {
      if (missing(value)) return(private$treatment_level_)
      stop("can't set field treatment_level")
    },
    #' @field trimming_rule (`character(1)`) \cr
    #' The propensity trimming rule, currently `"truncate"`.
    trimming_rule = function(value) {
      if (missing(value)) return(private$trimming_rule_)
      stop("can't set field trimming_rule")
    },
    #' @field trimming_threshold (`numeric(1)`) \cr
    #' Lower bound for propensity truncation; the upper bound is one minus it.
    trimming_threshold = function(value) {
      if (missing(value)) return(private$trimming_threshold_)
      stop("can't set field trimming_threshold")
    },
    #' @field normalize_ipw (`logical(1)`) \cr
    #' Whether inverse propensity weights should be normalized. Normalization
    #' is not yet available in this initial fixed-prediction implementation.
    normalize_ipw = function(value) {
      if (missing(value)) return(private$normalize_ipw_)
      stop("can't set field normalize_ipw")
    },
    #' @field propensity_diagnostics (`list()`, `NULL`) \cr
    #' Treatment-specific propensity diagnostics, currently `NULL`.
    propensity_diagnostics = function(value) {
      if (missing(value)) return(private$propensity_diagnostics_)
      stop("can't set field propensity_diagnostics")
    }
  ),
  public = list(
    #' @description Creates an average potential outcome model.
    #' @param data (`DoubleMLData`) \cr
    #' Data with exactly one treatment column and no instruments or clustering.
    #' @param ml_g (`LearnerRegr`, `character(1)`) \cr
    #' An mlr3 regression learner, or its identifier, for the conditional outcome
    #' at the selected treatment level. Required also for external predictions.
    #' @param ml_m (`LearnerClassif`, `character(1)`) \cr
    #' An mlr3 classification learner, or its identifier, for the probability
    #' of the selected treatment level. Required also for external predictions.
    #' @param treatment_level (`factor(1)`, `character(1)`, `numeric(1)`) \cr
    #' One observed nominal treatment label, in the same type family as the data.
    #' @param n_folds (`integer(1)`) \cr
    #' Number of sample-splitting folds. Default is `5`.
    #' @param n_rep (`integer(1)`) \cr
    #' Number of sample-splitting repetitions. Default is `1`.
    #' @param score (`character(1)`) \cr
    #' The built-in `"APO"` score. Custom scores are not supported.
    #' @param dml_procedure (`character(1)`) \cr
    #' Double machine learning procedure, `"dml1"` or `"dml2"` (default).
    #' @param trimming_rule (`character(1)`) \cr
    #' Propensity trimming rule; only `"truncate"` is supported.
    #' @param trimming_threshold (`numeric(1)`) \cr
    #' Truncation threshold in `[0, 0.5)`. Default is `0.01`.
    #' @param normalize_ipw (`logical(1)`) \cr
    #' Whether to normalize inverse propensity weights. Default is `FALSE`;
    #' fitting with `TRUE` is not yet implemented.
    #' @param draw_sample_splitting (`logical(1)`) \cr
    #' Whether to draw sample splits at initialization. Default is `TRUE`.
    #' @param apply_cross_fitting (`logical(1)`) \cr
    #' Whether to apply cross-fitting. Default is `TRUE`.
    initialize = function(data, ml_g, ml_m, treatment_level,
                          n_folds = 5, n_rep = 1, score = "APO",
                          dml_procedure = "dml2", trimming_rule = "truncate",
                          trimming_threshold = 0.01, normalize_ipw = FALSE,
                          draw_sample_splitting = TRUE, apply_cross_fitting = TRUE) {
      assert_class(data, "DoubleMLData")
      if (inherits(data, "DoubleMLClusterData")) {
        stop("DoubleMLClusterData is not supported by DoubleMLAPO.")
      }
      if (data$n_treat != 1L) stop("DoubleMLAPO requires exactly one treatment column.")
      if (!is.null(data$z_cols)) stop("DoubleMLAPO does not support instruments.")
      # Validate labels before the base initialization draws any sample splits.
      make_apo_indicator(data$data[[data$d_cols]], treatment_level)
      assert_choice(score, "APO")
      assert_choice(trimming_rule, "truncate")
      assert_number(trimming_threshold, lower = 0, upper = 0.5, finite = TRUE)
      if (trimming_threshold >= 0.5) stop("trimming_threshold must be less than 0.5.")
      assert_flag(normalize_ipw)
      private$treatment_level_ = treatment_level
      private$trimming_rule_ = trimming_rule
      private$trimming_threshold_ = trimming_threshold
      private$normalize_ipw_ = normalize_ipw

      super$initialize_double_ml(data, n_folds, n_rep, score, dml_procedure,
        draw_sample_splitting, apply_cross_fitting)
      ml_g = private$assert_learner(ml_g, "ml_g", Regr = TRUE, Classif = FALSE)
      ml_m = private$assert_learner(ml_m, "ml_m", Regr = FALSE, Classif = TRUE)
      private$learner_ = list(ml_g = ml_g, ml_m = ml_m)
      private$initialize_ml_nuisance_params()
    },
    #' @description
    #' Fits the APO model using validated fixed nuisance predictions and the
    #' inherited DoubleML fitting, aggregation and inference methods.
    #' @param store_predictions (`logical(1)`) \cr
    #' Whether to store nuisance predictions. Default is `FALSE`.
    #' @param store_models (`logical(1)`) \cr
    #' Whether to initialize model storage. No models are trained from external
    #' predictions, so the stored fold entries remain `NULL`.
    #' @param external_predictions (`list()`, `NULL`) \cr
    #' Named list with `ml_g` and `ml_m` numeric observation-by-repetition
    #' matrices. All values must be finite and raw `ml_m` probabilities must
    #' lie in `[0, 1]`. Predictions should be obtained out of fold.
    #' `NULL` requests the learner path, which is not yet implemented.
    #' @return self
    fit = function(store_predictions = FALSE, store_models = FALSE, external_predictions = NULL) {
      predictions = validate_apo_external_predictions(
        external_predictions, self$data$n_obs, self$n_rep)
      if (self$normalize_ipw) stop("normalize_ipw = TRUE is not yet implemented.")
      private$external_predictions_ = predictions
      on.exit({
        private$external_predictions_ = NULL
      }, add = TRUE)
      super$fit(store_predictions = store_predictions, store_models = store_models)
      names(private$coef_) = names(private$se_) = names(private$t_stat_) =
        names(private$pval_) = as.character(self$treatment_level)
      invisible(self)
    }
  ),
  private = list(
    treatment_level_ = NULL,
    trimming_rule_ = NULL,
    trimming_threshold_ = NULL,
    normalize_ipw_ = NULL,
    propensity_diagnostics_ = NULL,
    external_predictions_ = NULL,
    n_nuisance = 2,
    initialize_ml_nuisance_params = function() {
      nuisance = vector("list", self$data$n_treat)
      names(nuisance) = self$data$d_cols
      private$params_ = list(ml_g = nuisance, ml_m = nuisance)
      invisible(self)
    },
    nuisance_est = function(smpls, ...) {
      if (is.null(private$external_predictions_)) {
        stop(paste("Learner-based APO nuisance estimation is not yet implemented;",
          "supply external_predictions."))
      }
      g_hat = private$external_predictions_$ml_g[, private$i_rep]
      m_hat = private$external_predictions_$ml_m[, private$i_rep]
      indicator = make_apo_indicator(
        self$data$data_model[[self$data$treat_col]], self$treatment_level)
      y = self$data$data_model[[self$data$y_col]]
      res = private$score_elements(y, indicator, g_hat, m_hat)
      res$preds = list(ml_g = g_hat, ml_m = m_hat)
      res$models = list(ml_g = NULL, ml_m = NULL)
      res
    },
    score_elements = function(y, indicator, g_hat, m_hat) {
      if (self$trimming_rule == "truncate" && self$trimming_threshold > 0) {
        m_hat = pmin(pmax(m_hat, self$trimming_threshold), 1 - self$trimming_threshold)
      }
      list(psi_a = rep(-1, length(y)),
        psi_b = g_hat + indicator / m_hat * (y - g_hat))
    }
  )
)
