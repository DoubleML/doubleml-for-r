#' @title Double machine learning for average potential outcomes
#'
#' @description
#' Estimates the average potential outcome at one nominal treatment level.
#'
#' @format [R6::R6Class] object inheriting from [DoubleML].
#' @seealso [DoubleML], [DoubleMLAPOS], [DoubleMLFramework], [DoubleMLIRM]
#' @details
#' For the indicator \eqn{T_d = 1\{D = d\}}, the nuisance functions are
#' \eqn{g_d(X) = E[Y|D=d,X]} and \eqn{p_d(X) = P(D=d|X)}.
#' The target is \eqn{\theta_d = E[Y(d)]}, the population mean outcome if
#' everyone received label \eqn{d}. Under consistency, conditional
#' exchangeability \eqn{Y(d) \perp D \mid X}, and positivity
#' \eqn{P(D=d \mid X)>0}, it is identified by \eqn{E[g_d(X)]}.
#' Inference additionally requires independent observations, suitable moments,
#' overlap and sufficiently accurate nuisance estimates, including a vanishing
#' product of outcome and propensity errors at the root-sample-size scale.
#'
#' The orthogonal score is \eqn{g_d(X) + T_d (Y-g_d(X))/p_d(X) - \theta_d}.
#' Identification requires conditional exchangeability and positive treatment
#' probabilities. Numeric treatment values are labels, without a dose ordering.
#' Nuisance learners are cross-fitted: the outcome model uses only training
#' rows at the requested level, and the propensity model uses all treatment
#' levels in a one-vs-rest task. Fixed nuisance predictions are also supported.
#' Optional normalized weights divide inverse propensity weights by their mean
#' over the scoring sample, separately for each repetition. Diagnostics retain
#' raw and truncated propensities, scoring weights and fold class counts.
#' Selecting one level does not discard the remaining levels. Training folds
#' need both indicator classes and at least one selected-level outcome. Splits
#' are not automatically stratified; inspect rare-level support or supply splits.
#' Learner fitting rejects overlapping train/test rows. For no cross-fitting,
#' use one disjoint held-out evaluation set and `n_rep = 1`. The inherited
#' full-sample one-fold split is usable only with external nuisance predictions;
#' their out-of-sample provenance is the caller's responsibility.
#'
#' Truncation bounds probabilities by `trimming_threshold` and one minus it;
#' observations are not removed. It stabilizes weights but cannot establish
#' population overlap and may introduce bias. A warning is issued when more
#' than 5 percent of predictions change in a repetition.
#' With `"dml2"`, the estimate is the scoring-sample mean of the augmented
#' outcome. With `"dml1"`, it is the unweighted mean of fold estimates.
#' Repeated-split estimates and standard errors follow [DoubleML] aggregation.
#'
#' For binary treatment, level 1 minus level 0 reduces to the [DoubleMLIRM]
#' ATE score when splits, outcome predictions, complementary propensities,
#' truncation and unnormalized weights agree. Use [DoubleMLAPOS] for
#' covariance-aware contrasts; marginal standard errors alone are insufficient.
#' Separate binary fits need not produce complementary probabilities.
#'
#' Clustered data and instrumental variables are not supported. Group average
#' treatment effects (GATE) and sensitivity analysis remain pending their
#' separate reviewed memo gates; neither is part of this core API.
#' @examples
#' set.seed(42)
#' n = 120
#' x = rnorm(n)
#' d = rep(c("control", "filter", "boiling"), length.out = n)
#' y = x + (d == "filter") + rnorm(n)
#' data = DoubleMLData$new(data.frame(y, d, x), "y", "d", x_cols = "x")
#' apo = DoubleMLAPO$new(data, mlr3::lrn("regr.lm"),
#'   mlr3::lrn("classif.featureless"),
#'   treatment_level = "filter", n_folds = 2
#' )
#' apo$fit()
#' apo$summary()
#' apo$confint()
#' apo$propensity_diagnostics[[1]]$fold_class_counts
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
    #' Whether inverse propensity weights should be normalized to mean one.
    normalize_ipw = function(value) {
      if (missing(value)) return(private$normalize_ipw_)
      stop("can't set field normalize_ipw")
    },
    #' @field propensity_diagnostics (`list()`, `NULL`) \cr
    #' One treatment-specific entry per fitted repetition, containing raw minimum
    #' and quartiles, fraction trimmed, raw/effective propensities, effective
    #' weights and selected/complement class counts for each train/test fold.
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
    #' Whether to normalize inverse propensity weights. Default is `FALSE`.
    #' @param draw_sample_splitting (`logical(1)`) \cr
    #' Whether to draw sample splits at initialization. Default is `TRUE`.
    #' @param apply_cross_fitting (`logical(1)`) \cr
    #' Whether to apply cross-fitting. Default is `TRUE`.
    #' @return A new `DoubleMLAPO` object.
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

      super$initialize_double_ml(
        data, n_folds, n_rep, score, dml_procedure,
        draw_sample_splitting, apply_cross_fitting
      )
      ml_g = private$assert_learner(ml_g, "ml_g", Regr = TRUE, Classif = FALSE)
      ml_m = private$assert_learner(ml_m, "ml_m", Regr = FALSE, Classif = TRUE)
      private$learner_ = list(ml_g = ml_g, ml_m = ml_m)
      private$initialize_ml_nuisance_params()
    },
    #' @description
    #' Fits the APO model using cross-fitted learners or validated fixed nuisance
    #' predictions and the inherited fitting, aggregation and inference methods.
    #' @param store_predictions (`logical(1)`) \cr
    #' Whether to store nuisance predictions. Default is `FALSE`.
    #' @param store_models (`logical(1)`) \cr
    #' Whether to store fitted nuisance models for each fold. With external
    #' predictions, the stored fold entries remain `NULL`.
    #' @param external_predictions (`list()`, `NULL`) \cr
    #' Named list with `ml_g` and `ml_m` numeric observation-by-repetition
    #' matrices. All values must be finite and raw `ml_m` probabilities must
    #' lie in `[0, 1]`. With `trimming_threshold = 0`, probabilities must be
    #' strictly positive on every observation. Predictions should be obtained
    #' out of fold. Matrices cover all rows, even for held-out estimation.
    #' Stored predictions retain raw propensities and have dimensions observation
    #' by repetition by one treatment; diagnostics expose effective values.
    #' `NULL` fits the nuisance learners with the model's sample splits.
    #' @return Invisibly returns the fitted model.
    fit = function(store_predictions = FALSE, store_models = FALSE, external_predictions = NULL) {
      predictions = validate_apo_external_predictions(
        external_predictions, self$data$n_obs, self$n_rep
      )
      if (!is.null(predictions) && self$trimming_threshold == 0 && any(predictions$ml_m == 0)) {
        stop(paste(
          "APO positivity requires strictly positive propensities for treatment",
          as.character(self$treatment_level),
          "on every observation; supply positive probabilities or increase",
          "trimming_threshold above zero."
        ))
      }
      if (is.null(predictions)) {
        indicator = make_apo_indicator(self$data$data[[self$data$d_cols]], self$treatment_level)
        validate_apo_sample_splitting(self$smpls, indicator, self$treatment_level)
      }
      private$external_predictions_ = predictions
      on.exit(
        {
          private$external_predictions_ = NULL
        },
        add = TRUE
      )
      private$propensity_diagnostics_ = vector("list", self$n_rep)
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
      indicator = make_apo_indicator(
        self$data$data_model[[self$data$treat_col]], self$treatment_level
      )
      if (is.null(private$external_predictions_)) {
        # The local target preserves every nominal level in the complement.
        data_model = data.table::copy(self$data$data_model)
        data_model[[self$data$treat_col]] = indicator
        cond_smpls = get_cond_samples(smpls, indicator)$smpls_1
        m_result = private$predict_nuisance("ml_m", self$data$treat_col, data_model, smpls)
        g_result = private$predict_nuisance("ml_g", self$data$y_col, data_model, cond_smpls)
        g_hat = g_result$preds
        m_hat = m_result$preds
        models = list(ml_g = g_result$models, ml_m = m_result$models)
        if (self$trimming_threshold == 0 && any(m_hat == 0, na.rm = TRUE)) {
          stop(paste(
            "APO positivity requires strictly positive propensities for treatment",
            as.character(self$treatment_level),
            "on every observation; supply positive probabilities or increase",
            "trimming_threshold above zero."
          ))
        }
      } else {
        g_hat = private$external_predictions_$ml_g[, private$i_rep]
        m_hat = private$external_predictions_$ml_m[, private$i_rep]
        models = list(ml_g = NULL, ml_m = NULL)
      }
      y = self$data$data_model[[self$data$y_col]]
      res = private$score_elements(y, indicator, g_hat, m_hat, smpls)
      res$preds = list(ml_g = g_hat, ml_m = m_hat)
      res$models = models
      res
    },
    predict_nuisance = function(key, target, data_model, smpls) {
      params = self$get_params(key)
      predict_fold = function(split, est_params) {
        # The shared helper shallow-clones ParamSet; isolate it for APO only.
        dml_cv_predict(self$learner[[key]]$clone(deep = TRUE),
          self$data$x_cols, target, data_model,
          nuisance_id = paste0("nuis_", substring(key, 4)), smpls = split,
          est_params = est_params, return_train_preds = FALSE,
          task_type = private$task_type[[key]], fold_specific_params = FALSE
        )
      }
      if (!private$fold_specific_params) return(predict_fold(smpls, params))
      result = list(
        preds = rep(NA_real_, nrow(data_model)),
        models = vector("list", length(smpls$train_ids))
      )
      for (i_fold in seq_along(smpls$train_ids)) {
        split = list(train_ids = smpls$train_ids[i_fold], test_ids = smpls$test_ids[i_fold])
        fold_result = predict_fold(split, params[[i_fold]])
        ids = smpls$test_ids[[i_fold]]
        result$preds[ids] = fold_result$preds[ids]
        result$models[[i_fold]] = fold_result$models[[1]]
      }
      result
    },
    nuisance_tuning = function(smpls, param_set, tune_settings, tune_on_folds, ...) {
      indicator = make_apo_indicator(
        self$data$data_model[[self$data$treat_col]], self$treatment_level
      )
      data_model = data.table::copy(self$data$data_model)
      data_model[[self$data$treat_col]] = indicator
      data_m = if (tune_on_folds) {
        lapply(smpls$train_ids, function(ids) extract_training_data(data_model, ids))
      } else {
        list(data_model)
      }
      data_g = lapply(data_m, function(data) data[data[[self$data$treat_col]] == 1, ])
      result_m = dml_tune(self$learner$ml_m$clone(deep = TRUE),
        self$data$x_cols, self$data$treat_col, data_m,
        nuisance_id = "nuis_m",
        param_set$ml_m, tune_settings, tune_settings$measure$ml_m, private$task_type$ml_m
      )
      result_g = dml_tune(self$learner$ml_g$clone(deep = TRUE),
        self$data$x_cols, self$data$y_col, data_g,
        nuisance_id = "nuis_g",
        param_set$ml_g, tune_settings, tune_settings$measure$ml_g, private$task_type$ml_g
      )
      list(
        ml_g = list(result_g, params = result_g$params),
        ml_m = list(result_m, params = result_m$params)
      )
    },
    score_elements = function(y, indicator, g_hat, m_hat, smpls) {
      raw_m_hat = m_hat
      if (self$trimming_rule == "truncate" && self$trimming_threshold > 0) {
        m_hat = pmin(pmax(m_hat, self$trimming_threshold), 1 - self$trimming_threshold)
      }
      weights = indicator / m_hat
      if (self$normalize_ipw) {
        evaluation_ids = unique(unlist(smpls$test_ids, use.names = FALSE))
        denominator = mean(weights[evaluation_ids], na.rm = TRUE)
        if (!is.finite(denominator) || denominator <= 0) {
          stop(paste(
            "APO weight normalization requires a positive finite mean for treatment",
            as.character(self$treatment_level), "in repetition", private$i_rep
          ))
        }
        weights = weights / denominator
      }
      fraction_trimmed = mean(raw_m_hat != m_hat, na.rm = TRUE)
      fold_counts = lapply(seq_along(smpls$train_ids), function(i_fold) {
        train = indicator[smpls$train_ids[[i_fold]]]
        test = indicator[smpls$test_ids[[i_fold]]]
        data.frame(
          fold = i_fold, train_selected = sum(train == 1),
          train_complement = sum(train == 0), test_selected = sum(test == 1),
          test_complement = sum(test == 0)
        )
      })
      private$propensity_diagnostics_[[private$i_rep]] = list(
        treatment_level = self$treatment_level,
        raw_min = min(raw_m_hat, na.rm = TRUE),
        raw_quantiles = stats::quantile(raw_m_hat, probs = c(0, .25, .5, .75, 1), na.rm = TRUE),
        fraction_trimmed = fraction_trimmed,
        raw_propensity = raw_m_hat, effective_propensity = m_hat,
        effective_weight = weights, fold_class_counts = do.call(rbind, fold_counts)
      )
      if (fraction_trimmed > .05) {
        warning(
          paste(
            "APO propensity trimming for treatment", as.character(self$treatment_level),
            "in repetition", private$i_rep, "changed more than 5 percent of predictions."
          ),
          call. = FALSE
        )
      }
      list(psi_a = rep(-1, length(y)), psi_b = g_hat + weights * (y - g_hat))
    }
  )
)
