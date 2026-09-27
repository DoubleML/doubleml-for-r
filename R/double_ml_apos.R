#' @title Joint average potential outcomes for nominal treatments
#'
#' @description
#' Coordinates one [DoubleMLAPO] per requested treatment level with synchronized
#' sample splits. Joint covariance retains dependence between all potential
#' outcome estimates through their observation-level influence contributions.
#' @format [R6::R6Class] object.
#' @family DoubleML
#' @details
#' Treatment values are nominal labels. Inferred levels follow observed factor
#' level order (excluding unused levels), or first appearance for character and
#' numeric treatments. Explicit subsets retain the requested order.
#' Fitting is transactional: all external inputs are validated before any
#' component is fitted, and results are published only after every fit and joint
#' covariance calculation succeeds. A failed first fit leaves the coordinator
#' unfitted; a failed refit retains the previous complete result. Changing sample
#' splits or successfully tuning clears the published joint result.
#' @usage NULL
#' @export
DoubleMLAPOS = R6Class("DoubleMLAPOS",
  active = list(
    #' @field treatment_levels (factor, character or numeric vector) Requested
    #' nominal labels in estimation order; factor labels are stored as character.
    treatment_levels = function(value) {
      if (missing(value)) return(private$treatment_levels_)
      stop("can't set field treatment_levels")
    },
    #' @field apo_models (named list) Component [DoubleMLAPO] objects, keyed by
    #' treatment display label. Components exist before fitting.
    apo_models = function(value) {
      if (missing(value)) return(private$apo_models_)
      stop("can't set field apo_models")
    },
    #' @field framework ([DoubleMLFramework] or NULL) Joint inference object.
    #' NULL until a complete successful fit is available.
    framework = function(value) {
      if (missing(value)) return(private$framework_)
      stop("can't set field framework")
    },
    #' @field coef (numeric vector) Named average potential outcomes after fit().
    coef = function(value) {
      if (!missing(value)) stop("can't set field coef")
      private$require_fit()
      self$framework$coef
    },
    #' @field se (numeric vector) Joint framework's marginal standard errors.
    se = function(value) {
      if (!missing(value)) stop("can't set field se")
      private$require_fit()
      self$framework$se
    },
    #' @field t_stat (numeric vector) Estimates divided by standard errors.
    t_stat = function(value) {
      if (!missing(value)) stop("can't set field t_stat")
      private$require_fit()
      self$framework$t_stat
    },
    #' @field pval (numeric vector) Two-sided normal p-values.
    pval = function(value) {
      if (!missing(value)) stop("can't set field pval")
      private$require_fit()
      self$framework$pval
    },
    #' @field predictions (named list or NULL) Stored predictions, nested by
    #' display label then ml_g/ml_m; each entry retains the APO array dimensions
    #' observation by repetition by one treatment. NULL if storage was disabled.
    #' Access before a successful fit raises an error.
    predictions = function(value) {
      if (!missing(value)) stop("can't set field predictions")
      private$require_fit()
      if (!private$store_predictions_) return(NULL)
      lapply(self$apo_models, function(model) model$predictions)
    },
    #' @field models (named list or NULL) Stored nuisance models, nested by
    #' display label then ml_g/ml_m, followed by the APO treatment-column,
    #' repetition and fold lists. NULL if storage was disabled. Access before
    #' a successful fit raises an error.
    models = function(value) {
      if (!missing(value)) stop("can't set field models")
      private$require_fit()
      if (!private$store_models_) return(NULL)
      lapply(self$apo_models, function(model) model$models)
    },
    #' @field smpls (list or NULL) Shared train/test splits by repetition.
    #' NULL when splitting was deferred and has not yet been supplied.
    smpls = function(value) {
      if (missing(value)) return(private$smpls_)
      stop("can't set field smpls")
    }
  ),
  public = list(
    #' @description Create a synchronized collection of APO estimators.
    #' @param data (DoubleMLData) Data with exactly one nominal treatment column,
    #' no instruments and no clustering.
    #' @param ml_g (LearnerRegr or character) Outcome regression learner or its
    #' mlr3 identifier. Each component receives an independent deep clone.
    #' @param ml_m (LearnerClassif or character) Propensity classification learner
    #' or its mlr3 identifier. Each component receives an independent deep clone.
    #' @param treatment_levels (factor, character or numeric vector, or NULL)
    #' Requested observed labels, in the same type family as the treatment.
    #' NULL infers all observed levels. Display labels must be unique and nonblank.
    #' @param n_folds (integer) Number of folds, default 5.
    #' @param n_rep (integer) Number of splitting repetitions, default 1.
    #' @param score (character) The built-in "APO" score.
    #' @param dml_procedure (character) "dml1" or "dml2" (default).
    #' @param trimming_rule (character) Propensity rule, currently "truncate".
    #' @param trimming_threshold (numeric) Truncation threshold in [0, 0.5).
    #' @param normalize_ipw (logical) Normalize inverse propensity weights.
    #' @param draw_sample_splitting (logical) Draw one component's splits and
    #' propagate them to all components. Otherwise use set_sample_splitting().
    #' @param apply_cross_fitting (logical) Apply cross-fitting, default TRUE.
    initialize = function(data, ml_g, ml_m, treatment_levels = NULL,
                          n_folds = 5, n_rep = 1, score = "APO",
                          dml_procedure = "dml2", trimming_rule = "truncate",
                          trimming_threshold = 0.01, normalize_ipw = FALSE,
                          draw_sample_splitting = TRUE, apply_cross_fitting = TRUE) {
      assert_class(data, "DoubleMLData")
      if (inherits(data, "DoubleMLClusterData")) {
        stop("DoubleMLClusterData is not supported by DoubleMLAPOS.")
      }
      if (data$n_treat != 1L) stop("DoubleMLAPOS requires exactly one treatment column.")
      if (!is.null(data$z_cols)) stop("DoubleMLAPOS does not support instruments.")
      levels = get_apo_levels(data$data[[data$d_cols]], treatment_levels)
      labels = as.character(levels)
      if (anyDuplicated(labels) || any(!nzchar(trimws(labels)))) {
        stop("Treatment display labels must be unique and nonblank.")
      }
      assert_flag(draw_sample_splitting)
      private$data_ = data
      private$treatment_levels_ = levels
      private$apo_models_ = setNames(lapply(seq_along(levels), function(j) {
        DoubleMLAPO$new(data, private$clone_nested(ml_g), private$clone_nested(ml_m),
          levels[j], n_folds, n_rep, score, dml_procedure, trimming_rule,
          trimming_threshold, normalize_ipw,
          draw_sample_splitting = draw_sample_splitting && j == 1L,
          apply_cross_fitting = apply_cross_fitting)
      }), labels)
      if (draw_sample_splitting) {
        self$set_sample_splitting(private$apo_models_[[1]]$smpls)
      }
      invisible(self)
    },
    #' @description Validate and assign identical splits to every component.
    #' Invalid splits leave the coordinator unchanged. Successful replacement
    #' invalidates the published joint result.
    #' @param smpls (list) Train/test indices in the format accepted by
    #' [DoubleML]'s set_sample_splitting() method.
    #' @return Invisibly returns the coordinator.
    set_sample_splitting = function(smpls) {
      components = private$clone_components(self$apo_models)
      for (model in components) model$set_sample_splitting(smpls)
      private$apo_models_ = components
      private$smpls_ = components[[1]]$smpls
      private$framework_ = NULL
      invisible(self)
    },
    #' @description Fit all components and assemble joint inference atomically.
    #' @param store_predictions (logical) Store nuisance predictions, default FALSE.
    #' @param store_models (logical) Store nuisance models, default FALSE.
    #' @param external_predictions (named list or NULL) When supplied, exactly one
    #' entry for every requested display label, each containing ml_g and ml_m
    #' numeric observation-by-repetition matrices. Entries cannot be NULL.
    #' All entries must be finite; propensities must lie in [0, 1] and be strictly
    #' positive when trimming_threshold is zero. NULL fits all nuisance learners.
    #' @return Invisibly returns the coordinator after every component succeeds.
    fit = function(store_predictions = FALSE, store_models = FALSE, external_predictions = NULL) {
      assert_flag(store_predictions)
      assert_flag(store_models)
      private$check_components(self$apo_models)
      predictions = private$validate_predictions(external_predictions)
      components = private$clone_components(self$apo_models)
      for (j in seq_along(components)) {
        # The base APO retains old optional storage on refit; clear temporary
        # copies so disabling storage cannot expose results from a previous fit.
        state = components[[j]]$.__enclos_env__$private
        state$predictions_ = state$models_ = NULL
        components[[j]]$fit(store_predictions, store_models, predictions[[j]])
      }
      private$check_components(components)
      frameworks = lapply(seq_along(components), function(j) {
        model = components[[j]]
        all_coef = model$all_coef
        all_se = model$all_se
        label = names(components)[j]
        rownames(all_coef) = rownames(all_se) = label
        # APO has constant score derivative -1, hence its centered score is
        # already the influence contribution. Framework uses a different axis order.
        psi = aperm(model$psi, c(1, 3, 2))
        if (!model$apply_cross_fitting) {
          ids = model$smpls[[1]]$test_ids[[1]]
          psi[setdiff(seq_len(model$data$n_obs), ids), , ] = 0
        }
        dimnames(psi)[[2]] = label
        DoubleMLFramework$new(all_coef, all_se, psi, label,
          smpls = model$smpls, apply_cross_fitting = model$apply_cross_fitting)
      })
      framework = Reduce(function(left, right) left$concat(right), frameworks)
      private$apo_models_ = components
      private$framework_ = framework
      private$store_predictions_ = store_predictions
      private$store_models_ = store_models
      invisible(self)
    },
    #' @description Return jointly inferred causal contrasts without refitting.
    #' @param reference (factor, character or numeric scalar, or NULL) One
    #' requested treatment label in the treatment's type family. All other
    #' requested levels are contrasted against it in estimation order.
    #' @param contrast (numeric matrix or NULL) Finite K-by-J contrast matrix,
    #' where J is the number of requested levels. Named columns must match all
    #' treatment display labels uniquely and are reordered to estimation order.
    #' Row names must be unique and nonblank; unnamed rows receive contrast1,
    #' ..., contrastK. Supply exactly one of reference and contrast.
    #' @return A new [DoubleMLFramework] retaining joint influence contributions
    #' and covariance. Bootstrap this returned framework for joint inference.
    causal_contrast = function(reference = NULL, contrast = NULL) {
      private$require_fit()
      if (is.null(reference) == is.null(contrast)) {
        stop("Supply exactly one of reference and contrast.")
      }
      if (!is.null(reference)) {
        if (length(reference) != 1L) stop("reference must contain exactly one treatment label.")
        reference = get_apo_levels(self$treatment_levels, reference)
        reference_index = match(reference, self$treatment_levels)
        other = setdiff(seq_along(self$treatment_levels), reference_index)
        if (!length(other)) {
          stop("Reference contrasts require at least two requested treatment levels.")
        }
        labels = names(self$apo_models)
        contrast = matrix(0, length(other), length(labels),
          dimnames = list(paste(labels[other], "-", labels[reference_index]), labels))
        contrast[cbind(seq_along(other), other)] = 1
        contrast[, reference_index] = -1
      }
      self$framework$linear_transform(contrast)
    },
    #' @description Bootstrap the joint APOS framework using common multipliers.
    #' @param method (character) One of "normal", "Bayes", or "wild".
    #' @param n_rep_boot (integer) Positive number of bootstrap replications.
    #' @return Invisibly returns the coordinator. Bootstrap results are stored
    #' in its framework's boot_coef and boot_t_stat fields.
    bootstrap = function(method = "normal", n_rep_boot = 500) {
      private$require_fit()
      self$framework$bootstrap(method, n_rep_boot)
      invisible(self)
    },
    #' @description Compute marginal or joint APOS confidence intervals.
    #' @param parm (numeric or character vector) Requested parameter positions
    #' or display labels. Omit to include all requested treatment levels.
    #' @param joint (logical) Use joint bootstrap intervals, default FALSE.
    #' Requires bootstrap() first.
    #' @param level (numeric) Confidence level strictly between zero and one.
    #' @return A matrix with named rows and confidence-bound percentage columns.
    confint = function(parm, joint = FALSE, level = 0.95) {
      private$require_fit()
      if (missing(parm)) {
        self$framework$confint(joint = joint, level = level)
      } else {
        self$framework$confint(parm, joint, level)
      }
    },
    #' @description Adjust APOS p-values with joint Romano-Wolf stepdown or
    #' a method supported by stats::p.adjust(). Romano-Wolf requires bootstrap().
    #' @param method (character) Adjustment method, default "romano-wolf".
    #' @param return_matrix (logical) Return an estimates/p-values matrix
    #' (default TRUE), or an adjusted p-value vector.
    #' @return A named-row matrix or numeric vector in estimation order.
    p_adjust = function(method = "romano-wolf", return_matrix = TRUE) {
      private$require_fit()
      self$framework$p_adjust(method, return_matrix)
    },
    #' @description Tune each treatment-specific model on the shared outer splits.
    #' Results and tuned parameters are published only after every component
    #' succeeds. Successful tuning invalidates the joint fitted result.
    #' @param param_set (named list) ml_g and ml_m parameter spaces accepted by
    #' [DoubleML]'s tune() method.
    #' @param tune_settings (list) Settings accepted by [DoubleML]'s tune()
    #' method. Omit to use its defaults.
    #' @param tune_on_folds (logical) Tune separately on each outer training fold.
    #' @return A list of APO tuning results, named by treatment display label.
    tune = function(param_set, tune_settings, tune_on_folds = FALSE) {
      private$check_components(self$apo_models)
      components = private$clone_components(self$apo_models)
      for (model in components) {
        if (missing(tune_settings)) {
          model$tune(param_set, tune_on_folds = tune_on_folds)
        } else {
          model$tune(param_set, tune_settings, tune_on_folds)
        }
      }
      private$check_components(components)
      private$apo_models_ = components
      private$framework_ = NULL
      lapply(components, function(model) model$tuning_res)
    },
    #' @description Print the joint estimates and significance table.
    #' Raises an error until a complete fit is available.
    #' @param digits (integer) Significant digits used for printing.
    #' @return Invisibly returns the printed coefficient matrix.
    summary = function(digits = max(3L, getOption("digits") - 3L)) {
      private$require_fit()
      self$framework$summary(digits)
    },
    #' @description Print requested levels and, when available, the fit summary.
    #' @return Invisibly returns the coordinator.
    print = function() {
      cat("================== DoubleMLAPOS ==================\n")
      cat("Treatment levels:", paste(as.character(self$treatment_levels), collapse = ", "), "\n")
      if (is.null(self$framework)) cat("fit() not yet called successfully.\n") else self$summary()
      invisible(self)
    }
  ),
  private = list(
    data_ = NULL,
    treatment_levels_ = NULL,
    apo_models_ = NULL,
    framework_ = NULL,
    smpls_ = NULL,
    store_predictions_ = FALSE,
    store_models_ = FALSE,
    require_fit = function() {
      if (is.null(self$framework)) stop("Call fit() before accessing APOS fitted results.")
    },
    check_components = function(components) {
      if (is.null(self$smpls)) stop("Set sample splitting before fitting or tuning APOS.")
      reference = components[[1]]
      for (j in seq_along(components)) {
        model = components[[j]]
        if (!identical(model$smpls, self$smpls) ||
          model$n_rep != length(self$smpls) ||
          model$n_folds != reference$n_folds ||
          !identical(model$apply_cross_fitting, reference$apply_cross_fitting) ||
          !identical(model$data$data, private$data_$data) ||
          !identical(model$data$d_cols, private$data_$d_cols) ||
          model$data$n_obs != private$data_$n_obs ||
          !identical(model$treatment_level, self$treatment_levels[j])) {
          stop("APO components must have synchronized data, levels, repetitions and sample splits.")
        }
      }
      invisible(NULL)
    },
    validate_predictions = function(predictions) {
      labels = names(self$apo_models)
      if (is.null(predictions)) return(setNames(vector("list", length(labels)), labels))
      if (!is.list(predictions) || is.null(names(predictions)) ||
        anyDuplicated(names(predictions)) || !setequal(names(predictions), labels)) {
        stop("external_predictions must contain exactly one entry per treatment display label.")
      }
      predictions = predictions[labels]
      for (j in seq_along(predictions)) {
        if (is.null(predictions[[j]])) stop("external_predictions entries cannot be NULL.")
        predictions[[j]] = validate_apo_external_predictions(
          predictions[[j]], private$data_$n_obs, length(self$smpls))
        if (self$apo_models[[j]]$trimming_threshold == 0 && any(predictions[[j]]$ml_m == 0)) {
          stop("APO positivity requires strictly positive propensities for treatment ",
            labels[j], ".")
        }
      }
      predictions
    },
    clone_nested = function(value) {
      if (inherits(value, "R6")) return(value$clone(deep = TRUE))
      if (data.table::is.data.table(value)) return(data.table::copy(value))
      if (is.list(value)) return(lapply(value, private$clone_nested))
      value
    },
    clone_components = function(components) {
      lapply(components, function(model) {
        result = model$clone(deep = TRUE)
        source = model$.__enclos_env__$private
        target = result$.__enclos_env__$private
        # R6's default deep clone does not descend into lists of learners.
        for (field in c("learner_", "models_", "params_", "tuning_res_")) {
          target[[field]] = private$clone_nested(source[[field]])
        }
        result
      })
    },
    deep_clone = function(name, value) {
      if (name == "apo_models_") return(private$clone_components(value))
      private$clone_nested(value)
    }
  )
)
