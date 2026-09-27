# Nominal treatment labels are compared within their original type family.
get_apo_levels = function(d, treatment_levels = NULL) {
  validate_labels = function(labels, name) {
    if (!(is.factor(labels) || is.character(labels) || is.numeric(labels)) ||
      !is.null(dim(labels))) {
      stop(name, " must be a factor, character, or numeric vector.")
    }
    if (length(labels) == 0L) {
      stop(name, " must not be empty.")
    }
    if (anyNA(labels)) {
      condition = if (is.numeric(labels) && any(is.nan(labels))) "NaN" else "NA"
      stop(name, " contains ", condition, " labels.")
    }
    if (is.numeric(labels) && any(!is.finite(labels))) {
      stop(name, " contains nonfinite labels.")
    }
    if (any(as.character(labels) == "")) {
      stop(name, " contains an empty character label.")
    }
  }
  validate_labels(d, "Treatment")
  observed = if (is.factor(d)) {
    levels(d)[levels(d) %in% as.character(d)]
  } else {
    unique(d)
  }
  if (is.null(treatment_levels)) {
    return(observed)
  }
  validate_labels(treatment_levels, "Requested treatment levels")
  requested = if (is.factor(treatment_levels)) as.character(treatment_levels) else treatment_levels
  if (is.numeric(d) != is.numeric(requested)) {
    stop("Requested treatment levels must have the same label type as the treatment.")
  }
  if (anyDuplicated(requested)) {
    stop(
      "Requested treatment levels contain duplicated label ",
      requested[duplicated(requested)][1L], "."
    )
  }
  absent = requested[!requested %in% observed]
  if (length(absent) > 0L) {
    stop("Requested treatment label ", absent[1L], " is absent from the data.")
  }
  requested
}

make_apo_indicator = function(d, treatment_level) {
  if (length(treatment_level) != 1L) {
    stop("Exactly one treatment level must be supplied.")
  }
  level = get_apo_levels(d, treatment_level)
  if (is.factor(d)) {
    d = as.character(d)
  }
  as.integer(d == level)
}

# smpls uses the base model's outer repetition list and inner fold lists.
# General index/partition validation remains the base model's responsibility.
validate_apo_sample_splitting = function(smpls, indicator, treatment_level) {
  for (i_rep in seq_along(smpls)) {
    for (i_fold in seq_along(smpls[[i_rep]]$train_ids)) {
      training_indicator = indicator[smpls[[i_rep]]$train_ids[[i_fold]]]
      context = paste0("treatment ", treatment_level, ", repetition ", i_rep, ", fold ", i_fold)
      if (!any(training_indicator == 1L)) {
        stop(context, ": missing outcome-training rows (indicator class 1).")
      }
      if (!any(training_indicator == 0L)) {
        stop(context, ": missing indicator class 0 in propensity training.")
      }
      if (length(intersect(smpls[[i_rep]]$train_ids[[i_fold]],
        smpls[[i_rep]]$test_ids[[i_fold]])) > 0L) {
        stop(context, ": training and test rows overlap; supply disjoint ",
          "training/test splits for out-of-fold APO nuisance predictions.")
      }
    }
  }
  invisible(NULL)
}

# nolint start: object_length_linter.
# This internal function name is part of the approved APO interface.
validate_apo_external_predictions = function(external_predictions, n_obs, n_rep) {
  if (is.null(external_predictions)) {
    return(NULL)
  }
  keys = c("ml_g", "ml_m")
  if (!is.list(external_predictions) || is.null(names(external_predictions)) ||
    !setequal(names(external_predictions), keys) || anyDuplicated(names(external_predictions))) {
    stop("external_predictions must be a named list with keys ml_g and ml_m.")
  }
  for (key in keys) {
    predictions = external_predictions[[key]]
    if (!is.matrix(predictions) || !is.numeric(predictions) ||
      !identical(dim(predictions), as.integer(c(n_obs, n_rep)))) {
      stop(key, " external predictions must be a numeric ", n_obs, " by ", n_rep, " matrix.")
    }
    if (any(!is.finite(predictions))) {
      stop(key, " external predictions must be finite.")
    }
  }
  if (any(external_predictions$ml_m < 0 | external_predictions$ml_m > 1)) {
    stop("ml_m external probabilities must lie in [0, 1].")
  }
  external_predictions
}
# nolint end: object_length_linter.
