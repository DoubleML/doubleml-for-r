encode_apo_group_label = function(x) {
  utils::URLencode(as.character(x), reserved = TRUE, repeated = TRUE)
}

prepare_apo_groups = function(groups, n_obs) {
  checkmate::assert_count(n_obs, positive = TRUE)
  if (!(is.factor(groups) || is.character(groups) || is.logical(groups) ||
    is.numeric(groups)) || !is.null(dim(groups))) {
    stop("groups must be a factor, character, logical, or numeric vector.")
  }
  if (length(groups) != n_obs) {
    stop("groups must have one label for every observation.")
  }
  if (anyNA(groups)) {
    condition = if (is.numeric(groups) && any(is.nan(groups))) "NaN" else "NA"
    stop("groups contains ", condition, " labels.")
  }
  if (is.numeric(groups) && any(!is.finite(groups))) {
    stop("groups contains nonfinite labels.")
  }
  if (any(!nzchar(trimws(as.character(groups))))) {
    stop("groups contains a blank label.")
  }

  levels = if (is.factor(groups)) {
    base::levels(groups)[base::levels(groups) %in% as.character(groups)]
  } else {
    unique(groups)
  }
  labels = as.character(levels)
  if (anyDuplicated(labels)) {
    stop("Group display labels must be unique.")
  }
  values = if (is.factor(groups)) as.character(groups) else groups
  membership = vapply(levels, function(level) as.integer(values == level), integer(n_obs))
  if (length(levels) == 1L) {
    membership = matrix(membership, nrow = n_obs, ncol = 1L)
  }
  colnames(membership) = labels
  list(levels = levels, labels = labels, membership = membership)
}

# nolint start: object_usage_linter.
make_apo_group_framework = function(apo_models, groups) {
  if (!is.list(apo_models) || length(apo_models) == 0L) {
    stop("apo_models must be a nonempty named list of fitted DoubleMLAPO objects.")
  }
  model_names = names(apo_models)
  if (is.null(model_names) || anyNA(model_names) || any(!nzchar(trimws(model_names)))) {
    stop("apo_models must be named by treatment display label.")
  }
  if (anyDuplicated(model_names)) {
    stop("apo_models treatment names must be unique; duplicated names are not allowed.")
  }
  if (!all(vapply(apo_models, inherits, logical(1), "DoubleMLAPO"))) {
    stop("Every apo_models entry must be a DoubleMLAPO object.")
  }

  reference = apo_models[[1L]]
  prepared = prepare_apo_groups(groups, reference$data$n_obs)
  n_obs = reference$data$n_obs
  n_rep = reference$n_rep
  n_treatment = length(apo_models)
  n_group = length(prepared$levels)
  for (j in seq_along(apo_models)) {
    model = apo_models[[j]]
    if (!identical(model$score, "APO")) {
      stop("Group APO inference requires the built-in APO score.")
    }
    if (!identical(model$dml_procedure, "dml2") || !isTRUE(model$apply_cross_fitting)) {
      stop("Group APO inference requires fully cross-fitted DML2 models.")
    }
    if (!identical(model$n_rep, n_rep) || !identical(model$smpls, reference$smpls) ||
      !identical(model$data$data, reference$data$data) ||
      !identical(model$data$d_cols, reference$data$d_cols) ||
      model$data$n_obs != n_obs) {
      stop("APO models must have synchronized data, repetitions and sample splits.")
    }
    if (!identical(model_names[j], as.character(model$treatment_level))) {
      stop("apo_models names must match each treatment display label.")
    }
    if (any(!is.finite(model$all_coef))) {
      stop("Fit every APO model before constructing group effects.")
    }
  }

  treatment_names = encode_apo_group_label(model_names)
  group_names = encode_apo_group_label(prepared$labels)
  coef_names = unlist(lapply(seq_len(n_group), function(g) {
    paste0("GAPO[treatment=", treatment_names, ";group=", group_names[g], "]")
  }), use.names = FALSE)
  if (anyDuplicated(coef_names)) {
    stop("Encoded GAPO coefficient names must be unique.")
  }
  all_coef = matrix(0, n_group * n_treatment, n_rep,
    dimnames = list(coef_names, colnames(reference$all_coef)))
  all_se = matrix(0, n_group * n_treatment, n_rep,
    dimnames = list(coef_names, colnames(reference$all_se)))
  psi = array(0, c(n_obs, n_group * n_treatment, n_rep),
    dimnames = list(dimnames(reference$psi_b)[[1]], coef_names,
      dimnames(reference$psi_b)[[2]]))
  treatment = reference$data$data[[reference$data$d_cols]]

  for (i_rep in seq_len(n_rep)) {
    column = 0L
    for (i_group in seq_len(n_group)) {
      in_group = prepared$membership[, i_group] == 1L
      count = sum(in_group)
      group_label = prepared$labels[i_group]
      if (count < 2L) {
        stop("Group ", group_label, " in repetition ", i_rep,
          " must contain at least two evaluation observations; singleton groups are invalid.")
      }
      if (count <= 5L) {
        warning("Group ", group_label, " in repetition ", i_rep, " contains ", count,
          " evaluation observations; group inference may be unstable.", call. = FALSE)
      }
      prevalence = count / n_obs
      for (i_treatment in seq_len(n_treatment)) {
        column = column + 1L
        model = apo_models[[i_treatment]]
        treatment_label = model_names[i_treatment]
        indicator = make_apo_indicator(treatment, model$treatment_level)
        if (!any(indicator[in_group] == 1L)) {
          warning("Group ", group_label, " has no observed treatment ", treatment_label,
            " rows in repetition ", i_rep, ".", call. = FALSE)
        }
        diagnostic = model$propensity_diagnostics[[i_rep]]
        if (!is.null(diagnostic)) {
          trimmed = diagnostic$raw_propensity != diagnostic$effective_propensity
          if (mean(trimmed[in_group], na.rm = TRUE) > .05) {
            warning("Propensity trimming for treatment ", treatment_label, " in group ",
              group_label, " and repetition ", i_rep,
              " changed more than 5 percent of predictions.", call. = FALSE)
          }
        }
        signal = model$psi_b[, i_rep, 1L]
        if (any(!is.finite(signal))) {
          stop("Group APO signal must be finite for treatment ", treatment_label,
            ", repetition ", i_rep, ".")
        }
        theta = mean(signal[in_group])
        contribution = as.integer(in_group) / prevalence * (signal - theta)
        standard_error = sqrt(sum(contribution^2)) / n_obs
        if (!is.finite(standard_error) || standard_error <= 0) {
          stop("Group APO inference requires a positive standard error for treatment ",
            treatment_label, ", group ", group_label, ", repetition ", i_rep, ".")
        }
        all_coef[column, i_rep] = theta
        all_se[column, i_rep] = standard_error
        psi[, column, i_rep] = contribution
      }
    }
  }

  DoubleMLFramework$new(all_coef, all_se, psi, coef_names,
    smpls = reference$smpls, apply_cross_fitting = TRUE
  )
}
# nolint end: object_usage_linter.
