compute_sensitivity_bias = function(sigma2, nu2, psi_sigma2, psi_nu2) {
  max_bias = sqrt(sigma2 * nu2)
  numerator = sweep(psi_nu2, c(2, 3), sigma2, "*") +
    sweep(psi_sigma2, c(2, 3), nu2, "*")
  psi_max_bias = sweep(numerator, c(2, 3), 2 * max_bias, "/")
  list(max_bias = max_bias, psi_max_bias = psi_max_bias)
}

validate_sensitivity_elements = function(
  elements, n_obs, n_coef, n_rep, coef_names, rep_names
) {
  required = c(
    "sigma2", "nu2", "psi_sigma2", "psi_nu2", "riesz_rep",
    "max_bias", "psi_max_bias"
  )
  if (!is.list(elements) || is.null(names(elements)) ||
    anyDuplicated(names(elements)) || !setequal(names(elements), required)) {
    stop(
      "sensitivity_elements must contain exactly sigma2, nu2, psi_sigma2, ",
      "psi_nu2, riesz_rep, max_bias and psi_max_bias."
    )
  }
  elements = elements[required]
  scalar_keys = c("sigma2", "nu2", "max_bias")
  influence_keys = c("psi_sigma2", "psi_nu2", "riesz_rep", "psi_max_bias")
  scalar_dim = as.integer(c(n_coef, n_rep))
  influence_dim = as.integer(c(n_obs, n_coef, n_rep))

  for (key in scalar_keys) {
    value = elements[[key]]
    if (!is.matrix(value) || !is.numeric(value) || !identical(dim(value), scalar_dim)) {
      stop(key, " must be a numeric ", n_coef, " by ", n_rep, " matrix.")
    }
    if (any(!is.finite(value))) stop(key, " must contain only finite values.")
  }
  for (key in influence_keys) {
    value = elements[[key]]
    if (!is.numeric(value) || length(dim(value)) != 3L ||
      !identical(dim(value), influence_dim)) {
      stop(
        key, " must be a numeric ", n_obs, " by ", n_coef, " by ", n_rep,
        " array."
      )
    }
    if (any(!is.finite(value))) stop(key, " must contain only finite values.")
  }
  for (key in c("sigma2", "nu2", "max_bias")) {
    if (any(elements[[key]] <= 0)) stop(key, " must contain only positive values.")
  }
  centered_keys = c("psi_sigma2", "psi_nu2", "psi_max_bias")
  for (key in centered_keys) {
    means = apply(elements[[key]], c(2, 3), mean)
    scale = max(1, max(abs(elements[[key]])))
    if (any(abs(means) > sqrt(.Machine$double.eps) * scale)) {
      stop(key, " must be centered within every estimand and repetition.")
    }
  }

  expected = compute_sensitivity_bias(
    elements$sigma2, elements$nu2,
    elements$psi_sigma2, elements$psi_nu2
  )
  tolerance = sqrt(.Machine$double.eps)
  if (!isTRUE(all.equal(elements$max_bias, expected$max_bias,
    tolerance = tolerance, check.attributes = FALSE))) {
    stop("max_bias is inconsistent with sigma2 and nu2.")
  }
  if (!isTRUE(all.equal(elements$psi_max_bias, expected$psi_max_bias,
    tolerance = tolerance, check.attributes = FALSE))) {
    stop("psi_max_bias is inconsistent with the primitive sensitivity scores.")
  }

  for (key in scalar_keys) {
    dimnames(elements[[key]]) = list(coef_names, rep_names)
  }
  for (key in influence_keys) {
    dimnames(elements[[key]]) = list(NULL, coef_names, rep_names)
  }
  elements
}
