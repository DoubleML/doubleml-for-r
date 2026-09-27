# nolint start: object_usage_linter.
# Package internals and DoubleMLData are loaded by the testthat harness.
# Balanced nominal assignment and Y(d) = d_index + d_index * X + epsilon.
# Numeric label values do not enter the outcome model.
make_apo_data = function(
  n_obs = 60L, treatment_levels = c("control", "filter", "boiling"), seed = 314L
) {
  treatment_levels = get_apo_levels(treatment_levels, treatment_levels)
  n_levels = length(treatment_levels)
  checkmate::assert_count(n_obs)
  if (n_levels < 2L || n_obs < 2L * n_levels) {
    stop("APO fixtures require at least two levels and two observations per level.")
  }
  # Restore the caller's RNG state so fixtures cannot affect unrelated tests.
  has_seed = exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (has_seed) {
    old_seed = get(".Random.seed", envir = .GlobalEnv)
  }
  on.exit({
    if (has_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  set.seed(seed)
  level_index = rep(seq_len(n_levels), length.out = n_obs)
  x = rnorm(n_obs)
  conditional_means = outer(x, seq_len(n_levels), "*") +
    matrix(rep(seq_len(n_levels), each = n_obs), nrow = n_obs)
  colnames(conditional_means) = as.character(treatment_levels)
  df = data.frame(
    y = conditional_means[cbind(seq_len(n_obs), level_index)] + rnorm(n_obs),
    d = treatment_levels[level_index], x = x
  )
  # Alternating occurrences within each level keep both classes in every fold.
  fold = ((seq_len(n_obs) - 1L) %/% n_levels) %% 2L + 1L
  test_ids = lapply(1:2, function(k) which(fold == k))
  train_ids = lapply(test_ids, function(ids) setdiff(seq_len(n_obs), ids))
  list(
    df = df,
    dml_data = DoubleMLData$new(df, y_col = "y", d_cols = "d", x_cols = "x"),
    treatment_levels = treatment_levels,
    conditional_means = conditional_means,
    propensity = matrix(1 / n_levels,
      nrow = n_obs, ncol = n_levels,
      dimnames = list(NULL, as.character(treatment_levels))
    ),
    true_apo = setNames(seq_len(n_levels), as.character(treatment_levels)),
    smpls = list(list(train_ids = train_ids, test_ids = test_ids))
  )
}

# Independent test oracle: use direct equality rather than production label helpers.
apo_score_from_predictions = function(
  y, d, treatment_level, g_hat, p_hat,
  trimming_threshold = 0, normalize_ipw = FALSE
) {
  p_hat = pmin(pmax(p_hat, trimming_threshold), 1 - trimming_threshold)
  weights = as.integer(d == treatment_level) / p_hat
  if (normalize_ipw) {
    weights = weights / mean(weights)
  }
  psi_b = g_hat + weights * (y - g_hat)
  theta = mean(psi_b)
  psi = psi_b - theta
  list(
    psi_a = rep(-1, length(y)), psi_b = psi_b, theta = theta, psi = psi,
    se = sqrt(mean(psi^2) / length(y))
  )
}
# nolint end: object_usage_linter.
