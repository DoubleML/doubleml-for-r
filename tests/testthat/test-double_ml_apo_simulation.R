# Biased score shifts or ordinal treatment coding must fail these population checks.
# Oracle nuisances isolate the score and inference from learner approximation error.
test_that("APOs and contrasts recover analytical multinomial population means", {
  seeds = c(104L, 209L, 317L)
  n_obs = 900L
  labels = c(10, 2, 7)
  alpha = c(-1, 1, 3)
  beta = c(-.7, .5, 1.3)
  assignment_slope = c(-.4, 0, .4)
  # Prechosen two-sided Bonferroni normal bound: family error .001 over 15 checks.
  # Each standardized error uses the estimator's reported Monte Carlo SE.
  critical = qnorm(1 - .001 / (2 * length(seeds) * 5))

  for (seed in seeds) {
    set.seed(seed)
    x = rnorm(n_obs)
    propensity = exp(outer(x, assignment_slope))
    propensity = propensity / rowSums(propensity)
    u = runif(n_obs)
    category = 1L + (u > propensity[, 1]) +
      (u > rowSums(propensity[, 1:2]))
    means = sweep(outer(x, beta), 2, alpha, "+")
    y = means[cbind(seq_len(n_obs), category)] + rnorm(n_obs)
    data = DoubleMLData$new(data.frame(y = y, d = labels[category], x = x),
      y_col = "y", d_cols = "d", x_cols = "x")
    test_ids = list(seq(1L, n_obs, 2L), seq(2L, n_obs, 2L))
    split = list(train_ids = rev(test_ids), test_ids = test_ids)
    obj = DoubleMLAPOS$new(data, "regr.rpart", "classif.rpart",
      treatment_levels = labels, trimming_threshold = 0,
      draw_sample_splitting = FALSE)
    obj$set_sample_splitting(list(split))
    predictions = setNames(lapply(seq_along(labels), function(j) {
      list(ml_g = matrix(means[, j]), ml_m = matrix(propensity[, j]))
    }), as.character(labels))
    obj$fit(external_predictions = predictions)
    contrast = obj$causal_contrast(reference = 10)
    estimate = c(obj$coef, contrast$coef)
    truth = c(alpha, 2, 4)
    se = c(obj$se, contrast$se)
    z = abs(estimate - truth) / se
    info = paste0("seed=", seed, ", ", names(estimate),
      ", estimate=", signif(estimate, 5), ", truth=", truth,
      ", MC SE=", signif(se, 5), ", |z|=", signif(z, 4))
    expect_true(all(is.finite(se) & se > 0))
    for (j in seq_along(z)) {
      expect_true(unname(z[j]) < critical, info = info[j])
    }
  }
})
