# Wrong reference ordering, omitted covariance and coerced labels must fail here.
# Package classes and the fixture are loaded by the testthat harness.
# nolint start: object_usage_linter.
fitted_apos_contrast = function(labels = c("control", "chlorine", "filter", "boiling"),
                                n_rep = 1L, ...) {
  fixture = make_apo_data(n_obs = 64L, treatment_levels = labels)
  obj = DoubleMLAPOS$new(fixture$dml_data, "regr.rpart", "classif.rpart",
    draw_sample_splitting = FALSE, ...)
  obj$set_sample_splitting(rep(fixture$smpls, n_rep))
  predictions = setNames(lapply(seq_along(labels), function(j) {
    g = matrix(fixture$conditional_means[, j], 64L, n_rep)
    for (r in seq_len(n_rep)) {
      g[, r] = g[, r] + (r - 1L) * j * fixture$df$x / 10
    }
    list(ml_g = g,
      ml_m = matrix(fixture$propensity[, j], 64L, n_rep))
  }), as.character(labels))
  obj$fit(external_predictions = predictions[names(obj$apo_models)])
  obj
}
# nolint end: object_usage_linter.

test_that("reference contrasts retain APOS order and full covariance", {
  obj = fitted_apos_contrast(n_rep = 2L)
  before = obj$framework
  components = obj$apo_models
  contrast = obj$causal_contrast(reference = "control")
  labels = c("chlorine - control", "filter - control", "boiling - control")
  expect_r6(contrast, "DoubleMLFramework")
  expect_named(contrast$coef, labels)
  for (r in 1:2) {
    expect_equal(unname(contrast$all_coef[, r]),
      unname(before$all_coef[2:4, r] - before$all_coef[1, r]))
    covariance = before$all_cov[, , r]
    variance = diag(covariance)[2:4] + covariance[1, 1] - 2 * covariance[2:4, 1]
    expect_equal(unname(contrast$all_se[, r]^2), unname(variance))
    expect_equal(unname(contrast$psi[, , r]),
      unname(before$psi[, 2:4, r] - before$psi[, 1, r]))
    expect_true(any(abs(covariance[2:4, 1]) > .001))
  }
  expect_identical(obj$framework, before)
  expect_identical(obj$apo_models, components)
  expect_null(before$boot_coef)
})

test_that("references match nominal identity within the selected label family", {
  numeric_obj = fitted_apos_contrast(c(10, 2, 7, 1))
  contrast = numeric_obj$causal_contrast(reference = 7L)
  expect_named(contrast$coef, c("10 - 7", "2 - 7", "1 - 7"))
  expect_equal(unname(contrast$coef),
    unname(numeric_obj$coef[c(1, 2, 4)] - numeric_obj$coef[3]))
  expect_error(numeric_obj$causal_contrast(reference = "7"), "type")
  expect_error(numeric_obj$causal_contrast(reference = factor("7")), "type")
  character_obj = fitted_apos_contrast(c("10", "02", "2", "7"))
  expect_named(character_obj$causal_contrast(reference = factor("02"))$coef,
    c("10 - 02", "2 - 02", "7 - 02"))
  expect_error(character_obj$causal_contrast(reference = 2), "type")
  subset = fitted_apos_contrast(treatment_levels = c("boiling", "filter"))
  expect_error(subset$causal_contrast(reference = "control"), "reference|absent")
  expect_named(subset$causal_contrast(reference = "filter")$coef, "boiling - filter")
  singleton = fitted_apos_contrast(treatment_levels = "control")
  expect_error(singleton$causal_contrast(reference = "control"), "two|contrast")
})

# Permuted matrix columns must not silently change a weighted causal estimand.
test_that("arbitrary contrasts reorder named columns and preserve meaningful rows", {
  obj = fitted_apos_contrast(n_rep = 2L)
  weights = rbind("filter minus chlorine" = c(0, -1, 1, 0),
    "pooled minus control" = c(-1, .2, .3, .5))
  colnames(weights) = names(obj$coef)
  contrast = obj$causal_contrast(contrast = weights[, c(4, 2, 1, 3)])
  expect_equal(contrast$all_coef, weights %*% obj$framework$all_coef)
  for (r in 1:2) {
    expected = weights %*% obj$framework$all_cov[, , r] %*% t(weights)
    expect_equal(contrast$all_cov[, , r], expected)
    expect_equal(contrast$all_se[, r], sqrt(diag(expected)))
  }
  unnamed = obj$causal_contrast(contrast = unname(weights))
  expect_named(unnamed$coef, c("contrast1", "contrast2"))
  expect_equal(unname(unnamed$all_coef), unname(contrast$all_coef))
  expect_named(obj$causal_contrast(contrast = weights[1, , drop = FALSE])$coef,
    "filter minus chlorine")
})

test_that("contrast validation rejects ambiguous arguments and malformed matrices", {
  obj = fitted_apos_contrast()
  weights = matrix(c(-1, 1, 0, 0), 1L)
  expect_error(obj$causal_contrast(), "exactly one")
  expect_error(obj$causal_contrast("control", weights), "exactly one")
  for (reference in list("absent", character(), c("control", "filter"),
    NA_character_, Inf, TRUE, list("control"), matrix("control"))) {
    expect_error(obj$causal_contrast(reference = reference))
  }
  for (invalid in list(c(-1, 1, 0, 0), matrix(0, 1, 3), matrix(0, 1, 5), matrix(0, 0, 4),
    matrix("0", 1, 4), matrix(c(Inf, 1, 0, 0), 1),
    matrix(c(NA, 1, 0, 0), 1))) {
    expect_error(obj$causal_contrast(contrast = invalid), "contrast|matrix|finite")
  }
  for (labels in list(c("control", "chlorine", "filter", "extra"),
    c("control", "chlorine", "filter", "filter"),
    c("control", "chlorine", "filter", NA_character_))) {
    colnames(weights) = labels
    expect_error(obj$causal_contrast(contrast = weights), "column")
  }
  colnames(weights) = NULL
  for (labels in list(c("same", "same"), c("valid", " "), c("valid", NA_character_))) {
    invalid = rbind(weights, weights)
    rownames(invalid) = labels
    expect_error(obj$causal_contrast(contrast = invalid), "name")
  }
})

# Separate per-estimand multipliers or bootstrap on untransformed t's fails.
test_that("APOS and contrasts bootstrap common observation-level multipliers", {
  obj = fitted_apos_contrast(n_rep = 2L)
  contrast = obj$causal_contrast("control")
  for (method in c("normal", "Bayes", "wild")) {
    set.seed(82)
    returned = withVisible(obj$bootstrap(method, 400L))
    expect_identical(returned$value, obj)
    expect_false(returned$visible)
    set.seed(82)
    contrast$bootstrap(method, 400L)
    expected = sweep(obj$framework$boot_coef[2:4, , drop = FALSE], 2,
      obj$framework$boot_coef[1, ], "-")
    expect_equal(unname(contrast$boot_coef), unname(expected))
    expect_equal(unname(contrast$boot_t_stat),
      unname(expected / contrast$all_se[, rep(1:2, each = 400L)]))
  }
  for (framework in list(obj$framework, contrast)) {
    pointwise = framework$confint()
    joint = framework$confint(joint = TRUE)
    expect_true(all(joint[, 1] <= pointwise[, 1]))
    expect_true(all(joint[, 2] >= pointwise[, 2]))
    ordered = order(abs(framework$t_stat), decreasing = TRUE)
    raw = vapply(seq_along(ordered), function(j) {
      remaining = ordered[j:length(ordered)]
      maxima = apply(abs(framework$boot_t_stat[remaining, , drop = FALSE]), 2, max)
      mean(maxima > abs(framework$t_stat[ordered[j]]))
    }, numeric(1))
    expect_equal(unname(framework$p_adjust(return_matrix = FALSE)),
      cummax(raw)[order(ordered)])
  }
  expect_equal(obj$confint(), obj$framework$confint())
  expect_equal(obj$confint(2:1, TRUE, .9), obj$framework$confint(2:1, TRUE, .9))
  expect_equal(obj$p_adjust(), obj$framework$p_adjust())
  expect_equal(obj$p_adjust("holm", FALSE), p.adjust(obj$pval, "holm"))
  expect_equal(obj$p_adjust("rw", FALSE), obj$framework$p_adjust("rw", FALSE))
})

test_that("APOS inference reports missing fit and bootstrap before delegation", {
  fixture = make_apo_data()
  obj = DoubleMLAPOS$new(fixture$dml_data, "regr.rpart", "classif.rpart",
    draw_sample_splitting = FALSE)
  expect_error(obj$causal_contrast("control"), "fit")
  expect_error(obj$bootstrap(), "fit")
  expect_error(obj$confint(), "fit")
  expect_error(obj$p_adjust(), "fit")
  obj = fitted_apos_contrast()
  expect_error(obj$confint(joint = TRUE), "bootstrap")
  expect_error(obj$p_adjust(), "bootstrap")
  expect_equal(obj$p_adjust("bonferroni", FALSE), p.adjust(obj$pval, "bonferroni"))
  obj$set_sample_splitting(obj$smpls)
  expect_error(obj$causal_contrast("control"), "fit")
  expect_error(obj$bootstrap(), "fit")
  expect_error(obj$confint(), "fit")
  expect_error(obj$p_adjust(), "fit")
})
