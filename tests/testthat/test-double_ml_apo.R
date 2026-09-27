# nolint start: object_usage_linter.
# The package test harness loads the R6 classes used by this fixture.
# Fixed outcomes catch incorrect level selection, residual weighting and scaling.
apo_fixed_fixture = function(d = c("a", "b", "c", "a", "b", "a"), level = "a", n_rep = 1) {
  data = DoubleMLData$new(data.frame(y = c(3, 8, 5, 7, 2, 9), d = d, x = 1:6),
    y_col = "y", d_cols = "d", x_cols = "x")
  obj = DoubleMLAPO$new(data, "regr.rpart", "classif.rpart", level,
    n_folds = 2, n_rep = n_rep, draw_sample_splitting = FALSE)
  split = list(train_ids = list(4:6, 1:3), test_ids = list(1:3, 4:6))
  obj$set_sample_splitting(rep(list(split), n_rep))
  list(obj = obj, data = data, predictions = list(
    ml_g = matrix(c(1, 2, 3, 4, 5, 6), 6, n_rep),
    ml_m = matrix(c(.5, .25, .5, .25, .5, .5), 6, n_rep)))
}
# nolint end: object_usage_linter.

test_that("fixed APO scores and inference match hand calculations", {
  fixture = apo_fixed_fixture()
  obj = fixture$obj
  expect_identical(obj$fit(store_predictions = TRUE, store_models = TRUE,
    external_predictions = fixture$predictions), obj)
  expect_equal(as.vector(obj$psi_b), c(5, 2, 3, 16, 5, 12))
  expect_equal(as.vector(obj$psi_a), rep(-1, 6))
  expect_equal(unname(obj$coef), 43 / 6)
  expected_psi = c(-13, -31, -25, 53, -13, 29) / 6
  expect_equal(as.vector(obj$psi), expected_psi)
  expect_equal(unname(obj$se), sqrt(5574 / 1296))
  expect_equal(obj$t_stat, obj$coef / obj$se)
  expect_equal(obj$pval, 2 * pnorm(-abs(obj$t_stat)))
  for (value in list(obj$coef, obj$se, obj$t_stat, obj$pval)) {
    expect_named(value, "a")
  }
  for (key in c("ml_g", "ml_m")) {
    expect_equal(dim(obj$predictions[[key]]), c(6L, 1L, 1L))
    expect_equal(as.vector(obj$predictions[[key]]), as.vector(fixture$predictions[[key]]))
    expect_true(all(vapply(obj$models[[key]]$d[[1]], is.null, logical(1))))
  }
  expect_equal(fixture$data$data$d, c("a", "b", "c", "a", "b", "a"))
  expect_setequal(obj$params_names(), c("ml_g", "ml_m"))
})

test_that("APO level selection respects factor, character and numeric equality", {
  cases = list(
    list(d = c("a", "b", "c", "a", "b", "a"), level = "a"),
    list(d = factor(c("a", "b", "c", "a", "b", "a"), levels = c("c", "b", "a")),
      level = factor("a", levels = c("c", "b", "a"))),
    list(d = c(10, 2, 7, 10, 2, 10), level = 10))
  for (case in cases) {
    fixture = apo_fixed_fixture(case$d, case$level)
    fixture$obj$fit(external_predictions = fixture$predictions)
    expected = fixture$predictions$ml_g[, 1] + as.integer(case$d == case$level) /
      fixture$predictions$ml_m[, 1] * (fixture$data$data$y - fixture$predictions$ml_g[, 1])
    expect_equal(as.vector(fixture$obj$psi_b), expected)
    expect_named(fixture$obj$coef, as.character(case$level))
    expect_identical(fixture$obj$treatment_level, case$level)
  }
})

test_that("APO consumes each repetition without reshaping external predictions", {
  fixture = apo_fixed_fixture(n_rep = 2)
  fixture$predictions$ml_g[, 2] = 0
  fixture$obj$fit(store_predictions = TRUE, external_predictions = fixture$predictions)
  expect_equal(as.vector(fixture$obj$psi_b[, 2, 1]), c(6, 0, 0, 28, 0, 18))
  expect_equal(dim(fixture$obj$predictions$ml_g), c(6L, 2L, 1L))
  expect_equal(fixture$obj$all_coef[1, ], c(43 / 6, 52 / 6))
})

test_that("APO rejects malformed external predictions before fitting", {
  fixture = apo_fixed_fixture()
  for (key in c("ml_g", "ml_m")) {
    expect_error(fixture$obj$fit(external_predictions = fixture$predictions[key]), "keys")
    bad = fixture$predictions
    bad[[key]] = t(bad[[key]])
    expect_error(fixture$obj$fit(external_predictions = bad), "matrix")
    bad[[key]] = matrix(1, 6, 2)
    expect_error(fixture$obj$fit(external_predictions = bad), "matrix")
    for (value in c(NA_real_, Inf)) {
      bad[[key]] = fixture$predictions[[key]]
      bad[[key]][1, 1] = value
      expect_error(fixture$obj$fit(external_predictions = bad), "finite")
    }
  }
  for (value in c(-.1, 1.1)) {
    bad = fixture$predictions
    bad$ml_m[1, 1] = value
    expect_error(fixture$obj$fit(external_predictions = bad), "probabilities")
  }
  expect_true(all(is.na(fixture$obj$coef)))
})

test_that("APO validates its data, labels, score and learner roles", {
  data = apo_fixed_fixture()$data
  construct = function(...) DoubleMLAPO$new(data, "regr.rpart", "classif.rpart", ...)
  expect_error(construct("absent"), "absent")
  expect_error(construct(c("a", "b")), "one")
  expect_error(construct("a", score = "ATE"), "APO")
  expect_error(DoubleMLAPO$new(data, "classif.rpart", "classif.rpart", "a"), "ml_g")
  expect_error(DoubleMLAPO$new(data, "regr.rpart", "regr.rpart", "a"), "ml_m")
  data_iv = DoubleMLData$new(data.frame(y = 1:6, d = rep(0:1, 3), x = 1:6, z = 1:6),
    y_col = "y", d_cols = "d", x_cols = "x", z_cols = "z")
  expect_error(DoubleMLAPO$new(data_iv, "regr.rpart", "classif.rpart", 1), "instrument")
  data_multi = DoubleMLData$new(data.frame(y = 1:6, d = rep(0:1, 3), d2 = rep(1:2, 3), x = 1:6),
    y_col = "y", d_cols = c("d", "d2"), x_cols = "x")
  expect_error(DoubleMLAPO$new(data_multi, "regr.rpart", "classif.rpart", 1), "one treatment")
})

test_that("APO staged paths fail explicitly without reusing fixed predictions", {
  fixture = apo_fixed_fixture()
  fixture$obj$fit(external_predictions = fixture$predictions)
  expect_error(fixture$obj$fit(), "Learner-based")
  obj = DoubleMLAPO$new(fixture$data, "regr.rpart", "classif.rpart", "a",
    n_folds = 2, normalize_ipw = TRUE)
  expect_error(obj$fit(external_predictions = fixture$predictions), "normalize_ipw.*not yet")
})

test_that("APO rejects clustered data", {
  data = DoubleMLClusterData$new(data.frame(y = 1:6, d = rep(0:1, 3), x = 1:6,
    cluster = rep(1:3, each = 2)), y_col = "y", d_cols = "d",
    cluster_cols = "cluster", x_cols = "x")
  expect_error(DoubleMLAPO$new(data, "regr.rpart", "classif.rpart", 1), "Cluster")
})
