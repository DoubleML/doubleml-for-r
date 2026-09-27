# These tests catch recoding by rank, repetition mixing and probability extraction
# by position. Fixtures and deterministic learners come from helper-18-dml_apo.R.
# nolint start: object_usage_linter.
AposReversedClassif = R6::R6Class("AposReversedClassif",
  inherit = ApoLeakageClassif,
  private = list(
    .predict = function(task) {
      prediction = super$.predict(task)
      prediction$prob = prediction$prob[, c("1", "0"), drop = FALSE]
      prediction
    }
  )
)

apos_edge_fit = function(fixture, levels, learner = ApoLeakageClassif$new()) {
  obj = DoubleMLAPOS$new(fixture$dml_data, ApoLeakageRegr$new(), learner,
    treatment_levels = levels, draw_sample_splitting = FALSE)
  # A genuinely different partition in repetition two, stratified by category.
  second_fold = ((seq_len(nrow(fixture$df)) - 1L) %/% 8L) %% 2L + 1L
  second_test = lapply(1:2, function(k) which(second_fold == k))
  second = list(train_ids = rev(second_test), test_ids = second_test)
  obj$set_sample_splitting(list(fixture$smpls[[1]], second))
  obj$fit(store_predictions = TRUE)
  obj
}
# nolint end: object_usage_linter.

test_that("four nominal levels and requested subsets survive recoding and repeated splits", {
  fixture = make_apo_data(n_obs = 80L,
    treatment_levels = c("control", "chlorine", "filter", "boiling"))
  original = fixture$df$d
  families = list(
    c("control", "chlorine", "filter", "boiling"),
    c(10, -3, 7, 2),
    factor(c("control", "chlorine", "filter", "boiling"),
      levels = c("boiling", "filter", "control", "chlorine"))
  )
  baseline = NULL
  for (labels in families) {
    fixture$df$d = labels[match(original, c("control", "chlorine", "filter", "boiling"))]
    fixture$dml_data = DoubleMLData$new(fixture$df,
      y_col = "y", d_cols = "d", x_cols = "x")
    selected = labels[c(4, 2)]
    obj = apos_edge_fit(fixture, selected)
    expect_named(obj$coef, as.character(selected))
    expect_equal(dim(obj$framework$psi), c(80L, 2L, 2L))
    if (is.null(baseline)) baseline = obj
    expect_equal(unname(obj$coef), unname(baseline$coef))
    expect_equal(unname(obj$framework$all_cov), unname(baseline$framework$all_cov))
    for (j in seq_along(selected)) {
      label = as.character(selected[j])
      expected = (1:80) + as.integer(fixture$df$d == selected[j]) /
        (.2 + (1:80) / 1000) * (fixture$df$y - (1:80))
      for (r in 1:2) {
        expect_equal(as.vector(obj$apo_models[[label]]$psi_b[, r, 1]), expected)
      }
      expect_equal(unname(obj$coef[j]), mean(expected))
    }
    contrast = obj$causal_contrast(reference = selected[2])
    expect_equal(unname(contrast$coef), unname(obj$coef[1] - obj$coef[2]))
    # Inferred factor order must not alter the underlying label-to-score mapping.
    inferred = apos_edge_fit(fixture, NULL)
    expect_equal(inferred$coef[as.character(selected)], obj$coef)
    if (is.factor(labels)) expect_named(inferred$coef, levels(labels))
  }
})

test_that("APOS extracts positive probabilities by name in either column order", {
  fixture = make_apo_data(n_obs = 32L,
    treatment_levels = c("control", "chlorine", "filter", "boiling"))
  fits = lapply(list(ApoLeakageClassif$new(), AposReversedClassif$new()), function(m) {
    apos_edge_fit(fixture, c("filter", "control"), m)
  })
  for (obj in fits) {
    for (label in names(obj$coef)) {
      for (r in 1:2) {
        expect_equal(as.vector(obj$predictions[[label]]$ml_m[, r, 1]),
          .2 + (1:32) / 1000)
      }
    }
  }
  expect_equal(fits[[1]]$framework$psi, fits[[2]]$framework$psi)
  expect_equal(fits[[1]]$coef, fits[[2]]$coef)
})

test_that("a rare supported category fits and near-zero propensity is truncated", {
  labels = c("control", "chlorine", "filter", "rare")
  fixture = make_apo_data(n_obs = 120L, treatment_levels = labels)
  fixture$df$d = rep(labels, c(60, 36, 20, 4))
  fixture$dml_data = DoubleMLData$new(fixture$df,
    y_col = "y", d_cols = "d", x_cols = "x")
  test_ids = list(seq(1L, 120L, 2L), seq(2L, 120L, 2L))
  obj = DoubleMLAPOS$new(fixture$dml_data, ApoLeakageRegr$new(),
    ApoLeakageClassif$new(), treatment_levels = c("rare", "control"),
    trimming_threshold = .01, draw_sample_splitting = FALSE)
  obj$set_sample_splitting(list(list(train_ids = rev(test_ids), test_ids = test_ids)))
  obj$fit()
  expect_true(all(is.finite(c(obj$coef, obj$se, obj$framework$psi))))
  rare_ids = which(fixture$df$d == "rare")
  expect_equal(length(rare_ids), 4L)
  for (ids in obj$smpls[[1]]$train_ids) {
    expect_equal(sum(fixture$df$d[ids] == "rare"), 2L)
  }

  predictions = list(
    rare = list(ml_g = matrix(2, 120, 1), ml_m = matrix(4 / 120, 120, 1)),
    control = list(ml_g = matrix(1, 120, 1), ml_m = matrix(.5, 120, 1))
  )
  predictions$rare$ml_m[rare_ids[1], 1] = 1e-12
  obj$fit(external_predictions = predictions)
  # Only the first rare observation is clipped; other rare weights remain 30.
  expected = rep(2, 120)
  expected[rare_ids] = 2 + c(100, 30, 30, 30) * (fixture$df$y[rare_ids] - 2)
  expect_equal(as.vector(obj$apo_models$rare$psi_b), expected)
  expect_equal(unname(obj$coef["rare"]), mean(expected))
  expect_equal(unname(obj$se["rare"]), sqrt(mean((expected - mean(expected))^2) / 120))
})

test_that("invalid probabilities in later levels and repetitions fail before fitting", {
  fixture = make_apo_data(n_obs = 32L,
    treatment_levels = c("control", "chlorine", "filter", "boiling"))
  obj = DoubleMLAPOS$new(fixture$dml_data, ApoLeakageRegr$new(),
    ApoLeakageClassif$new(reject_training = TRUE),
    treatment_levels = c("boiling", "control"), trimming_threshold = 0,
    draw_sample_splitting = FALSE)
  obj$set_sample_splitting(rep(fixture$smpls, 2))
  good = setNames(lapply(1:2, function(j) {
    list(ml_g = matrix(j, 32, 2), ml_m = matrix(.25, 32, 2))
  }), c("boiling", "control"))
  for (value in c(-1e-12, 1 + 1e-12, NA_real_, Inf, 0)) {
    bad = good
    bad$control$ml_m[32, 2] = value
    expect_error(obj$fit(external_predictions = bad),
      if (!is.finite(value)) "finite" else if (value == 0) "positive" else "probabilities")
    expect_null(obj$framework)
  }
})
