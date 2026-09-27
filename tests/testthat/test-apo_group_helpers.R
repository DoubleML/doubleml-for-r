# Package internals are loaded by the testthat harness.
# nolint start: object_usage_linter.
test_that("group preparation preserves nominal order and row alignment", {
  cases = list(
    list(
      groups = factor(c("z", "a", "z", "a"), levels = c("a", "z", "unused")),
      levels = c("a", "z"), labels = c("a", "z"),
      membership = rbind(c(0L, 1L), c(1L, 0L), c(0L, 1L), c(1L, 0L))
    ),
    list(
      groups = c("z", "a", "z", "c"), levels = c("z", "a", "c"),
      labels = c("z", "a", "c"),
      membership = rbind(c(1L, 0L, 0L), c(0L, 1L, 0L),
        c(1L, 0L, 0L), c(0L, 0L, 1L))
    ),
    list(
      groups = c(10, 2, 7, 10), levels = c(10, 2, 7),
      labels = c("10", "2", "7"),
      membership = rbind(c(1L, 0L, 0L), c(0L, 1L, 0L),
        c(0L, 0L, 1L), c(1L, 0L, 0L))
    ),
    list(
      groups = c(TRUE, FALSE, TRUE, FALSE), levels = c(TRUE, FALSE),
      labels = c("TRUE", "FALSE"),
      membership = rbind(c(1L, 0L), c(0L, 1L), c(1L, 0L), c(0L, 1L))
    )
  )

  for (case in cases) {
    result = prepare_apo_groups(case$groups, length(case$groups))
    expect_identical(result$levels, case$levels)
    expect_identical(result$labels, case$labels)
    expect_identical(unname(result$membership), case$membership)
    expect_identical(colnames(result$membership), case$labels)
    expect_equal(rowSums(result$membership), rep(1, length(case$groups)))
  }

  named = setNames(c("b", "a", "b"), c("row-3", "row-1", "row-2"))
  prepared = prepare_apo_groups(named, 3L)
  expect_identical(prepared$levels, c("b", "a"))
  expect_identical(unname(prepared$membership),
    rbind(c(1L, 0L), c(0L, 1L), c(1L, 0L)))
})

test_that("group preparation rejects noncategorical or ambiguous labels", {
  invalid = list(
    character(),
    matrix(c("a", "b"), ncol = 1),
    data.frame(group = c("a", "b")),
    list("a", "b"),
    c("a", NA_character_),
    c(1, NaN),
    c(1, Inf),
    c("a", ""),
    c("a", "   ")
  )
  for (groups in invalid) {
    expect_error(prepare_apo_groups(groups, length(groups)))
  }
  expect_error(prepare_apo_groups(c("a", "b"), 3L), "length|observation")
  expect_error(prepare_apo_groups(c(1, 1 + 1e-15), 2L), "display|unique")
})

test_that("group name encoding is deterministic and collision free", {
  labels = c(";", "%3B", "%", "a b", "é", "10")
  encoded = encode_apo_group_label(labels)
  expect_identical(encoded, c("%3B", "%253B", "%25", "a%20b", "%C3%A9", "10"))
  expect_equal(anyDuplicated(encoded), 0L)
  expect_identical(encode_apo_group_label(labels), encoded)
})

make_group_apo_models = function(n_obs = 14L, n_rep = 2L,
                                  trimming_threshold = .01) {
  fixture = make_apo_data(n_obs = n_obs, treatment_levels = c("a", "b"))
  predictions = setNames(lapply(seq_along(fixture$treatment_levels), function(j) {
    list(
      ml_g = matrix(fixture$conditional_means[, j], n_obs, n_rep),
      ml_m = matrix(fixture$propensity[, j], n_obs, n_rep)
    )
  }), fixture$treatment_levels)
  predictions$a$ml_g[, 2] = predictions$a$ml_g[, 2] + seq_len(n_obs) / 50
  predictions$b$ml_g[, 2] = predictions$b$ml_g[, 2] - seq_len(n_obs) / 70
  models = setNames(lapply(fixture$treatment_levels, function(level) {
    model = DoubleMLAPO$new(fixture$dml_data, "regr.rpart", "classif.rpart", level,
      n_folds = 2, n_rep = n_rep, trimming_threshold = trimming_threshold,
      draw_sample_splitting = FALSE)
    model$set_sample_splitting(rep(fixture$smpls, n_rep))
    model$fit(external_predictions = predictions[[level]])
    model
  }), fixture$treatment_levels)
  list(fixture = fixture, predictions = predictions, models = models)
}

test_that("group APO framework matches prevalence-scaled hand calculations", {
  setup = make_group_apo_models()
  groups = c(rep("first", 6), rep("second", 8))
  before = lapply(setup$models, function(model) {
    list(
      data = data.table::copy(model$data$data), all_coef = model$all_coef,
      psi_b = model$psi_b, diagnostics = model$propensity_diagnostics,
      smpls = model$smpls
    )
  })

  framework = make_apo_group_framework(setup$models, groups)
  expected_names = c(
    "GAPO[treatment=a;group=first]", "GAPO[treatment=b;group=first]",
    "GAPO[treatment=a;group=second]", "GAPO[treatment=b;group=second]"
  )
  expected_coef = matrix(0, 4, 2, dimnames = list(expected_names, NULL))
  expected_psi = array(0, c(14, 4, 2),
    dimnames = list(NULL, expected_names, NULL))
  for (r in 1:2) {
    column = 0L
    for (group in c("first", "second")) {
      in_group = groups == group
      q = sum(in_group) / length(groups)
      for (treatment in c("a", "b")) {
        column = column + 1L
        signal = setup$models[[treatment]]$psi_b[, r, 1]
        theta = mean(signal[in_group])
        expected_coef[column, r] = theta
        expected_psi[, column, r] = as.integer(in_group) / q * (signal - theta)
      }
    }
    covariance = crossprod(expected_psi[, , r]) / length(groups)^2
    expect_equal(unname(framework$all_cov[, , r]), unname(covariance))
    expect_equal(unname(framework$all_se[, r]), unname(sqrt(diag(covariance))))
    expect_equal(unname(framework$all_cov[1:2, 3:4, r]), matrix(0, 2, 2))
    expect_true(abs(framework$all_cov[1, 2, r]) > 0)
    expect_true(abs(framework$all_cov[3, 4, r]) > 0)
  }
  expect_s3_class(framework, "DoubleMLFramework")
  expect_identical(rownames(framework$all_coef), expected_names)
  expect_identical(dimnames(framework$psi)[[2]], expected_names)
  expect_equal(framework$all_coef, expected_coef)
  expect_equal(framework$psi, expected_psi)
  framework_state = framework$.__enclos_env__$private
  expect_identical(framework_state$smpls_, setup$models[[1]]$smpls)
  expect_true(framework_state$apply_cross_fitting_)

  after = lapply(setup$models, function(model) {
    list(
      data = model$data$data, all_coef = model$all_coef,
      psi_b = model$psi_b, diagnostics = model$propensity_diagnostics,
      smpls = model$smpls
    )
  })
  expect_identical(after, before)
})

test_that("group APO framework validates fit compatibility and inference support", {
  setup = make_group_apo_models()
  groups = c(rep("first", 6), rep("second", 8))
  expect_error(make_apo_group_framework(list(), groups), "nonempty|non-empty")
  expect_error(make_apo_group_framework(unname(setup$models), groups), "named")
  duplicated = setup$models
  names(duplicated) = c("a", "a")
  expect_error(make_apo_group_framework(duplicated, groups), "unique|duplicate")

  unfitted = DoubleMLAPO$new(setup$fixture$dml_data, "regr.rpart", "classif.rpart", "a",
    n_folds = 2, draw_sample_splitting = FALSE)
  unfitted$set_sample_splitting(setup$fixture$smpls)
  expect_error(make_apo_group_framework(list(a = unfitted), groups), "[Ff]it")

  dml1 = DoubleMLAPO$new(setup$fixture$dml_data, "regr.rpart", "classif.rpart", "a",
    n_folds = 2, dml_procedure = "dml1", draw_sample_splitting = FALSE)
  dml1$set_sample_splitting(setup$fixture$smpls)
  dml1$fit(external_predictions = list(
    ml_g = setup$predictions$a$ml_g[, 1, drop = FALSE],
    ml_m = setup$predictions$a$ml_m[, 1, drop = FALSE]))
  expect_error(make_apo_group_framework(list(a = dml1), groups), "DML2|dml2")

  nonfinite = setup$models
  nonfinite[[1]] = nonfinite[[1]]$clone(deep = TRUE)
  nonfinite[[1]]$.__enclos_env__$private$psi_b_[1, 1, 1] = Inf
  expect_error(make_apo_group_framework(nonfinite, groups), "finite.*treatment a.*repetition 1")

  expect_error(make_apo_group_framework(setup$models,
    c("singleton", rep("other", 13))), "singleton.*repetition 1|two.*repetition 1")
  expect_warning(make_apo_group_framework(setup$models,
    c(rep("rare", 5), rep("common", 9))), "rare.*repetition 1.*5|5.*rare")
  expect_warning(make_apo_group_framework(setup$models,
    rep(c("a-only", "b-only"), 7)), "[Gg]roup.*treatment|treatment.*[Gg]roup")
})

test_that("group APO framework reports group-specific trimming and zero variance", {
  trimmed = make_group_apo_models()
  trimmed$predictions$a$ml_m[1, ] = 0
  model = DoubleMLAPO$new(trimmed$fixture$dml_data, "regr.rpart", "classif.rpart", "a",
    n_folds = 2, n_rep = 2, trimming_threshold = .01,
    draw_sample_splitting = FALSE)
  model$set_sample_splitting(rep(trimmed$fixture$smpls, 2))
  suppressWarnings(model$fit(external_predictions = trimmed$predictions$a))
  trimmed$models$a = model
  expect_warning(make_apo_group_framework(trimmed$models,
    c(rep("small", 6), rep("large", 8))), "trimming.*treatment a.*group small")

  constant = make_apo_data(n_obs = 14L, treatment_levels = c("a", "b"))
  constant$df$y = 1
  constant$dml_data = DoubleMLData$new(constant$df,
    y_col = "y", d_cols = "d", x_cols = "x")
  model = DoubleMLAPO$new(constant$dml_data, "regr.rpart", "classif.rpart", "a",
    n_folds = 2, draw_sample_splitting = FALSE)
  model$set_sample_splitting(constant$smpls)
  model$fit(external_predictions = list(
    ml_g = matrix(1, 14, 1), ml_m = matrix(.5, 14, 1)))
  expect_error(make_apo_group_framework(list(a = model),
    c(rep("first", 6), rep("second", 8))), "positive.*standard error|zero.*standard error")
})
# nolint end: object_usage_linter.
