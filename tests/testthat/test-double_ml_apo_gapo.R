# Package internals are loaded by the testthat harness.
# nolint start: object_usage_linter.
make_gapo_fixture = function(n_rep = 2L, dml_procedure = "dml2",
                              apply_cross_fitting = TRUE) {
  fixture = make_apo_data(n_obs = 14L, treatment_levels = c("a", "b"))
  model = DoubleMLAPO$new(
    fixture$dml_data, "regr.rpart", "classif.rpart", "a",
    n_folds = 2, n_rep = n_rep, dml_procedure = dml_procedure,
    draw_sample_splitting = FALSE,
    apply_cross_fitting = apply_cross_fitting
  )
  if (apply_cross_fitting) {
    model$set_sample_splitting(rep(fixture$smpls, n_rep))
  } else {
    model$set_sample_splitting(list(list(
      train_ids = list(1:7), test_ids = list(8:14)
    )))
  }
  predictions = list(
    ml_g = matrix(fixture$conditional_means[, "a"], 14L, n_rep),
    ml_m = matrix(fixture$propensity[, "a"], 14L, n_rep),
    ml_g_complement = matrix(fixture$conditional_means[, "b"], 14L, n_rep)
  )
  if (n_rep > 1L) {
    predictions$ml_g[, 2L] = predictions$ml_g[, 2L] + seq_len(14L) / 50
  }
  list(fixture = fixture, model = model, predictions = predictions)
}

test_that("gapo returns exact group APO estimates and reusable inference", {
  setup = make_gapo_fixture()
  groups = c(rep("first", 6L), rep("second", 8L))
  expect_error(setup$model$gapo(groups), "[Ff]it")
  setup$model$fit(
    store_predictions = TRUE, store_models = TRUE,
    external_predictions = setup$predictions
  )
  before = list(
    data = data.table::copy(setup$model$data$data),
    coef = setup$model$coef, all_coef = setup$model$all_coef,
    psi_b = setup$model$psi_b, predictions = setup$model$predictions,
    models = setup$model$models,
    diagnostics = setup$model$propensity_diagnostics,
    smpls = setup$model$smpls
  )

  result = setup$model$gapo(groups)
  expected_names = c(
    "GAPO[treatment=a;group=first]",
    "GAPO[treatment=a;group=second]"
  )
  expected_coef = matrix(0, 2L, 2L, dimnames = list(expected_names, NULL))
  expected_psi = array(0, c(14L, 2L, 2L),
    dimnames = list(NULL, expected_names, NULL))
  for (i_rep in 1:2) {
    for (i_group in 1:2) {
      in_group = groups == c("first", "second")[i_group]
      prevalence = mean(in_group)
      signal = setup$model$psi_b[, i_rep, 1L]
      expected_coef[i_group, i_rep] = mean(signal[in_group])
      expected_psi[, i_group, i_rep] = as.integer(in_group) / prevalence *
        (signal - expected_coef[i_group, i_rep])
    }
  }
  expected_cov = array(0, c(2L, 2L, 2L))
  for (i_rep in 1:2) {
    expected_cov[, , i_rep] = crossprod(expected_psi[, , i_rep]) / 14^2
  }

  expect_s3_class(result, "DoubleMLFramework")
  expect_identical(rownames(result$all_coef), expected_names)
  expect_equal(result$all_coef, expected_coef)
  expect_equal(result$psi, expected_psi)
  expect_equal(unname(result$all_cov), expected_cov)
  expect_equal(unname(result$all_se),
    apply(expected_cov, 3L, function(value) sqrt(diag(value))))
  expect_output(result$summary(), "Std. Error")
  expect_equal(dim(result$confint()), c(2L, 2L))
  set.seed(91)
  expect_identical(result$bootstrap(n_rep_boot = 20L), result)
  expect_equal(dim(result$confint(joint = TRUE)), c(2L, 2L))
  expect_equal(length(result$p_adjust(return_matrix = FALSE)), 2L)

  reason = paste(
    "Sensitivity analysis for GAPO/GATE is not implemented; conditional group",
    "prevalence requires a separate sensitivity influence derivation."
  )
  group_inference = list(
    coef = result$coef, all_coef = result$all_coef, se = result$se,
    all_se = result$all_se, psi = result$psi, all_cov = result$all_cov
  )
  expect_error(result$sensitivity_analysis(), paste0("^", reason, "$"))
  expect_identical(list(
    coef = result$coef, all_coef = result$all_coef, se = result$se,
    all_se = result$all_se, psi = result$psi, all_cov = result$all_cov
  ), group_inference)

  after = list(
    data = setup$model$data$data,
    coef = setup$model$coef, all_coef = setup$model$all_coef,
    psi_b = setup$model$psi_b, predictions = setup$model$predictions,
    models = setup$model$models,
    diagnostics = setup$model$propensity_diagnostics,
    smpls = setup$model$smpls
  )
  expect_identical(after, before)
})

test_that("gapo preserves categorical group order and validates its support", {
  setup = make_gapo_fixture()
  setup$model$fit(external_predictions = setup$predictions)
  groups = factor(c(rep("first", 6L), rep("second", 8L)),
    levels = c("second", "first", "unused"))
  result = setup$model$gapo(groups)
  expect_identical(rownames(result$all_coef), c(
    "GAPO[treatment=a;group=second]",
    "GAPO[treatment=a;group=first]"
  ))
  expect_error(setup$model$gapo(c("single", rep("other", 13L))),
    "singleton|two.*repetition 1")
  expect_error(setup$model$gapo(c(rep("first", 6L), NA, rep("second", 7L))),
    "NA")

  nonfinite = setup$model$clone(deep = TRUE)
  nonfinite$.__enclos_env__$private$psi_b_[1L, 1L, 1L] = Inf
  expect_error(nonfinite$gapo(rep(c("first", "second"), 7L)),
    "finite.*treatment a.*repetition 1")

  constant = make_apo_data(n_obs = 14L, treatment_levels = c("a", "b"))
  constant$df$y = 1
  constant$dml_data = DoubleMLData$new(constant$df,
    y_col = "y", d_cols = "d", x_cols = "x")
  zero = DoubleMLAPO$new(constant$dml_data, "regr.rpart", "classif.rpart", "a",
    n_folds = 2, draw_sample_splitting = FALSE)
  zero$set_sample_splitting(constant$smpls)
  zero$fit(external_predictions = list(
    ml_g = matrix(1, 14L, 1L), ml_m = matrix(.5, 14L, 1L)
  ))
  expect_error(zero$gapo(rep(c("first", "second"), 7L)),
    "positive.*standard error")
})

test_that("gapo rejects unsupported DML1 and no-cross-fitting states", {
  dml1 = make_gapo_fixture(n_rep = 1L, dml_procedure = "dml1")
  dml1$model$fit(external_predictions = dml1$predictions)
  expect_error(dml1$model$gapo(rep(c("first", "second"), 7L)), "DML2|dml2")

  holdout = make_gapo_fixture(
    n_rep = 1L, dml_procedure = "dml1", apply_cross_fitting = FALSE
  )
  holdout$model$fit(external_predictions = holdout$predictions)
  expect_error(holdout$model$gapo(rep(c("first", "second"), 7L)),
    "cross-fitted|DML2|dml2")
})
# nolint end: object_usage_linter.
