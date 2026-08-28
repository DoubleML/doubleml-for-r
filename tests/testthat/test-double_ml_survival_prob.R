# Load required packages for survival analysis
library(SuperLearner)
library(survSuperLearner)
library(fdrtool)
library(doParallel)

test_that("DoubleMLSurvivalProb with n_folds=2 works", {
  skip_if_not_installed("SuperLearner")
  skip_if_not_installed("survSuperLearner")
  skip_if_not_installed("MASS")
  skip_if_not_installed("simsurv")
  skip_if_not_installed("fdrtool")

  cl = makeCluster(2)
  registerDoParallel(cl)

  # Use make_survival_data() from dataset_censored.R
  set.seed(123)
  obj_dml_data = make_survival_data(
    n_obs = 2000,
    admin_cens = 10,
    time_grid = 0.01,
    return_type = "DoubleMLData"
  )

  ml_g_surv = c("survSL.km")
  ml_g_cens = c("survSL.km")
  ml_m = c("SL.glm")

  # Test initialization with n_folds=2
  expect_error(
    dml_surv_prob <- DoubleMLSurvivalProb$new(
      obj_dml_data,
      ml_g_surv = ml_g_surv,
      ml_g_cens = ml_g_cens,
      ml_m = ml_m,
      admin_cens = 10,
      time_col = "time",
      event_col = "event",
      tau = 4, # Original tau from taus <- c(1,2,3,4)
      n_folds = 2,
      n_rep = 1
    ),
    NA # No error expected
  )

  # Test that object has correct class
  expect_s3_class(dml_surv_prob, "DoubleMLSurvivalProb")
  expect_s3_class(dml_surv_prob, "DoubleMLSurvival")
  expect_s3_class(dml_surv_prob, "DoubleML")

  # Test tau parameter access (original values)
  expect_equal(dml_surv_prob$tau, 4)
  expect_equal(dml_surv_prob$admin_cens, 10)
  expect_equal(dml_surv_prob$n_folds, 2)

  # Test fitting
  expect_error(
    dml_surv_prob$fit(),
    NA # No error expected
  )

  # Test that we get REAL survival probability estimates (not placeholders)
  expect_true(is.numeric(dml_surv_prob$coef))
  expect_true(is.numeric(dml_surv_prob$se))
  expect_true(length(dml_surv_prob$coef) == 1)
  expect_true(length(dml_surv_prob$se) == 1)

  # CRITICAL: Verify these are NOT placeholder values
  expect_false(dml_surv_prob$coef == 0.1) # Not a placeholder
  expect_false(dml_surv_prob$se == 0.05) # Not a placeholder

  # Test that survival probability treatment effect is reasonable
  # Survival probability difference should be bounded by [-1, 1]
  expect_true(dml_surv_prob$coef >= -1 && dml_surv_prob$coef <= 1)
  expect_true(dml_surv_prob$se > 0) # Standard error should be positive

  # Test statistical properties of ATE point estimates and standard errors
  # Confidence interval construction
  ci_lower = dml_surv_prob$coef - 1.96 * dml_surv_prob$se
  ci_upper = dml_surv_prob$coef + 1.96 * dml_surv_prob$se
  expect_true(ci_lower < ci_upper)
  expect_true(is.finite(ci_lower) && is.finite(ci_upper))

  # T-statistic should be finite and reasonable
  t_stat = dml_surv_prob$coef / dml_surv_prob$se
  expect_true(is.finite(t_stat))
  expect_false(is.na(t_stat))

  # Test access to detailed survival probability estimates
  expect_true(is.list(dml_surv_prob$survival_prob_estimates))
  expect_true("tau" %in% names(dml_surv_prob$survival_prob_estimates))
  expect_true("estimate.diff" %in% names(dml_surv_prob$survival_prob_estimates))
  expect_equal(dml_surv_prob$survival_prob_estimates$tau, 4)

  # Test get_inference_curve returns a data frame with correct structure
  curve = dml_surv_prob$get_inference_curve(npath = 100)
  expect_true(is.data.frame(curve))
  expect_true("t" %in% names(curve))
  expect_true("estimate.diff" %in% names(curve))
  expect_true("estimate.diff.se" %in% names(curve))
  expect_true("estimate.diff.lower.ci" %in% names(curve))
  expect_true("estimate.diff.upper.ci" %in% names(curve))
  expect_true("estimate.diff.lower.cb" %in% names(curve))
  expect_true("estimate.diff.upper.cb" %in% names(curve))
  expect_true(nrow(curve) > 0)
  expect_true(all(is.finite(curve$estimate.diff)))
  stopCluster(cl)
})

test_that("DoubleMLSurvivalProb with multiple tau values works", {
  skip_if_not_installed("SuperLearner")
  skip_if_not_installed("survSuperLearner")
  skip_if_not_installed("MASS")
  skip_if_not_installed("simsurv")
  skip_if_not_installed("fdrtool")

  cl = makeCluster(2)
  registerDoParallel(cl)

  # Use make_survival_data() from dataset_censored.R
  set.seed(456)
  obj_dml_data = make_survival_data(
    n_obs = 5000,
    admin_cens = 10,
    time_grid = 0.01,
    return_type = "DoubleMLData"
  )

  ml_g_surv = c("survSL.km")
  ml_g_cens = c("survSL.km")
  ml_m = c("SL.glm")

  dml_surv_prob = DoubleMLSurvivalProb$new(
    obj_dml_data,
    ml_g_surv = ml_g_surv,
    ml_g_cens = ml_g_cens,
    ml_m = ml_m,
    admin_cens = 10, # Original admin.cens = 10
    time_col = "time",
    event_col = "event",
    tau = c(1, 2, 3, 4), # Multiple tau values from original simulation
    n_folds = 5
  )

  # Test tau parameter access
  expect_equal(dml_surv_prob$tau, c(1, 2, 3, 4))

  # Fit model
  dml_surv_prob$fit()

  # Test that we get valid estimates (for first tau)
  expect_true(is.numeric(dml_surv_prob$coef))
  expect_true(is.numeric(dml_surv_prob$se))
  expect_true(length(dml_surv_prob$coef) == 1)
  expect_true(length(dml_surv_prob$se) == 1)

  # Test that survival probability ATE is within bounds
  expect_true(dml_surv_prob$coef >= -1 && dml_surv_prob$coef <= 1)
  expect_true(dml_surv_prob$se > 0)

  # Test ATE statistical properties
  ci_lower = dml_surv_prob$coef - 1.96 * dml_surv_prob$se
  ci_upper = dml_surv_prob$coef + 1.96 * dml_surv_prob$se
  expect_true(ci_lower < ci_upper)
  expect_true(is.finite(ci_lower) && is.finite(ci_upper))

  # T-statistic validation
  t_stat = dml_surv_prob$coef / dml_surv_prob$se
  expect_true(is.finite(t_stat))
  expect_false(is.na(t_stat))

  # Test access to detailed estimates for all tau values
  expect_true(is.list(dml_surv_prob$survival_prob_estimates))
  expect_equal(length(dml_surv_prob$survival_prob_estimates$tau), 4)
  expect_equal(dml_surv_prob$survival_prob_estimates$tau, c(1, 2, 3, 4))
  expect_true(length(dml_surv_prob$survival_prob_estimates$estimate.diff) == 4)
  expect_true(length(dml_surv_prob$survival_prob_estimates$estimate.a0) == 4)
  expect_true(length(dml_surv_prob$survival_prob_estimates$estimate.a1) == 4)

  # Test get_inference_curve returns a data frame with correct structure
  curve = dml_surv_prob$get_inference_curve(npath = 100)
  expect_true(is.data.frame(curve))
  expect_true("t" %in% names(curve))
  expect_true("estimate.diff" %in% names(curve))
  expect_true("estimate.diff.se" %in% names(curve))
  expect_true("estimate.diff.lower.ci" %in% names(curve))
  expect_true("estimate.diff.upper.ci" %in% names(curve))
  expect_true("estimate.diff.lower.cb" %in% names(curve))
  expect_true("estimate.diff.upper.cb" %in% names(curve))
  expect_true(nrow(curve) > 0)
  expect_true(all(is.finite(curve$estimate.diff)))

  # Confidence bands should be wider than confidence intervals
  expect_true(all(curve$estimate.diff.lower.cb <= curve$estimate.diff.lower.ci))
  expect_true(all(curve$estimate.diff.upper.cb >= curve$estimate.diff.upper.ci))
  stopCluster(cl)
})
