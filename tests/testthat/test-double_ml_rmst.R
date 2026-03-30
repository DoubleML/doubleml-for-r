# Load required packages for survival analysis
library(SuperLearner)
library(survSuperLearner)
library(fdrtool)
library(doParallel)

test_that("DoubleMLRMST with n_folds=2 works", {
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
    dml_rmst <- DoubleMLRMST$new(
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
  expect_s3_class(dml_rmst, "DoubleMLRMST")
  expect_s3_class(dml_rmst, "DoubleMLSurvival")
  expect_s3_class(dml_rmst, "DoubleML")

  # Test tau parameter access (original values)
  expect_equal(dml_rmst$tau, 4)
  expect_equal(dml_rmst$admin_cens, 10)
  expect_equal(dml_rmst$n_folds, 2)

  # Test fitting
  expect_error(
    dml_rmst$fit(),
    NA # No error expected
  )

  # Test that we get REAL RMST estimates (not placeholders)
  expect_true(is.numeric(dml_rmst$coef))
  expect_true(is.numeric(dml_rmst$se))
  expect_true(length(dml_rmst$coef) == 1)
  expect_true(length(dml_rmst$se) == 1)

  # CRITICAL: Verify these are NOT placeholder values
  expect_false(dml_rmst$coef == 0.1) # Not a placeholder
  expect_false(dml_rmst$se == 0.05) # Not a placeholder

  # Test that RMST treatment effect is reasonable
  # RMST difference should be bounded by [-tau, tau] = [-4, 4] for tau=4
  expect_true(dml_rmst$coef >= -4 && dml_rmst$coef <= 4)
  expect_true(dml_rmst$se > 0) # Standard error should be positive

  # Test statistical properties of ATE point estimates and standard errors
  # Confidence interval construction
  ci_lower = dml_rmst$coef - 1.96 * dml_rmst$se
  ci_upper = dml_rmst$coef + 1.96 * dml_rmst$se
  expect_true(ci_lower < ci_upper)
  expect_true(is.finite(ci_lower) && is.finite(ci_upper))

  # T-statistic should be finite and reasonable
  t_stat = dml_rmst$coef / dml_rmst$se
  expect_true(is.finite(t_stat))
  expect_false(is.na(t_stat))

  # Test access to detailed RMST estimates
  expect_true(is.list(dml_rmst$rmst_estimates))
  expect_true("tau" %in% names(dml_rmst$rmst_estimates))
  expect_true("estimate.diff" %in% names(dml_rmst$rmst_estimates))
  expect_equal(dml_rmst$rmst_estimates$tau, 4)

  # Test get_inference_curve returns a data frame with correct structure
  curve = dml_rmst$get_inference_curve(npath = 100)
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

test_that("DoubleMLRMST with multiple tau values works", {
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

  dml_rmst_multi = DoubleMLRMST$new(
    obj_dml_data,
    ml_g_surv = ml_g_surv,
    ml_g_cens = ml_g_cens,
    ml_m = ml_m,
    admin_cens = 10, # Original admin.cens = 10
    time_col = "time",
    event_col = "event",
    tau = c(1, 2, 3, 4), # Original taus <- c(1,2,3,4)
    n_folds = 5
  )

  # Test multiple tau access
  expect_equal(dml_rmst_multi$tau, c(1, 2, 3, 4))

  # Fit model
  dml_rmst_multi$fit()

  # For multiple tau, base class coef/se only return first tau
  # (base class architecture doesn't support multiple treatments per model)
  expect_true(length(dml_rmst_multi$coef) == 1)
  expect_true(length(dml_rmst_multi$se) == 1)
  expect_true(is.numeric(dml_rmst_multi$coef))
  expect_true(is.numeric(dml_rmst_multi$se))

  # Test that we get estimates for all tau values via rmst_estimates
  rmst_est = dml_rmst_multi$rmst_estimates
  expect_true(length(rmst_est$tau) == 4)
  expect_equal(rmst_est$tau, c(1, 2, 3, 4))

  # Verify all tau have estimates
  expect_true(length(rmst_est$estimate.diff) == 4)
  expect_true(length(rmst_est$se.diff) == 4)
  expect_true(all(is.numeric(rmst_est$estimate.diff)))
  expect_true(all(is.numeric(rmst_est$se.diff)))

  # Test that all estimates are within reasonable bounds
  for (i in 1:4) {
    tau_i = rmst_est$tau[i]
    ate_i = rmst_est$estimate.diff[i]
    se_i = rmst_est$se.diff[i]

    expect_true(ate_i >= -tau_i && ate_i <= tau_i)
    expect_true(se_i > 0)

    # Test ATE statistical properties for each tau
    ci_lower_i = ate_i - 1.96 * se_i
    ci_upper_i = ate_i + 1.96 * se_i
    expect_true(ci_lower_i < ci_upper_i)
    expect_true(is.finite(ci_lower_i) && is.finite(ci_upper_i))

    # T-statistic for each tau
    t_stat_i = ate_i / se_i
    expect_true(is.finite(t_stat_i))
    expect_false(is.na(t_stat_i))
  }

  # Test get_inference_curve returns a data frame with correct structure
  curve = dml_rmst_multi$get_inference_curve(npath = 100)
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
