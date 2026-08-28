# Load required packages for competing risks survival analysis
library(SuperLearner)
library(survSuperLearner)
library(fdrtool)
library(doParallel)

test_that("DoubleMLRMTLJ with n_folds=2 works", {
  skip_if_not_installed("SuperLearner")
  skip_if_not_installed("survSuperLearner")
  skip_if_not_installed("MASS")
  skip_if_not_installed("simsurv")
  skip_if_not_installed("fdrtool")

  cl = makeCluster(2)
  registerDoParallel(cl)

  # Use make_competing_data() from dataset_censored.R
  set.seed(123)
  obj_dml_data = make_competing_data(
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
    dml_rmtlj <- DoubleMLRMTLJ$new(
      obj_dml_data,
      ml_g_surv = ml_g_surv,
      ml_g_surv_j = ml_g_surv, # Cause-specific survival
      ml_g_surv_jbar = ml_g_surv, # Complementary cause survival
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
  expect_s3_class(dml_rmtlj, "DoubleMLRMTLJ")
  expect_s3_class(dml_rmtlj, "DoubleMLCompeting")
  expect_s3_class(dml_rmtlj, "DoubleML")

  # Test tau parameter access (original values)
  expect_equal(dml_rmtlj$tau, 4)
  expect_equal(dml_rmtlj$admin_cens, 10)
  expect_equal(dml_rmtlj$n_folds, 2)

  # Test fitting
  expect_error(
    dml_rmtlj$fit(),
    NA # No error expected
  )

  # Test that we get REAL RMTLJ estimates (not placeholders)
  expect_true(is.numeric(dml_rmtlj$coef))
  expect_true(is.numeric(dml_rmtlj$se))
  expect_true(length(dml_rmtlj$coef) == 1)
  expect_true(length(dml_rmtlj$se) == 1)

  # CRITICAL: Verify these are NOT placeholder values
  expect_false(dml_rmtlj$coef == 0.1) # Not a placeholder
  expect_false(dml_rmtlj$se == 0.05) # Not a placeholder

  # Test that RMTLJ treatment effect is reasonable
  # RMTLJ difference should be bounded by [-tau, tau] = [-4, 4] for tau=4
  expect_true(dml_rmtlj$coef >= -4 && dml_rmtlj$coef <= 4)
  expect_true(dml_rmtlj$se > 0) # Standard error should be positive

  # Test statistical properties of ATE point estimates and standard errors
  # Confidence interval construction
  ci_lower = dml_rmtlj$coef - 1.96 * dml_rmtlj$se
  ci_upper = dml_rmtlj$coef + 1.96 * dml_rmtlj$se
  expect_true(ci_lower < ci_upper)
  expect_true(is.finite(ci_lower) && is.finite(ci_upper))

  # T-statistic should be finite and reasonable
  t_stat = dml_rmtlj$coef / dml_rmtlj$se
  expect_true(is.finite(t_stat))
  expect_false(is.na(t_stat))

  # Test access to detailed RMTLJ estimates
  expect_true(is.list(dml_rmtlj$rmtlj_estimates))
  expect_true("tau" %in% names(dml_rmtlj$rmtlj_estimates))
  expect_true("estimate.diff" %in% names(dml_rmtlj$rmtlj_estimates))
  expect_equal(dml_rmtlj$rmtlj_estimates$tau, 4)

  # Test get_inference_curve returns a data frame with correct structure
  curve = dml_rmtlj$get_inference_curve(npath = 100)
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

  # Test all 4 separable effects functions
  dml_rmtlj$get_separable_direct_astar1()
  sep_direct_a1 = dml_rmtlj$sep_direct_astar1_estimates
  dml_rmtlj$get_separable_indirect_astar1()
  sep_indirect_a1 = dml_rmtlj$sep_indirect_astar1_estimates
  dml_rmtlj$get_separable_direct_astar0()
  sep_direct_a0 = dml_rmtlj$sep_direct_astar0_estimates
  dml_rmtlj$get_separable_indirect_astar0()
  sep_indirect_a0 = dml_rmtlj$sep_indirect_astar0_estimates

  # Verify structure for separable direct A*=1
  expect_true(is.list(sep_direct_a1))
  expect_true("tau" %in% names(sep_direct_a1))
  expect_true("estimate.diff" %in% names(sep_direct_a1))
  expect_true("se.diff" %in% names(sep_direct_a1))
  expect_equal(sep_direct_a1$tau, 4)
  expect_true(is.numeric(sep_direct_a1$estimate.diff))
  expect_true(is.finite(sep_direct_a1$estimate.diff))
  expect_true(sep_direct_a1$se.diff > 0)

  # Verify separable indirect A*=1
  expect_true(is.list(sep_indirect_a1))
  expect_equal(sep_indirect_a1$tau, 4)
  expect_true(is.numeric(sep_indirect_a1$estimate.diff))
  expect_true(is.finite(sep_indirect_a1$estimate.diff))
  expect_true(sep_indirect_a1$se.diff > 0)

  # Verify separable direct A*=0
  expect_true(is.list(sep_direct_a0))
  expect_equal(sep_direct_a0$tau, 4)
  expect_true(is.numeric(sep_direct_a0$estimate.diff))
  expect_true(is.finite(sep_direct_a0$estimate.diff))
  expect_true(sep_direct_a0$se.diff > 0)

  # Verify separable indirect A*=0
  expect_true(is.list(sep_indirect_a0))
  expect_equal(sep_indirect_a0$tau, 4)
  expect_true(is.numeric(sep_indirect_a0$estimate.diff))
  expect_true(is.finite(sep_indirect_a0$estimate.diff))
  expect_true(sep_indirect_a0$se.diff > 0)

  # Verify diff = a1 - a0 for direct A*=1
  expect_equal(sep_direct_a1$estimate.diff,
    sep_direct_a1$estimate.a1 - sep_direct_a1$estimate.a0,
    tolerance = 1e-6)

  # Test inference curves with CBs for each separable effect
  curve_d1 = dml_rmtlj$get_inference_curve_sep_direct_astar1(npath = 100)
  expect_true(is.data.frame(curve_d1))
  expect_true("estimate.diff.lower.cb" %in% names(curve_d1))
  expect_true("estimate.diff.upper.cb" %in% names(curve_d1))
  expect_true(nrow(curve_d1) > 0)
  expect_true(all(is.finite(curve_d1$estimate.diff)))
  expect_true(all(curve_d1$estimate.diff.lower.cb <= curve_d1$estimate.diff.lower.ci))
  expect_true(all(curve_d1$estimate.diff.upper.cb >= curve_d1$estimate.diff.upper.ci))

  curve_i1 = dml_rmtlj$get_inference_curve_sep_indirect_astar1(npath = 100)
  expect_true(is.data.frame(curve_i1))
  expect_true("estimate.diff.lower.cb" %in% names(curve_i1))
  expect_true("estimate.diff.upper.cb" %in% names(curve_i1))
  expect_true(nrow(curve_i1) > 0)
  expect_true(all(is.finite(curve_i1$estimate.diff)))
  expect_true(all(curve_i1$estimate.diff.lower.cb <= curve_i1$estimate.diff.lower.ci))
  expect_true(all(curve_i1$estimate.diff.upper.cb >= curve_i1$estimate.diff.upper.ci))

  curve_d0 = dml_rmtlj$get_inference_curve_sep_direct_astar0(npath = 100)
  expect_true(is.data.frame(curve_d0))
  expect_true("estimate.diff.lower.cb" %in% names(curve_d0))
  expect_true("estimate.diff.upper.cb" %in% names(curve_d0))
  expect_true(nrow(curve_d0) > 0)
  expect_true(all(is.finite(curve_d0$estimate.diff)))
  expect_true(all(curve_d0$estimate.diff.lower.cb <= curve_d0$estimate.diff.lower.ci))
  expect_true(all(curve_d0$estimate.diff.upper.cb >= curve_d0$estimate.diff.upper.ci))

  curve_i0 = dml_rmtlj$get_inference_curve_sep_indirect_astar0(npath = 100)
  expect_true(is.data.frame(curve_i0))
  expect_true("estimate.diff.lower.cb" %in% names(curve_i0))
  expect_true("estimate.diff.upper.cb" %in% names(curve_i0))
  expect_true(nrow(curve_i0) > 0)
  expect_true(all(is.finite(curve_i0$estimate.diff)))
  expect_true(all(curve_i0$estimate.diff.lower.cb <= curve_i0$estimate.diff.lower.ci))
  expect_true(all(curve_i0$estimate.diff.upper.cb >= curve_i0$estimate.diff.upper.ci))
  stopCluster(cl)
})

test_that("DoubleMLRMTLJ with multiple tau values works", {
  skip_if_not_installed("SuperLearner")
  skip_if_not_installed("survSuperLearner")
  skip_if_not_installed("MASS")
  skip_if_not_installed("simsurv")
  skip_if_not_installed("fdrtool")

  cl = makeCluster(2)
  registerDoParallel(cl)

  # Use make_competing_data() from dataset_censored.R
  set.seed(456)
  obj_dml_data = make_competing_data(
    n_obs = 5000,
    admin_cens = 10,
    time_grid = 0.01,
    return_type = "DoubleMLData"
  )

  ml_g_surv = c("survSL.km")
  ml_g_cens = c("survSL.km")
  ml_m = c("SL.glm")

  dml_rmtlj_multi = DoubleMLRMTLJ$new(
    obj_dml_data,
    ml_g_surv = ml_g_surv,
    ml_g_surv_j = ml_g_surv, # Cause-specific survival
    ml_g_surv_jbar = ml_g_surv, # Complementary cause survival
    ml_g_cens = ml_g_cens,
    ml_m = ml_m,
    admin_cens = 10, # Original admin.cens = 10
    time_col = "time",
    event_col = "event",
    tau = c(1, 2, 3, 4), # Original taus <- c(1,2,3,4)
    n_folds = 5
  )

  # Test multiple tau access
  expect_equal(dml_rmtlj_multi$tau, c(1, 2, 3, 4))

  # Fit model
  dml_rmtlj_multi$fit()

  # For multiple tau, base class coef/se only return first tau
  # (base class architecture doesn't support multiple treatments per model)
  expect_true(length(dml_rmtlj_multi$coef) == 1)
  expect_true(length(dml_rmtlj_multi$se) == 1)
  expect_true(is.numeric(dml_rmtlj_multi$coef))
  expect_true(is.numeric(dml_rmtlj_multi$se))

  # Test that we get estimates for all tau values via rmtlj_estimates
  rmtlj_est = dml_rmtlj_multi$rmtlj_estimates
  expect_true(length(rmtlj_est$tau) == 4)
  expect_equal(rmtlj_est$tau, c(1, 2, 3, 4))

  # Verify all tau have estimates
  expect_true(length(rmtlj_est$estimate.diff) == 4)
  expect_true(length(rmtlj_est$se.diff) == 4)
  expect_true(all(is.numeric(rmtlj_est$estimate.diff)))
  expect_true(all(is.numeric(rmtlj_est$se.diff)))

  # Test that all estimates are within reasonable bounds
  for (i in 1:4) {
    tau_i = rmtlj_est$tau[i]
    ate_i = rmtlj_est$estimate.diff[i]
    se_i = rmtlj_est$se.diff[i]

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

  # Test all 4 separable effects functions with multiple tau
  dml_rmtlj_multi$get_separable_direct_astar1()
  sep_direct_a1 = dml_rmtlj_multi$sep_direct_astar1_estimates
  dml_rmtlj_multi$get_separable_indirect_astar1()
  sep_indirect_a1 = dml_rmtlj_multi$sep_indirect_astar1_estimates
  dml_rmtlj_multi$get_separable_direct_astar0()
  sep_direct_a0 = dml_rmtlj_multi$sep_direct_astar0_estimates
  dml_rmtlj_multi$get_separable_indirect_astar0()
  sep_indirect_a0 = dml_rmtlj_multi$sep_indirect_astar0_estimates

  # Verify all have length 4 for estimates (matching tau values)
  expect_equal(length(sep_direct_a1$estimate.diff), 4)
  expect_equal(length(sep_indirect_a1$estimate.diff), 4)
  expect_equal(length(sep_direct_a0$estimate.diff), 4)
  expect_equal(length(sep_indirect_a0$estimate.diff), 4)

  # Verify tau values match
  expect_equal(sep_direct_a1$tau, c(1, 2, 3, 4))
  expect_equal(sep_indirect_a1$tau, c(1, 2, 3, 4))
  expect_equal(sep_direct_a0$tau, c(1, 2, 3, 4))
  expect_equal(sep_indirect_a0$tau, c(1, 2, 3, 4))

  # All estimates should be numeric and finite
  expect_true(all(is.finite(sep_direct_a1$estimate.diff)))
  expect_true(all(is.finite(sep_indirect_a1$estimate.diff)))
  expect_true(all(is.finite(sep_direct_a0$estimate.diff)))
  expect_true(all(is.finite(sep_indirect_a0$estimate.diff)))

  # All SEs should be positive
  expect_true(all(sep_direct_a1$se.diff > 0))
  expect_true(all(sep_indirect_a1$se.diff > 0))
  expect_true(all(sep_direct_a0$se.diff > 0))
  expect_true(all(sep_indirect_a0$se.diff > 0))

  # Verify structure fields exist for first separable effect
  expect_true("estimate.a0" %in% names(sep_direct_a1))
  expect_true("estimate.a1" %in% names(sep_direct_a1))
  expect_true("se.a0" %in% names(sep_direct_a1))
  expect_true("se.a1" %in% names(sep_direct_a1))

  # Verify diff = a1 - a0 for first tau in direct A*=1
  expect_equal(sep_direct_a1$estimate.diff[1],
    sep_direct_a1$estimate.a1[1] - sep_direct_a1$estimate.a0[1],
    tolerance = 1e-6)

  # Test get_inference_curve returns a data frame with correct structure
  curve = dml_rmtlj_multi$get_inference_curve(npath = 100)
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

  # Test inference curves with CBs for each separable effect
  curve_d1 = dml_rmtlj_multi$get_inference_curve_sep_direct_astar1(npath = 100)
  expect_true(is.data.frame(curve_d1))
  expect_true("estimate.diff.lower.cb" %in% names(curve_d1))
  expect_true("estimate.diff.upper.cb" %in% names(curve_d1))
  expect_true(nrow(curve_d1) > 0)
  expect_true(all(is.finite(curve_d1$estimate.diff)))
  expect_true(all(curve_d1$estimate.diff.lower.cb <= curve_d1$estimate.diff.lower.ci))
  expect_true(all(curve_d1$estimate.diff.upper.cb >= curve_d1$estimate.diff.upper.ci))

  curve_i1 = dml_rmtlj_multi$get_inference_curve_sep_indirect_astar1(npath = 100)
  expect_true(is.data.frame(curve_i1))
  expect_true("estimate.diff.lower.cb" %in% names(curve_i1))
  expect_true("estimate.diff.upper.cb" %in% names(curve_i1))
  expect_true(nrow(curve_i1) > 0)
  expect_true(all(is.finite(curve_i1$estimate.diff)))
  expect_true(all(curve_i1$estimate.diff.lower.cb <= curve_i1$estimate.diff.lower.ci))
  expect_true(all(curve_i1$estimate.diff.upper.cb >= curve_i1$estimate.diff.upper.ci))

  curve_d0 = dml_rmtlj_multi$get_inference_curve_sep_direct_astar0(npath = 100)
  expect_true(is.data.frame(curve_d0))
  expect_true("estimate.diff.lower.cb" %in% names(curve_d0))
  expect_true("estimate.diff.upper.cb" %in% names(curve_d0))
  expect_true(nrow(curve_d0) > 0)
  expect_true(all(is.finite(curve_d0$estimate.diff)))
  expect_true(all(curve_d0$estimate.diff.lower.cb <= curve_d0$estimate.diff.lower.ci))
  expect_true(all(curve_d0$estimate.diff.upper.cb >= curve_d0$estimate.diff.upper.ci))

  curve_i0 = dml_rmtlj_multi$get_inference_curve_sep_indirect_astar0(npath = 100)
  expect_true(is.data.frame(curve_i0))
  expect_true("estimate.diff.lower.cb" %in% names(curve_i0))
  expect_true("estimate.diff.upper.cb" %in% names(curve_i0))
  expect_true(nrow(curve_i0) > 0)
  expect_true(all(is.finite(curve_i0$estimate.diff)))
  expect_true(all(curve_i0$estimate.diff.lower.cb <= curve_i0$estimate.diff.lower.ci))
  expect_true(all(curve_i0$estimate.diff.upper.cb >= curve_i0$estimate.diff.upper.ci))
  stopCluster(cl)
})
