context("Unit tests for PLR, bootstrap without cross-fitting")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

test_cases = data.frame(
  apply_cross_fitting = c(TRUE, FALSE, FALSE),
  n_folds = c(2, 2, 1)
)
test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Bootstrap for PLR:",
  .cases = test_cases,
  {
    set.seed(3141)
    double_mlplr_obj = DoubleMLPLR$new(
      data = data_plr$dml_data,
      ml_l = lrn("regr.lm"),
      ml_m = lrn("regr.lm"),
      n_folds = n_folds,
      apply_cross_fitting = apply_cross_fitting
    )
    double_mlplr_obj$fit()
    double_mlplr_obj$bootstrap(method = "normal", n_rep_boot = 1000)

    expect_equal(
      double_mlplr_obj$boot_t_stat,
      double_mlplr_obj$boot_coef / double_mlplr_obj$se,
      tolerance = 1e-8
    )

    # with one coefficient, the joint critical value is close to qnorm(0.975)
    ci_joint = double_mlplr_obj$confint(joint = TRUE, level = 0.95)
    crit = (ci_joint[, 2] - ci_joint[, 1]) / (2 * double_mlplr_obj$se)
    expect_equal(unname(crit), qnorm(0.975), tolerance = 0.15)
  }
)
