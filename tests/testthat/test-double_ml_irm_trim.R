context("Unit tests for IRM propensity score trimming")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "rpart",
    dml_procedure = "dml2",
    score = "ATTE",
    trimming_rule = c("truncate"),
    trimming_threshold = c(0.05),
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = "rpart",
    dml_procedure = c("dml1", "dml2"),
    score = c("ATE", "ATTE"),
    trimming_rule = c("truncate"),
    trimming_threshold = c(1e-12, 0.05),
    stringsAsFactors = FALSE)
}
test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for IRM:",
  .cases = test_cases, {
    learner_pars = get_default_mlmethod_irm(learner)
    n_rep_boot = 498

    set.seed(3141)
    irm_hat = dml_irm(data_irm$df,
      y = "y", d = "d",
      n_folds = 5,
      ml_g = learner_pars$ml_g$clone(),
      ml_m = learner_pars$ml_m$clone(),
      dml_procedure = dml_procedure, score = score,
      trimming_threshold = trimming_threshold)
    theta = irm_hat$coef
    se = irm_hat$se

    boot_theta = bootstrap_irm(irm_hat$thetas, irm_hat$ses,
      data_irm$df,
      y = "y", d = "d",
      n_folds = 5, smpls = irm_hat$smpls,
      all_preds = irm_hat$all_preds,
      score = score,
      bootstrap = "normal", n_rep_boot = n_rep_boot,
      trimming_threshold = trimming_threshold)$boot_coef

    set.seed(3141)
    double_mlirm_obj = DoubleMLIRM$new(
      data = data_irm$dml_data,
      n_folds = 5,
      ml_g = learner_pars$ml_g$clone(),
      ml_m = learner_pars$ml_m$clone(),
      dml_procedure = dml_procedure,
      score = score,
      trimming_rule = trimming_rule,
      trimming_threshold = trimming_threshold)

    double_mlirm_obj$fit()
    theta_obj = double_mlirm_obj$coef
    se_obj = double_mlirm_obj$se

    # bootstrap
    double_mlirm_obj$bootstrap(method = "normal", n_rep = n_rep_boot)
    boot_theta_obj = double_mlirm_obj$boot_coef

    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)
    expect_equal(as.vector(boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)
