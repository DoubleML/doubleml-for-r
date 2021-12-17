context("Unit tests for IIVM, binary outcome")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "log_reg",
    dml_procedure = "dml2",
    score = "LATE",
    trimming_threshold = 0.025,
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = "cv_glmnet",
    dml_procedure = c("dml1", "dml2"),
    score = "LATE",
    trimming_threshold = 0.025,
    stringsAsFactors = FALSE)
}

test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for IIVM:",
  .cases = test_cases, {
    learner_pars = get_default_mlmethod_iivm_binary(learner)
    n_rep_boot = 498

    set.seed(3141)
    iivm_hat = dml_irmiv(data_iivm_binary$df,
      y = "y", d = "d", z = "z",
      n_folds = 5,
      ml_g = learner_pars$ml_g$clone(),
      ml_m = learner_pars$ml_m$clone(),
      ml_r = learner_pars$ml_r$clone(),
      dml_procedure = dml_procedure,
      trimming_threshold = trimming_threshold,
      score = score)
    theta = iivm_hat$coef
    se = iivm_hat$se

    boot_theta = bootstrap_irmiv(iivm_hat$thetas, iivm_hat$ses,
      data_iivm_binary$df,
      y = "y", d = "d", z = "z",
      n_folds = 5, smpls = iivm_hat$smpls,
      all_preds = iivm_hat$all_preds,
      trimming_threshold = trimming_threshold,
      score = score,
      bootstrap = "normal", n_rep_boot = n_rep_boot)$boot_coef

    set.seed(3141)
    double_mliivm_obj = DoubleMLIIVM$new(
      data = data_iivm_binary$dml_data,
      n_folds = 5,
      ml_g = learner_pars$ml_g$clone(),
      ml_m = learner_pars$ml_m$clone(),
      ml_r = learner_pars$ml_r$clone(),
      dml_procedure = dml_procedure,
      trimming_threshold = trimming_threshold,
      score = score)
    double_mliivm_obj$fit()
    theta_obj = double_mliivm_obj$coef
    se_obj = double_mliivm_obj$se

    # bootstrap
    double_mliivm_obj$bootstrap(method = "normal", n_rep = n_rep_boot)
    boot_theta_obj = double_mliivm_obj$boot_coef

    # at the moment the object result comes without a name
    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)
    expect_equal(as.vector(boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)
