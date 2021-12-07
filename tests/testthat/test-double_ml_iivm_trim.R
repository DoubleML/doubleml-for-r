context("Unit tests for IIVM")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "rpart",
    dml_procedure = "dml2",
    score = "LATE",
    trimming_rule = c("truncate"),
    trimming_threshold = c(0.05),
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = "rpart",
    dml_procedure = c("dml1", "dml2"),
    score = "LATE",
    trimming_rule = c("truncate"),
    trimming_threshold = c(1e-12, 0.05),
    stringsAsFactors = FALSE)
}
test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for IIVM:",
  .cases = test_cases, {
    learner_pars = get_default_mlmethod_iivm(learner)
    n_rep_boot = 498

    set.seed(3141)
    iivm_hat = dml_irmiv(data_iivm$df,
      y = "y", d = "d", z = "z",
      n_folds = 5,
      ml_g = learner_pars$ml_g$clone(),
      ml_m = learner_pars$ml_m$clone(),
      ml_r = learner_pars$ml_r$clone(),
      dml_procedure = dml_procedure, score = score,
      trimming_threshold = trimming_threshold)
    theta = iivm_hat$coef
    se = iivm_hat$se

    boot_theta = bootstrap_irmiv(iivm_hat$thetas, iivm_hat$ses,
      data_iivm$df,
      y = "y", d = "d", z = "z",
      n_folds = 5, smpls = iivm_hat$smpls,
      all_preds = iivm_hat$all_preds,
      score = score,
      bootstrap = "normal", n_rep_boot = n_rep_boot,
      trimming_threshold = trimming_threshold)$boot_coef

    set.seed(3141)

    # we rename the z variable to have non default names in the unit tests
    data = data_iivm$df
    names(data)[names(data) == "z"] = "Z_IV"

    Xnames = names(data)[names(data) %in% c("y", "d", "Z_IV") == FALSE]

    data_ml = double_ml_data_from_data_frame(data,
      y_col = "y",
      d_cols = "d", x_cols = Xnames, z_col = "Z_IV")

    double_mliivm_obj = DoubleMLIIVM$new(data_ml,
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

    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)
    expect_equal(as.vector(boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)
