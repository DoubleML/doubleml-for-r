context("Unit tests for SSM, nonignorable nonresponse")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "cv_glmnet",
    dml_procedure = "dml1",
    score = "nonignorable",
    trimming_threshold = 0,
    stringsAsFactors = FALSE)
  test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")
} else {
  test_cases = expand.grid(
    learner = c("cv_glmnet", "graph_learner"),
    dml_procedure = c("dml1", "dml2"),
    score = "nonignorable",
    trimming_threshold = 0,
    stringsAsFactors = FALSE)
  test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")
}

patrick::with_parameters_test_that("Unit tests for SSM, nonignorable nonresponse:",
  .cases = test_cases, {
    learner_pars = get_default_mlmethod_ssm(learner)
    n_rep_boot = 498

    set.seed(3141)
    ssm_hat = dml_ssm(data_ssm_nonignorable$df,
      y = "y", d = "d", z = "z", s = "s",
      n_folds = 5,
      ml_pi = learner_pars$ml_pi$clone(), ml_m = learner_pars$ml_m$clone(), ml_g = learner_pars$ml_g$clone(),
      dml_procedure = dml_procedure, score = score)
    theta = ssm_hat$coef
    se = ssm_hat$se

    boot_theta = bootstrap_ssm(ssm_hat$thetas, ssm_hat$ses,
      data_ssm_nonignorable$df,
      y = "y", d = "d", s = "s",
      n_folds = 5, smpls = ssm_hat$smpls,
      all_preds = ssm_hat$all_preds,
      score = score,
      bootstrap = "normal", n_rep_boot = n_rep_boot)$boot_coef


    set.seed(3141)
    double_mlssm_obj = DoubleMLSSM$new(
      data = data_ssm_nonignorable$dml_data,
      n_folds = 5,
      ml_pi = learner_pars$ml_pi$clone(),
      ml_m = learner_pars$ml_m$clone(),
      ml_g = learner_pars$ml_g$clone(),
      dml_procedure = dml_procedure,
      score = score,
      trimming_threshold = trimming_threshold)

    double_mlssm_obj$fit()
    theta_obj = double_mlssm_obj$coef
    se_obj = double_mlssm_obj$se

    # bootstrap
    double_mlssm_obj$bootstrap(method = "normal", n_rep = n_rep_boot)
    boot_theta_obj = double_mlssm_obj$boot_coef

    # at the moment the object result comes without a name
    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)
    expect_equal(as.vector(boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)
