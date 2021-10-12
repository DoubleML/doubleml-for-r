context("Unit tests for PLR with external sample provision")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.cv_glmnet",
    dml_procedure = "dml2",
    score = "partialling out",
    n_folds = c(2),
    n_rep = c(1),
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = "regr.cv_glmnet",
    dml_procedure = c("dml1", "dml2"),
    score = c("IV-type", "partialling out"),
    n_folds = c(2, 3),
    n_rep = c(1, 3),
    stringsAsFactors = FALSE)
}
test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("PLR with external sample provision:",
  .cases = test_cases, {
    learner_pars = get_default_mlmethod_plr(learner)
    n_rep_boot = 346

    set.seed(3141)
    Xnames = names(data_plr$df)[names(data_plr$df) %in% c("y", "d", "z") == FALSE]
    data_ml = double_ml_data_from_data_frame(data_plr$df,
      y_col = "y",
      d_cols = "d", x_cols = Xnames)

    double_mlplr_obj = DoubleMLPLR$new(data_ml,
      ml_g = learner_pars$ml_g$clone(),
      ml_m = learner_pars$ml_m$clone(),
      dml_procedure = dml_procedure,
      n_folds = n_folds,
      score = score,
      n_rep = n_rep)

    set.seed(123)
    double_mlplr_obj$fit()
    theta_obj = double_mlplr_obj$coef
    se_obj = double_mlplr_obj$se
    double_mlplr_obj$bootstrap(method = "normal", n_rep = n_rep_boot)
    boot_theta_obj = double_mlplr_obj$boot_coef

    # External sample provision
    SAMPLES = double_mlplr_obj$smpls
    double_mlplr_obj_external = DoubleMLPLR$new(data_ml,
      ml_g = learner_pars$ml_g$clone(),
      ml_m = learner_pars$ml_m$clone(),
      dml_procedure = dml_procedure,
      score = score,
      draw_sample_splitting = FALSE)

    double_mlplr_obj_external$set_sample_splitting(SAMPLES)

    set.seed(123)
    double_mlplr_obj_external$fit()
    theta_obj_external = double_mlplr_obj_external$coef
    se_obj_external = double_mlplr_obj_external$se
    double_mlplr_obj_external$bootstrap(method = "normal", n_rep = n_rep_boot)
    boot_theta_obj_external = double_mlplr_obj_external$boot_coef

    expect_equal(theta_obj, theta_obj_external, tolerance = 1e-8)
    expect_equal(se_obj, se_obj_external, tolerance = 1e-8)
    expect_equal(as.vector(boot_theta_obj), as.vector(boot_theta_obj_external), tolerance = 1e-8)
  }
)
