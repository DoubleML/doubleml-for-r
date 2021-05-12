context("Unit tests for PLIV")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.glmnet",
    dml_procedure = "dml1",
    score = "partialling out",
    i_setting = 1:(length(data_pliv)),
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = c("regr.lm", "regr.glmnet"),
    dml_procedure = c("dml1", "dml2"),
    score = "partialling out",
    i_setting = 1:(length(data_pliv)),
    stringsAsFactors = FALSE)
}
test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for PLIV:",
  .cases = test_cases, {
    learner = get_default_mlmethod_pliv(learner)
    n_rep_boot = 498

    set.seed(i_setting)
    pliv_hat = dml_pliv(data_pliv[[i_setting]],
      y = "y", d = "d", z = "z",
      n_folds = 5,
      ml_g = learner$ml_g$clone(),
      ml_m = learner$ml_m$clone(),
      ml_r = learner$ml_r$clone(),
      dml_procedure = dml_procedure, score = score)
    theta = pliv_hat$coef
    se = pliv_hat$se
    
    boot_theta = bootstrap_pliv(pliv_hat$thetas, pliv_hat$ses,
                                 data_pliv[[i_setting]],
                                 y = "y", d = "d", z = "z",
                                 n_folds = 5, smpls = pliv_hat$smpls,
                                 all_preds= pliv_hat$all_preds,
                                 bootstrap = "normal", n_rep_boot = n_rep_boot)$boot_coef

    set.seed(i_setting)
    Xnames = names(data_pliv[[i_setting]])[names(data_pliv[[i_setting]]) %in% c("y", "d", "z") == FALSE]
    data_ml = double_ml_data_from_data_frame(data_pliv[[i_setting]],
      y_col = "y",
      d_cols = "d", x_cols = Xnames, z_cols = "z")

    double_mlpliv_obj = DoubleMLPLIV$new(data_ml,
      n_folds = 5,
      ml_g = learner$ml_g$clone(),
      ml_m = learner$ml_m$clone(),
      ml_r = learner$ml_r$clone(),
      dml_procedure = dml_procedure,
      score = score)

    double_mlpliv_obj$fit()
    theta_obj = double_mlpliv_obj$coef
    se_obj = double_mlpliv_obj$se

    # bootstrap
    double_mlpliv_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    boot_theta_obj = double_mlpliv_obj$boot_coef

    # at the moment the object result comes without a name
    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)
    expect_equal(as.vector(boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)
