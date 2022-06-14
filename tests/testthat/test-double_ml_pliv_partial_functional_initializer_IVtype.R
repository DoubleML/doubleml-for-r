context("Unit tests for PLIV, partialling out X, Z, XZ")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.lm",
    dml_procedure = "dml2",
    score = "IV-type",
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = c("regr.lm", "regr.cv_glmnet"),
    dml_procedure = c("dml1", "dml2"),
    score = "IV-type",
    stringsAsFactors = FALSE)
}
test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for PLIV (partialX functional initialization):",
  .cases = test_cases, {
    learner_pars = get_default_mlmethod_pliv(learner)
    df = data_pliv$df
    Xnames = names(df)[names(df) %in% c("y", "d", "z", "z2") == FALSE]
    data_ml = double_ml_data_from_data_frame(df,
      y_col = "y",
      d_cols = "d", x_cols = Xnames, z_cols = "z")

    # Partial out X (default PLIV)
    set.seed(3141)
    double_mlpliv_obj = DoubleMLPLIV$new(data_ml,
      n_folds = 5,
      ml_l = learner_pars$ml_l$clone(),
      ml_m = learner_pars$ml_m$clone(),
      ml_r = learner_pars$ml_r$clone(),
      ml_g = learner_pars$ml_g$clone(),
      dml_procedure = dml_procedure,
      score = score)

    double_mlpliv_obj$fit()
    theta_obj = double_mlpliv_obj$coef
    se_obj = double_mlpliv_obj$se

    # Partial out X
    set.seed(3141)
    double_mlpliv_partX = DoubleMLPLIV.partialX(data_ml,
      n_folds = 5,
      ml_l = learner_pars$ml_l$clone(),
      ml_m = learner_pars$ml_m$clone(),
      ml_r = learner_pars$ml_r$clone(),
      ml_g = learner_pars$ml_g$clone(),
      dml_procedure = dml_procedure,
      score = score)

    double_mlpliv_partX$fit()
    theta_partX = double_mlpliv_partX$coef
    se_partX = double_mlpliv_partX$se

    expect_equal(theta_partX, theta_obj, tolerance = 1e-8)
    expect_equal(se_partX, se_obj, tolerance = 1e-8)
  }
)
