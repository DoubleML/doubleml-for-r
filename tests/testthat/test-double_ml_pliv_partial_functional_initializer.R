context("Unit tests for PLIV, partialling out X, Z, XZ")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.cv_glmnet",
    dml_procedure = "dml2",
    score = "partialling out",
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = c("regr.lm", "regr.cv_glmnet"),
    dml_procedure = c("dml1", "dml2"),
    score = "partialling out",
    stringsAsFactors = FALSE)
}
test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for PLIV (partialX functional initialization):",
  .cases = test_cases, {
    learner_pars = get_default_mlmethod_pliv(learner)
    df = data_pliv$df
    Xnames = names(df)[names(df) %in% c("y", "d", "z", "z2") == FALSE]
    data_ml = double_ml_data_from_data_frame(df,
      y_col = "y",
      d_cols = "d", x_cols = Xnames, z_cols = c("z", "z2"))

    # Partial out X (default PLIV)
    set.seed(3141)
    double_mlpliv_obj = DoubleMLPLIV$new(data_ml,
      n_folds = 5,
      ml_g = learner_pars$ml_g$clone(),
      ml_m = learner_pars$ml_m$clone(),
      ml_r = learner_pars$ml_r$clone(),
      dml_procedure = dml_procedure,
      score = score)

    double_mlpliv_obj$fit()
    theta_obj = double_mlpliv_obj$coef
    se_obj = double_mlpliv_obj$se

    # Partial out X
    set.seed(3141)
    double_mlpliv_partX = DoubleMLPLIV.partialX(data_ml,
      n_folds = 5,
      ml_g = learner_pars$ml_g$clone(),
      ml_m = learner_pars$ml_m$clone(),
      ml_r = learner_pars$ml_r$clone(),
      dml_procedure = dml_procedure,
      score = score)

    double_mlpliv_partX$fit()
    theta_partX = double_mlpliv_partX$coef
    se_partX = double_mlpliv_partX$se
    
    expect_equal(theta_partX, theta_obj, tolerance = 1e-8)
    expect_equal(se_partX, se_obj, tolerance = 1e-8)
  }
)

patrick::with_parameters_test_that("Unit tests for PLIV (partialZ functional initialization):",
  .cases = test_cases, {
    learner_pars = get_default_mlmethod_pliv(learner)
    df = data_pliv$df
    Xnames = names(df)[names(df) %in% c("y", "d", "z", "z2") == FALSE]
    data_ml = double_ml_data_from_data_frame(df,
                                             y_col = "y",
                                             d_cols = "d", x_cols = Xnames, z_cols = c("z", "z2"))
    # Partial out Z
    set.seed(3141)
    double_mlpliv_partZ = DoubleMLPLIV$new(data_ml,
                                           n_folds = 5,
                                           ml_g = NULL,
                                           ml_m = NULL,
                                           ml_r = learner_pars$ml_r$clone(),
                                           dml_procedure = dml_procedure,
                                           score = score,
                                           partialX = FALSE, partialZ = TRUE)

    double_mlpliv_partZ$fit()
    theta_partZ = double_mlpliv_partZ$coef
    se_partZ = double_mlpliv_partZ$se

    set.seed(3141)
    double_mlpliv_partZ_fun = DoubleMLPLIV.partialZ(data_ml,
      n_folds = 5,
      ml_r = learner_pars$ml_r$clone(),
      dml_procedure = dml_procedure,
      score = score)

    double_mlpliv_partZ_fun$fit()
    theta_partZ_fun = double_mlpliv_partZ_fun$coef
    se_partZ_fun = double_mlpliv_partZ_fun$se
    
    expect_equal(theta_partZ, theta_partZ_fun, tolerance = 1e-8)
    expect_equal(se_partZ, se_partZ_fun, tolerance = 1e-8)
  }
)

patrick::with_parameters_test_that("Unit tests for PLIV (partialXZ functional initialization):",
  .cases = test_cases, {
    learner_pars = get_default_mlmethod_pliv(learner)
    df = data_pliv$df
    Xnames = names(df)[names(df) %in% c("y", "d", "z", "z2") == FALSE]
    data_ml = double_ml_data_from_data_frame(df,
                                             y_col = "y",
                                             d_cols = "d", x_cols = Xnames, z_cols = c("z", "z2"))

    set.seed(3141)
    double_mlpliv_partXZ = DoubleMLPLIV$new(data_ml,
                                           n_folds = 5,
                                           ml_g = learner_pars$ml_g$clone(),
                                           ml_m = learner_pars$ml_m$clone(),
                                           ml_r = learner_pars$ml_r$clone(),
                                           dml_procedure = dml_procedure,
                                           score = score,
                                           partialX = TRUE, partialZ = TRUE)
    
    double_mlpliv_partXZ$fit()
    theta_partXZ = double_mlpliv_partXZ$coef
    se_partXZ = double_mlpliv_partXZ$se
    
    set.seed(3141)
    double_mlpliv_partXZ_fun = DoubleMLPLIV.partialXZ(data_ml,
      n_folds = 5,
      ml_g = learner_pars$ml_g$clone(),
      ml_m = learner_pars$ml_m$clone(),
      ml_r = learner_pars$ml_r$clone(),
      dml_procedure = dml_procedure,
      score = score)

    double_mlpliv_partXZ_fun$fit()
    theta_partXZ_fun = double_mlpliv_partXZ_fun$coef
    se_partXZ_fun = double_mlpliv_partXZ_fun$se

    expect_equal(theta_partXZ, theta_partXZ_fun, tolerance = 1e-8)
    expect_equal(se_partXZ, se_partXZ_fun, tolerance = 1e-8)
  }
)
