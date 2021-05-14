context("Unit tests for PLIV, partialling out X, Z, XZ")

library("mlr3learners")

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

patrick::with_parameters_test_that("Unit tests for PLIV:",
  .cases = test_cases, {
    learner_pars = get_default_mlmethod_pliv(learner)
    n_rep_boot = 498
    #
    # set.seed(3141)
    # pliv_hat = dml_plriv(data_pliv, y = "y", d = "d", z = 'z',
    #                       n_folds = 5, mlmethod = learner_pars$mlmethod,
    #                       params = learner_pars$params,
    #                       dml_procedure = dml_procedure, score = score,
    #                       bootstrap = "normal",  n_rep_boot = n_rep_boot)
    # theta = coef(pliv_hat)
    # se = pliv_hat$se
    #

    set.seed(3141)
    df = data_pliv$df
    Xnames = names(df)[names(df) %in% c("y", "d", "z", "z2") == FALSE]
    data_ml = double_ml_data_from_data_frame(df,
      y_col = "y",
      d_cols = "d", x_cols = Xnames, z_cols = c("z", "z2"))

    # Partial out X (default PLIV)
    double_mlpliv_obj = DoubleMLPLIV$new(data_ml,
      n_folds = 5,
      ml_g = learner_pars$mlmethod$mlmethod_g,
      ml_m = learner_pars$mlmethod$mlmethod_m,
      ml_r = learner_pars$mlmethod$mlmethod_r,
      dml_procedure = dml_procedure,
      score = score)

    double_mlpliv_obj$set_ml_nuisance_params(
      learner = "ml_g",
      treat_var = "d",
      params = learner_pars$params$params_g)
    double_mlpliv_obj$set_ml_nuisance_params(
      learner = "ml_r",
      treat_var = "d",
      params = learner_pars$params$params_r)
    double_mlpliv_obj$set_ml_nuisance_params(
      learner = "ml_m_z",
      treat_var = "d",
      params = learner_pars$params$params_m)
    double_mlpliv_obj$set_ml_nuisance_params(
      learner = "ml_m_z2",
      treat_var = "d",
      params = learner_pars$params$params_m)

    double_mlpliv_obj$fit()
    theta_obj = double_mlpliv_obj$coef
    se_obj = double_mlpliv_obj$se

    # Partial out X
    set.seed(3141)
    double_mlpliv_partX = DoubleMLPLIV.partialX(data_ml,
      n_folds = 5,
      ml_g = learner_pars$mlmethod$mlmethod_g,
      ml_m = learner_pars$mlmethod$mlmethod_m,
      ml_r = learner_pars$mlmethod$mlmethod_r,
      dml_procedure = dml_procedure,
      score = score)

    double_mlpliv_partX$set_ml_nuisance_params(
      learner = "ml_g",
      treat_var = "d",
      params = learner_pars$params$params_g)
    double_mlpliv_partX$set_ml_nuisance_params(
      learner = "ml_r",
      treat_var = "d",
      params = learner_pars$params$params_r)
    double_mlpliv_partX$set_ml_nuisance_params(
      learner = "ml_m_z",
      treat_var = "d",
      params = learner_pars$params$params_m)
    double_mlpliv_partX$set_ml_nuisance_params(
      learner = "ml_m_z2",
      treat_var = "d",
      params = learner_pars$params$params_m)

    double_mlpliv_partX$fit()
    theta_partX = double_mlpliv_partX$coef
    se_partX = double_mlpliv_partX$se

    # Partial out Z
    set.seed(3141)
    double_mlpliv_partZ = DoubleMLPLIV.partialZ(data_ml,
      n_folds = 5,
      ml_r = learner_pars$mlmethod$mlmethod_r,
      dml_procedure = dml_procedure,
      score = score)

    double_mlpliv_partZ$set_ml_nuisance_params(
      learner = "ml_r",
      treat_var = "d",
      params = learner_pars$params$params_r)

    double_mlpliv_partZ$fit()
    theta_partZ = double_mlpliv_partZ$coef
    se_partZ = double_mlpliv_partZ$se

    set.seed(3141)
    double_mlpliv_partXZ = DoubleMLPLIV.partialXZ(data_ml,
      n_folds = 5,
      ml_g = learner_pars$mlmethod$mlmethod_g,
      ml_m = learner_pars$mlmethod$mlmethod_m,
      ml_r = learner_pars$mlmethod$mlmethod_r,
      dml_procedure = dml_procedure,
      score = score)

    double_mlpliv_partXZ$set_ml_nuisance_params(
      learner = "ml_g",
      treat_var = "d",
      params = learner_pars$params$params_g)

    double_mlpliv_partXZ$set_ml_nuisance_params(
      learner = "ml_m",
      treat_var = "d",
      params = learner_pars$params$params_m)

    double_mlpliv_partXZ$set_ml_nuisance_params(
      learner = "ml_r",
      treat_var = "d",
      params = learner_pars$params$params_r)
    double_mlpliv_partXZ$fit()
    theta_partXZ = double_mlpliv_partXZ$coef
    se_partXZ = double_mlpliv_partXZ$se
    # bootstrap
    # double_mlpliv_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    # boot_theta_obj = double_mlpliv_obj$boot_coef

    # at the moment the object result comes without a name
    expect_equal(theta_partX, theta_obj, tolerance = 1e-8)
    expect_equal(se_partX, se_obj, tolerance = 1e-8)
    # expect_equal(as.vector(pliv_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)
