context("Unit tests for parameter passing for PLIV, partial_x")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

skip_on_cran()

test_cases = expand.grid(
  learner = "regr.rpart",
  dml_procedure = c("dml1", "dml2"),
  score = "partialling out",
  stringsAsFactors = FALSE)

test_cases_nocf = expand.grid(
  learner = "regr.rpart",
  dml_procedure = "dml1",
  score = "partialling out",
  stringsAsFactors = FALSE)

test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")
test_cases_nocf[".test_name"] = apply(test_cases_nocf, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for parameter passing of PLIV.partialX (oop vs fun):",
  .cases = test_cases, {
    n_rep_boot = 498
    n_folds = 2
    n_rep = 3

    learner_pars = get_default_mlmethod_pliv(learner)
    df = data_pliv$df

    set.seed(3141)
    pliv_hat = dml_pliv_partial_x(df,
      y = "y", d = "d", z = c("z", "z2"),
      n_folds = n_folds, n_rep = n_rep,
      ml_l = mlr3::lrn(learner_pars$mlmethod$mlmethod_l),
      ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
      ml_r = mlr3::lrn(learner_pars$mlmethod$mlmethod_r),
      params_l = learner_pars$params$params_l,
      params_m = learner_pars$params$params_m,
      params_r = learner_pars$params$params_r,
      dml_procedure = dml_procedure, score = score)
    theta = pliv_hat$coef
    se = pliv_hat$se

    set.seed(3141)
    boot_theta = bootstrap_pliv_partial_x(pliv_hat$thetas, pliv_hat$ses,
      df,
      y = "y", d = "d", z = c("z", "z2"),
      n_folds = n_folds, n_rep = n_rep,
      smpls = pliv_hat$smpls,
      all_preds = pliv_hat$all_preds,
      bootstrap = "normal", n_rep_boot = n_rep_boot)$boot_coef

    set.seed(3141)
    Xnames = names(df)[names(df) %in% c("y", "d", "z", "z2") == FALSE]

    dml_data = double_ml_data_from_data_frame(df,
      y_col = "y",
      d_cols = "d", x_cols = Xnames, z_cols = c("z", "z2"))
    dml_pliv_obj = DoubleMLPLIV.partialX(
      data = dml_data,
      n_folds = n_folds, n_rep = n_rep,
      ml_l = mlr3::lrn(learner_pars$mlmethod$mlmethod_l),
      ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
      ml_r = mlr3::lrn(learner_pars$mlmethod$mlmethod_r),
      dml_procedure = dml_procedure,
      score = score)

    dml_pliv_obj$set_ml_nuisance_params(
      treat_var = "d",
      learner = "ml_l",
      params = learner_pars$params$params_l)
    dml_pliv_obj$set_ml_nuisance_params(
      learner = "ml_m_z",
      treat_var = "d",
      params = learner_pars$params$params_m)
    dml_pliv_obj$set_ml_nuisance_params(
      learner = "ml_m_z2",
      treat_var = "d",
      params = learner_pars$params$params_m)
    dml_pliv_obj$set_ml_nuisance_params(
      treat_var = "d",
      learner = "ml_r",
      params = learner_pars$params$params_r)

    dml_pliv_obj$fit()

    theta_obj = dml_pliv_obj$coef
    se_obj = dml_pliv_obj$se

    # bootstrap
    set.seed(3141)
    dml_pliv_obj$bootstrap(method = "normal", n_rep = n_rep_boot)
    boot_theta_obj = dml_pliv_obj$boot_coef

    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)

    expect_equal(as.vector(boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)

patrick::with_parameters_test_that("Unit tests for parameter passing of PLIV.partialX (fold-wise vs global)",
  .cases = test_cases, {
    n_folds = 2
    n_rep = 3

    learner_pars = get_default_mlmethod_pliv(learner)

    df = data_pliv$df
    Xnames = names(df)[names(df) %in% c("y", "d", "z", "z2") == FALSE]
    dml_data = double_ml_data_from_data_frame(df,
      y_col = "y",
      d_cols = "d", x_cols = Xnames, z_cols = c("z", "z2"))

    set.seed(3141)
    dml_pliv_obj = DoubleMLPLIV.partialX(dml_data,
      n_folds = n_folds, n_rep = n_rep,
      ml_l = mlr3::lrn(learner_pars$mlmethod$mlmethod_l),
      ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
      ml_r = mlr3::lrn(learner_pars$mlmethod$mlmethod_r),
      dml_procedure = dml_procedure,
      score = score)

    dml_pliv_obj$set_ml_nuisance_params(
      treat_var = "d",
      learner = "ml_l",
      params = learner_pars$params$params_l)
    dml_pliv_obj$set_ml_nuisance_params(
      learner = "ml_m_z",
      treat_var = "d",
      params = learner_pars$params$params_m)
    dml_pliv_obj$set_ml_nuisance_params(
      learner = "ml_m_z2",
      treat_var = "d",
      params = learner_pars$params$params_m)
    dml_pliv_obj$set_ml_nuisance_params(
      treat_var = "d",
      learner = "ml_r",
      params = learner_pars$params$params_r)

    dml_pliv_obj$fit()
    theta = dml_pliv_obj$coef
    se = dml_pliv_obj$se

    params_l_fold_wise = rep(list(rep(list(learner_pars$params$params_l), n_folds)), n_rep)
    params_m_fold_wise = rep(list(rep(list(learner_pars$params$params_m), n_folds)), n_rep)
    params_r_fold_wise = rep(list(rep(list(learner_pars$params$params_r), n_folds)), n_rep)

    set.seed(3141)
    dml_pliv_obj_fold_wise = DoubleMLPLIV.partialX(dml_data,
      n_folds = n_folds, n_rep = n_rep,
      ml_l = mlr3::lrn(learner_pars$mlmethod$mlmethod_l),
      ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
      ml_r = mlr3::lrn(learner_pars$mlmethod$mlmethod_r),
      dml_procedure = dml_procedure,
      score = score)

    dml_pliv_obj_fold_wise$set_ml_nuisance_params(
      treat_var = "d",
      learner = "ml_l",
      params = params_l_fold_wise,
      set_fold_specific = TRUE)
    dml_pliv_obj_fold_wise$set_ml_nuisance_params(
      treat_var = "d",
      learner = "ml_m_z",
      params = params_m_fold_wise,
      set_fold_specific = TRUE)
    dml_pliv_obj_fold_wise$set_ml_nuisance_params(
      treat_var = "d",
      learner = "ml_m_z2",
      params = params_m_fold_wise,
      set_fold_specific = TRUE)
    dml_pliv_obj_fold_wise$set_ml_nuisance_params(
      treat_var = "d",
      learner = "ml_r",
      params = params_r_fold_wise,
      set_fold_specific = TRUE)

    dml_pliv_obj_fold_wise$fit()
    theta_fold_wise = dml_pliv_obj_fold_wise$coef
    se_fold_wise = dml_pliv_obj_fold_wise$se

    expect_equal(theta, theta_fold_wise, tolerance = 1e-8)
    expect_equal(se, se_fold_wise, tolerance = 1e-8)
  }
)

patrick::with_parameters_test_that("Unit tests for parameter passing of PLIV.partialX (default vs explicit)",
  .cases = test_cases, {
    n_folds = 2
    n_rep = 3

    params_l = list(cp = 0.01, minsplit = 20) # this are defaults
    params_m = list(cp = 0.01, minsplit = 20) # this are defaults
    params_r = list(cp = 0.01, minsplit = 20) # this are defaults

    df = data_pliv$df
    Xnames = names(df)[names(df) %in% c("y", "d", "z", "z2") == FALSE]
    dml_data = double_ml_data_from_data_frame(df,
      y_col = "y",
      d_cols = "d", x_cols = Xnames, z_cols = c("z", "z2"))

    set.seed(3141)
    dml_pliv_default = DoubleMLPLIV.partialX(dml_data,
      n_folds = n_folds, n_rep = n_rep,
      ml_l = lrn("regr.rpart"),
      ml_m = lrn("regr.rpart"),
      ml_r = lrn("regr.rpart"),
      dml_procedure = dml_procedure,
      score = score)

    dml_pliv_default$fit()
    theta_default = dml_pliv_default$coef
    se_default = dml_pliv_default$se

    set.seed(3141)
    dml_pliv_obj = DoubleMLPLIV.partialX(dml_data,
      n_folds = n_folds, n_rep = n_rep,
      ml_l = lrn("regr.rpart"),
      ml_m = lrn("regr.rpart"),
      ml_r = lrn("regr.rpart"),
      dml_procedure = dml_procedure,
      score = score)

    dml_pliv_obj$set_ml_nuisance_params(
      treat_var = "d",
      learner = "ml_l",
      params = params_l)
    dml_pliv_obj$set_ml_nuisance_params(
      learner = "ml_m_z",
      treat_var = "d",
      params = params_m)
    dml_pliv_obj$set_ml_nuisance_params(
      learner = "ml_m_z2",
      treat_var = "d",
      params = params_m)
    dml_pliv_obj$set_ml_nuisance_params(
      treat_var = "d",
      learner = "ml_r",
      params = params_r)

    dml_pliv_obj$fit()
    theta = dml_pliv_obj$coef
    se = dml_pliv_obj$se

    expect_equal(theta, theta_default, tolerance = 1e-8)
    expect_equal(se, se_default, tolerance = 1e-8)
  }
)
