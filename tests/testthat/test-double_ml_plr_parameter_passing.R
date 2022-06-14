context("Unit tests for parameter passing for PLR")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.rpart",
    dml_procedure = "dml2",
    score = "partialling out",
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = "regr.rpart",
    dml_procedure = c("dml1", "dml2"),
    score = c("IV-type", "partialling out"),
    stringsAsFactors = FALSE)
}

test_cases_nocf = expand.grid(
  learner = "regr.rpart",
  dml_procedure = "dml1",
  score = "partialling out",
  stringsAsFactors = FALSE)

test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")
test_cases_nocf[".test_name"] = apply(test_cases_nocf, 1, paste, collapse = "_")

# skip('Skip tests for tuning')

patrick::with_parameters_test_that("Unit tests for parameter passing of PLR (oop vs fun)",
  .cases = test_cases, {
    n_rep_boot = 498
    n_folds = 2
    n_rep = 3

    learner_pars = get_default_mlmethod_plr(learner)
    params_l = rep(list(learner_pars$params$params_l), 2)
    params_m = rep(list(learner_pars$params$params_m), 2)
    params_g = rep(list(learner_pars$params$params_g), 2)

    set.seed(3141)
    if (score == "IV-type") {
      ml_g = mlr3::lrn(learner_pars$mlmethod$mlmethod_g)
    } else {
      ml_g = NULL
    }
    plr_hat = dml_plr_multitreat(data_plr_multi,
      y = "y", d = c("d1", "d2"),
      n_folds = n_folds, n_rep = n_rep,
      ml_l = mlr3::lrn(learner_pars$mlmethod$mlmethod_l),
      ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
      ml_g = ml_g,
      params_l = params_l,
      params_m = params_m,
      params_g = params_g,
      dml_procedure = dml_procedure, score = score)
    theta = plr_hat$coef
    se = plr_hat$se

    boot_theta = boot_plr_multitreat(plr_hat$thetas, plr_hat$ses,
      data_plr_multi,
      y = "y", d = c("d1", "d2"),
      n_folds = n_folds, n_rep = n_rep,
      smpls = plr_hat$smpls,
      all_preds = plr_hat$all_preds,
      bootstrap = "normal", n_rep_boot = n_rep_boot,
      score = score)$boot_coef

    Xnames = names(data_plr_multi)[names(data_plr_multi) %in% c("y", "d1", "d2", "z") == FALSE]
    data_ml = double_ml_data_from_data_frame(data_plr_multi,
      y_col = "y",
      d_cols = c("d1", "d2"), x_cols = Xnames)

    set.seed(3141)
    if (score == "IV-type") {
      ml_g = mlr3::lrn(learner_pars$mlmethod$mlmethod_g)
    } else {
      ml_g = NULL
    }
    double_mlplr_obj = DoubleMLPLR$new(data_ml,
      n_folds = n_folds,
      ml_l = mlr3::lrn(learner_pars$mlmethod$mlmethod_l),
      ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
      ml_g = ml_g,
      dml_procedure = dml_procedure,
      score = score,
      n_rep = n_rep)

    double_mlplr_obj$set_ml_nuisance_params(
      treat_var = "d1", learner = "ml_l",
      params = learner_pars$params$params_l)
    double_mlplr_obj$set_ml_nuisance_params(
      treat_var = "d2", learner = "ml_l",
      params = learner_pars$params$params_l)
    double_mlplr_obj$set_ml_nuisance_params(
      treat_var = "d1", learner = "ml_m",
      params = learner_pars$params$params_m)
    double_mlplr_obj$set_ml_nuisance_params(
      treat_var = "d2", learner = "ml_m",
      params = learner_pars$params$params_m)
    if (score == "IV-type") {
      double_mlplr_obj$set_ml_nuisance_params(
        treat_var = "d1", learner = "ml_g",
        params = learner_pars$params$params_g)
      double_mlplr_obj$set_ml_nuisance_params(
        treat_var = "d2", learner = "ml_g",
        params = learner_pars$params$params_g)
    }


    double_mlplr_obj$fit()
    theta_obj = double_mlplr_obj$coef
    se_obj = double_mlplr_obj$se

    # bootstrap
    double_mlplr_obj$bootstrap(method = "normal", n_rep = n_rep_boot)
    boot_theta_obj = double_mlplr_obj$boot_coef

    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)

    expect_equal(as.vector(boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)

patrick::with_parameters_test_that("Unit tests for parameter passing of PLR (no cross-fitting)",
  .cases = test_cases_nocf, {
    n_rep_boot = 498
    n_folds = 2

    learner_pars = get_default_mlmethod_plr(learner)
    params_l = rep(list(learner_pars$params$params_l), 2)
    params_m = rep(list(learner_pars$params$params_m), 2)
    params_g = rep(list(learner_pars$params$params_g), 2)

    # Passing for non-cross-fitting case
    set.seed(3141)
    my_task = Task$new("help task", "regr", data_plr_multi)
    my_sampling = rsmp("holdout", ratio = 0.5)$instantiate(my_task)
    train_ids = list(my_sampling$train_set(1))
    test_ids = list(my_sampling$test_set(1))
    smpls = list(list(train_ids = train_ids, test_ids = test_ids))

    if (score == "IV-type") {
      ml_g = mlr3::lrn(learner_pars$mlmethod$mlmethod_g)
    } else {
      ml_g = NULL
    }
    plr_hat = dml_plr_multitreat(data_plr_multi,
      y = "y", d = c("d1", "d2"),
      n_folds = 1,
      ml_l = mlr3::lrn(learner_pars$mlmethod$mlmethod_l),
      ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
      ml_g = ml_g,
      params_l = params_l,
      params_m = params_m,
      params_g = params_g,
      dml_procedure = dml_procedure, score = score,
      smpls = smpls)
    theta = plr_hat$coef
    se = plr_hat$se

    Xnames = names(data_plr_multi)[names(data_plr_multi) %in% c("y", "d1", "d2", "z") == FALSE]
    data_ml = double_ml_data_from_data_frame(data_plr_multi,
      y_col = "y",
      d_cols = c("d1", "d2"), x_cols = Xnames)

    set.seed(3141)
    if (score == "IV-type") {
      ml_g = mlr3::lrn(learner_pars$mlmethod$mlmethod_g)
    } else {
      ml_g = NULL
    }
    double_mlplr_obj_nocf = DoubleMLPLR$new(data_ml,
      n_folds = n_folds,
      ml_l = mlr3::lrn(learner_pars$mlmethod$mlmethod_l),
      ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
      ml_g = ml_g,
      dml_procedure = dml_procedure,
      score = score,
      apply_cross_fitting = FALSE)

    double_mlplr_obj_nocf$set_ml_nuisance_params(
      treat_var = "d1", learner = "ml_l",
      params = learner_pars$params$params_l)
    double_mlplr_obj_nocf$set_ml_nuisance_params(
      treat_var = "d2", learner = "ml_l",
      params = learner_pars$params$params_l)
    double_mlplr_obj_nocf$set_ml_nuisance_params(
      treat_var = "d1", learner = "ml_m",
      params = learner_pars$params$params_m)
    double_mlplr_obj_nocf$set_ml_nuisance_params(
      treat_var = "d2", learner = "ml_m",
      params = learner_pars$params$params_m)
    if (score == "IV-type") {
      double_mlplr_obj_nocf$set_ml_nuisance_params(
        treat_var = "d1", learner = "ml_g",
        params = learner_pars$params$params_g)
      double_mlplr_obj_nocf$set_ml_nuisance_params(
        treat_var = "d2", learner = "ml_g",
        params = learner_pars$params$params_g)
    }


    double_mlplr_obj_nocf$fit()
    theta_obj_nocf = double_mlplr_obj_nocf$coef
    se_obj_nocf = double_mlplr_obj_nocf$se

    expect_equal(theta, theta_obj_nocf, tolerance = 1e-8)
    expect_equal(se, se_obj_nocf, tolerance = 1e-8)
  }
)

patrick::with_parameters_test_that("Unit tests for parameter passing of PLR (fold-wise vs global)",
  .cases = test_cases, {
    n_rep_boot = 498
    n_folds = 2
    n_rep = 3

    learner_pars = get_default_mlmethod_plr(learner)

    Xnames = names(data_plr_multi)[names(data_plr_multi) %in% c("y", "d1", "d2", "z") == FALSE]
    data_ml = double_ml_data_from_data_frame(data_plr_multi,
      y_col = "y",
      d_cols = c("d1", "d2"), x_cols = Xnames)

    set.seed(3141)
    if (score == "IV-type") {
      ml_g = mlr3::lrn(learner_pars$mlmethod$mlmethod_g)
    } else {
      ml_g = NULL
    }
    double_mlplr_obj = DoubleMLPLR$new(data_ml,
      n_folds = n_folds,
      ml_l = mlr3::lrn(learner_pars$mlmethod$mlmethod_l),
      ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
      ml_g = ml_g,
      dml_procedure = dml_procedure,
      score = score,
      n_rep = n_rep)

    double_mlplr_obj$set_ml_nuisance_params(
      treat_var = "d1", learner = "ml_l",
      params = learner_pars$params$params_l)
    double_mlplr_obj$set_ml_nuisance_params(
      treat_var = "d2", learner = "ml_l",
      params = learner_pars$params$params_l)
    double_mlplr_obj$set_ml_nuisance_params(
      treat_var = "d1", learner = "ml_m",
      params = learner_pars$params$params_m)
    double_mlplr_obj$set_ml_nuisance_params(
      treat_var = "d2", learner = "ml_m",
      params = learner_pars$params$params_m)
    if (score == "IV-type") {
      double_mlplr_obj$set_ml_nuisance_params(
        treat_var = "d1", learner = "ml_g",
        params = learner_pars$params$params_g)
      double_mlplr_obj$set_ml_nuisance_params(
        treat_var = "d2", learner = "ml_g",
        params = learner_pars$params$params_g)
    }

    double_mlplr_obj$fit()
    theta = double_mlplr_obj$coef
    se = double_mlplr_obj$se

    params_l_fold_wise = rep(list(rep(list(learner_pars$params$params_l), n_folds)), n_rep)
    params_m_fold_wise = rep(list(rep(list(learner_pars$params$params_m), n_folds)), n_rep)
    params_g_fold_wise = rep(list(rep(list(learner_pars$params$params_g), n_folds)), n_rep)

    set.seed(3141)
    if (score == "IV-type") {
      ml_g = mlr3::lrn(learner_pars$mlmethod$mlmethod_g)
    } else {
      ml_g = NULL
    }
    dml_plr_fold_wise = DoubleMLPLR$new(data_ml,
      n_folds = n_folds,
      ml_l = mlr3::lrn(learner_pars$mlmethod$mlmethod_l),
      ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
      ml_g = ml_g,
      dml_procedure = dml_procedure,
      score = score,
      n_rep = n_rep)

    dml_plr_fold_wise$set_ml_nuisance_params(
      treat_var = "d1", learner = "ml_l",
      params = params_l_fold_wise,
      set_fold_specific = TRUE)
    dml_plr_fold_wise$set_ml_nuisance_params(
      treat_var = "d2", learner = "ml_l",
      params = params_l_fold_wise,
      set_fold_specific = TRUE)
    dml_plr_fold_wise$set_ml_nuisance_params(
      treat_var = "d1", learner = "ml_m",
      params = params_m_fold_wise,
      set_fold_specific = TRUE)
    dml_plr_fold_wise$set_ml_nuisance_params(
      treat_var = "d2", learner = "ml_m",
      params = params_m_fold_wise,
      set_fold_specific = TRUE)
    if (score == "IV-type") {
      dml_plr_fold_wise$set_ml_nuisance_params(
        treat_var = "d1", learner = "ml_g",
        params = params_g_fold_wise,
        set_fold_specific = TRUE)
      dml_plr_fold_wise$set_ml_nuisance_params(
        treat_var = "d2", learner = "ml_g",
        params = params_g_fold_wise,
        set_fold_specific = TRUE)
    }

    dml_plr_fold_wise$fit()
    theta_fold_wise = dml_plr_fold_wise$coef
    se_fold_wise = dml_plr_fold_wise$se

    expect_equal(theta, theta_fold_wise, tolerance = 1e-8)
    expect_equal(se, se_fold_wise, tolerance = 1e-8)
  }
)

patrick::with_parameters_test_that("Unit tests for parameter passing of PLR (default vs explicit)",
  .cases = test_cases, {
    n_rep_boot = 498
    n_folds = 2
    n_rep = 3

    params_l = list(cp = 0.01, minsplit = 20) # this are defaults
    params_m = list(cp = 0.01, minsplit = 20) # this are defaults
    params_g = list(cp = 0.01, minsplit = 20) # this are defaults

    Xnames = names(data_plr_multi)[names(data_plr_multi) %in% c("y", "d1", "d2", "z") == FALSE]
    data_ml = double_ml_data_from_data_frame(data_plr_multi,
      y_col = "y",
      d_cols = c("d1", "d2"), x_cols = Xnames)

    set.seed(3141)
    if (score == "IV-type") {
      ml_g = lrn("regr.rpart")
    } else {
      ml_g = NULL
    }
    dml_plr_default = DoubleMLPLR$new(data_ml,
      n_folds = n_folds,
      ml_l = lrn("regr.rpart"),
      ml_m = lrn("regr.rpart"),
      ml_g = ml_g,
      dml_procedure = dml_procedure,
      score = score,
      n_rep = n_rep)

    dml_plr_default$fit()
    theta_default = dml_plr_default$coef
    se_default = dml_plr_default$se

    set.seed(3141)
    if (score == "IV-type") {
      ml_g = lrn("regr.rpart")
    } else {
      ml_g = NULL
    }
    double_mlplr_obj = DoubleMLPLR$new(data_ml,
      n_folds = n_folds,
      ml_l = lrn("regr.rpart"),
      ml_m = lrn("regr.rpart"),
      ml_g = ml_g,
      dml_procedure = dml_procedure,
      score = score,
      n_rep = n_rep)
    double_mlplr_obj$set_ml_nuisance_params(
      treat_var = "d1", learner = "ml_l",
      params = params_l)
    double_mlplr_obj$set_ml_nuisance_params(
      treat_var = "d2", learner = "ml_l",
      params = params_l)
    double_mlplr_obj$set_ml_nuisance_params(
      treat_var = "d1", learner = "ml_m",
      params = params_m)
    double_mlplr_obj$set_ml_nuisance_params(
      treat_var = "d2", learner = "ml_m",
      params = params_m)
    if (score == "IV-type") {
      double_mlplr_obj$set_ml_nuisance_params(
        treat_var = "d1", learner = "ml_g",
        params = params_g)
      double_mlplr_obj$set_ml_nuisance_params(
        treat_var = "d2", learner = "ml_g",
        params = params_g)
    }

    double_mlplr_obj$fit()
    theta = double_mlplr_obj$coef
    se = double_mlplr_obj$se

    expect_equal(theta, theta_default, tolerance = 1e-8)
    expect_equal(se, se_default, tolerance = 1e-8)
  }
)
