context("Unit tests for parameter passing for PLIV")

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
    score = "partialling out",
    stringsAsFactors = FALSE)
}

test_cases_nocf = expand.grid(
  learner = "regr.rpart",
  dml_procedure = "dml1",
  score = "partialling out",
  stringsAsFactors = FALSE)

test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")
test_cases_nocf["test_name"] = apply(test_cases_nocf, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for parameter passing of PLIV (oop vs fun):",
  .cases = test_cases, {
    n_rep_boot = 498
    n_folds = 2
    n_rep = 3
    
    learner_pars = get_default_mlmethod_pliv(learner)
    
    set.seed(3141)
    pliv_hat = dml_pliv(data_pliv$df,
                        y = "y", d = "d", z = "z",
                        n_folds = n_folds, n_rep = n_rep,
                        ml_g = mlr3::lrn(learner_pars$mlmethod$mlmethod_g),
                        ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
                        ml_r = mlr3::lrn(learner_pars$mlmethod$mlmethod_r),
                        params_g = learner_pars$params$params_g,
                        params_m = learner_pars$params$params_m,
                        params_r = learner_pars$params$params_r,
                        dml_procedure = dml_procedure, score = score)
    theta = pliv_hat$coef
    se = pliv_hat$se
    
    boot_theta = bootstrap_pliv(pliv_hat$thetas, pliv_hat$ses,
                                data_pliv$df,
                                y = "y", d = "d", z = "z",
                                n_folds = n_folds, n_rep = n_rep,
                                smpls = pliv_hat$smpls,
                                all_preds= pliv_hat$all_preds,
                                bootstrap = "normal", n_rep_boot = n_rep_boot)$boot_coef

    set.seed(3141)
    dml_pliv_obj = DoubleMLPLIV$new(
      data = data_pliv$dml_data,
      n_folds = n_folds, n_rep = n_rep,
      ml_g = mlr3::lrn(learner_pars$mlmethod$mlmethod_g),
      ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
      ml_r = mlr3::lrn(learner_pars$mlmethod$mlmethod_r),
      dml_procedure = dml_procedure,
      score = score)
    
    dml_pliv_obj$set_ml_nuisance_params(treat_var = "d",
                                        learner = "ml_g",
                                        params = learner_pars$params$params_g)
    dml_pliv_obj$set_ml_nuisance_params(treat_var = "d",
                                        learner = "ml_m",
                                        params = learner_pars$params$params_m)
    dml_pliv_obj$set_ml_nuisance_params(treat_var = "d",
                                        learner = "ml_r",
                                        params = learner_pars$params$params_r)
    
    dml_pliv_obj$fit()

    theta_obj = dml_pliv_obj$coef
    se_obj = dml_pliv_obj$se
    
    # bootstrap
    dml_pliv_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    boot_theta_obj = dml_pliv_obj$boot_coef
    
    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)
    
    expect_equal(as.vector(boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)

patrick::with_parameters_test_that("Unit tests for parameter passing of PLIV (no cross-fitting)",
  .cases = test_cases_nocf, {
    n_folds = 2
    
    learner_pars = get_default_mlmethod_pliv(learner)
    
    # Passing for non-cross-fitting case
    set.seed(3141)
    my_task = Task$new("help task", "regr", data_pliv$df)
    my_sampling = rsmp("holdout", ratio = 0.5)$instantiate(my_task)
    train_ids = list(my_sampling$train_set(1))
    test_ids = list(my_sampling$test_set(1))
    smpls = list(list(train_ids = train_ids, test_ids = test_ids))
    
    pliv_hat = dml_pliv(data_pliv$df,
                        y = "y", d = "d", z = "z",
                        n_folds = 1,
                        ml_g = mlr3::lrn(learner_pars$mlmethod$mlmethod_g),
                        ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
                        ml_r = mlr3::lrn(learner_pars$mlmethod$mlmethod_r),
                        params_g = learner_pars$params$params_g,
                        params_m = learner_pars$params$params_m,
                        params_r = learner_pars$params$params_r,
                        dml_procedure = dml_procedure, score = score,
                        smpls=smpls)
    theta = pliv_hat$coef
    se = pliv_hat$se
    
    set.seed(3141)
    dml_pliv_nocf = DoubleMLPLIV$new(data = data_pliv$dml_data,
                                     n_folds = n_folds,
                                     ml_g = mlr3::lrn(learner_pars$mlmethod$mlmethod_g),
                                     ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
                                     ml_r = mlr3::lrn(learner_pars$mlmethod$mlmethod_r),
                                     dml_procedure = dml_procedure,
                                     score = score,
                                     apply_cross_fitting = FALSE)
    
    dml_pliv_nocf$set_ml_nuisance_params(treat_var = "d",
                                         learner = "ml_g",
                                         params = learner_pars$params$params_g)
    dml_pliv_nocf$set_ml_nuisance_params(treat_var = "d",
                                         learner = "ml_m",
                                         params = learner_pars$params$params_m)
    dml_pliv_nocf$set_ml_nuisance_params(treat_var = "d",
                                         learner = "ml_r",
                                         params = learner_pars$params$params_r)
    
    dml_pliv_nocf$fit()
    theta_obj = dml_pliv_nocf$coef
    se_obj = dml_pliv_nocf$se
    
    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)
  }
)

patrick::with_parameters_test_that("Unit tests for parameter passing of PLIV (fold-wise vs global)",
  .cases = test_cases, {
    n_folds = 2
    n_rep = 3
    
    learner_pars = get_default_mlmethod_pliv(learner)
    
    set.seed(3141)
    dml_pliv_obj = DoubleMLPLIV$new(data_pliv$dml_data,
                                    n_folds = n_folds, n_rep = n_rep,
                                    ml_g = mlr3::lrn(learner_pars$mlmethod$mlmethod_g),
                                    ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
                                    ml_r = mlr3::lrn(learner_pars$mlmethod$mlmethod_r),
                                    dml_procedure = dml_procedure,
                                    score = score)
    
    dml_pliv_obj$set_ml_nuisance_params(treat_var = "d",
                                        learner = "ml_g",
                                        params = learner_pars$params$params_g)
    dml_pliv_obj$set_ml_nuisance_params(treat_var = "d",
                                        learner = "ml_m",
                                        params = learner_pars$params$params_m)
    dml_pliv_obj$set_ml_nuisance_params(treat_var = "d",
                                        learner = "ml_r",
                                        params = learner_pars$params$params_r)
    
    dml_pliv_obj$fit()
    theta = dml_pliv_obj$coef
    se = dml_pliv_obj$se
    
    params_g_fold_wise = rep(list(rep(list(learner_pars$params$params_g), n_folds)), n_rep)
    params_m_fold_wise = rep(list(rep(list(learner_pars$params$params_m), n_folds)), n_rep)
    params_r_fold_wise = rep(list(rep(list(learner_pars$params$params_r), n_folds)), n_rep)
    
    set.seed(3141)
    dml_pliv_obj_fold_wise = DoubleMLPLIV$new(data_pliv$dml_data,
                                    n_folds = n_folds, n_rep = n_rep,
                                    ml_g = mlr3::lrn(learner_pars$mlmethod$mlmethod_g),
                                    ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
                                    ml_r = mlr3::lrn(learner_pars$mlmethod$mlmethod_r),
                                    dml_procedure = dml_procedure,
                                    score = score)
    
    dml_pliv_obj_fold_wise$set_ml_nuisance_params(treat_var = "d",
                                        learner = "ml_g",
                                        params = params_g_fold_wise,
                                        set_fold_specific = TRUE)
    dml_pliv_obj_fold_wise$set_ml_nuisance_params(treat_var = "d",
                                        learner = "ml_m",
                                        params = params_m_fold_wise,
                                        set_fold_specific = TRUE)
    dml_pliv_obj_fold_wise$set_ml_nuisance_params(treat_var = "d",
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

patrick::with_parameters_test_that("Unit tests for parameter passing of PLIV (default vs explicit)",
  .cases = test_cases, {
    n_folds = 2
    n_rep = 3
    
    params_g = list(cp = 0.01, minsplit = 20) # this are defaults
    params_m = list(cp = 0.01, minsplit = 20) # this are defaults
    params_r = list(cp = 0.01, minsplit = 20) # this are defaults
    
    set.seed(3141)
    dml_pliv_default = DoubleMLPLIV$new(data_pliv$dml_data,
                                    n_folds = n_folds, n_rep = n_rep,
                                    ml_g = lrn('regr.rpart'),
                                    ml_m = lrn('regr.rpart'),
                                    ml_r = lrn('regr.rpart'),
                                    dml_procedure = dml_procedure,
                                    score = score)
    
    dml_pliv_default$fit()
    theta_default = dml_pliv_default$coef
    se_default = dml_pliv_default$se
    
    set.seed(3141)
    dml_pliv_obj = DoubleMLPLIV$new(data = data_pliv$dml_data,
                                    n_folds = n_folds, n_rep = n_rep,
                                    ml_g = lrn('regr.rpart'),
                                    ml_m = lrn('regr.rpart'),
                                    ml_r = lrn('regr.rpart'),
                                    dml_procedure = dml_procedure,
                                    score = score)
    
    dml_pliv_obj$set_ml_nuisance_params(treat_var = "d",
                                        learner = "ml_g",
                                        params = params_g)
    dml_pliv_obj$set_ml_nuisance_params(treat_var = "d",
                                        learner = "ml_m",
                                        params = params_m)
    dml_pliv_obj$set_ml_nuisance_params(treat_var = "d",
                                        learner = "ml_r",
                                        params = params_r)
    
    dml_pliv_obj$fit()
    theta = dml_pliv_obj$coef
    se = dml_pliv_obj$se
    
    expect_equal(theta, theta_default, tolerance = 1e-8)
    expect_equal(se, se_default, tolerance = 1e-8)
  }
)
