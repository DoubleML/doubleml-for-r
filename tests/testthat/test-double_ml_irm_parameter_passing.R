context("Unit tests for parameter passing of IRM")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "rpart",
    dml_procedure = "dml2",
    score = "ATE",
    trimming_threshold = 1e-5,
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = "rpart",
    dml_procedure = c("dml1", "dml2"),
    score = c("ATE", "ATTE"),
    trimming_threshold = 1e-5,
    stringsAsFactors = FALSE)
}

test_cases_nocf = expand.grid(
  learner = "rpart",
  dml_procedure = "dml1",
  score = c("ATE", "ATTE"),
  trimming_threshold = 1e-5,
  stringsAsFactors = FALSE)

test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")
test_cases_nocf["test_name"] = apply(test_cases_nocf, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for parameter passing of IRM (oop vs fun):",
  .cases = test_cases, {
    n_rep_boot = 498
    n_folds = 2
    n_rep = 3
  
    learner_pars = get_default_mlmethod_irm(learner)
    
    set.seed(3141)
    irm_hat = dml_irm(data_irm$df,
                      y = "y", d = "d",
                      n_folds = n_folds,
                      n_rep = n_rep,
                      ml_g = mlr3::lrn(learner_pars$mlmethod$mlmethod_g),
                      ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m, predict_type = "prob"),
                      params_g = learner_pars$params$params_g,
                      params_m = learner_pars$params$params_m,
                      dml_procedure = dml_procedure, score = score,
                      trimming_threshold = trimming_threshold)
    theta = irm_hat$coef
    se = irm_hat$se
    
    boot_theta = bootstrap_irm(irm_hat$thetas, irm_hat$ses,
                               data_irm$df,
                               y = "y", d = "d",
                               n_folds = n_folds, n_rep = n_rep,
                               smpls = irm_hat$smpls,
                               all_preds= irm_hat$all_preds,
                               score = score,
                               bootstrap = "normal", n_rep_boot = n_rep_boot,
                               trimming_threshold = trimming_threshold)$boot_coef

    set.seed(3141)
    double_mlirm_obj = DoubleMLIRM$new(data = data_irm$dml_data,
      n_folds = n_folds,
      ml_g = lrn(learner_pars$mlmethod$mlmethod_g),
      ml_m = lrn(learner_pars$mlmethod$mlmethod_m, predict_type = "prob"),
      dml_procedure = dml_procedure,
      score = score,
      n_rep = n_rep, trimming_threshold = trimming_threshold)

    # set params for nuisance part m
    double_mlirm_obj$set_ml_nuisance_params(
      learner = "ml_m",
      treat_var = "d",
      params = learner_pars$params$params_m)
    # set params for nuisance part g
    double_mlirm_obj$set_ml_nuisance_params(
      learner = "ml_g0",
      treat_var = "d",
      params = learner_pars$params$params_g)
    double_mlirm_obj$set_ml_nuisance_params(
      learner = "ml_g1",
      treat_var = "d",
      params = learner_pars$params$params_g)

    double_mlirm_obj$fit()
    theta_obj = double_mlirm_obj$coef
    se_obj = double_mlirm_obj$se

    # bootstrap
    double_mlirm_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    boot_theta_obj = double_mlirm_obj$boot_coef

    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)

    expect_equal(as.vector(boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)

patrick::with_parameters_test_that("Unit tests for parameter passing of IRM (no cross-fitting)",
  .cases = test_cases_nocf, {
    n_folds = 2

    learner_pars = get_default_mlmethod_irm(learner)

    # Passing for non-cross-fitting case
    set.seed(3141)
    my_task = Task$new("help task", "regr", data_irm$df)
    my_sampling = rsmp("holdout", ratio = 0.5)$instantiate(my_task)
    train_ids = list(my_sampling$train_set(1))
    test_ids = list(my_sampling$test_set(1))
    smpls = list(list(train_ids = train_ids, test_ids = test_ids))

    irm_hat = dml_irm(data_irm$df,
                      y = "y", d = "d",
                      n_folds = 1,
                      ml_g = mlr3::lrn(learner_pars$mlmethod$mlmethod_g),
                      ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m, predict_type = "prob"),
                      params_g = learner_pars$params$params_g,
                      params_m = learner_pars$params$params_m,
                      dml_procedure = dml_procedure, score = score,
                      trimming_threshold = trimming_threshold,
                      smpls=smpls)
    theta = irm_hat$coef
    se = irm_hat$se
    
    set.seed(3141)
    dml_irm_nocf = DoubleMLIRM$new(data = data_irm$dml_data,
                                   n_folds = n_folds,
                                   ml_g = lrn(learner_pars$mlmethod$mlmethod_g),
                                   ml_m = lrn(learner_pars$mlmethod$mlmethod_m, predict_type = "prob"),
                                   dml_procedure = dml_procedure,
                                   score = score,
                                   trimming_threshold = trimming_threshold,
                                   apply_cross_fitting = FALSE)
    
    # set params for nuisance part m
    dml_irm_nocf$set_ml_nuisance_params(
      learner = "ml_m",
      treat_var = "d",
      params = learner_pars$params$params_m)
    # set params for nuisance part g
    dml_irm_nocf$set_ml_nuisance_params(
      learner = "ml_g0",
      treat_var = "d",
      params = learner_pars$params$params_g)
    dml_irm_nocf$set_ml_nuisance_params(
      learner = "ml_g1",
      treat_var = "d",
      params = learner_pars$params$params_g)
    
    dml_irm_nocf$fit()
    theta_obj = dml_irm_nocf$coef
    se_obj = dml_irm_nocf$se
    
    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)
  }
)

patrick::with_parameters_test_that("Unit tests for parameter passing of IRM (fold-wise vs global)",
  .cases = test_cases, {
    n_folds = 2
    n_rep = 3

    learner_pars = get_default_mlmethod_irm(learner)

    set.seed(3141)
    double_mlirm_obj = DoubleMLIRM$new(data = data_irm$dml_data,
                                       n_folds = n_folds,
                                       ml_g = lrn(learner_pars$mlmethod$mlmethod_g),
                                       ml_m = lrn(learner_pars$mlmethod$mlmethod_m, predict_type = "prob"),
                                       dml_procedure = dml_procedure,
                                       score = score,
                                       n_rep = n_rep, trimming_threshold = trimming_threshold)
    
    # set params for nuisance part m
    double_mlirm_obj$set_ml_nuisance_params(
      learner = "ml_m",
      treat_var = "d",
      params = learner_pars$params$params_m)
    # set params for nuisance part g
    double_mlirm_obj$set_ml_nuisance_params(
      learner = "ml_g0",
      treat_var = "d",
      params = learner_pars$params$params_g)
    double_mlirm_obj$set_ml_nuisance_params(
      learner = "ml_g1",
      treat_var = "d",
      params = learner_pars$params$params_g)
    
    double_mlirm_obj$fit()
    theta = double_mlirm_obj$coef
    se = double_mlirm_obj$se
    
    params_g_fold_wise = rep(list(rep(list(learner_pars$params$params_g), n_folds)), n_rep)
    params_m_fold_wise = rep(list(rep(list(learner_pars$params$params_m), n_folds)), n_rep)
    
    set.seed(3141)
    dml_irm_fold_wise = DoubleMLIRM$new(data = data_irm$dml_data,
                                       n_folds = n_folds,
                                       ml_g = lrn(learner_pars$mlmethod$mlmethod_g),
                                       ml_m = lrn(learner_pars$mlmethod$mlmethod_m, predict_type = "prob"),
                                       dml_procedure = dml_procedure,
                                       score = score,
                                       n_rep = n_rep, trimming_threshold = trimming_threshold)
    
    # set params for nuisance part m
    dml_irm_fold_wise$set_ml_nuisance_params(
      learner = "ml_m",
      treat_var = "d",
      params = params_m_fold_wise,
      set_fold_specific = TRUE)
    # set params for nuisance part g
    dml_irm_fold_wise$set_ml_nuisance_params(
      learner = "ml_g0",
      treat_var = "d",
      params = params_g_fold_wise,
      set_fold_specific = TRUE)
    dml_irm_fold_wise$set_ml_nuisance_params(
      learner = "ml_g1",
      treat_var = "d",
      params = params_g_fold_wise,
      set_fold_specific = TRUE)
    
    dml_irm_fold_wise$fit()
    theta_fold_wise = dml_irm_fold_wise$coef
    se_fold_wise = dml_irm_fold_wise$se
    
    expect_equal(theta, theta_fold_wise, tolerance = 1e-8)
    expect_equal(se, se_fold_wise, tolerance = 1e-8)
  }
)

patrick::with_parameters_test_that("Unit tests for parameter passing of IRM (default vs explicit)",
  .cases = test_cases, {
    n_folds = 2
    n_rep = 3
    
    params_g = list(cp = 0.01, minsplit = 20) # this are defaults
    params_m = list(cp = 0.01, minsplit = 20) # this are defaults
    
    set.seed(3141)
    dml_irm_default = DoubleMLIRM$new(data = data_irm$dml_data,
                                      n_folds = n_folds,
                                      ml_g = lrn('regr.rpart'),
                                      ml_m = lrn('classif.rpart', predict_type = "prob"),
                                      dml_procedure = dml_procedure,
                                      score = score,
                                      n_rep = n_rep, trimming_threshold = trimming_threshold)
    
    dml_irm_default$fit()
    theta_default = dml_irm_default$coef
    se_default = dml_irm_default$se
    
    set.seed(3141)
    double_mlirm_obj = DoubleMLIRM$new(data = data_irm$dml_data,
                                       n_folds = n_folds,
                                       ml_g = lrn('regr.rpart'),
                                       ml_m = lrn('classif.rpart', predict_type = "prob"),
                                       dml_procedure = dml_procedure,
                                       score = score,
                                       n_rep = n_rep, trimming_threshold = trimming_threshold)
    
    # set params for nuisance part m
    double_mlirm_obj$set_ml_nuisance_params(
      learner = "ml_m",
      treat_var = "d",
      params = params_m)
    # set params for nuisance part g
    double_mlirm_obj$set_ml_nuisance_params(
      learner = "ml_g0",
      treat_var = "d",
      params = params_g)
    double_mlirm_obj$set_ml_nuisance_params(
      learner = "ml_g1",
      treat_var = "d",
      params = params_g)
    
    double_mlirm_obj$fit()
    theta = double_mlirm_obj$coef
    se = double_mlirm_obj$se
    
    expect_equal(theta, theta_default, tolerance = 1e-8)
    expect_equal(se, se_default, tolerance = 1e-8)
  }
)
