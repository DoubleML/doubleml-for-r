context("Unit tests for PLR")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    dml_procedure = "dml1",
    score = "IV-type",
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    dml_procedure = c("dml1", "dml2"),
    score = c("IV-type", "partialling out"),
    stringsAsFactors = FALSE)
}
test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for PLR:",
  .cases = test_cases, {
    n_folds = 2
    n_rep_boot = 498
    set.seed(3141)

    # load learner by name
    learner_name = "regr.cv_glmnet"
    params = list("s" = "lambda.min", "family" = "gaussian", "nfolds" = 5)

    set.seed(123)
    double_mlplr = DoubleMLPLR$new(
      data = data_plr$dml_data,
      ml_g = learner_name,
      ml_m = learner_name,
      dml_procedure = dml_procedure,
      n_folds = n_folds,
      score = score)

    # set params for nuisance part m
    double_mlplr$set_ml_nuisance_params(
      learner = "ml_m",
      treat_var = "d",
      params = params)

    # set params for nuisance part g
    double_mlplr$set_ml_nuisance_params(
      learner = "ml_g",
      treat_var = "d",
      params = params)

    double_mlplr$fit()
    theta = double_mlplr$coef
    se = double_mlplr$se
    t = double_mlplr$t_stat
    pval = double_mlplr$pval
    ci = double_mlplr$confint(level = 0.95, joint = FALSE)
    double_mlplr$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    boot_theta = double_mlplr$boot_coef

    set.seed(123)
    loaded_learner = mlr3::lrn("regr.cv_glmnet", "s" = "lambda.min", "family" = "gaussian", "nfolds" = 5)
    double_mlplr_loaded = DoubleMLPLR$new(
      data = data_plr$dml_data,
      ml_g = loaded_learner,
      ml_m = loaded_learner,
      dml_procedure = dml_procedure,
      n_folds = n_folds,
      score = score)

    double_mlplr_loaded$fit()
    theta_loaded = double_mlplr_loaded$coef
    se_loaded = double_mlplr_loaded$se
    t_loaded = double_mlplr_loaded$t_stat
    pval_loaded = double_mlplr_loaded$pval
    ci_loaded = double_mlplr_loaded$confint(level = 0.95, joint = FALSE)
    double_mlplr$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    boot_theta_loaded = double_mlplr$boot_coef

    set.seed(123)
    semiloaded_learner = mlr3::lrn("regr.cv_glmnet")
    double_mlplr_semiloaded = DoubleMLPLR$new(
      data = data_plr$dml_data,
      ml_g = semiloaded_learner,
      ml_m = semiloaded_learner,
      dml_procedure = dml_procedure,
      n_folds = n_folds,
      score = score)
    # set params for nuisance part m
    double_mlplr_semiloaded$set_ml_nuisance_params(
      learner = "ml_m",
      treat_var = "d",
      params = params)

    # set params for nuisance part g
    double_mlplr_semiloaded$set_ml_nuisance_params(
      learner = "ml_g",
      treat_var = "d",
      params = params)

    double_mlplr_semiloaded$fit()
    theta_semiloaded = double_mlplr_semiloaded$coef
    se_semiloaded = double_mlplr_semiloaded$se
    t_semiloaded = double_mlplr_semiloaded$t_stat
    pval_semiloaded = double_mlplr_semiloaded$pval
    ci_semiloaded = double_mlplr_semiloaded$confint(level = 0.95, joint = FALSE)
    double_mlplr$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    boot_theta_semiloaded = double_mlplr$boot_coef

    expect_equal(theta, theta_loaded, tolerance = 1e-8)
    expect_equal(se, se_loaded, tolerance = 1e-8)
    expect_equal(t, t_loaded, tolerance = 1e-8)
    expect_equal(pval, pval_loaded, tolerance = 1e-8)
    expect_equal(ci, ci_loaded, tolerance = 1e-8)
    expect_equal(as.vector(boot_theta), as.vector(boot_theta_loaded), tolerance = 1e-8)
    
    expect_equal(theta_semiloaded, theta_loaded, tolerance = 1e-8)
    expect_equal(se_semiloaded, se_loaded, tolerance = 1e-8)
    expect_equal(t_semiloaded, t_loaded, tolerance = 1e-8)
    expect_equal(pval_semiloaded, pval_loaded, tolerance = 1e-8)
    expect_equal(ci_semiloaded, ci_loaded, tolerance = 1e-8)
    expect_equal(as.vector(boot_theta_semiloaded), as.vector(boot_theta_loaded), tolerance = 1e-8)
  }
)
