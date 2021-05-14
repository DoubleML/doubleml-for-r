context("Unit tests for PLR")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.cv_glmnet",
    dml_procedure = "dml1",
    score = "IV-type",
    n_folds = c(4),
    n_rep = c(3),
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = "regr.cv_glmnet",
    dml_procedure = "dml1",
    score = c("IV-type", "partialling out"),
    n_folds = c(4, 5),
    n_rep = c(1, 3),
    stringsAsFactors = FALSE)
}
test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for PLR:",
  .cases = test_cases, {
    learner = get_default_mlmethod_plr(learner)
    n_rep_boot = 498

    set.seed(3141)
    plr_hat = dml_plr(data_plr$df,
                      y = "y", d = "d",
                      n_folds = n_folds,
                      ml_g = learner$ml_g$clone(), ml_m = learner$ml_m$clone(),
                      dml_procedure = dml_procedure, score = score)
    theta = plr_hat$coef
    se = plr_hat$se
    t = plr_hat$t
    pval = plr_hat$pval
    #ci = confint(plr_hat, level = 0.95, joint = FALSE)
    
    boot_theta = bootstrap_plr(plr_hat$thetas, plr_hat$ses,
                               data_plr$df,
                               y = "y", d = "d",
                               n_folds = n_folds, smpls = plr_hat$smpls,
                               all_preds= plr_hat$all_preds,
                               bootstrap = "normal", n_rep_boot = n_rep_boot,
                               score = score)$boot_coef

    set.seed(3141)
    double_mlplr_obj = DoubleMLPLR$new(
      data = data_plr$dml_data,
      ml_g = learner$ml_g$clone(),
      ml_m = learner$ml_m$clone(),
      dml_procedure = dml_procedure,
      n_folds = n_folds,
      score = score)

    double_mlplr_obj$fit()
    theta_obj = double_mlplr_obj$coef
    se_obj = double_mlplr_obj$se
    t_obj = double_mlplr_obj$t_stat
    pval_obj = double_mlplr_obj$pval
    #ci_obj = double_mlplr_obj$confint(level = 0.95, joint = FALSE)
    
    # bootstrap
    double_mlplr_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    boot_theta_obj = double_mlplr_obj$boot_coef

    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)
    expect_equal(t, t_obj, tolerance = 1e-8)
    expect_equal(pval, pval_obj, tolerance = 1e-8)
    #expect_equal(ci, ci_obj, tolerance = 1e-8)
    
    expect_equal(as.vector(boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)
