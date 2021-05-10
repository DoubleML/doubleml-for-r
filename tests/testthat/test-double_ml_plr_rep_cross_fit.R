context("Unit tests for PLR with repeated cross-fitting")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.lm",
    dml_procedure = "dml1",
    score = "partialling out",
    i_setting = 1:(length(data_plr)),
    n_rep = c(5),
    stringsAsFactors = FALSE)
  test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")
} else {
  test_cases = expand.grid(
    learner = c("regr.lm", "regr.cv_glmnet"),
    dml_procedure = c("dml1", "dml2"),
    score = c("IV-type", "partialling out"),
    i_setting = 1:(length(data_plr)),
    n_rep = c(2, 5),
    stringsAsFactors = FALSE)
  test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")
}

patrick::with_parameters_test_that("Unit tests for PLR:",
  .cases = test_cases, {
    learner_pars = get_default_mlmethod_plr(learner)
    n_rep_boot = 498

    set.seed(i_setting)
    n_folds = 5
    plr_hat = dml_plr(data_plr[[i_setting]],
                      y = "y", d = "d",
                      n_folds = n_folds, n_rep = n_rep,
                      mlmethod = learner_pars$mlmethod,
                      params = learner_pars$params,
                      dml_procedure = dml_procedure, score = score)
    theta = plr_hat$coef
    se = plr_hat$se
    t = plr_hat$t
    pval = plr_hat$pval
    #ci = confint(plr_hat, level = 0.95, joint = FALSE)
    
    boot_theta = bootstrap_plr(plr_hat$thetas, plr_hat$ses,
                               data_plr[[i_setting]],
                               y = "y", d = "d",
                               n_folds = n_folds, n_rep = n_rep,
                               smpls = plr_hat$smpls,
                               all_preds= plr_hat$all_preds,
                               bootstrap = "normal", n_rep_boot = n_rep_boot,
                               score = score)$boot_coef

    set.seed(i_setting)
    Xnames = names(data_plr[[i_setting]])[names(data_plr[[i_setting]]) %in% c("y", "d", "z") == FALSE]
    data_ml = double_ml_data_from_data_frame(data_plr[[i_setting]],
      y_col = "y",
      d_cols = "d", x_cols = Xnames)

    double_mlplr_obj = DoubleMLPLR$new(data_ml,
      ml_g = learner_pars$mlmethod$mlmethod_g,
      ml_m = learner_pars$mlmethod$mlmethod_m,
      dml_procedure = dml_procedure,
      n_folds = n_folds,
      score = score,
      n_rep = n_rep)

    # set params for nuisance part m
    double_mlplr_obj$set_ml_nuisance_params(
      learner = "ml_m",
      treat_var = "d",
      params = learner_pars$params$params_m)

    # set params for nuisance part g
    double_mlplr_obj$set_ml_nuisance_params(
      learner = "ml_g",
      treat_var = "d",
      params = learner_pars$params$params_g)

    double_mlplr_obj$fit()
    theta_obj = double_mlplr_obj$coef
    se_obj = double_mlplr_obj$se
    t_obj = double_mlplr_obj$t_stat
    pval_obj = double_mlplr_obj$pval
    #ci_obj = double_mlplr_obj$confint(level = 0.95, joint = FALSE)

    # bootstrap
    double_mlplr_obj$bootstrap(method = "normal", n_rep_boot = n_rep_boot)
    boot_theta_obj = double_mlplr_obj$boot_coef
    
    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)
    expect_equal(t, t_obj, tolerance = 1e-8)
    expect_equal(pval, pval_obj, tolerance = 1e-8)
    #expect_equal(ci, ci_obj, tolerance = 1e-8)
    
    expect_equal(as.vector(boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)
