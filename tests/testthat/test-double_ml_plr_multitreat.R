context("Unit tests for PLR (mulitple treatment case)")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.lm",
    dml_procedure = "dml2",
    score = "partialling out",
    i_setting = 1:(length(data_plr)),
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = c("regr.lm", "regr.cv_glmnet"),
    dml_procedure = c("dml1", "dml2"),
    score = c("IV-type", "partialling out"),
    i_setting = 1:(length(data_plr)),
    stringsAsFactors = FALSE)
}

test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for PLR:",
  .cases = test_cases, {
    learner_pars = get_default_mlmethod_plr(learner)

    learner_pars_for_DML = learner_pars
    learner_pars_for_DML$params$params_g = rep(list(learner_pars_for_DML$params$params_g), 3)
    learner_pars_for_DML$params$params_m = rep(list(learner_pars_for_DML$params$params_m), 3)
    n_rep_boot = 498

    n_folds = 5

    set.seed(i_setting)
    plr_hat = dml_plr_multitreat(data_plr_multi[[i_setting]],
                                 y = "y", d = c("d1", "d2", "d3"),
                                 n_folds = n_folds,
                                 mlmethod = learner_pars$mlmethod,
                                 params = learner_pars_for_DML$params,
                                 dml_procedure = dml_procedure, score = score)
    theta = plr_hat$coef
    se = plr_hat$se
    t = plr_hat$t
    pval = plr_hat$pval
    #ci_ptwise = confint(plr_hat, joint = FALSE, level = 0.95)

    set.seed(i_setting)
    boot_theta = boot_plr_multitreat(plr_hat$thetas, plr_hat$ses,
                                     data_plr_multi[[i_setting]],
                                     y = "y", d = c("d1", "d2", "d3"),
                                     n_folds = n_folds, smpls = plr_hat$smpls,
                                     all_preds= plr_hat$all_preds,
                                     bootstrap = "normal", n_rep_boot = n_rep_boot,
                                     score = score)$boot_coef
    
    set.seed(i_setting)
    Xnames = names(data_plr_multi[[i_setting]])[names(data_plr_multi[[i_setting]]) %in% c("y", "d1", "d2", "d3", "z") == FALSE]
    data_ml = double_ml_data_from_data_frame(data_plr_multi[[i_setting]],
      y_col = "y",
      d_cols = c("d1", "d2", "d3"), x_cols = Xnames)

    if (learner == "regr.lm") {
      DoubleML_learner = lrn(learner)
    } else if (learner == "regr.cv_glmnet") {
      DoubleML_learner = lrn(learner, s = "lambda.min", family = "gaussian")
    }

    double_mlplr_obj = DoubleMLPLR$new(data_ml,
      ml_g = DoubleML_learner$clone(),
      ml_m = DoubleML_learner$clone(),
      dml_procedure = dml_procedure,
      n_folds = n_folds,
      score = score)

    double_mlplr_obj$fit()
    theta_obj = double_mlplr_obj$coef
    se_obj = double_mlplr_obj$se
    t_obj = double_mlplr_obj$t_stat
    pval_obj = double_mlplr_obj$pval

    # bootstrap
    set.seed(i_setting)
    double_mlplr_obj$bootstrap(method = "normal", n_rep_boot = n_rep_boot)
    boot_theta_obj = double_mlplr_obj$boot_coef

    # joint confint
    ci_ptwise_obj = double_mlplr_obj$confint(joint = FALSE, level = 0.95)
    ci_joint_obj = double_mlplr_obj$confint(joint = TRUE, level = 0.95)

    # at the moment the object result comes without a name
    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)
    expect_equal(t, t_obj, tolerance = 1e-8)
    expect_equal(pval, pval_obj, tolerance = 1e-8)

    expect_equal(as.vector(boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
    #expect_equal(ci_ptwise, ci_ptwise_obj)
    #expect_equal(ci_joint, ci_joint_obj)
  }
)
