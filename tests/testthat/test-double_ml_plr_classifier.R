context("Unit tests for PLR with a classifier for ml_m")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    g_learner = c("regr.cv_glmnet", "classif.cv_glmnet"),
    m_learner = "classif.cv_glmnet",
    dml_procedure = "dml2",
    score = "partialling out",
    i_setting = 1:(length(data_plr)),
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    g_learner = "regr.cv_glmnet",
    m_learner = "classif.cv_glmnet",
    dml_procedure = c("dml1", "dml2"),
    score = c("IV-type", "partialling out"),
    i_setting = 1:(length(data_plr)),
    stringsAsFactors = FALSE)
}
test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for PLR with classifier for ml_m:",
  .cases = test_cases, {
    n_rep_boot = 498
    n_folds = 3

    if (g_learner == "regr.cv_glmnet") {
      ml_g = mlr3::lrn(g_learner)
      ml_m = mlr3::lrn(m_learner)
      
      set.seed(i_setting)
      plr_hat = dml_plr(data_irm[[i_setting]],
                        y = "y", d = "d",
                        n_folds = n_folds,
                        ml_g = ml_g$clone(), ml_m = ml_m$clone(),
                        dml_procedure = dml_procedure, score = score)
      theta = plr_hat$coef
      se = plr_hat$se
      
      boot_theta = bootstrap_plr(plr_hat$thetas, plr_hat$ses,
                                 data_irm[[i_setting]],
                                 y = "y", d = "d",
                                 n_folds = n_folds, smpls = plr_hat$smpls,
                                 all_preds= plr_hat$all_preds,
                                 bootstrap = "normal", n_rep_boot = n_rep_boot,
                                 score = score)$boot_coef
      
      t = plr_hat$t
      pval = plr_hat$pval
      
      set.seed(i_setting)
      Xnames = names(data_irm[[i_setting]])[names(data_irm[[i_setting]]) %in% c("y", "d", "z") == FALSE]
      data_ml = double_ml_data_from_data_frame(data_irm[[i_setting]],
        y_col = "y",
        d_cols = "d", x_cols = Xnames)

      double_mlplr_obj = DoubleMLPLR$new(
        data = data_ml,
        ml_g = ml_g$clone(),
        ml_m = ml_m$clone(),
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

    } else if (g_learner == "classif.cv_glmnet") {
      expect_error(DoubleMLPLR$new(
        data = data_ml,
        ml_g = lrn(g_learner),
        ml_m = lrn(m_learner),
        dml_procedure = dml_procedure,
        n_folds = n_folds,
        score = score))
    }
  }
)
