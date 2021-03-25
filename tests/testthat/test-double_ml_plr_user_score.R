context("Unit tests for PLR with user provided score")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.glmnet",
    dml_procedure = "dml2",
    i_setting = 1:(length(data_plr)),
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = c("regr.lm", "regr.glmnet"),
    dml_procedure = c("dml1", "dml2"),
    i_setting = 1:(length(data_plr)),
    stringsAsFactors = FALSE)
}
test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for PLR:",
  .cases = test_cases, {
    n_folds = 3
    set.seed(i_setting)
    Xnames = names(data_plr[[i_setting]])[names(data_plr[[i_setting]]) %in% c("y", "d", "z") == FALSE]
    data_ml = double_ml_data_from_data_frame(data_plr[[i_setting]],
      y_col = "y",
      d_cols = "d", x_cols = Xnames)

    double_mlplr_obj = DoubleMLPLR$new(
      data = data_ml,
      ml_g = lrn(learner),
      ml_m = lrn(learner),
      dml_procedure = dml_procedure,
      n_folds = n_folds,
      score = "partialling out")

    double_mlplr_obj$fit()
    theta_obj = double_mlplr_obj$coef
    se_obj = double_mlplr_obj$se
    t_obj = double_mlplr_obj$t_stat
    pval_obj = double_mlplr_obj$pval
    ci_obj = double_mlplr_obj$confint(level = 0.95, joint = FALSE)
    
    score_fct = function(y, d, g_hat, m_hat, smpls) {
      v_hat = d - m_hat
      u_hat = y - g_hat
      v_hatd = v_hat * d
      psi_a = -v_hat * v_hat
      psi_b = v_hat * u_hat
      psis = list(
          psi_a = psi_a,
          psi_b = psi_b)
      return(psis)
    }
    
    set.seed(i_setting)
    double_mlplr_obj_score = DoubleMLPLR$new(
      data = data_ml,
      ml_g = lrn(learner),
      ml_m = lrn(learner),
      dml_procedure = dml_procedure,
      n_folds = n_folds,
      score = score_fct)

    double_mlplr_obj_score$fit()
    theta_obj_score = double_mlplr_obj_score$coef
    se_obj_score = double_mlplr_obj_score$se
    t_obj_score = double_mlplr_obj_score$t_stat
    pval_obj_score = double_mlplr_obj_score$pval
    ci_obj_score = double_mlplr_obj_score$confint(level = 0.95, joint = FALSE)

    expect_equal(theta_obj_score, theta_obj, tolerance = 1e-8)
    expect_equal(se_obj_score, se_obj, tolerance = 1e-8)
    expect_equal(t_obj_score, t_obj, tolerance = 1e-8)
    expect_equal(pval_obj_score, pval_obj, tolerance = 1e-8)
    expect_equal(ci_obj_score, ci_obj, tolerance = 1e-8)

    # expect_equal(as.vector(plr_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)
