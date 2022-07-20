context("Unit tests for PLR, callable score")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

score_fct = function(y, d, l_hat, m_hat, g_hat, smpls) {
  v_hat = d - m_hat
  u_hat = y - l_hat
  v_hatd = v_hat * d
  psi_a = -v_hat * v_hat
  psi_b = v_hat * u_hat
  psis = list(psi_a = psi_a, psi_b = psi_b)
  return(psis)
}

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.lm",
    dml_procedure = "dml1",
    n_folds = c(3),
    n_rep = c(2),
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = c("regr.lm", "regr.glmnet"),
    dml_procedure = c("dml1", "dml2"),
    n_folds = c(2, 3),
    n_rep = c(1, 2),
    stringsAsFactors = FALSE)
}
test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for PLR, callable score:",
  .cases = test_cases, {
    n_rep_boot = 498
    set.seed(3141)

    double_mlplr_obj = DoubleMLPLR$new(
      data = data_plr$dml_data,
      ml_l = lrn(learner),
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

    set.seed(3141)
    double_mlplr_obj_score = DoubleMLPLR$new(
      data = data_plr$dml_data,
      ml_l = lrn(learner),
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
  }
)
