context("Unit tests for PLIV, callable score")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

score_fct_po = function(y, z, d, l_hat, m_hat, r_hat, g_hat, smpls) {
  u_hat = y - l_hat
  w_hat = d - r_hat
  v_hat = z - m_hat
  psi_a = -w_hat * v_hat
  psi_b = v_hat * u_hat
  psis = list(
    psi_a = psi_a,
    psi_b = psi_b)
}

score_fct_iv = function(y, z, d, l_hat, m_hat, r_hat, g_hat, smpls) {
  v_hat = z - m_hat
  psi_a = -d * v_hat
  psi_b = v_hat * (y - g_hat)
  psis = list(
    psi_a = psi_a,
    psi_b = psi_b)
}

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.lm",
    dml_procedure = "dml2",
    score = "partialling out",
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = c("regr.lm", "regr.glmnet"),
    dml_procedure = c("dml1", "dml2"),
    score = c("partialling out", "IV-type"),
    stringsAsFactors = FALSE)
}
test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for PLIV, callable score:",
  .cases = test_cases, {
    n_rep_boot = 498

    if (score == "partialling out") {
      score_fct = score_fct_po
      ml_g = NULL
    } else if (score == "IV-type") {
      score_fct = score_fct_iv
      ml_g = lrn(learner)
    }

    set.seed(3141)
    double_mlpliv_obj = DoubleMLPLIV$new(
      data = data_pliv$dml_data,
      n_folds = 5,
      ml_l = lrn(learner),
      ml_m = lrn(learner),
      ml_r = lrn(learner),
      ml_g = ml_g,
      dml_procedure = dml_procedure,
      score = score)

    double_mlpliv_obj$fit()
    theta_obj = double_mlpliv_obj$coef
    se_obj = double_mlpliv_obj$se

    double_mlpliv_obj$bootstrap(method = "normal", n_rep = n_rep_boot)
    boot_theta_obj = double_mlpliv_obj$boot_coef

    set.seed(3141)
    double_mlpliv_obj_score = DoubleMLPLIV$new(
      data = data_pliv$dml_data,
      n_folds = 5,
      ml_l = lrn(learner),
      ml_m = lrn(learner),
      ml_r = lrn(learner),
      ml_g = ml_g,
      dml_procedure = dml_procedure,
      score = score_fct)

    double_mlpliv_obj_score$fit()
    theta_obj_score = double_mlpliv_obj_score$coef
    se_obj_score = double_mlpliv_obj_score$se

    double_mlpliv_obj_score$bootstrap(method = "normal", n_rep = n_rep_boot)
    boot_theta_score = double_mlpliv_obj_score$boot_coef

    expect_equal(theta_obj, theta_obj_score, tolerance = 1e-8)
    expect_equal(se_obj, se_obj_score, tolerance = 1e-8)
    expect_equal(as.vector(boot_theta_score), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)
