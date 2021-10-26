context("Unit tests for IIVM, callable score")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

score_fct = function(y, z, d, g0_hat, g1_hat, m_hat, r0_hat,
  r1_hat, smpls) {

  u0_hat = y - g0_hat
  u1_hat = y - g1_hat
  w0_hat = d - r0_hat
  w1_hat = d - r1_hat
  psi_b = g1_hat - g0_hat + z * (u1_hat) / m_hat -
    (1 - z) * u0_hat / (1 - m_hat)
  psi_a = -1 * (r1_hat - r0_hat + z * (w1_hat) / m_hat -
    (1 - z) * w0_hat / (1 - m_hat))

  psis = list(psi_a = psi_a, psi_b = psi_b)
  return(psis)
}

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.rpart",
    learner_m = "classif.rpart",
    dml_procedure = "dml2",
    trimming_threshold = c(0),
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = "regr.glmnet",
    learner_m = "classif.glmnet",
    dml_procedure = c("dml1", "dml2"),
    trimming_threshold = c(0),
    stringsAsFactors = FALSE)
}

test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for IIVM, callable score:",
  .cases = test_cases, {
    n_rep_boot = 498
    set.seed(3141)
    double_mliivm_obj = DoubleMLIIVM$new(
      data = data_iivm$dml_data,
      n_folds = 5,
      ml_m = lrn(learner_m),
      ml_g = lrn(learner),
      ml_r = lrn(learner_m),
      dml_procedure = dml_procedure,
      trimming_threshold = trimming_threshold,
      score = "LATE")
    double_mliivm_obj$fit()
    theta_obj = double_mliivm_obj$coef
    se_obj = double_mliivm_obj$se

    set.seed(3141)
    double_mliivm_obj_score = DoubleMLIIVM$new(
      data = data_iivm$dml_data,
      n_folds = 5,
      ml_m = lrn(learner_m),
      ml_g = lrn(learner),
      ml_r = lrn(learner_m),
      dml_procedure = dml_procedure,
      trimming_threshold = trimming_threshold,
      score = score_fct)
    double_mliivm_obj_score$fit()
    theta_obj_score = double_mliivm_obj_score$coef
    se_obj_score = double_mliivm_obj_score$se

    expect_equal(theta_obj, theta_obj_score, tolerance = 1e-8)
    expect_equal(se_obj, se_obj_score, tolerance = 1e-8)
  }
)
