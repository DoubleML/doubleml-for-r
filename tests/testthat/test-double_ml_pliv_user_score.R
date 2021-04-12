context("Unit tests for PLIV, callable score")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

score_fct = function(y, z, d, g_hat, m_hat, r_hat, smpls) {
  u_hat = y - g_hat
  w_hat = d - r_hat
  v_hat = z - m_hat
  psi_a = -w_hat * v_hat
  psi_b = v_hat * u_hat
  psis = list(
    psi_a = psi_a,
    psi_b = psi_b)
}

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.glmnet",
    dml_procedure = "dml2",
    i_setting = 1:(length(data_pliv)),
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = c("regr.lm", "regr.glmnet"),
    dml_procedure = c("dml1", "dml2"),
    i_setting = 1:(length(data_pliv)),
    stringsAsFactors = FALSE)
}
test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for PLIV, callable score:",
  .cases = test_cases, {
    n_rep_boot = 498

    set.seed(i_setting)
    Xnames = names(data_pliv[[i_setting]])[names(data_pliv[[i_setting]]) %in% c("y", "d", "z") == FALSE]
    data_ml = double_ml_data_from_data_frame(data_pliv[[i_setting]],
      y_col = "y",
      d_cols = "d", x_cols = Xnames, z_cols = "z")

    double_mlpliv_obj = DoubleMLPLIV$new(data_ml,
      n_folds = 5,
      ml_g = lrn(learner),
      ml_m = lrn(learner),
      ml_r = lrn(learner),
      dml_procedure = dml_procedure,
      score = "partialling out")

    double_mlpliv_obj$fit()
    theta_obj = double_mlpliv_obj$coef
    se_obj = double_mlpliv_obj$se

    set.seed(i_setting)
    double_mlpliv_obj_score = DoubleMLPLIV$new(data_ml,
      n_folds = 5,
      ml_g = lrn(learner),
      ml_m = lrn(learner),
      ml_r = lrn(learner),
      dml_procedure = dml_procedure,
      score = score_fct)

    double_mlpliv_obj_score$fit()
    theta_obj_score = double_mlpliv_obj_score$coef
    se_obj_score = double_mlpliv_obj_score$se

    # bootstrap
    # double_mlpliv_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    # boot_theta_obj = double_mlpliv_obj$boot_coef

    # at the moment the object result comes without a name
    expect_equal(theta_obj, theta_obj_score, tolerance = 1e-8)
    expect_equal(se_obj, se_obj_score, tolerance = 1e-8)
    # expect_equal(as.vector(pliv_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)
