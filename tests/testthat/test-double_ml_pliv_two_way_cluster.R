context("Unit tests for PLIV with two-way clustering")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.lm",
    dml_procedure = "dml1",
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

set.seed(1234)
N = 25 # number of observations (first dimension)
M = 25 # number of observations (second dimension)
dim_x = 100 # dimension of x
data_two_way = make_pliv_multiway_cluster_CKMS2021(N, M, dim_x)

patrick::with_parameters_test_that("Unit tests for PLIV with two-way clustering:",
  .cases = test_cases, {
    learner_pars = get_default_mlmethod_pliv(learner)

    set.seed(3141)
    if (score == "IV-type") {
      ml_g = learner_pars$ml_g$clone()
    } else {
      ml_g = NULL
    }
    double_mlpliv_obj = DoubleMLPLIV$new(
      data = data_two_way,
      n_folds = 2,
      ml_l = learner_pars$ml_l$clone(),
      ml_m = learner_pars$ml_m$clone(),
      ml_r = learner_pars$ml_r$clone(),
      ml_g = ml_g,
      dml_procedure = dml_procedure,
      score = score)

    set.seed(3141)
    double_mlpliv_obj$fit()
    theta_obj = double_mlpliv_obj$coef
    se_obj = double_mlpliv_obj$se

    set.seed(3141)
    df = as.data.frame(data_two_way$data)
    cluster_var1 = df$cluster_var_i
    cluster_var2 = df$cluster_var_j
    # need to drop variables as x is not explicitly set
    df = df[, !(names(df) %in% c("cluster_var_i", "cluster_var_j"))]
    if (score == "IV-type") {
      ml_g = learner_pars$ml_g$clone()
    } else {
      ml_g = NULL
    }
    pliv_hat = dml_pliv(df,
      y = "Y", d = "D", z = "Z",
      n_folds = 4,
      ml_l = learner_pars$ml_l$clone(),
      ml_m = learner_pars$ml_m$clone(),
      ml_r = learner_pars$ml_r$clone(),
      ml_g = ml_g,
      dml_procedure = dml_procedure, score = score,
      smpls = double_mlpliv_obj$smpls)


    this_smpl = double_mlpliv_obj$smpls[[1]]
    residuals = compute_pliv_residuals(df,
      y = "Y", d = "D", z = "Z",
      n_folds = 4,
      smpls = this_smpl,
      all_preds = pliv_hat$all_preds[[1]])
    y_minus_l_hat = residuals$y_minus_l_hat
    d_minus_r_hat = residuals$d_minus_r_hat
    z_minus_m_hat = residuals$z_minus_m_hat
    y_minus_g_hat = residuals$y_minus_g_hat
    D = df[, "D"]

    if (score == "partialling out") psi_a = -z_minus_m_hat * d_minus_r_hat
    if (score == "IV-type") psi_a = -D * z_minus_m_hat
    if (dml_procedure == "dml2") {
      if (score == "partialling out") psi_b = z_minus_m_hat * y_minus_l_hat
      if (score == "IV-type") psi_b = z_minus_m_hat * y_minus_g_hat
      theta = est_two_way_cluster_dml2(
        psi_a, psi_b,
        cluster_var1,
        cluster_var2,
        this_smpl)
    } else {
      theta = pliv_hat$coef
    }
    if (score == "partialling out") psi = (y_minus_l_hat - d_minus_r_hat * theta) * z_minus_m_hat
    if (score == "IV-type") psi = (y_minus_g_hat - D * theta) * z_minus_m_hat
    var = var_two_way_cluster(
      psi, psi_a,
      cluster_var1,
      cluster_var2,
      this_smpl)
    se = sqrt(var)
    names(theta) = "D"
    names(se) = "D"

    # at the moment the object result comes without a name
    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)
  }
)
