context("Unit tests for PLR with a classifier for ml_m")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    l_learner = c("regr.rpart", "classif.rpart"),
    m_learner = "classif.rpart",
    g_learner = "regr.rpart",
    dml_procedure = "dml2",
    score = "partialling out",
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    l_learner = c("regr.rpart", "classif.rpart"),
    m_learner = "classif.cv_glmnet",
    g_learner = "regr.cv_glmnet",
    dml_procedure = c("dml1", "dml2"),
    score = c("IV-type", "partialling out"),
    stringsAsFactors = FALSE)
}
test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for PLR with classifier for ml_m:",
  .cases = test_cases, {
    n_rep_boot = 498
    n_folds = 3

    ml_l = mlr3::lrn(l_learner)
    ml_m = mlr3::lrn(m_learner)
    ml_g = mlr3::lrn(g_learner)

    if (ml_l$task_type == "regr") {

      set.seed(3141)
      if (score == "IV-type") {
        ml_g = ml_g$clone()
      } else {
        ml_g = NULL
      }
      plr_hat = dml_plr(data_irm$df,
        y = "y", d = "d",
        n_folds = n_folds,
        ml_l = ml_l$clone(),
        ml_m = ml_m$clone(),
        ml_g = ml_g,
        dml_procedure = dml_procedure, score = score)
      theta = plr_hat$coef
      se = plr_hat$se

      boot_theta = bootstrap_plr(plr_hat$thetas, plr_hat$ses,
        data_irm$df,
        y = "y", d = "d",
        n_folds = n_folds, smpls = plr_hat$smpls,
        all_preds = plr_hat$all_preds,
        bootstrap = "normal", n_rep_boot = n_rep_boot,
        score = score)$boot_coef

      t = plr_hat$t
      pval = plr_hat$pval

      set.seed(3141)
      if (score == "IV-type") {
        ml_g = ml_g$clone()
      } else {
        ml_g = NULL
      }
      double_mlplr_obj = DoubleMLPLR$new(
        data = data_irm$dml_data,
        ml_l = ml_l$clone(),
        ml_m = ml_m$clone(),
        ml_g = ml_g,
        dml_procedure = dml_procedure,
        n_folds = n_folds,
        score = score)
      double_mlplr_obj$fit()
      theta_obj = double_mlplr_obj$coef
      se_obj = double_mlplr_obj$se
      t_obj = double_mlplr_obj$t_stat
      pval_obj = double_mlplr_obj$pval
      # ci_obj = double_mlplr_obj$confint(level = 0.95, joint = FALSE)

      # bootstrap
      double_mlplr_obj$bootstrap(method = "normal", n_rep = n_rep_boot)
      boot_theta_obj = double_mlplr_obj$boot_coef

      expect_equal(theta, theta_obj, tolerance = 1e-8)
      expect_equal(se, se_obj, tolerance = 1e-8)
      expect_equal(t, t_obj, tolerance = 1e-8)
      expect_equal(pval, pval_obj, tolerance = 1e-8)
      # expect_equal(ci, ci_obj, tolerance = 1e-8)

    } else if (ml_l$task_type == "classif") {
      msg = "Invalid learner provided for ml_l: 'learner\\$task_type' must be 'regr'"
      if (score == "IV-type") {
        ml_g = ml_g$clone()
      } else {
        ml_g = NULL
      }
      expect_error(DoubleMLPLR$new(
        data = data_irm$dml_data,
        ml_l = ml_l$clone(),
        ml_m = ml_m$clone(),
        ml_g = ml_g,
        dml_procedure = dml_procedure,
        n_folds = n_folds,
        score = score),
      regexp = msg)
    }
  }
)

test_that("Unit tests for exception handling of PLR with classifier for ml_m:", {
  # Only binary outcome with values 0 and 1 is allowed when ml_m is a classifier

  # Test with 0 and 2
  df = data_irm$df
  df["d"] = df["d"] * 2
  dml_data = double_ml_data_from_data_frame(df, y_col = "y", d_cols = "d")
  double_mlplr_obj = DoubleMLPLR$new(
    data = dml_data,
    ml_l = mlr3::lrn("regr.rpart"),
    ml_m = mlr3::lrn("classif.rpart"))
  msg = paste(
    "Assertion on 'levels\\(data\\[\\[target\\]\\])' failed: .* set \\{'0','1'\\}")
  expect_error(double_mlplr_obj$fit(),
    regexp = msg)

  # Test with 0.5 and 1
  df = data_irm$df
  df["d"] = (df["d"] + 2) / 2
  dml_data = double_ml_data_from_data_frame(df, y_col = "y", d_cols = "d")
  double_mlplr_obj = DoubleMLPLR$new(
    data = dml_data,
    ml_l = mlr3::lrn("regr.rpart"),
    ml_m = mlr3::lrn("classif.rpart"))
  msg = paste(
    "Assertion on 'levels\\(data\\[\\[target\\]\\])' failed: .* set \\{'0','1'\\}")
  expect_error(double_mlplr_obj$fit(),
    regexp = msg)
}
)
