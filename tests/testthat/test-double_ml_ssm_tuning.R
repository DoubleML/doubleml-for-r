context("Unit tests for tuning of SSM")

requireNamespace("lgr")

logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")
lgr::get_logger("mlr3")$set_threshold("warn")

tune_settings = list(
  rsmp_tune = rsmp("cv", folds = 3),
  measure = list(
    "ml_m" = "classif.ce",
    "ml_g" = "regr.mse",
    "ml_pi" = "classif.ce"),
  terminator = mlr3tuning::trm("evals", n_evals = 5),
  algorithm = tnr("random_search"))

learner = "rpart"


on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner_list = learner,
    dml_procedure = "dml2",
    score = c("missing-at-random", "nonignorable"),
    n_rep = c(1),
    tune_on_folds = FALSE,
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner_list = learner,
    dml_procedure = c("dml1", "dml2"),
    score = c("missing-at-random", "nonignorable"),
    n_rep = c(1, 3),
    tune_on_folds = c(FALSE, TRUE),
    stringsAsFactors = FALSE)
}

test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for tuning of SSM:",
  .cases = test_cases, {
    n_rep_boot = 498
    n_folds = 5

    if (score == "missing-at-random") {
      dml_data = data_ssm_mar$dml_data
    } else {
      dml_data = data_ssm_nonignorable$dml_data
    }

    set.seed(3141)
    double_mlssm_obj_tuned = DoubleMLSSM$new(
      data = dml_data,
      n_folds = n_folds,
      ml_g = lrn("regr.rpart"),
      ml_m = lrn("classif.rpart"),
      ml_pi = lrn("classif.rpart"),
      dml_procedure = dml_procedure,
      score = score,
      n_rep = n_rep)

    param_grid = list(
      "ml_m" = paradox::ps(
        cp = paradox::p_dbl(lower = 0.01, upper = 0.02),
        minsplit = paradox::p_int(lower = 1, upper = 2)),
      "ml_g" = paradox::ps(
        cp = paradox::p_dbl(lower = 0.01, upper = 0.02),
        minsplit = paradox::p_int(lower = 1, upper = 2)),
      "ml_pi" = paradox::ps(
        cp = paradox::p_dbl(lower = 0.01, upper = 0.02),
        minsplit = paradox::p_int(lower = 1, upper = 2)))

    double_mlssm_obj_tuned$tune(param_set = param_grid, tune_on_folds = tune_on_folds, tune_settings = tune_settings)

    # skip if tune_on_folds = TRUE & score == "nonignorable"
    if (tune_on_folds && score == "nonignorable") {
      skip("Skipping test for tuning on folds with nonignorable score")
    } else {
      double_mlssm_obj_tuned$fit()

      theta_obj_tuned = double_mlssm_obj_tuned$coef
      se_obj_tuned = double_mlssm_obj_tuned$se

      expect_is(theta_obj_tuned, "numeric")
      expect_is(se_obj_tuned, "numeric")
    }
  }
)
