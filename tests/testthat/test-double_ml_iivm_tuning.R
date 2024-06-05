context("Unit tests for tuning of IIVM")

requireNamespace("lgr")

logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")
lgr::get_logger("mlr3")$set_threshold("warn")

tune_settings = list(
  rsmp_tune = rsmp("cv", folds = 3),
  measure = list(
    "ml_m" = "classif.ce",
    "ml_g" = "regr.mse",
    "ml_r" = "classif.ce"),
  terminator = mlr3tuning::trm("evals", n_evals = 5),
  algorithm = tnr("random_search"))

learner = "rpart"

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner_list = learner,
    dml_procedure = "dml2",
    score = "LATE",
    AT = c(TRUE),
    NT = c(TRUE),
    n_rep = c(1),
    tune_on_folds = FALSE,
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner_list = learner,
    dml_procedure = c("dml1", "dml2"),
    score = "LATE",
    AT = c(TRUE, FALSE),
    NT = c(TRUE, FALSE),
    n_rep = c(1, 3),
    tune_on_folds = c(FALSE, TRUE),
    stringsAsFactors = FALSE)
}


test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for tuning of IIVM:",
  .cases = test_cases, {
    n_rep_boot = 498
    n_folds = 2

    set.seed(3141)
    double_mliivm_obj_tuned = DoubleMLIIVM$new(
      data = data_iivm$dml_data,
      n_folds = n_folds,
      ml_g = "regr.rpart",
      ml_m = "classif.rpart",
      ml_r = "classif.rpart",
      subgroups = list(
        always_takers = AT,
        never_takers = NT),
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
      "ml_r" = paradox::ps(
        cp = paradox::p_dbl(lower = 0.01, upper = 0.02),
        minsplit = paradox::p_int(lower = 1, upper = 2)))

    double_mliivm_obj_tuned$tune(param_set = param_grid, tune_on_folds = tune_on_folds, tune_settings = tune_settings)
    double_mliivm_obj_tuned$fit()

    theta_obj_tuned = double_mliivm_obj_tuned$coef
    se_obj_tuned = double_mliivm_obj_tuned$se

    # bootstrap
    # double_mlirm_obj_tuned$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    # boot_theta_obj_tuned = double_mlirm_obj_tuned$boot_coef


    # restrictions to test
    # Functional (tbd) vs OOP implementation (handling randomness in param selection!?)
    expect_is(theta_obj_tuned, "numeric")
    expect_is(se_obj_tuned, "numeric")
  }
)
