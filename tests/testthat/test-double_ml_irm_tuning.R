context("Unit tests for tuning of IRM")

requireNamespace("lgr")

logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")
lgr::get_logger("mlr3")$set_threshold("warn")

# settings for parameter provision
learner = "rpart"

learner_list = list("mlmethod_m" = learner, "mlmethod_g" = learner)

# tune_settings = list(n_folds_tune = 3,
#                       n_rep_tune = 1,
#                       rsmp_tune = "cv",
#                       measure = list("ml_g" = "regr.mse",
#                                      "ml_m" = "classif.ce"),
#                       terminator = mlr3tuning::trm("evals", n_evals = 5),
#                       algorithm = "grid_search",
#                       tuner = "grid_search",
#                       resolution = 5)

# only minimum amount of input for tuning
tune_settings = list(
  terminator = mlr3tuning::trm("evals", n_evals = 5),
  resolution = 5)

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = learner,
    dml_procedure = "dml2",
    score = "ATE",
    tune_on_folds = FALSE,
    n_rep = c(1),
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = learner,
    dml_procedure = c("dml1", "dml2"),
    score = c("ATE", "ATTE"),
    tune_on_folds = c(FALSE, TRUE),
    n_rep = c(1, 3),
    stringsAsFactors = FALSE)
}

test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")

# skip('Skip tests for tuning')
patrick::with_parameters_test_that("Unit tests for tuning of PLR:",
  .cases = test_cases, {
    n_rep_boot = 498
    n_folds = 2

    # TODO: Functional Test Case
    set.seed(3141)
    learner_pars = get_default_mlmethod_irm(learner)

    double_mlirm_obj_tuned = DoubleMLIRM$new(
      data = data_irm$dml_data,
      n_folds = n_folds,
      ml_g = learner_pars$mlmethod$mlmethod_g,
      ml_m = learner_pars$mlmethod$mlmethod_m,
      dml_procedure = dml_procedure,
      score = score)

    param_grid = list(
      "ml_g" = paradox::ParamSet$new(list(
        paradox::ParamDbl$new("cp", lower = 0.01, upper = 0.02),
        paradox::ParamInt$new("minsplit", lower = 1, upper = 2))),
      "ml_m" = paradox::ParamSet$new(list(
        paradox::ParamDbl$new("cp", lower = 0.01, upper = 0.02),
        paradox::ParamInt$new("minsplit", lower = 1, upper = 2))))

    double_mlirm_obj_tuned$tune(param_set = param_grid, tune_on_folds = tune_on_folds, tune_settings)
    double_mlirm_obj_tuned$fit()

    theta_obj_tuned = double_mlirm_obj_tuned$coef
    se_obj_tuned = double_mlirm_obj_tuned$se

    # TODO: bootstrap
    # double_mlirm_obj_tuned$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    # boot_theta_obj_tuned = double_mlirm_obj_tuned$boot_coef


    # restrictions to test
    # Functional (tbd) vs OOP implementation (handling randomness in param selection!?)
    expect_is(theta_obj_tuned, "numeric")
    expect_is(se_obj_tuned, "numeric")

    # loaded learner
    loaded_regr_learner = mlr3::lrn("regr.rpart", "cp" = 0.1, "minsplit" = 20)
    loaded_classif_learner = mlr3::lrn("classif.rpart", "cp" = 0.1, "minsplit" = 20)
    double_mlirm_obj_loaded_tuned = DoubleMLIRM$new(
      data = data_irm$dml_data,
      n_folds = n_folds,
      ml_g = loaded_regr_learner,
      ml_m = loaded_classif_learner,
      dml_procedure = dml_procedure,
      score = score)
    double_mlirm_obj_loaded_tuned$tune(param_set = param_grid, tune_on_folds = tune_on_folds, tune_settings)
    double_mlirm_obj_loaded_tuned$fit()

    theta_obj_loaded_tuned = double_mlirm_obj_loaded_tuned$coef
    se_obj_loaded_tuned = double_mlirm_obj_loaded_tuned$se

    # TODO: bootstrap
    # double_mlirm_obj_loaded_tuned$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    # boot_theta_obj_loaded_tuned = double_mlirm_obj_loaded_tuned$boot_coef


    # restrictions to test
    # Functional (tbd) vs OOP implementation (handling randomness in param selection!?)
    expect_is(theta_obj_loaded_tuned, "numeric")
    expect_is(se_obj_loaded_tuned, "numeric")
  }
)
