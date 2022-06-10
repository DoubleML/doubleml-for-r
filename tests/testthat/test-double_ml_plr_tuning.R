context("Unit tests for tuning of PLR")

requireNamespace("lgr")

logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")
lgr::get_logger("mlr3")$set_threshold("warn")

# settings for parameter provision
# learner = c('regr.rpart')
#
# learner_list = list("mlmethod_m" = learner, "mlmethod_g" = learner)

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.rpart",
    m_learner = "regr.rpart",
    dml_procedure = "dml2",
    score = "partialling out",
    n_rep = c(1),
    tune_on_folds = c(FALSE, TRUE),
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = "regr.rpart",
    m_learner = c("regr.rpart", "classif.rpart"),
    dml_procedure = c("dml1", "dml2"),
    score = c("IV-type", "partialling out"),
    n_rep = c(1, 3),
    tune_on_folds = c(FALSE, TRUE),
    stringsAsFactors = FALSE)
}


test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")

# skip('Skip tests for PLR tuning')
patrick::with_parameters_test_that("Unit tests for tuning of PLR:",
  .cases = test_cases, {
    n_rep_boot = 498
    n_folds = 4

    set.seed(3141)
    Xnames = names(data_plr_multi)[names(data_plr_multi) %in% c("y", "d1", "d2", "z") == FALSE]
    if (m_learner == "regr.rpart") {
      data_ml = double_ml_data_from_data_frame(data_plr_multi,
        y_col = "y",
        d_cols = c("d1", "d2"), x_cols = Xnames)

    } else if (m_learner == "classif.rpart") {
      data_plr_binary = data_plr_multi
      data_plr_binary$d1 = as.numeric(data_plr_binary$d1 > 0)
      data_plr_binary$d2 = as.numeric(data_plr_binary$d2 > 0)
      data_ml = double_ml_data_from_data_frame(data_plr_binary,
        y_col = "y",
        d_cols = c("d1", "d2"), x_cols = Xnames)
    }
    if (score == "IV-type") {
      ml_g = learner
    } else {
      ml_g = NULL
    }
    double_mlplr_obj_tuned = DoubleMLPLR$new(data_ml,
      n_folds = n_folds,
      ml_l = learner,
      ml_m = m_learner,
      ml_g = ml_g,
      dml_procedure = dml_procedure,
      score = score,
      n_rep = n_rep)

    tune_sets = list(
      n_folds_tune = 2,
      n_folds_tune = 1,
      rsmp_tune = "cv",
      terminator = mlr3tuning::trm("evals", n_evals = 2),
      algorithm = "grid_search",
      resolution = 5)

    param_grid = list(
      "ml_l" = paradox::ParamSet$new(list(
        paradox::ParamDbl$new("cp", lower = 0.02, upper = 0.03),
        paradox::ParamInt$new("minsplit", lower = 1, upper = 2))),
      "ml_m" = paradox::ParamSet$new(list(
        paradox::ParamDbl$new("cp", lower = 0.03, upper = 0.04),
        paradox::ParamInt$new("minsplit", lower = 2, upper = 3))))

    if (score == "IV-type") {
      param_grid[["ml_g"]] = paradox::ParamSet$new(list(
        paradox::ParamDbl$new("cp", lower = 0.015, upper = 0.025),
        paradox::ParamInt$new("minsplit", lower = 3, upper = 4)))
    }

    double_mlplr_obj_tuned$tune(param_set = param_grid, tune_on_folds = tune_on_folds, tune_settings = tune_sets)

    double_mlplr_obj_tuned$fit()

    theta_obj_tuned = double_mlplr_obj_tuned$coef
    se_obj_tuned = double_mlplr_obj_tuned$se

    # bootstrap
    # double_mlplr_obj_tuned$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    # boot_theta_obj_tuned = double_mlplr_obj_tuned$boot_coef

    # restrictions to test
    # Functional (tbd) vs OOP implementation (handling randomness in param selection!?)

    # Test case without including "other" treatment variables

    expect_is(theta_obj_tuned, "numeric")
    expect_is(se_obj_tuned, "numeric")
    #
    # data_ml$use_other_treat_as_covariate = FALSE
    # double_mlplr_obj_tuned$tune()
    # double_mlplr_obj_tuned$fit()
    # theta_obj_tuned = double_mlplr_obj_tuned$coef
    # se_obj_tuned = double_mlplr_obj_tuned$se
    #
    # expect_is(theta_obj_tuned, "numeric")
    # expect_is(se_obj_tuned, "numeric")
  }
)
