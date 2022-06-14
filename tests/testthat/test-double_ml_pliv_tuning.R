context("Unit tests for tuning for PLIV")

requireNamespace("lgr")

logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")
lgr::get_logger("mlr3")$set_threshold("warn")


# settings for parameter provision
learner = "regr.rpart"

tune_settings = list(
  n_folds_tune = 2,
  n_rep_tune = 1,
  rsmp_tune = "cv",
  measure = list(
    "ml_l" = "regr.mse",
    "ml_r" = "regr.mse",
    "ml_m" = "regr.mse"),
  terminator = mlr3tuning::trm("evals", n_evals = 2),
  algorithm = "grid_search",
  tuning_instance_l = NULL,
  tuning_instance_m = NULL,
  tuner = "grid_search",
  resolution = 5)

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases_one_z = expand.grid(
    dml_procedure = "dml2",
    score = "partialling out",
    n_rep = c(1),
    tune_on_folds = FALSE,
    stringsAsFactors = FALSE)
} else {
  test_cases_one_z = expand.grid(
    dml_procedure = c("dml1", "dml2"),
    score = c("partialling out", "IV-type"),
    n_rep = c(1, 3),
    tune_on_folds = c(FALSE, TRUE),
    stringsAsFactors = FALSE)
}

test_cases_one_z[".test_name"] = apply(test_cases_one_z, 1, paste, collapse = "_")

# skip('Skip tests for tuning')

patrick::with_parameters_test_that("Unit tests for tuning of PLIV",
  .cases = test_cases_one_z, {

    # TBD: Functional Test Case

    set.seed(3141)
    n_folds = 2
    n_rep_boot = 498

    z_cols = "z"
    set.seed(3141)
    df = data_pliv$df
    Xnames = names(df)[names(df) %in% c("y", "d", "z", "z2") == FALSE]
    data_ml = double_ml_data_from_data_frame(df,
      y_col = "y",
      d_cols = "d", x_cols = Xnames, z_cols = z_cols)

    if (score == "IV-type") {
      ml_g = learner
    } else {
      ml_g = NULL
    }
    double_mlpliv_obj_tuned = DoubleMLPLIV$new(data_ml,
      n_folds = n_folds,
      ml_l = learner,
      ml_m = learner,
      ml_r = learner,
      ml_g = ml_g,
      dml_procedure = dml_procedure,
      score = score,
      n_rep = n_rep)

    param_grid = list(
      "ml_l" = paradox::ParamSet$new(list(
        paradox::ParamDbl$new("cp", lower = 0.01, upper = 0.02),
        paradox::ParamInt$new("minsplit", lower = 1, upper = 2))),
      "ml_m" = paradox::ParamSet$new(list(
        paradox::ParamDbl$new("cp", lower = 0.01, upper = 0.02),
        paradox::ParamInt$new("minsplit", lower = 1, upper = 2))),
      "ml_r" = paradox::ParamSet$new(list(
        paradox::ParamDbl$new("cp", lower = 0.01, upper = 0.02),
        paradox::ParamInt$new("minsplit", lower = 1, upper = 2))))
    if (score == "IV-type") {
      param_grid[["ml_g"]] = paradox::ParamSet$new(list(
        paradox::ParamDbl$new("cp", lower = 0.01, upper = 0.02),
        paradox::ParamInt$new("minsplit", lower = 1, upper = 2)))
      tune_settings[["measure"]][["ml_g"]] = "regr.mse"
    }

    double_mlpliv_obj_tuned$tune(param_set = param_grid, tune_settings = tune_settings, tune_on_folds = tune_on_folds)
    double_mlpliv_obj_tuned$fit()

    theta_obj_tuned = double_mlpliv_obj_tuned$coef
    se_obj_tuned = double_mlpliv_obj_tuned$se

    # bootstrap
    # double_mlplr_obj_exact$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    # boot_theta_obj_exact = double_mlplr_obj_exact$boot_coef

    expect_is(theta_obj_tuned, "numeric")
    expect_is(se_obj_tuned, "numeric")

    # if (data_ml$n_instr() == 1) {
    #   double_mlpliv_obj_tuned_Z = DoubleMLPLIV.partialZ(data_ml,
    #                                      n_folds = n_folds,
    #                                      ml_r = learner,
    #                                      dml_procedure = dml_procedure,
    #                                      score = score,
    #                                      n_rep = n_rep)
    #
    #   double_mlpliv_obj_tuned_Z$tune(param_set = param_grid, tune_on_folds = tune_on_folds)
    #   double_mlpliv_obj_tuned_Z$fit()
    #
    #   theta_obj_tuned_Z = double_mlpliv_obj_tuned_Z$coef
    #   se_obj_tuned_Z = double_mlpliv_obj_tuned_Z$se
    #
    #   expect_is(theta_obj_tuned_Z, "numeric")
    #   expect_is(se_obj_tuned_Z, "numeric")
    # }
    #
  }
)

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases_multiple_z = expand.grid(
    dml_procedure = "dml2",
    score = "partialling out",
    n_rep = c(1),
    tune_on_folds = FALSE,
    stringsAsFactors = FALSE)
} else {
  test_cases_multiple_z = expand.grid(
    dml_procedure = c("dml1", "dml2"),
    score = "partialling out",
    n_rep = c(1, 3),
    tune_on_folds = c(FALSE, TRUE),
    stringsAsFactors = FALSE)
}

test_cases_multiple_z[".test_name"] = apply(test_cases_multiple_z, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for tuning of PLIV (multiple Z)",
  .cases = test_cases_multiple_z, {

    # TBD: Functional Test Case

    set.seed(3141)
    n_folds = 2
    n_rep_boot = 498

    z_cols = c("z", "z2")
    set.seed(3141)
    df = data_pliv$df
    Xnames = names(df)[names(df) %in% c("y", "d", "z", "z2") == FALSE]
    data_ml = double_ml_data_from_data_frame(df,
      y_col = "y",
      d_cols = "d", x_cols = Xnames, z_cols = z_cols)

    double_mlpliv_obj_tuned = DoubleMLPLIV$new(data_ml,
      n_folds = n_folds,
      ml_l = learner,
      ml_m = learner,
      ml_r = learner,
      dml_procedure = dml_procedure,
      score = score,
      n_rep = n_rep)

    param_grid = list(
      "ml_l" = paradox::ParamSet$new(list(
        paradox::ParamDbl$new("cp", lower = 0.01, upper = 0.02),
        paradox::ParamInt$new("minsplit", lower = 1, upper = 2))),
      "ml_m" = paradox::ParamSet$new(list(
        paradox::ParamDbl$new("cp", lower = 0.01, upper = 0.02),
        paradox::ParamInt$new("minsplit", lower = 1, upper = 2))),
      "ml_r" = paradox::ParamSet$new(list(
        paradox::ParamDbl$new("cp", lower = 0.01, upper = 0.02),
        paradox::ParamInt$new("minsplit", lower = 1, upper = 2))))

    double_mlpliv_obj_tuned$tune(param_set = param_grid, tune_settings = tune_settings, tune_on_folds = tune_on_folds)
    double_mlpliv_obj_tuned$fit()

    theta_obj_tuned = double_mlpliv_obj_tuned$coef
    se_obj_tuned = double_mlpliv_obj_tuned$se

    # bootstrap
    # double_mlplr_obj_exact$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    # boot_theta_obj_exact = double_mlplr_obj_exact$boot_coef

    expect_is(theta_obj_tuned, "numeric")
    expect_is(se_obj_tuned, "numeric")

    # if (data_ml$n_instr() == 1) {
    #   double_mlpliv_obj_tuned_Z = DoubleMLPLIV.partialZ(data_ml,
    #                                      n_folds = n_folds,
    #                                      ml_r = learner,
    #                                      dml_procedure = dml_procedure,
    #                                      score = score,
    #                                      n_rep = n_rep)
    #
    #   double_mlpliv_obj_tuned_Z$tune(param_set = param_grid, tune_on_folds = tune_on_folds)
    #   double_mlpliv_obj_tuned_Z$fit()
    #
    #   theta_obj_tuned_Z = double_mlpliv_obj_tuned_Z$coef
    #   se_obj_tuned_Z = double_mlpliv_obj_tuned_Z$se
    #
    #   expect_is(theta_obj_tuned_Z, "numeric")
    #   expect_is(se_obj_tuned_Z, "numeric")
    # }
    #
    if (data_ml$n_instr > 1) {
      set.seed(3141)
      double_mlpliv_obj_tuned_Z = DoubleMLPLIV.partialZ(data_ml,
        n_folds = n_folds,
        ml_r = learner,
        dml_procedure = dml_procedure,
        score = score,
        n_rep = n_rep)

      param_grid_r = list("ml_r" = param_grid[["ml_r"]])
      tune_settings_r = tune_settings
      tune_settings_r$measure$ml_l = tune_settings_r$measure$ml_m = NULL
      double_mlpliv_obj_tuned_Z$tune(
        param_set = param_grid_r, tune_on_folds = tune_on_folds,
        tune_settings = tune_settings_r)
      double_mlpliv_obj_tuned_Z$fit()

      theta_obj_tuned_Z = double_mlpliv_obj_tuned_Z$coef
      se_obj_tuned_Z = double_mlpliv_obj_tuned_Z$se

      expect_is(theta_obj_tuned_Z, "numeric")
      expect_is(se_obj_tuned_Z, "numeric")

      set.seed(3141)
      double_mlpliv_obj_tuned_XZ = DoubleMLPLIV.partialXZ(data_ml,
        n_folds = n_folds,
        ml_l = learner,
        ml_m = learner,
        ml_r = learner,
        dml_procedure = dml_procedure,
        score = score,
        n_rep = n_rep)

      double_mlpliv_obj_tuned_XZ$tune(
        param_set = param_grid, tune_on_folds = tune_on_folds,
        tune_settings = tune_settings)
      double_mlpliv_obj_tuned_XZ$fit()

      theta_obj_tuned_XZ = double_mlpliv_obj_tuned_XZ$coef
      se_obj_tuned_XZ = double_mlpliv_obj_tuned_XZ$se

      expect_is(theta_obj_tuned_XZ, "numeric")
      expect_is(se_obj_tuned_XZ, "numeric")
    }
  }
)
