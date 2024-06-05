context("Unit tests for exception handling and deprecation warnings of PLIV")

library("mlr3learners")

logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")
lgr::get_logger("mlr3")$set_threshold("warn")

test_that("Unit tests for deprecation warnings of PLIV", {
  set.seed(3141)
  dml_data_pliv = make_pliv_CHS2015(n_obs = 51, dim_z = 1)
  ml_l = lrn("regr.ranger")
  ml_g = lrn("regr.ranger")
  ml_m = lrn("regr.ranger")
  ml_r = lrn("regr.ranger")
  msg = paste0("The argument ml_g was renamed to ml_l.")
  expect_warning(DoubleMLPLIV$new(dml_data_pliv,
    ml_g = ml_g, ml_m = ml_m, ml_r = ml_r),
  regexp = msg)

  msg = paste(
    "For score = 'IV-type', learners",
    "ml_l, ml_m, ml_r and ml_g need to be specified.")
  expect_error(DoubleMLPLIV$new(dml_data_pliv,
    ml_l = ml_l, ml_m = ml_m, ml_r = ml_r,
    score = "IV-type"),
  regexp = msg)

  dml_obj = DoubleMLPLIV$new(dml_data_pliv,
    ml_l = ml_g, ml_m = ml_m, ml_r = ml_r)

  msg = paste0("Learner ml_g was renamed to ml_l.")
  expect_warning(dml_obj$set_ml_nuisance_params(
    "ml_g", "d", list("num.trees" = 10)),
  regexp = msg)

  par_grids = list(
    "ml_g" = paradox::ps(
      num.trees = paradox::p_int(lower = 9, upper = 10)),
    "ml_m" = paradox::ps(
      num.trees = paradox::p_int(lower = 10, upper = 11)),
    "ml_r" = paradox::ps(
      num.trees = paradox::p_int(lower = 10, upper = 11)))

  msg = paste0("Learner ml_g was renamed to ml_l.")
  expect_warning(dml_obj$tune(par_grids),
    regexp = msg)

  tune_settings = list(
    n_folds_tune = 5,
    rsmp_tune = mlr3::rsmp("cv", folds = 5),
    measure = list(ml_g = "regr.mse", ml_m = "regr.mae"),
    terminator = mlr3tuning::trm("evals", n_evals = 20),
    algorithm = mlr3tuning::tnr("grid_search"),
    resolution = 5)
  expect_warning(dml_obj$tune(par_grids, tune_settings = tune_settings),
    regexp = msg)
}
)

test_that("Unit tests of exception handling for DoubleMLPLIV", {
  set.seed(3141)
  dml_data_pliv = make_pliv_CHS2015(n_obs = 51, dim_z = 1)
  ml_l = lrn("regr.ranger")
  ml_m = lrn("regr.ranger")
  ml_r = lrn("regr.ranger")
  ml_g = lrn("regr.ranger")


  msg = paste0(
    "A learner ml_g has been provided for ",
    "score = 'partialling out' but will be ignored.")
  expect_warning(DoubleMLPLIV$new(dml_data_pliv,
    ml_l = ml_l, ml_m = ml_m, ml_r = ml_r,
    ml_g = ml_g,
    score = "partialling out"),
  regexp = msg)
}
)
