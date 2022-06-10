context("Unit tests for the export of predictions via fit(store_predictions = TRUE); uses PLR")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    l_learner = "regr.rpart",
    m_learner = "regr.rpart",
    g_learner = "regr.rpart",
    dml_procedure = "dml2",
    score = "partialling out",
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    l_learner = c("regr.rpart", "regr.lm"),
    m_learner = c("regr.rpart", "regr.lm"),
    g_learner = c("regr.rpart", "regr.lm"),
    dml_procedure = "dml2",
    score = c("partialling out", "IV-type"),
    stringsAsFactors = FALSE)
}
test_cases[".test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for for the export of predictions:",
  .cases = test_cases, {
    n_folds = 3

    set.seed(3141)
    df = data_plr$df
    dml_data = data_plr$dml_data

    if (score == "IV-type") {
      ml_g = lrn(g_learner)
    } else {
      ml_g = NULL
    }
    double_mlplr_obj = DoubleMLPLR$new(
      data = dml_data,
      ml_l = lrn(l_learner),
      ml_m = lrn(m_learner),
      ml_g = ml_g,
      dml_procedure = dml_procedure,
      n_folds = n_folds,
      score = score)
    set.seed(3141)
    double_mlplr_obj$fit(store_predictions = TRUE)

    set.seed(3141)
    Xnames = names(df)[names(df) %in% c("y", "d", "z") == FALSE]
    indx = (names(df) %in% c(Xnames, "y"))
    data = df[, indx]
    task = mlr3::TaskRegr$new(id = "ml_l", backend = data, target = "y")
    resampling_smpls = rsmp("custom")$instantiate(
      task,
      double_mlplr_obj$smpls[[1]]$train_ids,
      double_mlplr_obj$smpls[[1]]$test_ids)
    resampling_pred = resample(task, lrn(l_learner), resampling_smpls)
    preds_l = as.data.table(resampling_pred$prediction())
    data.table::setorder(preds_l, "row_ids")

    Xnames = names(df)[names(df) %in% c("y", "d", "z") == FALSE]
    indx = (names(df) %in% c(Xnames, "d"))
    data = df[, indx]
    task = mlr3::TaskRegr$new(id = "ml_m", backend = data, target = "d")
    resampling_smpls = rsmp("custom")$instantiate(
      task,
      double_mlplr_obj$smpls[[1]]$train_ids,
      double_mlplr_obj$smpls[[1]]$test_ids)
    resampling_pred = resample(task, lrn(m_learner), resampling_smpls)
    preds_m = as.data.table(resampling_pred$prediction())
    data.table::setorder(preds_m, "row_ids")

    if (score == "IV-type") {
      d = df[["d"]]
      y = df[["y"]]
      psi_a = -(d - preds_m$response) * (d - preds_m$response)
      psi_b = (d - preds_m$response) * (y - preds_l$response)
      theta_initial = -mean(psi_b, na.rm = TRUE) / mean(psi_a, na.rm = TRUE)

      data_aux = cbind(df, "y_minus_theta_d" = y - theta_initial * d)
      Xnames = names(data_aux)[names(data_aux) %in%
        c("y", "d", "z", "y_minus_theta_d") == FALSE]
      indx = (names(data_aux) %in% c(Xnames, "y_minus_theta_d"))
      data = data_aux[, indx]
      task = mlr3::TaskRegr$new(
        id = "ml_g", backend = data,
        target = "y_minus_theta_d")
      resampling_smpls = rsmp("custom")$instantiate(
        task,
        double_mlplr_obj$smpls[[1]]$train_ids,
        double_mlplr_obj$smpls[[1]]$test_ids)
      resampling_pred = resample(task, lrn(g_learner), resampling_smpls)
      preds_g = as.data.table(resampling_pred$prediction())
      data.table::setorder(preds_g, "row_ids")

      expect_equal(as.vector(double_mlplr_obj$predictions$ml_g),
        as.vector(preds_g$response),
        tolerance = 1e-8)
    }

    expect_equal(as.vector(double_mlplr_obj$predictions$ml_l),
      as.vector(preds_l$response),
      tolerance = 1e-8)

    expect_equal(as.vector(double_mlplr_obj$predictions$ml_m),
      as.vector(preds_m$response),
      tolerance = 1e-8)
  }
)
