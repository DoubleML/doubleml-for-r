context("Unit tests for the export of predictions via fit(store_predictions = TRUE); uses PLR")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

test_cases = expand.grid(
  g_learner = "regr.cv_glmnet",
  m_learner = "regr.cv_glmnet",
  dml_procedure = "dml2",
  score = "partialling out",
  i_setting = 1:(length(data_plr)),
  stringsAsFactors = FALSE)
test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for PLR with classifier for ml_m:",
  .cases = test_cases, {
    n_folds = 3

    set.seed(i_setting)
    df = data_plr[[i_setting]]$df
    dml_data = data_plr[[i_setting]]$dml_data

    double_mlplr_obj = DoubleMLPLR$new(
      data = dml_data,
      ml_g = lrn(g_learner),
      ml_m = lrn(m_learner),
      dml_procedure = dml_procedure,
      n_folds = n_folds,
      score = score)
    set.seed(i_setting)
    double_mlplr_obj$fit(store_predictions = TRUE)

    set.seed(i_setting)
    indx = (names(df) %in% c(Xnames, "y"))
    data = df[, indx]
    task = mlr3::TaskRegr$new(id = "ml_g", backend = data, target = "y")
    resampling_smpls = rsmp("custom")$instantiate(
      task,
      double_mlplr_obj$smpls[[1]]$train_ids,
      double_mlplr_obj$smpls[[1]]$test_ids)
    resampling_pred = resample(task, lrn(g_learner), resampling_smpls)
    preds_g = as.data.table(resampling_pred$prediction())
    data.table::setorder(preds_g, "row_ids")

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

    expect_equal(as.vector(double_mlplr_obj$predictions$ml_g),
      as.vector(preds_g$response),
      tolerance = 1e-8)

    expect_equal(as.vector(double_mlplr_obj$predictions$ml_m),
      as.vector(preds_m$response),
      tolerance = 1e-8)
  }
)
