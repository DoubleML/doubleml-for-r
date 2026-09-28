# The tuned outcome tasks must contain selected rows only; propensity tasks contain all levels.
test_that("APO tunes both nuisances globally and on folds without mutating nominal data", {
  fixture = make_apo_data(n_obs = 60L, treatment_levels = c(10, 2, 7))
  before = data.table::copy(fixture$dml_data$data)
  model_before = data.table::copy(fixture$dml_data$data_model)
  lgr::get_logger("bbotk")$set_threshold("warn")
  for (on_folds in c(FALSE, TRUE)) {
    obj = DoubleMLAPO$new(fixture$dml_data, "regr.rpart", "classif.rpart", 2,
      n_folds = 2, n_rep = 2, draw_sample_splitting = FALSE)
    obj$set_sample_splitting(rep(fixture$smpls, 2))
    grid = list(ml_g = paradox::ps(cp = paradox::p_dbl(.01, .02)),
      ml_m = paradox::ps(cp = paradox::p_dbl(.01, .02)))
    settings = list(n_folds_tune = 2, terminator = mlr3tuning::trm("evals", n_evals = 2),
      resolution = 2)
    reference_data = data.table::copy(fixture$dml_data$data_model)
    reference_data$d = as.integer(fixture$df$d == 2)
    reference_rows = if (on_folds) fixture$smpls[[1]]$train_ids else list(1:60)
    reference_m = lapply(reference_rows, function(rows) reference_data[rows, ])
    reference_g = lapply(reference_m, function(data) data[data$d == 1, ])
    reference_settings = obj$.__enclos_env__$private$assert_tune_settings(settings)
    set.seed(619)
    expected_m = dml_tune(mlr3::lrn("classif.rpart"), "x", "d", reference_m,
      "reference_m", grid$ml_m, reference_settings, reference_settings$measure$ml_m, "classif")
    expected_g = dml_tune(mlr3::lrn("regr.rpart"), "x", "y", reference_g,
      "reference_g", grid$ml_g, reference_settings, reference_settings$measure$ml_g, "regr")
    set.seed(619)
    expect_identical(obj$tune(grid, settings, tune_on_folds = on_folds), obj)
    expect_setequal(obj$params_names(), c("ml_g", "ml_m"))
    results = if (on_folds) obj$tuning_res$d else list(obj$tuning_res$d)
    expect_length(results, if (on_folds) 2 else 1)
    for (key in c("ml_g", "ml_m")) {
      expected = if (key == "ml_g") expected_g else expected_m
      expect_equal(results[[1]][[key]]$params, expected$params)
      for (i_fold in seq_along(expected$params)) {
        columns = c("cp", if (key == "ml_g") "regr.mse" else "classif.ce")
        actual_archive = results[[1]][[key]][[1]]$tuning_result[[i_fold]]$tuning_archive
        expected_archive = expected$tuning_result[[i_fold]]$tuning_archive
        expect_equal(actual_archive[, columns, with = FALSE],
          expected_archive[, columns, with = FALSE])
      }
    }
    for (i_rep in seq_along(results)) {
      expect_named(results[[i_rep]], c("ml_g", "ml_m"))
      for (key in c("ml_g", "ml_m")) {
        result = results[[i_rep]][[key]]
        expect_length(result$params, if (on_folds) 2 else 1)
        for (i_fold in seq_along(result$params)) {
          expect_true(result$params[[i_fold]]$cp %in% c(.01, .02))
        }
      }
    }
    suppressWarnings(obj$fit(store_models = TRUE))
    for (key in c("ml_g", "ml_m")) {
      for (i_rep in 1:2) {
        for (i_fold in 1:2) {
          expected = if (on_folds) results[[i_rep]][[key]]$params[[i_fold]]$cp else
            results[[1]][[key]]$params[[1]]$cp
          expect_equal(obj$models[[key]]$d[[i_rep]][[i_fold]]$param_set$values$cp, expected)
        }
      }
    }
    expect_true(all(is.finite(c(obj$coef, obj$se))))
    expect_identical(fixture$dml_data$data, before)
    expect_identical(fixture$dml_data$data_model, model_before)
  }
})
