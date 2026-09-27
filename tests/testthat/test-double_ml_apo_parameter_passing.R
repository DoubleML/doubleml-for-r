# Parameters must reach both cloned nuisances in every repetition and fold.
test_that("APO passes global and fold-specific nuisance parameters to stored models", {
  fixture = make_apo_data(n_obs = 60L)
  for (fold_specific in c(FALSE, TRUE)) {
    ml_g = mlr3::lrn("regr.rpart", cp = .1)
    ml_m = mlr3::lrn("classif.rpart", cp = .1)
    obj = DoubleMLAPO$new(fixture$dml_data, ml_g, ml_m, "filter",
      n_folds = 2, n_rep = 2, draw_sample_splitting = FALSE)
    obj$set_sample_splitting(rep(fixture$smpls, 2))
    values = if (fold_specific) c(.02, .03, .04, .05) else rep(.02, 4)
    for (key in c("ml_g", "ml_m")) {
      params = if (fold_specific) {
        list(list(list(cp = .02), list(cp = .03)), list(list(cp = .04), list(cp = .05)))
      } else {
        list(cp = .02)
      }
      obj$set_ml_nuisance_params(key, "d", params, set_fold_specific = fold_specific)
    }
    suppressWarnings(obj$fit(store_models = TRUE, store_predictions = TRUE))
    expect_named(obj$predictions, c("ml_g", "ml_m"))
    expect_named(obj$models, c("ml_g", "ml_m"))
    for (key in c("ml_g", "ml_m")) {
      expect_equal(dim(obj$predictions[[key]]), c(60L, 2L, 1L))
      for (i_rep in 1:2) {
        for (i_fold in 1:2) {
          model = obj$models[[key]]$d[[i_rep]][[i_fold]]
          expect_equal(model$param_set$values$cp, values[(i_rep - 1) * 2 + i_fold])
        }
      }
    }
    expect_equal(ml_g$param_set$values$cp, .1)
    expect_equal(ml_m$param_set$values$cp, .1)
  }
})

# Reversed source factor levels and class 0 first must still yield probability of class 1.
test_that("APO extracts the positive probability by name for reordered binary factors", {
  fixture = make_apo_data(n_obs = 24L, treatment_levels = c("a", "b"))
  fixture$df$d = factor(fixture$df$d, levels = c("b", "a"))
  data = DoubleMLData$new(fixture$df, "y", "d", x_cols = "x")
  before = data.table::copy(data$data)
  obj = DoubleMLAPO$new(data, ApoLeakageRegr$new(), ApoLeakageClassif$new(),
    factor("a", levels = c("b", "a")), n_folds = 2, draw_sample_splitting = FALSE)
  obj$set_sample_splitting(fixture$smpls)
  obj$fit(store_predictions = TRUE)
  expect_equal(as.vector(obj$predictions$ml_m), .2 + (1:24) / 1000)
  expect_identical(data$data, before)
})
