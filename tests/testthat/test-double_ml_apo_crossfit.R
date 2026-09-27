# Wrong conditional train IDs, reindexed tests or train predictions trigger leakage.
test_that("APO nuisance predictions are genuinely out of fold in every repetition", {
  fixture = make_apo_data(n_obs = 24L, treatment_levels = c("a", "b", "c", "d"))
  data_before = data.table::copy(fixture$dml_data$data)
  model_before = data.table::copy(fixture$dml_data$data_model)
  obj = DoubleMLAPO$new(fixture$dml_data, ApoLeakageRegr$new(), ApoLeakageClassif$new(),
    "b", n_folds = 2, n_rep = 2, draw_sample_splitting = FALSE)
  second = list(train_ids = rev(fixture$smpls[[1]]$train_ids),
    test_ids = rev(fixture$smpls[[1]]$test_ids))
  obj$set_sample_splitting(list(fixture$smpls[[1]], second))
  expect_identical(obj$fit(store_predictions = TRUE, store_models = TRUE), obj)
  expect_named(obj$predictions, c("ml_g", "ml_m"))
  expect_named(obj$models, c("ml_g", "ml_m"))
  for (i_rep in 1:2) {
    expect_equal(as.vector(obj$predictions$ml_g[, i_rep, 1]), as.numeric(1:24))
    expect_equal(as.vector(obj$predictions$ml_m[, i_rep, 1]), .2 + (1:24) / 1000)
    for (key in c("ml_g", "ml_m")) {
      models = obj$models[[key]]$d[[i_rep]]
      predicted = unlist(lapply(models, function(model) model$model$log$test_ids))
      expect_equal(tabulate(predicted, nbins = 24L), rep(1L, 24))
      for (i_fold in 1:2) {
        model = models[[i_fold]]$model
        train_ids = obj$smpls[[i_rep]]$train_ids[[i_fold]]
        expected_train = if (key == "ml_g") {
          train_ids[fixture$df$d[train_ids] == "b"]
        } else {
          train_ids
        }
        expect_setequal(model$train_ids, expected_train)
        expect_setequal(model$log$test_ids, obj$smpls[[i_rep]]$test_ids[[i_fold]])
        if (key == "ml_m") {
          expect_equal(as.integer(as.character(model$target)),
            as.integer(fixture$df$d[model$train_ids] == "b"))
          expect_setequal(fixture$df$d[model$train_ids], c("a", "b", "c", "d"))
        }
      }
    }
  }
  expect_identical(fixture$dml_data$data, data_before)
  expect_identical(fixture$dml_data$data_model, model_before)
})

# Missing support in repetition 2 must fail before any earlier fold is trained.
test_that("APO support failures identify the treatment, repetition and fold", {
  fixture = make_apo_data(n_obs = 24L, treatment_levels = c("a", "b", "c", "d"))
  for (requested_only in c(FALSE, TRUE)) {
    bad_train = which(if (requested_only) fixture$df$d == "b" else fixture$df$d != "b")
    bad_split = list(train_ids = list(bad_train, setdiff(1:24, bad_train)),
      test_ids = list(setdiff(1:24, bad_train), bad_train))
    obj = DoubleMLAPO$new(fixture$dml_data, ApoLeakageRegr$new(),
      ApoLeakageClassif$new(reject_training = TRUE),
      "b", n_folds = 2, n_rep = 2, draw_sample_splitting = FALSE)
    obj$set_sample_splitting(list(fixture$smpls[[1]], bad_split))
    expect_error(obj$fit(), paste0("treatment b, repetition 2, fold 1.*",
      if (requested_only) "class 0" else "outcome-training"))
  }
})

test_that("APO learner estimates retain nominal label semantics", {
  for (labels in list(c(10, 2, 7, 19), factor(c("a", "b", "c", "d"),
    levels = c("d", "c", "b", "a")))) {
    fixture = make_apo_data(n_obs = 24L, treatment_levels = labels)
    level = labels[2]
    if (is.factor(labels)) {
      fixture$df$d = factor(fixture$df$d, levels = levels(labels))
      fixture$dml_data = DoubleMLData$new(fixture$df,
        y_col = "y", d_cols = "d", x_cols = "x")
    }
    obj = DoubleMLAPO$new(fixture$dml_data, ApoLeakageRegr$new(), ApoLeakageClassif$new(),
      level, n_folds = 2, draw_sample_splitting = FALSE)
    obj$set_sample_splitting(fixture$smpls)
    obj$fit(store_predictions = TRUE)
    expected = (1:24) + as.integer(fixture$df$d == level) /
      (.2 + (1:24) / 1000) * (fixture$df$y - (1:24))
    expect_equal(as.vector(obj$psi_b), expected)
    expect_named(obj$coef, as.character(level))
    expect_identical(obj$treatment_level, level)
  }
})

test_that("APO learned zero propensities require positive truncation", {
  fixture = make_apo_data(n_obs = 24L)
  for (threshold in c(0, .01)) {
    obj = DoubleMLAPO$new(fixture$dml_data, ApoLeakageRegr$new(),
      ApoLeakageClassif$new(zero_propensity = TRUE), "filter", n_folds = 2,
      trimming_threshold = threshold, draw_sample_splitting = FALSE)
    obj$set_sample_splitting(fixture$smpls)
    if (threshold == 0) {
      expect_error(obj$fit(), "positivity.*strictly positive.*trimming_threshold")
    } else {
      obj$fit(store_predictions = TRUE)
      expect_equal(as.vector(obj$predictions$ml_m), rep(0, 24))
      expect_true(all(is.finite(c(obj$coef, obj$se, obj$psi_b))))
    }
  }
})
