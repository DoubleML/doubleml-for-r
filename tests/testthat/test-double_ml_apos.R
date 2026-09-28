# APOS must preserve nominal identity and fit every level on the same observations.
apos_predictions = function(fixture, n_rep = 1L) {
  setNames(lapply(seq_along(fixture$treatment_levels), function(j) {
    list(ml_g = matrix(fixture$conditional_means[, j], nrow(fixture$df), n_rep),
      ml_m = matrix(fixture$propensity[, j], nrow(fixture$df), n_rep))
  }), as.character(fixture$treatment_levels))
}

# Package classes are loaded by the test harness.
# nolint start: object_usage_linter.
new_apos = function(fixture, ...) {
  DoubleMLAPOS$new(fixture$dml_data, "regr.rpart", "classif.rpart",
    n_folds = 2, draw_sample_splitting = FALSE, ...)
}
# nolint end: object_usage_linter.

test_that("APOS preserves inferred nominal order and explicit subsets", {
  for (labels in list(c("filter", "control", "boiling"), c(10, 2, 7))) {
    fixture = make_apo_data(treatment_levels = labels)
    obj = new_apos(fixture)
    expect_identical(obj$treatment_levels, labels)
    expect_named(obj$apo_models, as.character(labels))
    expect_null(obj$smpls)
    obj = new_apos(fixture, treatment_levels = labels[c(3, 1)])
    obj$set_sample_splitting(fixture$smpls)
    obj$fit(external_predictions = apos_predictions(fixture)[c(3, 1)])
    expect_named(obj$coef, as.character(labels[c(3, 1)]))
    expect_length(obj$apo_models, 2)
  }
  fixture$df$d = factor(fixture$df$d, levels = c(7, 10, 2, 99))
  fixture$dml_data = DoubleMLData$new(fixture$df, y_col = "y", d_cols = "d", x_cols = "x")
  expect_identical(new_apos(fixture)$treatment_levels, c("7", "10", "2"))
})

test_that("APOS synchronizes drawn and supplied splits", {
  fixture = make_apo_data()
  obj = DoubleMLAPOS$new(fixture$dml_data, "regr.rpart", "classif.rpart", n_folds = 2)
  for (model in obj$apo_models) expect_identical(model$smpls, obj$smpls)
  expect_identical(obj$set_sample_splitting(rep(fixture$smpls, 2)), obj)
  for (model in obj$apo_models) {
    expect_identical(model$smpls, rep(fixture$smpls, 2))
    expect_equal(model$n_rep, 2)
  }
  before = obj$smpls
  expect_error(obj$set_sample_splitting(list(train_ids = list(1:5), test_ids = list(1:5))))
  expect_identical(obj$smpls, before)
})

# Direct scores expose independence approximations, axis transposition and rep mixing.
test_that("APOS stacks centered contributions into the full joint covariance", {
  fixture = make_apo_data()
  predictions = apos_predictions(fixture, 2)
  for (label in names(predictions)) predictions[[label]]$ml_g[, 2] =
    predictions[[label]]$ml_g[, 2] + seq_len(60) / 100
  for (procedure in c("dml1", "dml2")) {
    obj = new_apos(fixture, dml_procedure = procedure)
    obj$set_sample_splitting(rep(fixture$smpls, 2))
    expect_identical(obj$fit(TRUE, TRUE, predictions[c(3, 1, 2)]), obj)
    expected_coef = matrix(0, 3, 2)
    for (r in 1:2) {
      psi = matrix(0, 60, 3)
      for (j in 1:3) {
        label = fixture$treatment_levels[j]
        score = apo_score_from_predictions(fixture$df$y, fixture$df$d, label,
          predictions[[label]]$ml_g[, r], predictions[[label]]$ml_m[, r])
        expected_coef[j, r] = score$theta
        psi[, j] = score$psi
      }
      expect_equal(unname(obj$framework$all_cov[, , r]), crossprod(psi) / 60^2)
      expect_equal(unname(obj$framework$psi[, , r]), psi)
      expect_true(any(abs(obj$framework$all_cov[, , r][upper.tri(diag(3))]) > .001))
    }
    expect_equal(unname(obj$framework$all_coef), expected_coef)
    for (label in fixture$treatment_levels) {
      separate = DoubleMLAPO$new(fixture$dml_data, "regr.rpart", "classif.rpart", label,
        dml_procedure = procedure, draw_sample_splitting = FALSE)
      separate$set_sample_splitting(obj$smpls)
      separate$fit(external_predictions = predictions[[label]])
      expect_equal(obj$coef[label], separate$coef)
      expect_equal(obj$se[label], separate$se)
      expect_named(obj$predictions[[label]], c("ml_g", "ml_m"))
      expect_named(obj$models[[label]], c("ml_g", "ml_m"))
      expect_equal(obj$predictions[[label]]$ml_g[, , 1], predictions[[label]]$ml_g)
    }
    expect_equal(obj$t_stat, obj$coef / obj$se)
    expect_equal(obj$pval, 2 * pnorm(-abs(obj$t_stat)))
    expect_output({
      table = obj$summary()
    }, "Std. Error")
    expect_equal(table[, 1], obj$coef)
    expect_output(obj$print(), "DoubleMLAPOS")
  }
})

test_that("APOS guards unavailable results and rejects invalid labels and data", {
  fixture = make_apo_data()
  obj = new_apos(fixture)
  expect_null(obj$framework)
  for (field in c(
    "coef", "se", "t_stat", "pval", "predictions", "models",
    "sensitivity_elements", "sensitivity_params", "sensitivity_summary"
  )) {
    expect_error(obj[[field]], "fit")
  }
  expect_error(obj$summary(), "fit")
  expect_output(obj$print(), "not.*fit|fit.*not")
  expect_error(obj$fit(), "sample.*split")
  for (levels in list(c("control", "control"), "absent", numeric(), 1)) {
    expect_error(new_apos(fixture, treatment_levels = levels))
  }
  fixture$df$d = rep(c(1, 1 + 1e-15, 2), 20)
  fixture$dml_data = DoubleMLData$new(fixture$df, y_col = "y", d_cols = "d", x_cols = "x")
  expect_error(new_apos(fixture), "display|unique")
  fixture$df$cluster = rep(1:10, each = 6)
  clustered = DoubleMLClusterData$new(fixture$df,
    y_col = "y", d_cols = "d", cluster_cols = "cluster", x_cols = "x")
  expect_error(DoubleMLAPOS$new(clustered, "regr.rpart", "classif.rpart"), "Cluster")
})

test_that("APOS validates every external entry before starting any component", {
  fixture = make_apo_data()
  obj = DoubleMLAPOS$new(fixture$dml_data, ApoLeakageRegr$new(),
    ApoLeakageClassif$new(reject_training = TRUE), draw_sample_splitting = FALSE,
    trimming_threshold = 0)
  obj$set_sample_splitting(fixture$smpls)
  # If the coordinator starts a component fit before validating the last entry,
  # this boundary probe fails immediately, even when predictions bypass learners.
  first = obj$apo_models[[1]]
  unlockBinding("fit", first)
  first$fit = function(...) stop("Component fit reached before nested validation.")
  good = apos_predictions(fixture)
  bad = good
  bad[[1]] = NULL
  expect_error(obj$fit(external_predictions = bad), "label")
  bad = good
  bad[1] = list(NULL)
  expect_error(obj$fit(external_predictions = bad), "external_predictions|ml_g")
  bad = good
  bad[[2]]$ml_m = t(bad[[2]]$ml_m)
  expect_error(obj$fit(external_predictions = bad), "matrix")
  bad = good
  bad[[2]]$ml_g[1] = Inf
  expect_error(obj$fit(external_predictions = bad), "finite")
  bad = good
  bad[[2]]$ml_m[1] = 0
  expect_error(obj$fit(external_predictions = bad), "positive")
  bad = good
  names(bad)[2] = names(bad)[1]
  expect_error(obj$fit(external_predictions = bad), "label|unique")
  expect_null(obj$framework)
  expect_error(obj$coef, "fit")
})

# A real learner fails in component two after component one has produced estimates.
test_that("APOS publishes no partial fit when a later component fails", {
  fixture = make_apo_data()
  obj = DoubleMLAPOS$new(fixture$dml_data, ApoLeakageRegr$new(), ApoLeakageClassif$new(),
    draw_sample_splitting = FALSE)
  obj$set_sample_splitting(fixture$smpls)
  second_m = obj$apo_models[[2]]$learner$ml_m
  second_m$reject_training = TRUE
  expect_error(suppressWarnings(obj$fit(TRUE, TRUE)), "training was reached")
  expect_null(obj$framework)
  expect_error(obj$coef, "fit")
  for (model in obj$apo_models) {
    expect_true(all(is.na(model$coef)))
    expect_null(model$predictions)
    expect_null(model$models)
  }
  obj$fit(TRUE, TRUE, apos_predictions(fixture))
  before = obj$framework
  expect_error(suppressWarnings(obj$fit(TRUE, TRUE)), "training was reached")
  expect_identical(obj$framework, before)
  expect_equal(obj$coef, before$coef)
})

test_that("APOS isolates learners, stored models and deep clones", {
  fixture = make_apo_data()
  g = mlr3::lrn("regr.rpart", cp = .01)
  m = mlr3::lrn("classif.rpart", cp = .01)
  obj = DoubleMLAPOS$new(fixture$dml_data, g, m, draw_sample_splitting = FALSE)
  obj$set_sample_splitting(fixture$smpls)
  first_g = obj$apo_models[[1]]$learner$ml_g
  first_g$param_set$values$cp = .02
  expect_equal(g$param_set$values$cp, .01)
  expect_equal(obj$apo_models[[2]]$learner$ml_g$param_set$values$cp, .01)
  obj$fit(TRUE, TRUE)
  fitted_g = obj$models[[1]]$ml_g$d[[1]][[1]]
  fitted_g$param_set$values$cp = .03
  expect_equal(obj$models[[2]]$ml_g$d[[1]][[1]]$param_set$values$cp, .01)
  copy = obj$clone(deep = TRUE)
  copied_m = copy$apo_models[[2]]$learner$ml_m
  copied_m$param_set$values$cp = .04
  expect_equal(obj$apo_models[[2]]$learner$ml_m$param_set$values$cp, .01)
  obj$fit(external_predictions = apos_predictions(fixture))
  expect_null(obj$predictions)
  expect_null(obj$models)
  obj$set_sample_splitting(fixture$smpls)
  expect_null(obj$framework)
  expect_error(obj$coef, "fit")
})

test_that("APOS detects component split drift before fitting", {
  fixture = make_apo_data()
  obj = new_apos(fixture)
  obj$set_sample_splitting(fixture$smpls)
  obj$apo_models[[2]]$set_sample_splitting(rep(fixture$smpls, 2))
  expect_error(obj$fit(external_predictions = apos_predictions(fixture)),
    "synchron|split|repetition")
  expect_null(obj$framework)
})

test_that("APOS tunes each level while preserving synchronized folds", {
  fixture = make_apo_data()
  lgr::get_logger("bbotk")$set_threshold("warn")
  for (on_folds in c(FALSE, TRUE)) {
    obj = new_apos(fixture, treatment_levels = c("boiling", "control"))
    obj$set_sample_splitting(rep(fixture$smpls, 2))
    grid = list(ml_g = paradox::ps(cp = paradox::p_dbl(.01, .02)),
      ml_m = paradox::ps(cp = paradox::p_dbl(.01, .02)))
    settings = list(n_folds_tune = 2, terminator = mlr3tuning::trm("evals", n_evals = 2),
      resolution = 2)
    result = obj$tune(grid, settings, tune_on_folds = on_folds)
    expect_named(result, c("boiling", "control"))
    for (label in names(result)) {
      expect_identical(obj$apo_models[[label]]$smpls, rep(fixture$smpls, 2))
      expect_equal(result[[label]], obj$apo_models[[label]]$tuning_res)
    }
    suppressWarnings(obj$fit(store_models = TRUE))
    for (label in names(result)) {
      for (key in c("ml_g", "ml_m")) {
        for (r in 1:2) {
          for (k in 1:2) {
            expected = if (on_folds) result[[label]]$d[[r]][[key]]$params[[k]]$cp else
              result[[label]]$d[[key]]$params[[1]]$cp
            expect_equal(obj$models[[label]][[key]]$d[[r]][[k]]$param_set$values$cp, expected)
          }
        }
      }
    }
  }
})

# Only held-out observations contribute, and single-level subsets retain array axes.
test_that("APOS joint covariance respects held-out samples and singleton subsets", {
  fixture = make_apo_data()
  split = list(train_ids = fixture$smpls[[1]]$train_ids[1],
    test_ids = fixture$smpls[[1]]$test_ids[1])
  ids = split$test_ids[[1]]
  for (labels in list(fixture$treatment_levels, "filter")) {
    obj = DoubleMLAPOS$new(fixture$dml_data, ApoLeakageRegr$new(),
      ApoLeakageClassif$new(), treatment_levels = labels, dml_procedure = "dml1",
      draw_sample_splitting = FALSE)
    obj$set_sample_splitting(split)
    obj$fit()
    psi = matrix(0, length(ids), length(labels))
    for (j in seq_along(labels)) {
      label = labels[j]
      score = apo_score_from_predictions(fixture$df$y[ids], fixture$df$d[ids], label,
        as.numeric(ids), .2 + ids / 1000)
      expect_equal(unname(obj$coef[label]), score$theta)
      expect_equal(unname(obj$se[label]), score$se)
      psi[, j] = score$psi
    }
    expect_equal(dim(obj$framework$psi), c(60L, length(labels), 1L))
    expect_equal(unname(obj$framework$all_cov[, , 1, drop = FALSE]),
      array(crossprod(psi) / length(ids)^2, c(length(labels), length(labels), 1L)))
    expect_true(all(obj$framework$psi[-ids, , , drop = FALSE] == 0))
  }
})

test_that("APOS read-only bindings and fit storage flags reject assignment or invalid input", {
  fixture = make_apo_data()
  obj = new_apos(fixture)
  for (field in c("treatment_levels", "apo_models", "framework", "coef", "se",
    "t_stat", "pval", "predictions", "models", "smpls")) {
    expect_error({
      obj[[field]] = NULL
    }, "can't set")
  }
  obj$set_sample_splitting(fixture$smpls)
  expect_error(obj$fit(store_predictions = NA), "store_predictions")
  expect_error(obj$fit(store_models = 1), "store_models")
})
