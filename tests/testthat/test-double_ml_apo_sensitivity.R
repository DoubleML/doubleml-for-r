# nolint start: object_usage_linter.
make_external_apo_sensitivity = function(threshold = 0, normalized = FALSE) {
  fixture = make_apo_data(30, c("control", "filter", "boiling"), seed = 419)
  n = fixture$dml_data$n_obs
  treatment = fixture$df$d
  indicator = as.integer(treatment == "filter")
  observed_index = match(treatment, fixture$treatment_levels)
  observed_g = fixture$conditional_means[cbind(seq_len(n), observed_index)]
  g_selected = cbind(
    fixture$conditional_means[, "filter"] + .1,
    fixture$conditional_means[, "filter"] - .2
  )
  g_complement = cbind(observed_g + .05, observed_g - .15)
  p = cbind(.2 + (seq_len(n) %% 7) / 50, .22 + (seq_len(n) %% 7) / 50)
  p[1, 1] = .02
  p[2, 2] = .98
  obj = DoubleMLAPO$new(
    fixture$dml_data, "regr.rpart", "classif.featureless", "filter",
    n_folds = 2, n_rep = 2, trimming_threshold = threshold,
    normalize_ipw = normalized, draw_sample_splitting = FALSE
  )
  obj$set_sample_splitting(rep(fixture$smpls, 2))
  list(
    fixture = fixture, obj = obj, indicator = indicator,
    predictions = list(
      ml_g = g_selected, ml_m = p, ml_g_complement = g_complement
    )
  )
}

test_that("APO sensitivity elements match direct cross-fitted calculations", {
  for (threshold in c(0, .05)) {
    for (normalized in c(FALSE, TRUE)) {
      setup = make_external_apo_sensitivity(threshold, normalized)
      setup$obj$fit(external_predictions = setup$predictions)
      elements = setup$obj$sensitivity_elements
      expect_named(elements, c(
        "sigma2", "nu2", "psi_sigma2", "psi_nu2", "riesz_rep",
        "max_bias", "psi_max_bias"
      ))
      for (i_rep in 1:2) {
        oracle = apo_sensitivity_from_predictions(
          setup$fixture$df$y, setup$indicator,
          setup$predictions$ml_g[, i_rep],
          setup$predictions$ml_g_complement[, i_rep],
          setup$predictions$ml_m[, i_rep], threshold, normalized
        )
        for (key in c("sigma2", "nu2", "max_bias")) {
          expect_equal(unname(elements[[key]][1, i_rep]), oracle[[key]])
        }
        for (key in c("psi_sigma2", "psi_nu2", "riesz_rep", "psi_max_bias")) {
          expect_equal(unname(elements[[key]][, 1, i_rep]), oracle[[key]])
        }
      }
      expect_identical(dim(elements$psi_max_bias), c(30L, 1L, 2L))
      expect_identical(rownames(elements$sigma2), "filter")
    }
  }
})

test_that("learned complement outcome regression is nominal and out of fold", {
  sensitivity_sigma = numeric(3)
  labels = list(
    factor(c("control", "filter", "boiling"),
      levels = c("control", "filter", "boiling")),
    c("control", "filter", "boiling"),
    c(10, 200, 9000)
  )
  for (j in seq_along(labels)) {
    fixture = make_apo_data(30, labels[[j]], seed = 721)
    expected = as.character(fixture$treatment_levels)
    selected = fixture$treatment_levels[2]
    obj = DoubleMLAPO$new(
      fixture$dml_data,
      ApoSensitivityRegr$new(selected, expected), ApoLeakageClassif$new(),
      selected, n_folds = 2, draw_sample_splitting = FALSE
    )
    obj$set_sample_splitting(fixture$smpls)
    expect_no_error(obj$fit())
    expect_false(is.null(obj$sensitivity_elements))
    sensitivity_sigma[j] = obj$sensitivity_elements$sigma2[1, 1]
  }
  expect_equal(sensitivity_sigma, rep(sensitivity_sigma[1], 3))
})

test_that("learned complement does not require selected-level predictions", {
  fixture = make_apo_data(
    30, c("control", "filter", "boiling"), seed = 722
  )
  obj = DoubleMLAPO$new(
    fixture$dml_data, mlr3::lrn("regr.lm"),
    mlr3::lrn("classif.featureless"), "filter",
    n_folds = 2, draw_sample_splitting = FALSE
  )
  obj$set_sample_splitting(fixture$smpls)
  expect_no_error(obj$fit())
  expect_false(is.null(obj$sensitivity_elements))
})

test_that("legacy APO external predictions remain valid without sensitivity", {
  setup = make_external_apo_sensitivity()
  legacy = setup$predictions[c("ml_g", "ml_m")]
  expect_error(setup$obj$sensitivity_analysis(), "Fit the DoubleMLAPO")
  setup$obj$fit(store_predictions = TRUE, external_predictions = legacy)
  before = list(
    coef = setup$obj$coef, se = setup$obj$se,
    psi = setup$obj$psi, predictions = setup$obj$predictions
  )
  expect_null(setup$obj$sensitivity_elements)
  expect_error(setup$obj$sensitivity_analysis(), "ml_g_complement")
  expect_identical(names(setup$obj$predictions), c("ml_g", "ml_m"))
  expect_identical(setup$obj$coef, before$coef)
  expect_identical(setup$obj$se, before$se)
  expect_identical(setup$obj$psi, before$psi)
  expect_identical(setup$obj$predictions, before$predictions)
})

test_that("optional complement predictions validate atomically", {
  setup = make_external_apo_sensitivity()
  reordered = setup$predictions[c("ml_g_complement", "ml_m", "ml_g")]
  expect_silent(setup$obj$fit(external_predictions = reordered))
  expect_false(is.null(setup$obj$sensitivity_elements))

  extra = setup$predictions
  extra$extra = matrix(0, 30, 2)
  wrong_dim = setup$predictions
  wrong_dim$ml_g_complement = matrix(0, 29, 2)
  wrong_type = setup$predictions
  wrong_type$ml_g_complement = matrix("x", 30, 2)
  nonfinite = setup$predictions
  nonfinite$ml_g_complement[1, 1] = Inf
  invalid = list(extra, wrong_dim, wrong_type, nonfinite)
  previous_coef = setup$obj$coef
  previous_elements = setup$obj$sensitivity_elements
  for (value in invalid) {
    expect_error(setup$obj$fit(external_predictions = value),
      "external_predictions|ml_g_complement|finite|numeric")
    expect_identical(setup$obj$coef, previous_coef)
    expect_identical(setup$obj$sensitivity_elements, previous_elements)
  }
})

test_that("APO sensitivity requires full DML2 cross-fitting", {
  setup = make_external_apo_sensitivity()
  obj = DoubleMLAPO$new(
    setup$fixture$dml_data, "regr.rpart", "classif.featureless", "filter",
    n_folds = 2, n_rep = 2, dml_procedure = "dml1",
    draw_sample_splitting = FALSE
  )
  obj$set_sample_splitting(rep(setup$fixture$smpls, 2))
  obj$fit(external_predictions = setup$predictions)
  expect_error(obj$sensitivity_analysis(), "fully cross-fitted DML2")
})

test_that("factor-incompatible outcome learners preserve APO estimation", {
  fixture = make_apo_data(30, c("control", "filter", "boiling"), seed = 812)
  obj = DoubleMLAPO$new(
    fixture$dml_data, ApoLeakageRegr$new(), ApoLeakageClassif$new(),
    "filter", n_folds = 2, draw_sample_splitting = FALSE
  )
  obj$set_sample_splitting(fixture$smpls)
  expect_no_error(obj$fit())
  expect_true(all(is.finite(c(obj$coef, obj$se))))
  expect_error(obj$sensitivity_analysis(), "supports factor treatment features")
})

test_that("degenerate outcome variation leaves APO estimation available", {
  setup = make_external_apo_sensitivity()
  y = setup$fixture$df$y
  setup$predictions$ml_g[, ] = y
  setup$predictions$ml_g_complement[, ] = y
  expect_silent(setup$obj$fit(external_predictions = setup$predictions))
  expect_true(all(is.finite(setup$obj$coef)))
  expect_error(setup$obj$sensitivity_analysis(), "positive residual outcome variance")
})

test_that("APO sensitivity delegates without mutating fitted inference", {
  setup = make_external_apo_sensitivity(.05, TRUE)
  setup$obj$fit(external_predictions = setup$predictions)
  before = list(
    coef = setup$obj$coef, all_coef = setup$obj$all_coef,
    se = setup$obj$se, psi = setup$obj$psi,
    diagnostics = setup$obj$propensity_diagnostics,
    boot_coef = setup$obj$boot_coef
  )
  returned = withVisible(setup$obj$sensitivity_analysis(
    cf_y = .1, cf_d = .2, rho = -.5, level = .9
  ))
  expect_identical(returned$value, setup$obj)
  expect_false(returned$visible)
  expect_identical(setup$obj$sensitivity_params, setup$obj$framework$sensitivity_params)
  expect_match(setup$obj$sensitivity_summary, "Sensitivity Analysis")
  expect_identical(setup$obj$coef, before$coef)
  expect_identical(setup$obj$all_coef, before$all_coef)
  expect_identical(setup$obj$se, before$se)
  expect_identical(setup$obj$psi, before$psi)
  expect_identical(setup$obj$propensity_diagnostics, before$diagnostics)
  expect_identical(setup$obj$boot_coef, before$boot_coef)
  expect_error(setup$obj$framework <- NULL, "can't set field framework")
  expect_error(setup$obj$sensitivity_elements <- NULL, "can't set field sensitivity_elements")
  expect_error(setup$obj$sensitivity_params <- NULL, "can't set field sensitivity_params")
  expect_error(setup$obj$sensitivity_summary <- NULL, "can't set field sensitivity_summary")
})
# nolint end: object_usage_linter.
