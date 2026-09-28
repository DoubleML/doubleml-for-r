# Package classes and APO fixtures are loaded by the testthat harness.
# nolint start: object_usage_linter.
make_apos_sensitivity_fixture = function(
  labels = c("control", "chlorine", "filter", "boiling"),
  order = seq_along(labels), n_rep = 2L
) {
  fixture = make_apo_data(60L, labels, seed = 927)
  level_names = as.character(fixture$treatment_levels)
  observed_index = match(as.character(fixture$df$d), level_names)
  observed_g = fixture$conditional_means[cbind(seq_len(60L), observed_index)]
  predictions = setNames(lapply(seq_along(level_names), function(j) {
    selected = complement = propensity = matrix(0, 60L, n_rep)
    for (i_rep in seq_len(n_rep)) {
      selected[, i_rep] = fixture$conditional_means[, j] + .03 * j * i_rep
      complement[, i_rep] = observed_g - .02 * j * i_rep
      propensity[, i_rep] = 1 / length(level_names) + .005 * (i_rep - 1L)
    }
    list(
      ml_g = selected, ml_m = propensity,
      ml_g_complement = complement
    )
  }), level_names)
  requested = fixture$treatment_levels[order]
  obj = DoubleMLAPOS$new(
    fixture$dml_data, "regr.rpart", "classif.rpart",
    treatment_levels = requested, n_folds = 2, n_rep = n_rep,
    trimming_threshold = 0, draw_sample_splitting = FALSE
  )
  obj$set_sample_splitting(rep(fixture$smpls, n_rep))
  obj$fit(external_predictions = predictions[names(obj$apo_models)])
  list(fixture = fixture, obj = obj, predictions = predictions)
}

contrast_sensitivity_oracle = function(elements, contrast) {
  squared = contrast^2
  residual_weights = squared / rowSums(squared)
  n_obs = dim(elements$riesz_rep)[1]
  n_contrast = nrow(contrast)
  n_rep = ncol(elements$sigma2)
  sigma2 = residual_weights %*% elements$sigma2
  nu2 = squared %*% elements$nu2
  psi_sigma2 = psi_nu2 = riesz_rep = array(0, c(n_obs, n_contrast, n_rep))
  for (i_rep in seq_len(n_rep)) {
    psi_sigma2[, , i_rep] = elements$psi_sigma2[, , i_rep] %*% t(residual_weights)
    psi_nu2[, , i_rep] = elements$psi_nu2[, , i_rep] %*% t(squared)
    riesz_rep[, , i_rep] = elements$riesz_rep[, , i_rep] %*% t(contrast)
  }
  max_bias = sqrt(sigma2 * nu2)
  psi_max_bias = array(0, c(n_obs, n_contrast, n_rep))
  for (i_rep in seq_len(n_rep)) {
    psi_max_bias[, , i_rep] = sweep(
      psi_nu2[, , i_rep, drop = FALSE], 2, sigma2[, i_rep], "*"
    )[, , 1] + sweep(
      psi_sigma2[, , i_rep, drop = FALSE], 2, nu2[, i_rep], "*"
    )[, , 1]
    psi_max_bias[, , i_rep] = sweep(
      psi_max_bias[, , i_rep, drop = FALSE], 2, 2 * max_bias[, i_rep], "/"
    )[, , 1]
  }
  list(
    sigma2 = sigma2, nu2 = nu2,
    psi_sigma2 = psi_sigma2, psi_nu2 = psi_nu2,
    riesz_rep = riesz_rep, max_bias = max_bias,
    psi_max_bias = psi_max_bias
  )
}

test_that("APOS sensitivity elements concatenate components in requested order", {
  cases = list(
    list(factor(c("control", "filter", "boiling"),
      levels = c("filter", "control", "boiling")), c(3, 1, 2)),
    list(c("control", "chlorine", "filter", "boiling"), c(4, 2, 1, 3)),
    list(c(10, 200, 9000, 42), c(2, 4, 1, 3))
  )
  for (case in cases) {
    setup = make_apos_sensitivity_fixture(case[[1]], case[[2]])
    obj = setup$obj
    elements = obj$sensitivity_elements
    expect_identical(rownames(elements$sigma2), names(obj$apo_models))
    for (j in seq_along(obj$apo_models)) {
      component = obj$apo_models[[j]]$sensitivity_elements
      for (key in c("sigma2", "nu2", "max_bias")) {
        expect_equal(elements[[key]][j, ], component[[key]][1, ])
      }
      for (key in c("psi_sigma2", "psi_nu2", "riesz_rep", "psi_max_bias")) {
        expect_equal(elements[[key]][, j, ], component[[key]][, 1, ])
      }
    }
  }
})

test_that("APOS sensitivity delegates without mutating component state", {
  obj = make_apos_sensitivity_fixture()$obj
  components = obj$apo_models
  component_state = lapply(components, function(model) {
    list(
      coef = model$coef, se = model$se, psi = model$psi,
      sensitivity_params = model$sensitivity_params
    )
  })
  before = list(
    coef = obj$coef, se = obj$se, covariance = obj$framework$all_cov,
    boot_coef = obj$framework$boot_coef
  )
  returned = withVisible(obj$sensitivity_analysis(
    cf_y = .1, cf_d = .2, rho = -.5, level = .9
  ))
  expect_identical(returned$value, obj)
  expect_false(returned$visible)
  expect_identical(obj$sensitivity_params, obj$framework$sensitivity_params)
  expect_match(obj$sensitivity_summary, "Sensitivity Analysis")
  expect_identical(obj$apo_models, components)
  expect_identical(lapply(obj$apo_models, function(model) {
    list(
      coef = model$coef, se = model$se, psi = model$psi,
      sensitivity_params = model$sensitivity_params
    )
  }), component_state)
  expect_identical(obj$coef, before$coef)
  expect_identical(obj$se, before$se)
  expect_identical(obj$framework$all_cov, before$covariance)
  expect_identical(obj$framework$boot_coef, before$boot_coef)
  for (field in c("sensitivity_elements", "sensitivity_params", "sensitivity_summary")) {
    expect_error({
      obj[[field]] = NULL
    }, "can't set")
  }
})

test_that("APOS contrasts rebuild exact sensitivity primitives and scenarios", {
  obj = make_apos_sensitivity_fixture()$obj
  contrast = rbind(
    "filter - chlorine" = c(0, -1, 1, 0),
    "scaled boiling - control" = c(-2, 0, 0, 2),
    "average potential outcome" = rep(.25, 4),
    "signed functional" = c(-1, .5, 0, .25)
  )
  colnames(contrast) = names(obj$coef)
  permuted = contrast[, c(4, 2, 1, 3)]
  before = permuted
  obj$sensitivity_analysis(cf_y = .04, cf_d = .05)
  parent_params = obj$sensitivity_params
  actual = obj$causal_contrast(contrast = permuted)
  expect_null(actual$sensitivity_params)
  expect_identical(obj$sensitivity_params, parent_params)
  expected_elements = contrast_sensitivity_oracle(obj$sensitivity_elements, contrast)
  expected = obj$framework$linear_transform(
    contrast, rownames(contrast), sensitivity_elements = expected_elements
  )
  expect_identical(permuted, before)
  for (key in names(expected_elements)) {
    expect_equal(actual$sensitivity_elements[[key]], expected$sensitivity_elements[[key]])
  }
  actual$sensitivity_analysis(
    cf_y = .12, cf_d = .18, rho = -.7, level = .9,
    null_hypothesis = c(0, 0, 2, 0)
  )
  expected$sensitivity_analysis(
    cf_y = .12, cf_d = .18, rho = -.7, level = .9,
    null_hypothesis = c(0, 0, 2, 0)
  )
  expect_equal(actual$sensitivity_params, expected$sensitivity_params)
  expect_identical(obj$sensitivity_params, parent_params)
})

test_that("APOS contrast sensitivity rejects zero rows and generic transforms drop it", {
  obj = make_apos_sensitivity_fixture()$obj
  invalid = rbind(valid = c(-1, 1, 0, 0), zero = rep(0, 4))
  colnames(invalid) = names(obj$coef)
  expect_error(obj$causal_contrast(contrast = invalid), "nonzero")
  generic = obj$framework$linear_transform(diag(4))
  expect_null(generic$sensitivity_elements)
  expect_error(generic$sensitivity_analysis(), "not propagated.*linear_transform")
})

test_that("APOS preserves inference when one component lacks sensitivity", {
  setup = make_apos_sensitivity_fixture()
  predictions = setup$predictions[names(setup$obj$apo_models)]
  predictions[[2]]$ml_g_complement = NULL
  setup$obj$fit(external_predictions = predictions)
  expect_true(all(is.finite(c(setup$obj$coef, setup$obj$se))))
  expect_null(setup$obj$sensitivity_elements)
  expect_error(setup$obj$sensitivity_analysis(), "ml_g_complement")
})
# nolint end: object_usage_linter.
