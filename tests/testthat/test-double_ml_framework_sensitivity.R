framework_sensitivity_fixture = function() {
  psi = array(c(
    -3, -1, 1, 3, -2, 0, 0, 2,
    -2, -2, 2, 2, -4, 0, 0, 4
  ), c(4, 2, 2))
  psi_max_bias = array(c(
    -1, 0, 0, 1, 0, -1, 1, 0,
    -2, 0, 0, 2, -1, 1, -1, 1
  ), c(4, 2, 2))
  psi_sigma2 = array(0, c(4, 2, 2))
  psi_nu2 = 3 * psi_max_bias
  elements = list(
    sigma2 = matrix(4, 2, 2),
    nu2 = matrix(9, 2, 2),
    psi_sigma2 = psi_sigma2,
    psi_nu2 = psi_nu2,
    riesz_rep = array(c(
      3, 0, 0, 0, 0, 3, 0, 0,
      2, 0, 1, 0, 0, 2, 0, 1
    ), c(4, 2, 2)),
    max_bias = matrix(6, 2, 2),
    psi_max_bias = psi_max_bias
  )
  list(
    all_coef = matrix(c(1, 2, 3, 6), 2),
    all_se = matrix(c(sqrt(1.25), sqrt(.5), 1, sqrt(2)), 2),
    psi = psi,
    coef_names = c("a", "b"),
    sensitivity_elements = elements
  )
}

single_sensitivity_framework = function() {
  psi = array(c(-1, -1, 1, 1), c(4, 1, 1))
  elements = list(
    sigma2 = matrix(1, 1, 1),
    nu2 = matrix(1, 1, 1),
    psi_sigma2 = array(0, c(4, 1, 1)),
    psi_nu2 = array(0, c(4, 1, 1)),
    riesz_rep = array(c(1, 0, 1, 0), c(4, 1, 1)),
    max_bias = matrix(1, 1, 1),
    psi_max_bias = array(0, c(4, 1, 1))
  )
  DoubleMLFramework$new(
    matrix(2, 1, 1), matrix(.5, 1, 1), psi, "theta",
    sensitivity_elements = elements
  )
}

test_that("framework sensitivity uses repetition-specific bounds and intervals", {
  obj = do.call(DoubleMLFramework$new, framework_sensitivity_fixture())
  returned = withVisible(obj$sensitivity_analysis(
    cf_y = .25, cf_d = .2, rho = 1, level = .8,
    null_hypothesis = c(a = 0, b = 1)
  ))
  expect_identical(returned$value, obj)
  expect_false(returned$visible)

  result = obj$sensitivity_params
  expect_named(result, c("theta", "se", "ci", "rv", "rva", "input"),
    ignore.order = FALSE)
  expect_equal(result$theta$lower, c(a = .5, b = 2.5))
  expect_equal(result$theta$upper, c(a = 3.5, b = 5.5))
  expect_equal(result$se$lower, sqrt(c(a = 1.17578125, b = 2.13671875)))
  expect_equal(result$se$upper, sqrt(c(a = 1.61328125, b = 2.38671875)))

  z = qnorm(.8)
  expected_lower = c(
    a = median(c(-.5 - z * sqrt(1.0703125), 1.5 - z * sqrt(.78125))),
    b = median(c(.5 - z * sqrt(.5078125), 4.5 - z * sqrt(1.765625)))
  )
  expected_upper = c(
    a = median(c(2.5 + z * sqrt(1.4453125), 4.5 + z * sqrt(1.28125))),
    b = median(c(3.5 + z * sqrt(.5078125), 7.5 + z * sqrt(2.265625)))
  )
  expect_equal(result$ci$lower, expected_lower)
  expect_equal(result$ci$upper, expected_upper)
  expect_identical(result$input$null_hypothesis, c(a = 0, b = 1))
})

test_that("framework sensitivity validates inputs atomically", {
  obj = single_sensitivity_framework()
  expect_identical(obj$sensitivity_params, NULL)
  expect_silent(obj$sensitivity_analysis(cf_y = 0, cf_d = 0, rho = -1, level = .5))
  expect_silent(obj$sensitivity_analysis(cf_y = 0, cf_d = 0, rho = 1, level = .5))
  obj$sensitivity_analysis(cf_y = .1, cf_d = .2, null_hypothesis = 0)
  before = obj$sensitivity_params

  for (value in list(-.1, 1, NA_real_, Inf, c(.1, .2), "0.1")) {
    expect_error(obj$sensitivity_analysis(cf_y = value), "cf_y")
    expect_identical(obj$sensitivity_params, before)
  }
  for (value in list(-.1, 1, NA_real_, Inf, c(.1, .2), "0.1")) {
    expect_error(obj$sensitivity_analysis(cf_d = value), "cf_d")
    expect_identical(obj$sensitivity_params, before)
  }
  for (value in list(-1.1, 1.1, NA_real_, Inf, c(0, 1), "1")) {
    expect_error(obj$sensitivity_analysis(rho = value), "rho")
    expect_identical(obj$sensitivity_params, before)
  }
  for (value in list(0, 1, -.1, NA_real_, Inf, c(.9, .95), "0.9")) {
    expect_error(obj$sensitivity_analysis(level = value), "level")
    expect_identical(obj$sensitivity_params, before)
  }
  for (value in list(numeric(), c(0, 1), NA_real_, Inf, "0")) {
    expect_error(obj$sensitivity_analysis(null_hypothesis = value), "null_hypothesis")
    expect_identical(obj$sensitivity_params, before)
  }
})

test_that("robustness values return the smallest attainable confounding share", {
  obj = single_sensitivity_framework()
  level = .8
  obj$sensitivity_analysis(level = level, null_hypothesis = 0)

  expected_rv = -2 + 2 * sqrt(2)
  ci_distance = 2 - qnorm(level) * .5
  expected_rva = (-ci_distance^2 + sqrt(ci_distance^4 + 4 * ci_distance^2)) / 2
  expect_equal(unname(obj$sensitivity_params$rv), expected_rv, tolerance = 1e-8)
  expect_equal(unname(obj$sensitivity_params$rva), expected_rva, tolerance = 1e-8)

  obj$sensitivity_analysis(level = level, null_hypothesis = 2.2)
  expect_gt(obj$sensitivity_params$rv, 0)
  expect_equal(unname(obj$sensitivity_params$rva), 0)
  obj$sensitivity_analysis(rho = 0, level = level, null_hypothesis = 0)
  expect_identical(unname(obj$sensitivity_params$rv), 1)
  expect_identical(unname(obj$sensitivity_params$rva), 1)
  obj$sensitivity_analysis(level = level, null_hypothesis = 2)
  expect_identical(unname(obj$sensitivity_params$rv), 0)
  expect_identical(unname(obj$sensitivity_params$rva), 0)
})

test_that("sensitivity summaries and fields are read-only", {
  obj = single_sensitivity_framework()
  expect_match(obj$sensitivity_summary, "Apply sensitivity_analysis")
  obj$sensitivity_analysis(cf_y = .1, cf_d = .2, rho = -.5, level = .9)
  expect_match(obj$sensitivity_summary, "Sensitivity Analysis")
  expect_match(obj$sensitivity_summary, "cf_y=0.1")
  expect_match(obj$sensitivity_summary, "RV")
  for (field in c("sensitivity_elements", "sensitivity_params", "sensitivity_summary")) {
    expect_error({
      obj[[field]] = NULL
    }, "can't set")
  }
})

test_that("generic transforms drop sensitivity unless given derived elements", {
  args = framework_sensitivity_fixture()
  obj = do.call(DoubleMLFramework$new, args)
  generic = obj$linear_transform(diag(2), c("a", "b"))
  expect_null(generic$sensitivity_elements)
  expect_error(generic$sensitivity_analysis(), "not propagated.*linear_transform")

  derived = obj$linear_transform(
    diag(2), c("a", "b"), sensitivity_elements = args$sensitivity_elements
  )
  expect_equal(derived$sensitivity_elements$max_bias, matrix(6, 2, 2,
    dimnames = list(c("a", "b"), NULL)))
  expect_null(derived$sensitivity_params)

  specific = DoubleMLFramework$new(
    args$all_coef, args$all_se, args$psi, args$coef_names,
    sensitivity_unavailable_reason = "specific reason"
  )
  expect_error(specific$linear_transform(diag(2))$sensitivity_analysis(), "specific reason")
})

test_that("framework concatenation stacks compatible marginal sensitivity", {
  make_one = function(name, shift) {
    psi = array(c(-1, -1, 1, 1), c(4, 1, 1))
    elements = list(
      sigma2 = matrix(1 + shift, 1, 1),
      nu2 = matrix(1, 1, 1),
      psi_sigma2 = array(0, c(4, 1, 1)),
      psi_nu2 = array(0, c(4, 1, 1)),
      riesz_rep = array(c(1, 0, 1, 0), c(4, 1, 1)),
      max_bias = matrix(sqrt(1 + shift), 1, 1),
      psi_max_bias = array(0, c(4, 1, 1))
    )
    DoubleMLFramework$new(
      matrix(shift, 1, 1), matrix(.5, 1, 1), psi, name,
      sensitivity_elements = elements
    )
  }
  combined = make_one("a", 0)$concat(make_one("b", 3))
  expect_equal(combined$sensitivity_elements$sigma2[, 1], c(a = 1, b = 4))
  expect_equal(combined$sensitivity_elements$max_bias[, 1], c(a = 1, b = 2))
  expect_equal(dim(combined$sensitivity_elements$psi_max_bias), c(4L, 2L, 1L))
  expect_null(combined$sensitivity_params)

  missing = DoubleMLFramework$new(
    matrix(0, 1, 1), matrix(.5, 1, 1),
    array(c(-1, -1, 1, 1), c(4, 1, 1)), "c",
    sensitivity_unavailable_reason = "missing complement"
  )
  unavailable = make_one("a", 0)$concat(missing)
  expect_null(unavailable$sensitivity_elements)
  expect_error(unavailable$sensitivity_analysis(), "missing complement")
})
