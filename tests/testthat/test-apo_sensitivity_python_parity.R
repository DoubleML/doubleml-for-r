# Generated with DoubleML Python 0.11.4 at official repository commit
# 96e32c9517812a8b8875dd74956704d7998f6510:
# uv run --python 3.12 --with doubleml==0.11.4 python <temporary-generator.py>
# The CSV stores only deterministic inputs and Python's numerical outputs.
# It uses max_bias = sqrt(sigma2 * nu2) and
# psi_max_bias = (sigma2 * psi_nu2 + nu2 * psi_sigma2) / (2 * max_bias).

test_that("sensitivity Python parity matches DoubleML 0.11.4", {
  fixture = read.csv(
    testthat::test_path("fixtures", "apo-sensitivity-python-0.11.4.csv"),
    stringsAsFactors = FALSE
  )
  input = fixture[fixture$section == "input", ]
  scenario = fixture[fixture$section == "scenario", ]
  expected = fixture[fixture$section == "expected", ]
  n_obs = max(input$obs)
  n_rep = max(input$rep)

  scalar_input = function(name) {
    rows = input[input$name == name, ]
    matrix(rows$value[order(rows$rep)], nrow = 1L)
  }
  influence_input = function(name) {
    rows = input[input$name == name, ]
    value = array(0, c(n_obs, 1L, n_rep))
    for (i in seq_len(nrow(rows))) value[rows$obs[i], 1L, rows$rep[i]] = rows$value[i]
    value
  }
  scenario_value = function(name) scenario$value[scenario$name == name]
  expected_value = function(name) expected$value[expected$name == name]

  elements = list(
    sigma2 = scalar_input("sigma2"),
    nu2 = scalar_input("nu2"),
    psi_sigma2 = influence_input("psi_sigma2"),
    psi_nu2 = influence_input("psi_nu2"),
    riesz_rep = influence_input("riesz_rep"),
    max_bias = scalar_input("max_bias"),
    psi_max_bias = influence_input("psi_max_bias")
  )
  framework = DoubleMLFramework$new(
    all_coef = scalar_input("all_coef"),
    all_se = scalar_input("all_se"),
    psi = influence_input("psi"),
    coef_names = "effect",
    sensitivity_elements = elements
  )
  framework$sensitivity_analysis(
    cf_y = scenario_value("cf_y"),
    cf_d = scenario_value("cf_d"),
    rho = scenario_value("rho"),
    level = scenario_value("level"),
    null_hypothesis = scenario_value("null_hypothesis")
  )

  expect_equal(unname(framework$coef), expected_value("coef"), tolerance = 1e-14)
  expect_equal(unname(framework$se), expected_value("se"), tolerance = 1e-14)
  for (quantity in c("theta", "se", "ci")) {
    for (side in c("lower", "upper")) {
      name = paste(quantity, side, sep = "_")
      expect_equal(
        unname(framework$sensitivity_params[[quantity]][[side]]),
        expected_value(name), tolerance = 1e-12
      )
    }
  }
  # Python uses bounded minimization; R uses a root solve for the same crossing.
  expect_equal(
    unname(framework$sensitivity_params$rv), expected_value("rv"), tolerance = 1e-5
  )
  expect_equal(
    unname(framework$sensitivity_params$rva), expected_value("rva"), tolerance = 1e-5
  )

  rv = unname(framework$sensitivity_params$rv)
  rva = unname(framework$sensitivity_params$rva)
  framework$sensitivity_analysis(
    rv, rv, rho = scenario_value("rho"), level = scenario_value("level"),
    null_hypothesis = scenario_value("null_hypothesis")
  )
  expect_equal(unname(framework$sensitivity_params$theta$lower), 0, tolerance = 1e-8)
  framework$sensitivity_analysis(
    rva, rva, rho = scenario_value("rho"), level = scenario_value("level"),
    null_hypothesis = scenario_value("null_hypothesis")
  )
  expect_equal(unname(framework$sensitivity_params$ci$lower), 0, tolerance = 1e-8)
})
