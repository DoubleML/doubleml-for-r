make_sensitivity_helper_fixture = function() {
  psi_sigma2 = array(c(-3, -1, 1, 3), c(4, 1, 1))
  psi_nu2 = array(c(-1, 1, -1, 1), c(4, 1, 1))
  list(
    sigma2 = matrix(4, 1, 1),
    nu2 = matrix(9, 1, 1),
    psi_sigma2 = psi_sigma2,
    psi_nu2 = psi_nu2,
    riesz_rep = array(c(2, 0, 3, 0), c(4, 1, 1)),
    max_bias = matrix(6, 1, 1),
    psi_max_bias = array(c(-31, -5, 5, 31) / 12, c(4, 1, 1))
  )
}

test_that("sensitivity bias helper implements the orthogonal product rule", {
  fixture = make_sensitivity_helper_fixture()
  result = compute_sensitivity_bias(
    fixture$sigma2, fixture$nu2,
    fixture$psi_sigma2, fixture$psi_nu2
  )

  expect_equal(result$max_bias, matrix(6, 1, 1))
  expect_equal(result$psi_max_bias, array(c(-31, -5, 5, 31) / 12, c(4, 1, 1)))
})

test_that("sensitivity elements are validated and canonically named", {
  fixture = make_sensitivity_helper_fixture()
  result = validate_sensitivity_elements(
    fixture, n_obs = 4, n_coef = 1, n_rep = 1,
    coef_names = "filter", rep_names = "rep1"
  )

  expect_named(result, c(
    "sigma2", "nu2", "psi_sigma2", "psi_nu2", "riesz_rep",
    "max_bias", "psi_max_bias"
  ), ignore.order = FALSE)
  expect_identical(dimnames(result$sigma2), list("filter", "rep1"))
  expect_identical(dimnames(result$nu2), list("filter", "rep1"))
  for (key in c("psi_sigma2", "psi_nu2", "riesz_rep", "psi_max_bias")) {
    expect_identical(dimnames(result[[key]]), list(NULL, "filter", "rep1"))
  }
  expect_null(dimnames(fixture$sigma2))
  expect_null(dimnames(fixture$psi_sigma2))
})

test_that("sensitivity elements reject malformed or inconsistent inputs", {
  fixture = make_sensitivity_helper_fixture()
  validate = function(x) {
    validate_sensitivity_elements(x, 4, 1, 1, "filter", "rep1")
  }

  expect_error(validate(fixture[names(fixture) != "nu2"]), "exactly.*nu2")
  expect_error(validate(c(fixture, extra = list(1))), "exactly")

  bad = fixture
  bad$sigma2 = array(4, c(1, 1, 1))
  expect_error(validate(bad), "sigma2.*matrix")
  bad = fixture
  bad$psi_sigma2 = array(0, c(3, 1, 1))
  expect_error(validate(bad), "psi_sigma2.*4.*1.*1")
  bad = fixture
  bad$riesz_rep[1] = Inf
  expect_error(validate(bad), "riesz_rep.*finite")

  for (key in c("sigma2", "nu2")) {
    bad = fixture
    bad[[key]][1] = 0
    expect_error(validate(bad), paste0(key, ".*positive"))
  }
  for (key in c("psi_sigma2", "psi_nu2", "psi_max_bias")) {
    bad = fixture
    bad[[key]][1] = bad[[key]][1] + 1
    expect_error(validate(bad), paste0(key, ".*centered"))
  }

  bad = fixture
  bad$max_bias[1] = 7
  expect_error(validate(bad), "max_bias.*inconsistent")
  bad = fixture
  bad$psi_max_bias = -bad$psi_max_bias
  expect_error(validate(bad), "psi_max_bias.*inconsistent")
})
