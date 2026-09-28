# Package classes and the deterministic IRM fixture are loaded by testthat.
# nolint start: object_usage_linter.
irm_sensitivity_framework = function(fixture, threshold) {
  irm = fixture$irm
  n_obs = irm$data$n_obs
  n_rep = irm$n_rep
  elements = list(
    sigma2 = matrix(0, 1L, n_rep),
    nu2 = matrix(0, 1L, n_rep),
    psi_sigma2 = array(0, c(n_obs, 1L, n_rep)),
    psi_nu2 = array(0, c(n_obs, 1L, n_rep)),
    riesz_rep = array(0, c(n_obs, 1L, n_rep)),
    max_bias = matrix(0, 1L, n_rep),
    psi_max_bias = array(0, c(n_obs, 1L, n_rep))
  )
  d = irm$data$data$d
  y = irm$data$data$y
  for (i_rep in seq_len(n_rep)) {
    oracle = irm_sensitivity_oracle(
      y, d,
      irm$predictions$ml_g0[, i_rep, 1],
      irm$predictions$ml_g1[, i_rep, 1],
      irm$predictions$ml_m[, i_rep, 1], threshold
    )
    for (key in c("sigma2", "nu2", "max_bias")) {
      elements[[key]][1, i_rep] = oracle[[key]]
    }
    for (key in c("psi_sigma2", "psi_nu2", "riesz_rep", "psi_max_bias")) {
      elements[[key]][, 1, i_rep] = oracle[[key]]
    }
  }
  DoubleMLFramework$new(
    all_coef = unname(irm$all_coef), all_se = unname(irm$all_se),
    psi = aperm(irm$psi, c(1, 3, 2)), coef_names = "ATE",
    smpls = fixture$smpls, sensitivity_elements = elements
  )
}

for (threshold in c(0, .05)) {
  test_that(paste("binary APOS sensitivity reduces to IRM at threshold", threshold), {
    fixture = apo_irm_equivalence_fixture(threshold)
    actual = fixture$apos$causal_contrast(reference = 0)
    oracle = irm_sensitivity_framework(fixture, threshold)
    tolerance = 1e-12

    expect_equal(unname(actual$all_coef), unname(oracle$all_coef), tolerance = tolerance)
    expect_equal(unname(actual$psi), unname(oracle$psi), tolerance = tolerance)
    expect_equal(unname(actual$all_se), unname(oracle$all_se), tolerance = tolerance)
    for (key in names(oracle$sensitivity_elements)) {
      expect_equal(
        unname(actual$sensitivity_elements[[key]]),
        unname(oracle$sensitivity_elements[[key]]), tolerance = tolerance
      )
    }

    d = fixture$irm$data$data$d
    for (i_rep in 1:2) {
      p = fixture$irm$predictions$ml_m[, i_rep, 1]
      p = pmin(pmax(p, threshold), 1 - threshold)
      expect_equal(
        fixture$apo1$sensitivity_elements$riesz_rep[, 1, i_rep],
        d / p, tolerance = tolerance
      )
      expect_equal(
        fixture$apo0$sensitivity_elements$riesz_rep[, 1, i_rep],
        (1 - d) / (1 - p), tolerance = tolerance
      )
    }

    scenario = list(
      cf_y = .12, cf_d = .18, rho = 1,
      level = .9, null_hypothesis = 0
    )
    do.call(actual$sensitivity_analysis, scenario)
    do.call(oracle$sensitivity_analysis, scenario)
    for (key in c("theta", "se", "ci")) {
      for (side in c("lower", "upper")) {
        expect_equal(
          unname(actual$sensitivity_params[[key]][[side]]),
          unname(oracle$sensitivity_params[[key]][[side]]), tolerance = tolerance
        )
      }
    }
    expect_equal(unname(actual$sensitivity_params$rv), unname(oracle$sensitivity_params$rv),
      tolerance = 1e-8
    )
    expect_equal(unname(actual$sensitivity_params$rva), unname(oracle$sensitivity_params$rva),
      tolerance = 1e-8
    )

    rv = unname(actual$sensitivity_params$rv)
    rva = unname(actual$sensitivity_params$rva)
    actual$sensitivity_analysis(rv, rv, rho = 1, level = .9, null_hypothesis = 0)
    expect_equal(unname(actual$sensitivity_params$theta$lower), 0, tolerance = 1e-7)
    actual$sensitivity_analysis(rva, rva, rho = 1, level = .9, null_hypothesis = 0)
    expect_equal(unname(actual$sensitivity_params$ci$lower), 0, tolerance = 1e-7)
  })
}
# nolint end: object_usage_linter.
