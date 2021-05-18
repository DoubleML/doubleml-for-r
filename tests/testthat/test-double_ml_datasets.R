context("Unit tests for datasets functionalities")

test_cases = expand.grid(
  return_type = c(
    "data.frame", "data.table",
    "matrix", "DoubleMLData"),
  polynomial_features = c(TRUE, FALSE),
  instrument = c(TRUE, FALSE),
  stringsAsFactors = FALSE)

test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

testthat::skip_on_cran()
patrick::with_parameters_test_that("Unit tests for datasets functionalities:",
  .cases = test_cases, {
    n_obs = 100

    # Test CCDDHNR2018
    if (return_type != "matrix") {
      df = make_plr_CCDDHNR2018(return_type = return_type)
      expect_is(df, paste0(return_type))
    } else {
      df = make_plr_CCDDHNR2018(return_type = return_type)
      expect_is(df, "list")
      expect_is(df$X, "matrix")
      expect_is(df$y, "matrix")
      expect_is(df$d, "matrix")
    }

    # Test CHS2015
    if (return_type != "matrix") {
      df = make_pliv_CHS2015(n_obs, return_type = return_type)
      expect_is(df, paste0(return_type))
    } else {
      df = make_pliv_CHS2015(n_obs, return_type = return_type)
      expect_is(df, "list")
      expect_is(df$X, "matrix")
      expect_is(df$y, "matrix")
      expect_is(df$d, "matrix")
      expect_is(df$z, "matrix")
    }

    # Test IRM
    if (return_type != "matrix") {
      df = make_irm_data(return_type = return_type)
      expect_is(df, paste0(return_type))
    } else {
      df = make_irm_data(return_type = return_type)
      expect_is(df, "list")
      expect_is(df$X, "matrix")
      expect_is(df$y, "matrix")
      expect_is(df$d, "matrix")
    }

    # Test IIVM
    if (return_type != "matrix") {
      df = make_iivm_data(return_type = return_type)
      expect_is(df, paste0(return_type))
    } else {
      df = make_iivm_data(return_type = return_type)
      expect_is(df, "list")
      expect_is(df$X, "matrix")
      expect_is(df$y, "matrix")
      expect_is(df$d, "matrix")
      expect_is(df$z, "matrix")
    }

    # Test IRM
    if (return_type != "matrix") {
      df = make_plr_turrell2018(return_type = return_type)
      expect_is(df, paste0(return_type))
    } else {
      df = make_plr_turrell2018(return_type = return_type)
      expect_is(df, "list")
      expect_is(df$X, "matrix")
      expect_is(df$y, "matrix")
      expect_is(df$d, "matrix")
    }

    # Test fetch_401k
    if (return_type != "matrix") {
      df = fetch_401k(
        return_type = return_type, polynomial_features = polynomial_features,
        instrument = instrument)
      expect_is(df, paste0(return_type))
    }

    # Test fetch_bonus
    if (return_type != "matrix") {
      df = fetch_bonus(return_type = return_type, polynomial_features = polynomial_features)
      expect_is(df, paste0(return_type))
    }
  }
)
