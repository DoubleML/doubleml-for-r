test_that("APO labels preserve categorical identity", {
  d = c(10, 2, 7, 10)
  expect_identical(get_apo_levels(d), c(10, 2, 7))
  expect_identical(make_apo_indicator(d, 2), c(0L, 1L, 0L, 0L))
  expect_identical(make_apo_indicator(c("filter", "control"), "control"), c(0L, 1L))
  expect_identical(
    make_apo_indicator(factor(c("filter", "control"), levels = c("control", "filter")), "control"),
    c(0L, 1L)
  )
  expect_error(make_apo_indicator(d, "2"), "type")
  expect_error(make_apo_indicator(c("10", "2"), 2), "type")
  expect_error(make_apo_indicator(d, c(10, 2)), "one")
})

test_that("APOS inferred levels have stable order", {
  d = factor(c("filter", "control"), levels = c("control", "filter", "unused"))
  expect_identical(get_apo_levels(d), c("control", "filter"))
  expect_identical(get_apo_levels(c("filter", "control", "filter")), c("filter", "control"))
  expect_identical(get_apo_levels(c(10, 2, 7, 2)), c(10, 2, 7))
  expect_identical(get_apo_levels(d, c("filter", "control")), c("filter", "control"))
  expect_identical(get_apo_levels(c(10, 2, 7), c(7, 10)), c(7, 10))
})

test_that("invalid APO labels fail early", {
  expect_error(get_apo_levels(c("control", "filter"), "absent"), "absent")
  expect_error(
    get_apo_levels(factor("control", levels = c("control", "unused")), "unused"),
    "unused"
  )
  expect_error(get_apo_levels(c(10, 2), c(10, 10)), "duplicat.*10")
  expect_error(get_apo_levels(c("control", NA_character_)), "NA")
  expect_error(get_apo_levels(c(10, NaN)), "NaN")
  expect_error(get_apo_levels(c("control", "")), "empty")
  expect_error(get_apo_levels(c("control", "filter"), NA_character_), "NA")
  expect_error(get_apo_levels(c(10, 2), NaN), "NaN")
  expect_error(get_apo_levels(c("control", "filter"), ""), "empty")
  expect_error(get_apo_levels(character()), "empty")
  expect_error(get_apo_levels(c(10, 2), numeric()), "empty")
})

test_that("APO folds report local support failures", {
  indicator = c(1L, 0L, 1L, 0L)
  valid = list(train_ids = list(c(1L, 2L), c(3L, 4L)), test_ids = list(c(3L, 4L), c(1L, 2L)))
  expect_silent(validate_apo_sample_splitting(list(valid, valid), indicator, "filter"))
  bad = valid
  bad$train_ids[[2]] = c(2L, 4L)
  expect_error(
    validate_apo_sample_splitting(list(valid, bad), indicator, "filter"),
    "treatment filter, repetition 2, fold 2.*outcome"
  )
  bad$train_ids[[2]] = c(1L, 3L)
  expect_error(
    validate_apo_sample_splitting(list(valid, bad), indicator, "filter"),
    "treatment filter, repetition 2, fold 2.*class 0"
  )
})

test_that("APO external predictions validate before use", {
  predictions = list(ml_g = matrix(1:6, 3, 2), ml_m = matrix(c(0, 1, .5, .1, .3, .7), 3, 2))
  expect_identical(validate_apo_external_predictions(predictions, 3, 2), predictions)
  expect_null(validate_apo_external_predictions(NULL, 3, 2))
  expect_error(validate_apo_external_predictions(predictions["ml_g"], 3, 2), "ml_m")
  bad = predictions
  bad$ml_g = t(bad$ml_g)
  expect_error(validate_apo_external_predictions(bad, 3, 2), "ml_g.*3.*2")
  bad$ml_g = matrix(1, 3, 1)
  expect_error(validate_apo_external_predictions(bad, 3, 2), "ml_g")
  bad$ml_g = matrix(NA_real_, 3, 2)
  expect_error(validate_apo_external_predictions(bad, 3, 2), "ml_g.*finite")
  bad$ml_g = predictions$ml_g
  bad$ml_m[1, 1] = Inf
  expect_error(validate_apo_external_predictions(bad, 3, 2), "ml_m.*finite")
  bad$ml_m[1, 1] = 1.1
  expect_error(validate_apo_external_predictions(bad, 3, 2), "ml_m.*\\[0, 1\\]")
  bad$ml_m[1, 1] = -.1
  expect_error(validate_apo_external_predictions(bad, 3, 2), "ml_m.*\\[0, 1\\]")
})

test_that("APO fixtures supply reproducible data and independent score checks", {
  fixture = make_apo_data(24, c(10, 2, 7), seed = 19)
  expect_identical(fixture$df, make_apo_data(24, c(10, 2, 7), seed = 19)$df)
  expect_s3_class(fixture$dml_data, "DoubleMLData")
  expect_identical(unique(fixture$df$d), c(10, 2, 7))
  expect_equal(dim(fixture$conditional_means), c(24L, 3L))
  expect_equal(fixture$conditional_means[, 1], 1 + fixture$df$x)
  expect_equal(fixture$conditional_means[, 2], 2 + 2 * fixture$df$x)
  expect_equal(fixture$conditional_means[, 3], 3 + 3 * fixture$df$x)
  for (level in c(10, 2, 7)) {
    expect_silent(validate_apo_sample_splitting(
      fixture$smpls, make_apo_indicator(fixture$df$d, level), level
    ))
  }
  score = apo_score_from_predictions(c(3, 8, 5), c("a", "b", "a"), "a", c(1, 2, 3), c(.5, .5, .25))
  expect_equal(score$psi_b, c(5, 2, 11))
  expect_equal(score$theta, 6)
  expect_equal(score$psi, c(-1, -4, 5))
  expect_equal(score$se, sqrt(14 / 3))
})
