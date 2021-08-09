context("Unit tests for the method set_sample_splitting of class DoubleML")

set.seed(3141)
dml_data = make_plr_CCDDHNR2018(n_obs = 10)
ml_g = lrn("regr.ranger")
ml_m = ml_g$clone()
dml_plr = DoubleMLPLR$new(dml_data, ml_g, ml_m, n_folds = 7, n_rep = 8)

test_that("Unit tests for the method set_sample_splitting of class DoubleML", {

  # simple sample splitting with two folds and without cross-fitting
  smpls = list(
    train_ids = list(c(1, 2, 3, 4, 5)),
    test_ids = list(c(6, 7, 8, 9, 10)))
  dml_plr$set_sample_splitting(smpls)

  expect_equal(dml_plr$n_folds, 2)
  expect_equal(dml_plr$n_rep, 1)
  expect_equal(dml_plr$apply_cross_fitting, FALSE)
  expect_equal(list(smpls), dml_plr$smpls)

  # no cross-fitting, no sample-splitting
  smpls = list(
    train_ids = list(seq(10)),
    test_ids = list(seq(10)))
  dml_plr$set_sample_splitting(smpls)

  expect_equal(dml_plr$n_folds, 1)
  expect_equal(dml_plr$n_rep, 1)
  expect_equal(dml_plr$apply_cross_fitting, FALSE)
  expect_equal(list(smpls), dml_plr$smpls)

  smpls = list(
    train_ids = list(c(1, 2, 3, 4, 5)),
    test_ids = list(c(6, 7)),
    ids = list(c(8, 9, 10)))
  msg = paste(
    "Assertion on 'names\\(smpl\\)' failed: Must be equal to set",
    "\\{'train_ids','test_ids'\\}, but is",
    "\\{'train_ids','test_ids','ids'\\}.")
  expect_error(dml_plr$set_sample_splitting(smpls),
    regexp = msg)

  # sample splitting with two folds and cross-fitting but no repeated cross-fitting
  smpls = list(
    train_ids = list(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10)),
    test_ids = list(c(6, 7, 8, 9, 10), c(1, 2, 3, 4, 5)))
  dml_plr$set_sample_splitting(smpls)

  expect_equal(dml_plr$n_folds, 2)
  expect_equal(dml_plr$n_rep, 1)
  expect_equal(dml_plr$apply_cross_fitting, TRUE)
  expect_equal(list(smpls), dml_plr$smpls)

  # sample splitting with two folds and cross-fitting but no repeated cross-fitting
  smpls = list(list(
    train_ids = list(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10)),
    test_ids = list(c(6, 7, 8, 9, 10), c(1, 2, 3, 4, 5))))
  dml_plr$set_sample_splitting(smpls)

  expect_equal(dml_plr$n_folds, 2)
  expect_equal(dml_plr$n_rep, 1)
  expect_equal(dml_plr$apply_cross_fitting, TRUE)
  expect_equal(smpls, dml_plr$smpls)

  smpls = list(
    train_ids = list(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10)),
    test_ids = list(c(6, 7, 8, 9, 10), c(1, 2), c(3, 4, 5)))
  msg = "Number of folds for train and test samples do not match."
  expect_error(dml_plr$set_sample_splitting(smpls),
    regexp = msg)

  # simple sample splitting with two folds and without cross-fitting
  smpls = list(list(
    train_ids = list(c(1, 2, 3, 4, 5)),
    test_ids = list(c(6, 7, 8, 9, 10))))
  dml_plr$set_sample_splitting(smpls)

  expect_equal(dml_plr$n_folds, 2)
  expect_equal(dml_plr$n_rep, 1)
  expect_equal(dml_plr$apply_cross_fitting, FALSE)
  expect_equal(smpls, dml_plr$smpls)

  # sample splitting with cross-fitting and two folds that do not form a partition
  smpls = list(
    train_ids = list(c(1, 2, 3, 4, 5), c(6, 7, 8, 9)),
    test_ids = list(c(6, 7, 8, 9, 10), c(1, 2, 3, 4, 5, 10)))
  msg = paste(
    "Invalid partition provided. Tuples \\(train_ids, test_ids\\)",
    "for more than one fold provided that don't form a partition.")
  expect_error(dml_plr$set_sample_splitting(smpls),
    regexp = msg)

  # sample splitting with cross-fitting and two folds that do not form a partition
  smpls = list(list(
    train_ids = list(c(1, 2, 3, 4, 5), c(6, 7, 8, 9)),
    test_ids = list(c(6, 7, 8, 9, 10), c(1, 2, 3, 4, 5, 10))))
  msg = paste(
    "Invalid partition provided. Tuples \\(train_ids, test_ids\\)",
    "for more than one fold provided that don't form a partition.")
  expect_error(dml_plr$set_sample_splitting(smpls),
    regexp = msg)

  # sample splitting with two folds and repeated cross-fitting with n_rep = 2
  smpls = list(
    list(
      train_ids = list(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10)),
      test_ids = list(c(6, 7, 8, 9, 10), c(1, 2, 3, 4, 5))),
    list(
      train_ids = list(c(1, 3, 5, 7, 9), c(2, 4, 6, 8, 10)),
      test_ids = list(c(2, 4, 6, 8, 10), c(1, 3, 5, 7, 9))))
  dml_plr$set_sample_splitting(smpls)

  expect_equal(dml_plr$n_folds, 2)
  expect_equal(dml_plr$n_rep, 2)
  expect_equal(dml_plr$apply_cross_fitting, TRUE)
  expect_equal(smpls, dml_plr$smpls)

  smpls = list(
    list(
      train_ids = list(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10)),
      test_ids = list(c(6, 7, 8, 9, 10), c(1, 2, 3, 4, 5))),
    list(
      train_ids = list(
        c(1, 3, 5, 7, 9),
        c(2, 4, 6, 7, 8, 9, 10),
        c(1, 2, 3, 5, 4, 6, 8, 10)),
      test_ids = list(
        c(2, 4, 6, 8, 10),
        c(1, 3, 5),
        c(7, 9))))
  msg = "Different number of folds for repeated cross-fitting."
  expect_error(dml_plr$set_sample_splitting(smpls),
    regexp = msg)

  # repeated no-cross-fitting
  smpls = list(
    list(
      train_ids = list(c(1, 2, 3, 4, 5)),
      test_ids = list(c(6, 7, 8, 9, 10))),
    list(
      train_ids = list(c(2, 4, 6, 8, 10)),
      test_ids = list(c(1, 3, 5, 7, 9))))
  msg = "Repeated sample splitting without cross-fitting not implemented."
  expect_error(dml_plr$set_sample_splitting(smpls),
    regexp = msg)

  # no-cross-fitting
  smpls = list(list(
    train_ids = list(c(1, 2, 3, 4, 5)),
    test_ids = list(c(6, 7, 8, 9, 10))))
  dml_plr$set_sample_splitting(smpls)

  expect_equal(dml_plr$n_folds, 2)
  expect_equal(dml_plr$n_rep, 1)
  expect_equal(dml_plr$apply_cross_fitting, FALSE)
  expect_equal(smpls, dml_plr$smpls)
}
)

assert_resampling_pars = function(dml_obj0, dml_obj1) {
  expect_equal(dml_obj0$n_folds, dml_obj1$n_folds)
  expect_equal(dml_obj0$n_rep, dml_obj1$n_rep)
  expect_equal(dml_obj0$apply_cross_fitting, dml_obj1$apply_cross_fitting)
  expect_equal(dml_obj0$smpls, dml_obj1$smpls)
}

test_that("Unit tests for the method set_sample_splitting of class DoubleML (draw vs set)", {
  set.seed(3141)
  dml_plr_set = DoubleMLPLR$new(dml_data, ml_g, ml_m, n_folds = 7, n_rep = 8)

  dml_plr_drawn = DoubleMLPLR$new(dml_data, ml_g, ml_m,
    n_folds = 1, n_rep = 1, apply_cross_fitting = FALSE)
  dml_plr_set$set_sample_splitting(dml_plr_drawn$smpls)
  assert_resampling_pars(dml_plr_drawn, dml_plr_set)
  dml_plr_set$set_sample_splitting(dml_plr_drawn$smpls[[1]])
  assert_resampling_pars(dml_plr_drawn, dml_plr_set)

  dml_plr_drawn = DoubleMLPLR$new(dml_data, ml_g, ml_m,
    n_folds = 2, n_rep = 1, apply_cross_fitting = FALSE)
  dml_plr_set$set_sample_splitting(dml_plr_drawn$smpls)
  assert_resampling_pars(dml_plr_drawn, dml_plr_set)
  dml_plr_set$set_sample_splitting(dml_plr_drawn$smpls[[1]])
  assert_resampling_pars(dml_plr_drawn, dml_plr_set)

  dml_plr_drawn = DoubleMLPLR$new(dml_data, ml_g, ml_m,
    n_folds = 2, n_rep = 1, apply_cross_fitting = TRUE)
  dml_plr_set$set_sample_splitting(dml_plr_drawn$smpls)
  assert_resampling_pars(dml_plr_drawn, dml_plr_set)
  dml_plr_set$set_sample_splitting(dml_plr_drawn$smpls[[1]])
  assert_resampling_pars(dml_plr_drawn, dml_plr_set)

  dml_plr_drawn = DoubleMLPLR$new(dml_data, ml_g, ml_m,
    n_folds = 5, n_rep = 1, apply_cross_fitting = TRUE)
  dml_plr_set$set_sample_splitting(dml_plr_drawn$smpls)
  assert_resampling_pars(dml_plr_drawn, dml_plr_set)
  dml_plr_set$set_sample_splitting(dml_plr_drawn$smpls[[1]])
  assert_resampling_pars(dml_plr_drawn, dml_plr_set)

  dml_plr_drawn = DoubleMLPLR$new(dml_data, ml_g, ml_m,
    n_folds = 5, n_rep = 3, apply_cross_fitting = TRUE)
  dml_plr_set$set_sample_splitting(dml_plr_drawn$smpls)
  assert_resampling_pars(dml_plr_drawn, dml_plr_set)
}
)

test_that("Unit tests for the method set_sample_splitting of class DoubleML (invalid sets)", {
  smpls = list(
    list(
      train_ids = list(c(1, 2.2, 3, 4, 5), c(6, 7, 8, 9, 10)),
      test_ids = list(c(6, 7, 8, 9, 10), c(1, 2, 3, 4, 5))),
    list(
      train_ids = list(c(1, 3, 5, 7, 9), c(2, 4, 6, 8, 10)),
      test_ids = list(c(2, 4, 6, 8, 10), c(1, 3, 5, 7, 9))))
  msg = paste(
    "Assertion on 'train_ids' failed: Must be a subset of",
    "\\{'1','2','3','4','5','6','7','8','9','10'\\},",
    "but is \\{'1','2.2','3','4','5'\\}.")
  expect_error(dml_plr$set_sample_splitting(smpls),
    regexp = msg)

  smpls = list(
    list(
      train_ids = list(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10)),
      test_ids = list(c(6, 7, 8, 9, 10), c(1, 2, 3, 4, 5))),
    list(
      train_ids = list(c(1, 3, 5, 7, 9), c(2, 4.5, 6, 8, 10)),
      test_ids = list(c(2, 4, 6, 8, 10), c(1, 3, 5, 7, 9))))
  msg = paste(
    "Assertion on 'train_ids' failed: Must be a subset of",
    "\\{'1','2','3','4','5','6','7','8','9','10'\\},",
    "but is \\{'2','4.5','6','8','10'\\}.")
  expect_error(dml_plr$set_sample_splitting(smpls),
    regexp = msg)

  smpls = list(
    list(
      train_ids = list(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10)),
      test_ids = list(c(6, 7, 8, 9, 10), c(1, 2, 3, 4, 5))),
    list(
      train_ids = list(c(1, 3, 4, 5, 7, 9), c(2, 4, 6, 8, 10)),
      test_ids = list(c(2, 4, 6, 8, 10), c(1, 3, 5, 7, 9))))
  msg = paste(
    "Assertion on 'smpl\\$train_ids\\[\\[i_fold\\]\\]' failed:",
    "Must be disjunct from set \\{'2','4','6','8','10'\\},",
    "but has \\{'4'\\}.")
  expect_error(dml_plr$set_sample_splitting(smpls),
    regexp = msg)

  smpls = list(
    list(
      train_ids = list(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10)),
      test_ids = list(c(6, 7, 8, 8, 9, 10), c(1, 2, 3, 4, 5))),
    list(
      train_ids = list(c(1, 3, 5, 7, 9), c(2, 4, 6, 8, 10)),
      test_ids = list(c(2, 4, 6, 8, 10), c(1, 3, 5, 7, 9))))
  msg = paste(
    "Assertion on 'test_ids' failed:",
    "Contains duplicated values, position 4.")
  expect_error(dml_plr$set_sample_splitting(smpls),
    regexp = msg)

  smpls = list(
    list(
      train_ids = list(c(1, 2, 3, 4, 5), c(6, 6, 7, 8, 9, 10)),
      test_ids = list(c(6, 7, 8, 9, 10), c(1, 2, 3, 4, 5))),
    list(
      train_ids = list(c(1, 3, 5, 7, 9), c(2, 4, 6, 8, 10)),
      test_ids = list(c(2, 4, 6, 8, 10), c(1, 3, 5, 7, 9))))
  msg = paste(
    "Assertion on 'train_ids' failed:",
    "Contains duplicated values, position 2.")
  expect_error(dml_plr$set_sample_splitting(smpls),
    regexp = msg)

  smpls = list(
    list(
      train_ids = list(c(1, 2, 3, 4, 5, 20), c(6, 7, 8, 9, 10)),
      test_ids = list(c(6, 7, 8, 9, 10), c(1, 2, 3, 4, 5))),
    list(
      train_ids = list(c(1, 3, 5, 7, 9), c(2, 4, 6, 8, 10)),
      test_ids = list(c(2, 4, 6, 8, 10), c(1, 3, 5, 7, 9))))
  msg = paste(
    "Assertion on 'train_ids' failed: Must be a subset of",
    "\\{'1','2','3','4','5','6','7','8','9','10'\\},",
    "but is \\{'1','2','3','4','5','20'\\}.")
  expect_error(dml_plr$set_sample_splitting(smpls),
    regexp = msg)

  smpls = list(
    list(
      train_ids = list(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10)),
      test_ids = list(c(6, -7, 8, 9, 10), c(1, 2, 3, 4, 5))),
    list(
      train_ids = list(c(1, 3, 5, 7, 9), c(2, 4, 6, 8, 10)),
      test_ids = list(c(2, 4, 6, 8, 10), c(1, 3, 5, 7, 9))))
  msg = paste(
    "Assertion on 'test_ids' failed: Must be a subset of",
    "\\{'1','2','3','4','5','6','7','8','9','10'\\},",
    "but is \\{'6','-7','8','9','10'\\}.")
  expect_error(dml_plr$set_sample_splitting(smpls),
    regexp = msg)
}
)
