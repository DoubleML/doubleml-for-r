context("Unit tests for active bindings of class DoubleML")

test_that("Not setable fields", {
  set.seed(3141)
  dml_data = make_plr_CCDDHNR2018(n_obs = 101)
  ml_g = lrn("regr.ranger")
  ml_m = ml_g$clone()
  dml_plr = DoubleMLPLR$new(dml_data, ml_g, ml_m)

  msg = "can't set field all_coef"
  expect_error(dml_plr$all_coef <- 5,
    regexp = msg)
  msg = "can't set field all_dml1_coef"
  expect_error(dml_plr$all_dml1_coef <- 5,
    regexp = msg)
  msg = "can't set field all_se"
  expect_error(dml_plr$all_se <- 5,
    regexp = msg)
  msg = "can't set field apply_cross_fitting"
  expect_error(dml_plr$apply_cross_fitting <- FALSE,
    regexp = msg)
  msg = "can't set field boot_coef"
  expect_error(dml_plr$boot_coef <- 5,
    regexp = msg)
  msg = "can't set field boot_t_stat"
  expect_error(dml_plr$boot_t_stat <- 5,
    regexp = msg)
  msg = "can't set field coef"
  expect_error(dml_plr$coef <- 5,
    regexp = msg)
  msg = "can't set field data"
  expect_error(dml_plr$data <- "abc",
    regexp = msg)
  msg = "can't set field dml_procedure"
  expect_error(dml_plr$dml_procedure <- "abc",
    regexp = msg)
  msg = "can't set field draw_sample_splitting"
  expect_error(dml_plr$draw_sample_splitting <- FALSE,
    regexp = msg)
  msg = "can't set field learner"
  expect_error(dml_plr$learner <- "abc",
    regexp = msg)
  msg = "can't set field models"
  expect_error(dml_plr$models <- 5,
    regexp = msg)
  msg = "can't set field n_folds"
  expect_error(dml_plr$n_folds <- 5,
    regexp = msg)
  msg = "can't set field n_rep"
  expect_error(dml_plr$n_rep <- 5,
    regexp = msg)
  msg = "can't set field params"
  expect_error(dml_plr$params <- 5,
    regexp = msg)
  msg = "can't set field psi"
  expect_error(dml_plr$psi <- 5,
    regexp = msg)
  msg = "can't set field psi_a"
  expect_error(dml_plr$psi_a <- 5,
    regexp = msg)
  msg = "can't set field psi_b"
  expect_error(dml_plr$psi_b <- 5,
    regexp = msg)
  msg = "can't set field predictions"
  expect_error(dml_plr$predictions <- 5,
    regexp = msg)
  msg = "can't set field pval"
  expect_error(dml_plr$pval <- 5,
    regexp = msg)
  msg = "can't set field score"
  expect_error(dml_plr$score <- "abc",
    regexp = msg)
  msg = "can't set field se"
  expect_error(dml_plr$se <- 5,
    regexp = msg)
  msg = "can't set field smpls"
  expect_error(dml_plr$smpls <- 5,
    regexp = msg)
  msg = "can't set field smpls_cluster"
  expect_error(dml_plr$smpls_cluster <- 5,
    regexp = msg)
  msg = "can't set field t_stat"
  expect_error(dml_plr$t_stat <- 5,
    regexp = msg)
  msg = "can't set field tuning_res"
  expect_error(dml_plr$tuning_res <- list(a = 5),
    regexp = msg)

  dml_data = make_pliv_CHS2015(n_obs = 101)
  ml_g = lrn("regr.ranger")
  ml_m = ml_g$clone()
  ml_r = ml_g$clone()
  dml_pliv = DoubleMLPLIV$new(dml_data, ml_g, ml_m, ml_r)

  msg = "can't set field partialX"
  expect_error(dml_pliv$partialX <- FALSE,
    regexp = msg)
  msg = "can't set field partialZ"
  expect_error(dml_pliv$partialZ <- FALSE,
    regexp = msg)

  dml_data = make_irm_data(n_obs = 101)
  ml_g = lrn("regr.ranger")
  ml_m = lrn("classif.ranger")
  dml_irm = DoubleMLIRM$new(dml_data, ml_g, ml_m)

  msg = "can't set field trimming_rule"
  expect_error(dml_irm$trimming_rule <- "abc",
    regexp = msg)
  msg = "can't set field trimming_threshold"
  expect_error(dml_irm$trimming_threshold <- 0.1,
    regexp = msg)

  dml_data = make_iivm_data(n_obs = 101)
  ml_g = lrn("regr.ranger")
  ml_m = lrn("classif.ranger")
  ml_r = ml_m$clone()
  dml_iivm = DoubleMLIIVM$new(dml_data, ml_g, ml_m, ml_r)

  msg = "can't set field subgroups"
  expect_error(dml_iivm$subgroups <- "abc",
    regexp = msg)
  msg = "can't set field trimming_rule"
  expect_error(dml_iivm$trimming_rule <- "abc",
    regexp = msg)
  msg = "can't set field trimming_threshold"
  expect_error(dml_iivm$trimming_threshold <- 0.1,
    regexp = msg)
})
