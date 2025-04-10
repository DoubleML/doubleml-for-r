context("Unit tests for not yet implemented cluster features")

lgr::get_logger("mlr3")$set_threshold("warn")

test_that("Not yet implemented cluster features", {
  ml_g = ml_m = ml_r = "regr.rpart"

  set.seed(3141)
  dml_cluster_data_pliv = make_pliv_multiway_cluster_CKMS2021(N = 10, M = 10)
  dml_pliv_cluster = DoubleMLPLIV$new(dml_cluster_data_pliv,
    ml_g, ml_m, ml_r,
    n_folds = 2)
  dml_pliv_cluster$fit()

  msg = "bootstrap not yet implemented with clustering."
  expect_error(dml_pliv_cluster$bootstrap(), regexp = msg)

  dml_data = make_plr_CCDDHNR2018(n_obs = 100)
  dml_plr = DoubleMLPLR$new(
    dml_data,
    ml_g, ml_m)
  smpls = dml_plr$smpls
  msg = paste(
    "Externally setting the sample splitting for DoubleML is not",
    "yet implemented with clustering.")
  expect_error(dml_pliv_cluster$set_sample_splitting(smpls), regexp = msg)

  dt = data.table::copy(dml_cluster_data_pliv$data)
  dt$cluster_var_k = dt$cluster_var_i + dt$cluster_var_j - 2
  data_multiway = DoubleMLClusterData$new(dt,
    y_col = "Y", d_cols = "D",
    x_cols = c("X1", "X5"), z_cols = "Z",
    cluster_cols = c(
      "cluster_var_i",
      "cluster_var_j",
      "cluster_var_k"))
  expect_equal(data_multiway$n_cluster_vars, 3)
  msg = "Multi-way \\(n_ways > 2\\) clustering not yet implemented."
  expect_error(DoubleMLPLIV$new(
    data_multiway,
    ml_g, ml_m, ml_r), regexp = msg)


  msg = paste(
    "No cross-fitting \\(`apply_cross_fitting = False`\\) is not yet",
    "implemented with clustering.")
  expect_error(DoubleMLPLIV$new(dml_cluster_data_pliv,
    ml_g, ml_m, ml_r,
    n_folds = 1), regexp = msg)
  expect_error(DoubleMLPLIV$new(dml_cluster_data_pliv,
    ml_g, ml_m, ml_r,
    apply_cross_fitting = FALSE, n_folds = 2),
  regexp = msg)
})
