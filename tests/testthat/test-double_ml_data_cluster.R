context("Unit tests for DoubleMLCluster (Additional tests)")

test_that("Unit tests for DoubleMLData", {

  set.seed(1234)
  N = 25 # number of observations (first dimension)
  M = 25 # number of observations (second dimension)
  dim_x = 10 # dimension of x
  data_one_way = make_pliv_multiway_cluster_CKMS2021(N, M, dim_x,
    omega_X = c(0.25, 0),
    omega_epsilon = c(0.25, 0),
    omega_v = c(0.25, 0),
    omega_V = c(0.25, 0))
  data_one_way$cluster_cols = "cluster_var_i"

  data_model = data_one_way$data_model
  data_model$S = rbinom(nrow(data_model), prob = 0.5, 1)

  msg = paste("At least one variable/column is set as selection variable",
    "\\('s_col'\\) and as a cluster variable \\('cluster_cols'\\).")

  expect_error(DoubleMLClusterData$new(data_model,
    x_cols = c("X1", "X2"),
    y_col = "Y",
    d_cols = "D",
    z_cols = "Z",
    s_col = "S",
    cluster_cols = "S"),
  regexp = msg)
  
  # TODO: Check z_cols but no s_col (L. 495)
  # TODO: Check z_cols but no cluster_cols (L. 502)
  

})
