context("Unit tests for active bindings of class DoubleMLData")

test_that("x_cols setter", {
  set.seed(3141)
  df = as.data.frame(matrix(rnorm(20), ncol = 4))
  names(df) = c("yy", "dd", "xx1", "xx2")

  dml_data = double_ml_data_from_data_frame(df, y_col = "yy", d_cols = "dd")
  expect_equal(dml_data$x_cols, c("xx1", "xx2"))

  dml_data = make_plr_CCDDHNR2018(n_obs = 100)
  orig_x_cols = dml_data$x_cols

  # check that after changing the x_cols, the data_model gets updated
  data_comp = as.data.frame(dml_data$data_model)[, c(c("X1", "X11", "X13"), dml_data$y_col, dml_data$d_cols)]
  dml_data$x_cols = c("X1", "X11", "X13")
  expect_equal(as.data.frame(dml_data$data_model), data_comp)

  msg = paste0(
    "Assertion on 'x_cols' failed: Must be a subset of",
    " \\{'X1','X2','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12','X13','X14','X15','X16','X17','X18','X19','X20','y','d'\\}")
  expect_error(dml_data$x_cols <- c("X1", "X11", "A13"),
    regexp = msg)

  msg = "Assertion on 'x_cols' failed: Must be of type 'character', not 'double'."
  expect_error(dml_data$x_cols <- 5,
    regexp = msg)

  # check single covariate
  dml_data$x_cols = "X13"
  expect_equal(dml_data$x_cols, "X13")

  # check setting None brings us back to orig_x_cols
  dml_data$x_cols = NULL
  expect_equal(dml_data$x_cols, orig_x_cols)
}
)

test_that("d_cols setter", {
  set.seed(3141)
  dml_data = make_plr_CCDDHNR2018(n_obs = 101, return_type = "data.frame")
  df = dml_data[, 1:10]
  names(df) = c(paste0("X", 1:7), c("y", "d1", "d2"))
  dml_data = double_ml_data_from_data_frame(df,
    y_col = "y",
    d_cols = c("d1", "d2"),
    x_cols = paste0("X", 1:7))
  expect_equal(dml_data$n_obs, 101)

  # check that after changing d_cols, the data_model gets updated
  data_comp = df[, c(paste0("X", 1:7), c("y", "d2", "d1"))]
  dml_data$d_cols = c("d2", "d1")
  expect_equal(as.data.frame(dml_data$data_model), data_comp)

  msg = paste0(
    "Assertion on 'd_cols' failed: Must be a subset of",
    " \\{'X1','X2','X3','X4','X5','X6','X7','y','d1','d2'\\}")
  expect_error(dml_data$d_cols <- c("d1", "d13"),
    regexp = msg)

  msg = paste0(
    "Assertion on 'd_cols' failed: Must be a subset of",
    " \\{'X1','X2','X3','X4','X5','X6','X7','y','d1','d2'\\}")
  expect_error(dml_data$d_cols <- "d13",
    regexp = msg)

  msg = "Assertion on 'd_cols' failed: Must be of type 'character', not 'double'."
  expect_error(dml_data$d_cols <- 5,
    regexp = msg)

  # check single treatment variable
  dml_data$d_cols = "d2"
  expect_equal(dml_data$d_cols, "d2")
  expect_equal(dml_data$n_treat, 1)
}
)

test_that("z_cols setter", {
  set.seed(3141)
  dml_data = make_plr_CCDDHNR2018(n_obs = 101, return_type = "data.frame")
  df = dml_data[, 1:10]
  names(df) = c(paste0("X", 1:4), paste0("z", 1:3), c("y", "d1", "d2"))
  dml_data = double_ml_data_from_data_frame(df,
    y_col = "y",
    d_cols = c("d1", "d2"),
    x_cols = paste0("X", 1:4),
    z_cols = paste0("z", 1:3))
  expect_equal(dml_data$n_obs, 101)
  expect_equal(dml_data$n_treat, 2)
  expect_equal(dml_data$n_instr, 3)

  # check that z_cols gets updated
  dml_data$z_cols = c("z1", "z2")
  expect_equal(dml_data$z_cols, c("z1", "z2"))
  expect_equal(dml_data$n_instr, 2)

  msg = paste0(
    "Assertion on 'z_cols' failed: Must be a subset of",
    " \\{'X1','X2','X3','X4','z1','z2','z3','y','d1','d2'\\}")
  expect_error(dml_data$z_cols <- c("z1", "a13"),
    regexp = msg)

  msg = paste0(
    "Assertion on 'z_cols' failed: Must be a subset of",
    " \\{'X1','X2','X3','X4','z1','z2','z3','y','d1','d2'\\}")
  expect_error(dml_data$z_cols <- "a13",
    regexp = msg)

  msg = "Assertion on 'z_cols' failed: Must be of type 'character', not 'double'."
  expect_error(dml_data$z_cols <- 5,
    regexp = msg)

  # check single instrument
  dml_data$z_cols = "z2"
  expect_equal(dml_data$z_cols, "z2")
  expect_equal(dml_data$n_instr, 1)

  # check NULL
  dml_data$z_cols = NULL
  expect_equal(dml_data$z_cols, NULL)
  expect_equal(dml_data$n_instr, 0)
}
)

test_that("y_col setter", {
  set.seed(3141)
  dml_data = make_plr_CCDDHNR2018(n_obs = 101, return_type = "data.frame")
  df = dml_data[, 1:10]
  names(df) = c(paste0("X", 1:7), c("y", "y123", "d"))
  dt = data.table::as.data.table(df)
  dml_data = DoubleMLData$new(dt,
    y_col = "y",
    d_cols = "d",
    x_cols = paste0("X", 1:7))
  # with the following wrapper the column 'y123' gets removed
  # dml_data = double_ml_data_from_data_frame(df, y_col = 'y',
  #                                          d_cols = 'd',
  #                                          x_cols = paste0("X", 1:7))
  expect_equal(dml_data$n_obs, 101)

  # check that after changing d_cols, the data_model gets updated
  data_comp = df[, c(paste0("X", 1:7), c("y123", "d"))]
  dml_data$y_col = "y123"
  expect_equal(dml_data$y_col, "y123")
  expect_equal(as.data.frame(dml_data$data_model), data_comp)

  msg = paste0(
    "Assertion on 'y_col' failed: Must be a subset of",
    " \\{'X1','X2','X3','X4','X5','X6','X7','y','y123','d'\\}")
  expect_error(dml_data$y_col <- "d13",
    regexp = msg)

  msg = "Assertion on 'y_col' failed: Must be of type 'character', not 'double'."
  expect_error(dml_data$y_col <- 5,
    regexp = msg)
}
)

test_that("cluster_cols setter", {
  set.seed(3141)
  dml_data = make_plr_CCDDHNR2018(n_obs = 101, return_type = "data.frame")
  df = dml_data[, 1:10]
  names(df) = c(paste0("X", 1:5), c("y", "d1", "d2", "c1", "c2"))
  dml_data = double_ml_data_from_data_frame(df,
    y_col = "y",
    d_cols = c("d1", "d2"),
    x_cols = paste0("X", 1:5),
    cluster_cols = c("c1", "c2"))
  expect_equal(dml_data$n_obs, 101)

  # check that after changing d_cols, the data_model gets updated
  data_comp = df[, c(paste0("X", 1:5), c("y", "d1", "d2", "c2", "c1"))]
  dml_data$cluster_cols = c("c2", "c1")
  expect_equal(as.data.frame(dml_data$data_model), data_comp)

  msg = paste0(
    "Assertion on 'cluster_cols' failed: Must be a subset of",
    " \\{'X1','X2','X3','X4','X5','y','d1','d2','c1','c2'\\}")
  expect_error(dml_data$cluster_cols <- c("c1", "c13"),
    regexp = msg)

  msg = paste0(
    "Assertion on 'cluster_cols' failed: Must be a subset of",
    " \\{'X1','X2','X3','X4','X5','y','d1','d2','c1','c2'\\}")
  expect_error(dml_data$cluster_cols <- "c13",
    regexp = msg)

  msg = "Assertion on 'cluster_cols' failed: Must be of type 'character', not 'double'."
  expect_error(dml_data$cluster_cols <- 5,
    regexp = msg)

  # check single treatment variable
  dml_data$cluster_cols = "c2"
  expect_equal(dml_data$cluster_cols, "c2")
  expect_equal(dml_data$n_cluster_vars, 1)
}
)

test_that("Tests for use_other_treat_as_covariate", {
  set.seed(3141)
  dml_data = make_plr_CCDDHNR2018(n_obs = 101, return_type = "data.frame")
  df = dml_data[, 1:10]
  names(df) = c(paste0("X", 1:7), c("y", "d1", "d2"))
  dml_data = double_ml_data_from_data_frame(df,
    y_col = "y",
    d_cols = c("d1", "d2"),
    x_cols = paste0("X", 1:7),
    use_other_treat_as_covariate = TRUE)
  expect_equal(dml_data$n_obs, 101)

  dml_data$set_data_model("d1")
  expect_equal(dml_data$treat_col, "d1")
  expect_equal(dml_data$other_treat_cols, "d2")

  dml_data$set_data_model("d2")
  expect_equal(dml_data$treat_col, "d2")
  expect_equal(dml_data$other_treat_cols, "d1")

  dml_data$use_other_treat_as_covariate = FALSE
  expect_equal(dml_data$other_treat_cols, NULL)

  dml_data$set_data_model("d1")
  expect_equal(dml_data$treat_col, "d1")
  expect_equal(dml_data$other_treat_cols, NULL)

  dml_data$set_data_model("d2")
  expect_equal(dml_data$treat_col, "d2")
  expect_equal(dml_data$other_treat_cols, NULL)

  msg = "Assertion on 'use_other_treat_as_covariate' failed: Must be of type 'logical', not 'double'."
  expect_error(dml_data$use_other_treat_as_covariate <- 5,
    regexp = msg)

  msg = "Assertion on 'treatment_var' failed: Must be a subset of \\{'d1','d2'\\}"
  expect_error(dml_data$set_data_model("d3"),
    regexp = msg)

  msg = "Assertion on 'treatment_var' failed: Must have length <= 1, but has length 2."
  expect_error(dml_data$set_data_model(c("d1", "d2")),
    regexp = msg)
}
)

test_that("Disjoint sets", {
  set.seed(3141)
  df = as.data.frame(matrix(rnorm(20), ncol = 4))
  names(df) = c("yy", "dd1", "xx1", "xx2")
  dt = data.table::as.data.table(df)

  msg = paste0(
    "At least one variable/column is set as treatment variable \\('d_cols'\\) and as a covariate \\('x_cols'\\).",
    " Consider using parameter 'use_other_treat_as_covariate'.")
  expect_error(DoubleMLData$new(dt,
    y_col = "yy",
    d_cols = c("dd1", "xx1"),
    x_cols = c("xx1", "xx2")),
  regexp = msg)

  msg = "yy cannot be set as outcome variable 'y_col' and treatment variable in 'd_cols'."
  expect_error(DoubleMLData$new(dt,
    y_col = "yy",
    d_cols = c("dd1", "yy"),
    x_cols = c("xx1", "xx2")),
  regexp = msg)

  msg = "yy cannot be set as outcome variable 'y_col' and covariate in 'x_cols'."
  expect_error(DoubleMLData$new(dt,
    y_col = "yy",
    d_cols = "dd1",
    x_cols = c("xx1", "yy", "xx2")),
  regexp = msg)

  msg = "yy cannot be set as outcome variable 'y_col' and instrumental variable in 'z_cols'."
  expect_error(DoubleMLData$new(dt,
    y_col = "yy",
    d_cols = "dd1",
    x_cols = c("xx1", "xx2"),
    z_cols = "yy"),
  regexp = msg)

  msg = "At least one variable/column is set as treatment variable \\('d_cols'\\) and instrumental variable in 'z_cols'."
  expect_error(DoubleMLData$new(dt,
    y_col = "yy",
    d_cols = "dd1",
    x_cols = c("xx1", "xx2"),
    z_cols = "dd1"),
  regexp = msg)

  msg = "At least one variable/column is set as covariate \\('x_cols'\\) and instrumental variable in 'z_cols'."
  expect_error(DoubleMLData$new(dt,
    y_col = "yy",
    d_cols = "dd1",
    x_cols = c("xx1", "xx2"),
    z_cols = "xx2"),
  regexp = msg)

  msg = "yy cannot be set as outcome variable 'y_col' and cluster variable in 'cluster_cols'."
  expect_error(DoubleMLClusterData$new(dt,
    y_col = "yy",
    d_cols = c("dd1"),
    x_cols = c("xx1", "xx2"),
    cluster_cols = "yy"),
  regexp = msg)

  msg = "At least one variable/column is set as treatment variable \\('d_cols'\\) and as a cluster variable \\('cluster_cols'\\)."
  expect_error(DoubleMLClusterData$new(dt,
    y_col = "yy",
    d_cols = c("dd1", "xx2"),
    x_cols = c("xx1"),
    cluster_cols = "xx2"),
  regexp = msg)

  msg = "At least one variable/column is set as covariate \\('x_cols'\\) and as a cluster variable \\('cluster_cols'\\)."
  expect_error(DoubleMLClusterData$new(dt,
    y_col = "yy",
    d_cols = "dd1",
    x_cols = c("xx1", "xx2"),
    cluster_cols = "xx2"),
  regexp = msg)

  msg = "At least one variable/column is set as instrumental variable \\('z_cols'\\) and as a cluster variable \\('cluster_cols'\\)."
  expect_error(DoubleMLClusterData$new(dt,
    y_col = "yy",
    d_cols = "dd1",
    x_cols = c("xx1"),
    z_cols = "xx2",
    cluster_cols = "xx2"),
  regexp = msg)
}
)

test_that("Test duplicates", {
  set.seed(3141)
  dt = make_plr_CCDDHNR2018(n_obs = 101, return_type = "data.table")
  dml_data = DoubleMLData$new(dt,
    y_col = "y",
    d_cols = "d")
  dml_cluster_data = DoubleMLClusterData$new(dt,
    y_col = "y",
    d_cols = "d",
    cluster_cols = "X2")
  msg = "Assertion on 'd_cols' failed: Contains duplicated values, position 2."
  expect_error(DoubleMLData$new(dt,
    y_col = "y",
    d_cols = c("d", "d", "X1"),
    x_cols = c("X3", "X2")),
  regexp = msg)
  expect_error(dml_data$d_cols <- c("d", "d", "X1"),
    regexp = msg)

  msg = "Assertion on 'x_cols' failed: Contains duplicated values, position 3."
  expect_error(DoubleMLData$new(dt,
    y_col = "y",
    d_cols = "d",
    x_cols = c("X3", "X2", "X3")),
  regexp = msg)
  expect_error(dml_data$x_cols <- c("X3", "X2", "X3"),
    regexp = msg)

  msg = "Assertion on 'z_cols' failed: Contains duplicated values, position 3."
  expect_error(DoubleMLData$new(dt,
    y_col = "y",
    d_cols = "d",
    x_cols = c("X3", "X2"),
    z_cols = c("X15", "X12", "X12", "X15")),
  regexp = msg)
  expect_error(dml_data$z_cols <- c("X15", "X12", "X12", "X15"),
    regexp = msg)

  msg = "Assertion on 'cluster_cols' failed: Contains duplicated values, position 2."
  expect_error(DoubleMLClusterData$new(dt,
    y_col = "y",
    d_cols = c("X1"),
    x_cols = c("X3", "X2"),
    cluster_cols = c("d", "d")),
  regexp = msg)
  expect_error(dml_cluster_data$cluster_cols <- c("d", "d"),
    regexp = msg)

  df = as.data.frame(matrix(rnorm(20), ncol = 5))
  names(df) = c("y", "d", "X3", "X2", "y")
  dt = data.table::as.data.table(df)
  msg = "Assertion on 'names\\(data\\)' failed: Contains duplicated values, position 5."
  expect_error(DoubleMLData$new(dt,
    y_col = "y",
    d_cols = "d",
    x_cols = c("X3", "X2")),
  regexp = msg)
  expect_error(DoubleMLClusterData$new(dt,
    y_col = "y",
    d_cols = "d",
    x_cols = c("X3"),
    cluster_cols = "X2"),
  regexp = msg)
}
)

test_that("Not setable fields", {
  set.seed(3141)
  dml_data = make_plr_CCDDHNR2018(n_obs = 101)
  dml_cluster_data = DoubleMLClusterData$new(dml_data$data,
    y_col = "y",
    d_cols = "d",
    cluster_cols = "X2")
  msg = "can't set field n_cluster_vars"
  expect_error(dml_cluster_data$n_cluster_vars <- 5,
    regexp = msg)

  msg = "can't set field all_variables"
  expect_error(dml_data$all_variables <- "abc",
    regexp = msg)
  expect_error(dml_cluster_data$all_variables <- "abc",
    regexp = msg)
  msg = "can't set field data"
  expect_error(dml_data$data <- "abc",
    regexp = msg)
  expect_error(dml_cluster_data$data <- "abc",
    regexp = msg)
  msg = "can't set field data_model"
  expect_error(dml_data$data_model <- "abc",
    regexp = msg)
  expect_error(dml_cluster_data$data_model <- "abc",
    regexp = msg)

  msg = "can't set field n_instr"
  expect_error(dml_data$n_instr <- 5,
    regexp = msg)
  expect_error(dml_cluster_data$n_instr <- 5,
    regexp = msg)
  msg = "can't set field n_obs"
  expect_error(dml_data$n_obs <- 5,
    regexp = msg)
  expect_error(dml_cluster_data$n_obs <- 5,
    regexp = msg)
  msg = "can't set field n_treat"
  expect_error(dml_data$n_treat <- 5,
    regexp = msg)
  expect_error(dml_cluster_data$n_treat <- 5,
    regexp = msg)

  msg = "can't set field other_treat_cols"
  expect_error(dml_data$other_treat_cols <- "abc",
    regexp = msg)
  expect_error(dml_cluster_data$other_treat_cols <- "abc",
    regexp = msg)
  msg = "can't set field treat_col"
  expect_error(dml_data$treat_col <- "abc",
    regexp = msg)
  expect_error(dml_cluster_data$treat_col <- "abc",
    regexp = msg)
}
)
