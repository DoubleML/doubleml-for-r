context("Unit tests for active bindings of class DoubleMLData")

test_that("x_cols setter", {
  set.seed(3141)
  df = as.data.frame(matrix(rnorm(20), ncol = 4))
  names(df) = c('yy', 'dd', 'xx1', 'xx2')
  
  dml_data = double_ml_data_from_data_frame(df, y_col='yy', d_cols='dd')
  expect_equal(dml_data$x_cols, c("xx1", "xx2"))
  
  dml_data = make_plr_CCDDHNR2018(n_obs=100)
  orig_x_cols = dml_data$x_cols
  
  # check that after changing the x_cols, the x array gets updated
  data_comp = as.data.frame(dml_data$data_model)[, c(c('X1', 'X11', 'X13'), dml_data$y_col, dml_data$d_cols)]
  dml_data$x_cols = c('X1', 'X11', 'X13')
  expect_equal(as.data.frame(dml_data$data_model), data_comp)
  
  msg = paste0("Assertion on 'value' failed: Must be a subset of",
               " \\{'X1','X2','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12','X13','X14','X15','X16','X17','X18','X19','X20','y','d'\\},",
               " but is \\{'X1','X11','A13'\\}.") 
  expect_error(dml_data$x_cols <- c('X1', 'X11', 'A13'),
               regexp = msg)
  
  msg = "Assertion on 'value' failed: Must be of type 'character', not 'double'."
  expect_error(dml_data$x_cols <- 5,
               regexp = msg)
  
  # check single covariate
  dml_data$x_cols = 'X13'
  expect_equal(dml_data$x_cols, c('X13'))
  
  # check setting None brings us back to orig_x_cols
  dml_data$x_cols = NULL
  expect_equal(dml_data$x_cols, orig_x_cols)
  }
)

test_that("d_cols setter", {
  set.seed(3141)
  dml_data = make_plr_CCDDHNR2018(n_obs=101, return_type = "data.frame")
  df = dml_data[,1:10]
  names(df) = c(paste0("X", 1:7), c('y', 'd1', 'd2'))
  dml_data = double_ml_data_from_data_frame(df, y_col = 'y',
                                            d_cols = c('d1', 'd2'),
                                            x_cols = paste0("X", 1:7))
  expect_equal(dml_data$n_obs, 101)
  
  # check that after changing d_cols, the d array gets updated
  data_comp = as.data.frame(dml_data$data_model)[, c(paste0("X", 1:7), c('y', 'd2', 'd1'))]
  dml_data$d_cols = c('d2', 'd1')
  expect_equal(as.data.frame(dml_data$data_model), data_comp)
  
  msg = paste0("Assertion on 'value' failed: Must be a subset of",
               " \\{'X1','X2','X3','X4','X5','X6','X7','y','d1','d2'\\},",
               " but is \\{'d1','d13'\\}.") 
  expect_error(dml_data$d_cols <- c('d1', 'd13'),
               regexp = msg)
  
  msg = paste0("Assertion on 'value' failed: Must be a subset of",
               " \\{'X1','X2','X3','X4','X5','X6','X7','y','d1','d2'\\},",
               " but is \\{'d13'\\}.") 
  expect_error(dml_data$d_cols <- 'd13',
               regexp = msg)
  
  msg = "Assertion on 'value' failed: Must be of type 'character', not 'double'."
  expect_error(dml_data$d_cols <- 5,
               regexp = msg)
  
  # check single treatment variable
  dml_data$d_cols = 'd2'
  expect_equal(dml_data$d_cols, c('d2'))
  expect_equal(dml_data$n_treat, 1)
  
  }
)
