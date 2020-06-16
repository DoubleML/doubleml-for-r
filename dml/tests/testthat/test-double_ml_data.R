context("Unit tests for DoubleMLData")

library('data.table')

test_cases = expand.grid(i_setting = 1:(length(data_iivm)))

test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for DoubleMLData:",
                                   .cases = test_cases, {
  
  data = data_iivm[[i_setting]]
  
  # Input: Matrix and vectors
  y = data[, "y"] # input: numeric
  d = data[, "d"] # input: integer
  z = data[, "z"] # input: integer 
  d2 = as.matrix(cbind(d, d*2), ncol = 2)
  colnames(d2) = c("d1", "d2")
    
  X_indx1 = names(data) %in% c("y", "d", "z") == FALSE
  X = as.matrix(data[ , X_indx1])
  
  # With z
  D1 = double_ml_data_from_matrix(X, y, d, z)
  
  # Without z
  D2 = double_ml_data_from_matrix(X, y, d)
  
  # Two d variables
  X_indx2 = names(data) %in% c("y", "d", "z", "d1", "d2") == FALSE
  X2 = as.matrix(data[ , X_indx2])
  
  D3 = double_ml_data_from_matrix(X2, y, d2)

  # Input: Data frame, assign columns by names
  d_indx = "d"
  y_indx = "y"
  z_null = NULL
  z_indx = "z"
  X_cols1 = names(data[, X_indx1])
  
  D4 =  double_ml_data_from_data_frame(data, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = d_indx, 
                                          z_col = z_indx)
  
  D5 = double_ml_data_from_data_frame(data, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = d_indx)
  
  D6 =  double_ml_data_from_data_frame(data, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = d_indx, 
                                          z_col = z_null)
  
  # Two d Variables 
  data2 = data.frame(data, d2)
  d2_indx = colnames(d2)
  
  D7 = double_ml_data_from_data_frame(data2, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = d2_indx, 
                                          z_col = z_null)
  
  
  expect_error(double_ml_data_from_data_frame(data))
  
  expect_identical(D1$data, D4$data)
  expect_identical(D2$data, D5$data)   
  expect_identical(D2$data, D6$data)     
  expect_identical(D3$data, D7$data)
  
  # Instantiate DoubleMLData
  D8 = DoubleMLData$new(data, x_cols = X_cols1, 
                              y_col = y_indx, 
                              d_cols = d_indx, 
                              z_col = z_indx)
  
  D9 =  DoubleMLData$new(data, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = d_indx)
  
  
  D10 = DoubleMLData$new(data2, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = d2_indx, 
                                          z_col = z_null)
  

    
  expect_identical(D1$data, D8$data)
  expect_identical(D2$data, D9$data)   
  expect_identical(D3$data, D10$data)
  } 
)

