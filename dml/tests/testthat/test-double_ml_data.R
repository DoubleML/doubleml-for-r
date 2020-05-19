context("Unit tests for DoubleMLData")

library('data.table')

test_cases = expand.grid(i_setting = 1:(length(data_iivm)))

test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for DoubleMLData:",
                                   .cases = test_cases, {
  

  data = data_iivm[[i_setting]]
  
  y = data[, "y"] # input: numeric
  d = data[, "d"] # input: integer
  z = data[, "z"] # input: integer 
  d2 = as.matrix(cbind(d, d*2), ncol = 2)
  colnames(d2) = c("d1", "d2")
    
  X_indx = names(data) %in% c("y", "d", "z") == FALSE
  X = as.matrix(data[ , X_indx])
  
  D1 = double_ml_data_from_matrix(X, y, d, z)
  
  D2 = double_ml_data_from_matrix(X, y, d)
  
  X_indx = names(data) %in% c("y", "d1", "d2") == FALSE
  X = as.matrix(data[ , X_indx])
  
  D3 = double_ml_data_from_matrix(X, y, d2)
    
  expect_class(D1, "data.table")                               
  expect_class(D2, "data.table")
  expect_class(D3, "data.table")
  } 
)

