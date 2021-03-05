context("Unit tests for DoubleMLData")

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
  D1b = double_ml_data_from_matrix(X = NULL, y, d, z)
  
  # with multiple z
  z_mult = cbind(z, d2[,2])
  D1_multZ = double_ml_data_from_matrix(X, y, d, z_mult)
  expect_is(D1_multZ$data, "data.table")
  D1b_multZ = double_ml_data_from_matrix(X = NULL, y, d, z_mult)
  expect_is(D1b_multZ$data, "data.table")

   # Without z
  D2 = double_ml_data_from_matrix(X, y, d)
  D2b = double_ml_data_from_matrix(X = NULL, y, d)
  expect_is(D2b$data, "data.table")
  
  # test with only 1 d, 1 X, 1 Z
  X = as.matrix(data$X1)
  D2_1X = double_ml_data_from_matrix(X, y, d)
  
  # Two d variables
  X_indx2 = names(data) %in% c("y", "d", "z", "d1", "d2") == FALSE
  X2 = as.matrix(data[ , X_indx2])
  D3 = double_ml_data_from_matrix(X2, y, d2)
  D3_1d = double_ml_data_from_matrix(X2, y, d2, use_other_treat_as_covariate = FALSE)

  D3_setd = D3$set_data_model("d2")

  # Input: Data frame, assign columns by names
  d_indx = "d"
  y_indx = "y"
  z_null = NULL
  z_indx = "z"
  X_cols1 = names(data[, X_indx1])
  
  D4 =  double_ml_data_from_data_frame(data, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = d_indx, 
                                          z_cols = z_indx)
  
  D4b =  double_ml_data_from_data_frame(data, x_cols = NULL, 
                                          y_col = y_indx, 
                                          d_cols = d_indx, 
                                          z_cols = z_indx)
  
  # with renamed variables
  data_renamed = data
  names(data_renamed) = c("outc", "exposure", "instr", paste0("Explr", 1:(ncol(data_renamed)-3)))
  Expl_cols1 = names(data_renamed[, X_indx1])
  D4_renamed = double_ml_data_from_data_frame(data_renamed, x_cols = Expl_cols1, 
                                          y_col = "outc", 
                                          d_cols = "exposure", 
                                          z_cols = "instr")
  
  D5 = double_ml_data_from_data_frame(data, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = d_indx)
  
  D5b = double_ml_data_from_data_frame(data, x_cols = NULL, 
                                          y_col = y_indx, 
                                          d_cols = d_indx)

  # test with only 1 d, 1 X, 1 Z
  D5_1X =  double_ml_data_from_data_frame(data, x_cols = X_cols1[1], 
                                          y_col = y_indx, 
                                          d_cols = d_indx)
  
  D6 =  double_ml_data_from_data_frame(data, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = d_indx, 
                                          z_cols = z_null)
  
  # Two d Variables 
  data2 = data.frame(data, d2)
  d2_indx = colnames(d2)
  
  D7 = double_ml_data_from_data_frame(data2, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = d2_indx, 
                                          z_cols = z_null)
  D7_1d = double_ml_data_from_data_frame(data2, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = d2_indx, 
                                          z_cols = z_null, 
                                          use_other_treat_as_covariate = FALSE)
  D7_setd = D7$set_data_model("d2")
  
  expect_error(double_ml_data_from_data_frame(data))
  
  expect_equal(D1$data, D4$data)
  expect_equal(D2$data, D5$data)  
  expect_equal(D2_1X$data, D5_1X$data)   
  expect_equal(D2$data, D6$data)
  expect_equal(D3$data, D7$data)
  expect_identical(D3_1d$data_model, D7_1d$data_model)
  expect_identical(D3_setd$data_model, D7_setd$data_model)

  # renaming / enforced names
  expect_equivalent(D4$data, D4_renamed$data)
  expect_equivalent(D4$data_model, D4_renamed$data_model)
  
  # NULL input for x_cols
  expect_identical(D1$data[, c("y", "d", "z")], D1b$data)
  expect_identical(D4$data, D4b$data)
  expect_identical(D5$data[, sort(names(D5$data_model))], D5b$data[, sort(names(D5$data_model))])

  
  
  
  
  # Instantiate DoubleMLData
  data = data.table::data.table(data)
  data2 = data.table::data.table(data2)
  D8 = DoubleMLData$new(data, x_cols = X_cols1, 
                              y_col = y_indx, 
                              d_cols = d_indx, 
                              z_cols = z_indx)
  
  D8_noXcols = DoubleMLData$new(data, y_col = y_indx, 
                              d_cols = d_indx, 
                              z_cols = z_indx)
  # with renamed variables
  data_renamed = data.table::copy(data)
  data_renamed = data.table::setnames(data_renamed, c("outc", "exposure", "instr", 
                                          paste0("Explr", 1:(ncol(data_renamed)-3))))
  
  Expl_cols1 = names(data_renamed[, X_indx1, with = FALSE])
  D8_renamed = DoubleMLData$new(data_renamed, x_cols = Expl_cols1,
                                          y_col = "outc", 
                                          d_cols = "exposure", 
                                          z_cols = "instr")
 
  D9 =  DoubleMLData$new(data, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = d_indx)
  
  # skip z if not X indx specified
  data_noz = data[, which(names(data)!="z"), with = FALSE]
  D9_noXcols =  DoubleMLData$new(data_noz,    y_col = y_indx, 
                                          d_cols = d_indx)
  
  
  D9_1X =  DoubleMLData$new(data, x_cols = "X1", 
                                          y_col = y_indx, 
                                          d_cols = d_indx)
  
  D10 = DoubleMLData$new(data2, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = d2_indx, 
                                          z_cols = z_null)
  
  D10_1d = DoubleMLData$new(data2, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = d2_indx, 
                                          z_cols = z_null, 
                                          use_other_treat_as_covariate = FALSE)
  D10_setd = D10$set_data_model("d2")
  
   expect_error(double_ml_data_from_data_frame(data, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = c(d_indx, X_cols1[1]), 
                                          z_cols = z_indx), 
               regexp = "Invalid model specification.\n Treatment variables must not be elements of the control variables X.")
  

  expect_error(double_ml_data_from_data_frame(data, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = d_indx, 
                                          z_cols = c(z_indx, X_cols1[1])), 
               regexp = "Invalid model specification.\n Instrumental variables must not be an element of the control variables X.")
  
  expect_error(double_ml_data_from_data_frame(data, x_cols = c(y_indx, X_cols1), 
                                          y_col = y_indx, 
                                          d_cols = d_indx, 
                                          z_cols = z_indx), 
               regexp = "Invalid model specification.\n Dependent variable must not be an element of the control variables X.")
  
  expect_error(double_ml_data_from_data_frame(data, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = c(z_indx, d_indx), 
                                          z_cols = z_indx), 
               regexp = "Invalid model specification.\n Instrumental variables must not be an element of the treatment variables d.")
  
  # to do: check correct behavior when changing treat from d1 to d2
  
  expect_identical(D1$data_model, D8$data_model) # order of columns currently only enforced for data_model
  expect_identical(D2$data_model, D9$data_model)  
  expect_identical(D2_1X$data_model, D9_1X$data_model)   
  expect_identical(D3$data_model, D10$data_model)
  expect_identical(D3_1d$data_model, D10_1d$data_model)
  expect_identical(D3_setd$data_model, D10_setd$data_model)
  
  expect_identical(D4_renamed$data_model, D8_renamed$data_model)
  expect_equivalent(D8$data_model, D8_renamed$data_model)
  expect_identical(D8$data_model, D8_noXcols$data_model)
  expect_identical(D9$data_model, D9_noXcols$data_model)
  
  # Exception handling
   expect_error(DoubleMLData$new(data, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = c(d_indx, X_cols1[1]), 
                                          z_cols = z_indx), 
               regexp = "Invalid model specification.\n Treatment variables must not be elements of the control variables X.")
  

  expect_error(DoubleMLData$new(data, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = d_indx, 
                                          z_cols = c(z_indx, X_cols1[1])), 
               regexp = "Invalid model specification.\n Instrumental variables must not be an element of the control variables X.")
  
  expect_error(DoubleMLData$new(data, x_cols = c(y_indx, X_cols1), 
                                          y_col = y_indx, 
                                          d_cols = d_indx, 
                                          z_cols = z_indx), 
               regexp = "Invalid model specification.\n Dependent variable must not be an element of the control variables X.")
  
  expect_error(DoubleMLData$new(data, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = c(z_indx, d_indx), 
                                          z_cols = z_indx), 
               regexp = "Invalid model specification.\n Instrumental variables must not be an element of the treatment variables d.")
  
  D11 = DoubleMLData$new(data, x_cols = X_cols1, 
                                          y_col = y_indx, 
                                          d_cols = d_indx, 
                                          z_cols = z_indx)
  expect_error(D11$set_data_model(X_cols1[1]), 
               regexp = "The specified treatment variable must not be an element of the control variables X.")
  } 
)

