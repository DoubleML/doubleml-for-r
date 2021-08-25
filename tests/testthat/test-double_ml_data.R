context("Unit tests for DoubleMLData")

test_that("Unit tests for DoubleMLData", {
  data = data_iivm$df

  # Input: Matrix and vectors
  y = data[, "y"] # input: numeric
  d = data[, "d"] # input: integer
  z = data[, "z"] # input: integer
  d2 = as.matrix(cbind(d, d * 2), ncol = 2)
  colnames(d2) = c("d1", "d2")

  X_indx1 = names(data) %in% c("y", "d", "z") == FALSE

  check_indx1 = c(names(data)[X_indx1], "y", "d", "z")
  X_dt1 = as.data.table(data)[, check_indx1, with = FALSE]
  X = as.matrix(data[, X_indx1])

  # With z and X
  D1 = double_ml_data_from_matrix(X, y, d, z)
  expect_equal(D1$data, X_dt1)
  expect_identical(D1$data_model, X_dt1)

  # No X
  D1b = double_ml_data_from_matrix(X = NULL, y, d, z)
  X_dt1b = as.data.table(data)[, c("y", "d", "z")]
  expect_equal(D1b$data, X_dt1b)
  expect_identical(D1b$data_model, X_dt1b)

  # with multiple z
  z_mult = cbind(z, d2[, 2])
  # with X
  D1_multZ = double_ml_data_from_matrix(X, y, d, z_mult)
  multZ_dt1 = as.data.table(
    data.frame(data, "z1" = z, "z2" = d2[, 2]))[, c(
    names(data)[X_indx1],
    "y", "d", "z1", "z2"),
  with = FALSE]
  expect_equal(D1_multZ$data, multZ_dt1)
  expect_equal(D1_multZ$data_model, multZ_dt1)

  # No X
  D1b_multZ = double_ml_data_from_matrix(X = NULL, y, d, z_mult)
  multZ_dt1b = as.data.table(
    data.frame(data, "z1" = z, "z2" = d2[, 2]))[, c("y", "d", "z1", "z2"),
    with = FALSE]
  expect_equal(D1_multZ$data, multZ_dt1)
  expect_equal(D1_multZ$data_model, multZ_dt1)

  # No X
  D1b_multZ = double_ml_data_from_matrix(X = NULL, y, d, z_mult)
  multZ_dt1b = as.data.table(
    data.frame(data, "z1" = z, "z2" = d2[, 2]))[, c("y", "d", "z1", "z2"),
    with = FALSE]
  expect_equal(D1b_multZ$data, multZ_dt1b)
  expect_equal(D1b_multZ$data_model, multZ_dt1b)

  # No z, with X
  D2 = double_ml_data_from_matrix(X, y, d)
  check_indx2 = c(names(data)[X_indx1], "y", "d")
  X_dt2 = as.data.table(data)[, check_indx2, with = FALSE]
  expect_equal(D2$data, X_dt2)
  expect_equal(D2$data_model, X_dt2)

  # No z, no X
  D2b = double_ml_data_from_matrix(X = NULL, y, d)
  X_dt2b = as.data.table(data)[, c("y", "d"), with = FALSE]
  expect_equal(D2b$data, X_dt2b)
  expect_equal(D2b$data_model, X_dt2b)

  # test with only 1 d, 1 X, 1 Z
  X = as.matrix(data$X1)
  D2_1X = double_ml_data_from_matrix(X, y, d)
  X_dt21X = as.data.table(data)[, c("X1", "y", "d"), with = FALSE]
  expect_equal(D2_1X$data, X_dt21X)
  expect_equal(D2_1X$data_model, X_dt21X)

  # Two d variables
  X_indx2 = names(data) %in% c("y", "d", "z", "d1", "d2") == FALSE
  X2 = as.matrix(data[, X_indx2])
  D3 = double_ml_data_from_matrix(X2, y, d2)

  X_dt3 = as.data.table(
    data.frame(data, "d1" = d2[, 1], "d2" = d2[, 2]))[, c(
    names(data)[X_indx2],
    "y", "d1", "d2"),
  with = FALSE]
  expect_equal(D3$data, X_dt3)
  expect_equal(D3$data_model, X_dt3)

  # set_data_model
  D3_setd_multd = D3$clone()$set_data_model("d2")
  X_dt3_setd_multd = data.table::copy(X_dt3)[, c(
    names(data)[X_indx2],
    "y", "d2", "d1"),
  with = FALSE]
  expect_equal(D3_setd_multd$data, X_dt3)
  expect_equal(D3_setd_multd$data_model, X_dt3_setd_multd)

  # Do not include other treatment var in nuisance part
  D3_1d = double_ml_data_from_matrix(X2, y, d2,
    use_other_treat_as_covariate = FALSE)
  # Data backend
  expect_equal(D3_1d$data, X_dt3)

  # Data model
  X_dt31d = data.table::copy(X_dt3)[, d2 := NULL]
  expect_equal(D3_1d$data_model, X_dt31d)

  # set_data_model
  D3_setd = D3_1d$clone()$set_data_model("d2")
  X_dt3_setd = data.table::copy(X_dt3)[, d1 := NULL]
  expect_equal(D3_setd$data_model, X_dt3_setd)

  # Input: Data frame, assign columns by names
  d_indx = "d"
  y_indx = "y"
  z_null = NULL
  z_indx = "z"
  X_cols1 = names(data[, X_indx1])

  D4 = double_ml_data_from_data_frame(data,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = d_indx,
    z_cols = z_indx)

  D4b = double_ml_data_from_data_frame(data,
    x_cols = NULL,
    y_col = y_indx,
    d_cols = d_indx,
    z_cols = z_indx)

  # with renamed variables
  data_renamed = data
  names(data_renamed) = c("outc", "exposure", "instr", paste0("Explr", 1:(ncol(data_renamed) - 3)))
  Expl_cols1 = names(data_renamed[, X_indx1])
  D4_renamed = double_ml_data_from_data_frame(data_renamed,
    x_cols = Expl_cols1,
    y_col = "outc",
    d_cols = "exposure",
    z_cols = "instr")

  D5 = double_ml_data_from_data_frame(data,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = d_indx)

  D5b = double_ml_data_from_data_frame(data,
    x_cols = NULL,
    y_col = y_indx,
    d_cols = d_indx)

  # test with only 1 d, 1 X, 1 Z
  D5_1X = double_ml_data_from_data_frame(data,
    x_cols = X_cols1[1],
    y_col = y_indx,
    d_cols = d_indx)

  D6 = double_ml_data_from_data_frame(data,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = d_indx,
    z_cols = z_null)

  # Two d Variables
  data2 = data.frame(data, d2)
  d2_indx = colnames(d2)

  D7 = double_ml_data_from_data_frame(data2,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = d2_indx,
    z_cols = z_null)
  D7_setd_multd = D7$clone()$set_data_model("d2")

  D7_1d = double_ml_data_from_data_frame(data2,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = d2_indx,
    z_cols = z_null,
    use_other_treat_as_covariate = FALSE)
  D7_setd = D7_1d$clone()$set_data_model("d2")

  expect_error(double_ml_data_from_data_frame(data),
    regexp = "Assertion on 'y_col' failed: Must be of type 'character', not 'NULL'.")

  expect_equal(D1$data_model, D4$data_model)
  expect_equal(D2$data_model, D5$data_model)
  expect_equal(D2_1X$data_model, D5_1X$data_model)
  expect_equal(D2$data_model, D6$data_model)
  expect_equal(D3$data_model, D7$data_model)
  expect_identical(D3_1d$data_model, D7_1d$data_model)
  expect_identical(D3_setd_multd$data_model, D7_setd_multd$data_model)
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
  D8 = DoubleMLData$new(data,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = d_indx,
    z_cols = z_indx)

  D8_noXcols = DoubleMLData$new(data,
    y_col = y_indx,
    d_cols = d_indx,
    z_cols = z_indx)
  # with renamed variables
  data_renamed = data.table::copy(data)
  data_renamed = data.table::setnames(data_renamed, c(
    "outc", "exposure", "instr",
    paste0("Explr", 1:(ncol(data_renamed) - 3))))

  Expl_cols1 = names(data_renamed[, X_indx1, with = FALSE])
  D8_renamed = DoubleMLData$new(data_renamed,
    x_cols = Expl_cols1,
    y_col = "outc",
    d_cols = "exposure",
    z_cols = "instr")

  D9 = DoubleMLData$new(data,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = d_indx)

  # skip z if not X indx specified
  data_noz = data[, which(names(data) != "z"), with = FALSE]
  D9_noXcols = DoubleMLData$new(data_noz,
    y_col = y_indx,
    d_cols = d_indx)

  D9_1X = DoubleMLData$new(data,
    x_cols = "X1",
    y_col = y_indx,
    d_cols = d_indx)

  D10 = DoubleMLData$new(data2,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = d2_indx,
    z_cols = z_null)

  D10_setd = D10$clone()$set_data_model("d2")

  D10_1d = DoubleMLData$new(data2,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = d2_indx,
    z_cols = z_null,
    use_other_treat_as_covariate = FALSE)

  D10_1d_setd = D10_1d$clone()$set_data_model("d2")

  msg1 = "At least one variable/column is set as treatment variable \\('d_cols'\\) and as a covariate \\('x_cols'\\). Consider using parameter 'use_other_treat_as_covariate'."

  expect_error(double_ml_data_from_data_frame(data,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = c(d_indx, X_cols1[1]),
    z_cols = z_indx),
  regexp = msg1)

  msg2 = "At least one variable/column is set as covariate \\('x_cols'\\) and instrumental variable in 'z_cols'."
  expect_error(double_ml_data_from_data_frame(data,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = d_indx,
    z_cols = c(z_indx, X_cols1[1])),
  regexp = msg2)

  msg3 = "y cannot be set as outcome variable 'y_col' and covariate in 'x_cols'."
  expect_error(double_ml_data_from_data_frame(data,
    x_cols = c(y_indx, X_cols1),
    y_col = y_indx,
    d_cols = d_indx,
    z_cols = z_indx),
  regexp = msg3)

  msg4 = "At least one variable/column is set as treatment variable \\('d_cols'\\) and instrumental variable in 'z_cols'."
  expect_error(double_ml_data_from_data_frame(data,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = c(z_indx, d_indx),
    z_cols = z_indx),
  regexp = msg4)

  msg5 = "y cannot be set as outcome variable 'y_col' and treatment variable in 'd_cols'."
  expect_error(double_ml_data_from_data_frame(data,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = c(d_indx, y_indx),
    z_cols = z_indx),
  regexp = msg5)

  msg6 = "y cannot be set as outcome variable 'y_col' and instrumental variable in 'z_cols'."
  expect_error(double_ml_data_from_data_frame(data,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = d_indx,
    z_cols = c(z_indx, y_indx)),
  regexp = msg6)

  msg7 = "Assertion on 'x_cols' failed: Contains duplicated values, position 21."
  expect_error(double_ml_data_from_data_frame(data,
    x_cols = rep(X_cols1, 2),
    y_col = y_indx,
    d_cols = d_indx,
    z_cols = z_indx),
  regexp = msg7)

  expect_identical(D1$data_model, D8$data_model)
  expect_identical(D2$data_model, D9$data_model)
  expect_identical(D2_1X$data_model, D9_1X$data_model)
  expect_identical(D3$data_model, D10$data_model)
  expect_identical(D3_setd_multd$data_model, D10_setd$data_model)
  expect_identical(D3_1d$data_model, D10_1d$data_model)
  expect_identical(D3_setd$data_model, D10_1d_setd$data_model)

  expect_identical(D4_renamed$data_model, D8_renamed$data_model)
  expect_equivalent(D8$data_model, D8_renamed$data_model)
  expect_identical(D8$data_model, D8_noXcols$data_model)
  expect_identical(D9$data_model, D9_noXcols$data_model)

  # Exception handling
  msg8 = "At least one variable/column is set as treatment variable \\('d_cols'\\) and as a covariate \\('x_cols'\\). Consider using parameter 'use_other_treat_as_covariate'."
  expect_error(DoubleMLData$new(data,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = c(d_indx, X_cols1[1]),
    z_cols = z_indx),
  regexp = msg8)

  msg9 = "At least one variable/column is set as covariate \\('x_cols'\\) and instrumental variable in 'z_cols'."
  expect_error(DoubleMLData$new(data,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = d_indx,
    z_cols = c(z_indx, X_cols1[1])),
  regexp = msg9)

  msg10 = "y cannot be set as outcome variable 'y_col' and covariate in 'x_cols'."
  expect_error(DoubleMLData$new(data,
    x_cols = c(y_indx, X_cols1),
    y_col = y_indx,
    d_cols = d_indx,
    z_cols = z_indx),
  regexp = msg10)

  msg11 = "At least one variable/column is set as treatment variable \\('d_cols'\\) and instrumental variable in 'z_cols'."
  expect_error(DoubleMLData$new(data,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = c(z_indx, d_indx),
    z_cols = z_indx),
  regexp = msg11)

  D11 = DoubleMLData$new(data,
    x_cols = X_cols1,
    y_col = y_indx,
    d_cols = d_indx,
    z_cols = z_indx)

  msg12 = "Assertion on 'treatment_var' failed: Must be a subset of \\{'d'\\}, but is \\{'X1'\\}."
  expect_error(D11$set_data_model(X_cols1[1]),
    regexp = msg12)
}
)

test_that("Unit tests for invalid data", {
  # PLR with IV
  msg = paste(
    "Incompatible data.\\n",
    "z has been set as instrumental variable\\(s\\).\\n",
    "To fit a partially linear IV regression model use",
    "DoubleMLPLIV instead of DoubleMLPLR.")
  expect_error(DoubleMLPLR$new(
    data = data_pliv$dml_data,
    ml_g = mlr3::lrn("regr.rpart"),
    ml_m = mlr3::lrn("regr.rpart")),
  regexp = msg)

  # PLIV without IV
  msg = paste(
    "Incompatible data.\\n",
    "At least one variable must be set as instrumental variable.\\n",
    "To fit a partially linear regression model without instrumental",
    "variable\\(s\\) use DoubleMLPLR instead of DoubleMLPLIV.")
  expect_error(DoubleMLPLIV$new(
    data = data_plr$dml_data,
    ml_g = mlr3::lrn("regr.rpart"),
    ml_m = mlr3::lrn("regr.rpart"),
    ml_r = mlr3::lrn("regr.rpart")),
  regexp = msg)

  # IRM with IV
  msg = paste(
    "Incompatible data.\\n",
    "z has been set as instrumental variable\\(s\\).\\n",
    "To fit an interactive IV regression model use",
    "DoubleMLIIVM instead of DoubleMLIRM.")
  expect_error(DoubleMLIRM$new(
    data = data_iivm$dml_data,
    ml_g = mlr3::lrn("regr.rpart"),
    ml_m = mlr3::lrn("classif.rpart", predict_type = "prob")),
  regexp = msg)

  # IIVM without IV
  msg = paste(
    "Incompatible data.\\n",
    "To fit an IIVM model with DoubleML",
    "exactly one binary variable with values 0 and 1",
    "needs to be specified as instrumental variable.")
  expect_error(double_mlplr_obj <- DoubleMLIIVM$new(
    data = data_irm$dml_data,
    ml_g = mlr3::lrn("regr.rpart"),
    ml_m = mlr3::lrn("classif.rpart", predict_type = "prob")),
  regexp = msg)

  # non-binary D for IRM
  df = data_irm$df
  df["d"] = df["d"] * 5
  dml_data = double_ml_data_from_data_frame(df, y_col = "y", d_cols = "d")
  msg = paste(
    "Incompatible data.\\n",
    "To fit an IRM model with DoubleML",
    "exactly one binary variable with values 0 and 1",
    "needs to be specified as treatment variable.")
  expect_error(DoubleMLIRM$new(
    data = dml_data,
    ml_g = mlr3::lrn("regr.rpart"),
    ml_m = mlr3::lrn("classif.rpart", predict_type = "prob")),
  regexp = msg)

  # non-binary D for IIVM
  df = data_iivm$df
  df["d"] = df["d"] * 5
  dml_data = double_ml_data_from_data_frame(df,
    y_col = "y",
    d_cols = "d",
    z_cols = "z")
  msg = paste(
    "Incompatible data.\\n",
    "To fit an IIVM model with DoubleML",
    "exactly one binary variable with values 0 and 1",
    "needs to be specified as treatment variable.")
  expect_error(DoubleMLIIVM$new(
    data = dml_data,
    ml_g = mlr3::lrn("regr.rpart"),
    ml_m = mlr3::lrn("classif.rpart", predict_type = "prob")),
  regexp = msg)

  # non-binary Z for IIVM
  df = data_iivm$df
  df["z"] = df["z"] * 5
  dml_data = double_ml_data_from_data_frame(df,
    y_col = "y",
    d_cols = "d",
    z_cols = "z")
  msg = paste(
    "Incompatible data.\\n",
    "To fit an IIVM model with DoubleML",
    "exactly one binary variable with values 0 and 1",
    "needs to be specified as instrumental variable.")
  expect_error(DoubleMLIIVM$new(
    data = dml_data,
    ml_g = mlr3::lrn("regr.rpart"),
    ml_m = mlr3::lrn("classif.rpart", predict_type = "prob")),
  regexp = msg)

  # multiple D for IRM
  df = data_irm$df
  dml_data = double_ml_data_from_data_frame(df,
    y_col = "y",
    d_cols = c("d", "X1"))
  msg = paste(
    "Incompatible data.\\n",
    "To fit an IRM model with DoubleML",
    "exactly one binary variable with values 0 and 1",
    "needs to be specified as treatment variable.")
  expect_error(DoubleMLIRM$new(
    data = dml_data,
    ml_g = mlr3::lrn("regr.rpart"),
    ml_m = mlr3::lrn("classif.rpart", predict_type = "prob")),
  regexp = msg)

  # multiple D for IIVM
  df = data_iivm$df
  dml_data = double_ml_data_from_data_frame(df,
    y_col = "y",
    d_cols = c("d", "X1"),
    z_cols = "z")
  msg = paste(
    "Incompatible data.\\n",
    "To fit an IIVM model with DoubleML",
    "exactly one binary variable with values 0 and 1",
    "needs to be specified as treatment variable.")
  expect_error(DoubleMLIIVM$new(
    data = dml_data,
    ml_g = mlr3::lrn("regr.rpart"),
    ml_m = mlr3::lrn("classif.rpart", predict_type = "prob")),
  regexp = msg)

  # multiple Z for IIVM
  df = data_iivm$df
  dml_data = double_ml_data_from_data_frame(df,
    y_col = "y",
    d_cols = "d",
    z_cols = c("z", "X1"))
  msg = paste(
    "Incompatible data.\\n",
    "To fit an IIVM model with DoubleML",
    "exactly one binary variable with values 0 and 1",
    "needs to be specified as instrumental variable.")
  expect_error(DoubleMLIIVM$new(
    data = dml_data,
    ml_g = mlr3::lrn("regr.rpart"),
    ml_m = mlr3::lrn("classif.rpart", predict_type = "prob")),
  regexp = msg)
}
)
