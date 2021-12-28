
# simulate data sets
setting = list(theta = 0.5, n = 1000, p = 20)

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  setting_irm = list(theta = 0.5, n = 1000, p = 20)
} else {
  setting_irm = list(theta = 0.5, n = 5000, p = 20)
}

setting_pliv_partial = list(theta = 1.0, n = 500)

set.seed(1282)
df = dgp1_plr(
  setting$theta,
  setting$n,
  setting$p
)
Xnames = names(df)[names(df) %in% c("y", "d", "z") == FALSE]
dml_data = double_ml_data_from_data_frame(df,
  y_col = "y",
  d_cols = "d", x_cols = Xnames
)
data_plr = list(
  df = df,
  dml_data = dml_data
)

set.seed(1282)
df = dgp1_iv(
  setting$theta,
  setting$n,
  setting$p
)
Xnames = names(df)[names(df) %in% c("y", "d", "z") == FALSE] # note that Xnames includes z2
dml_data = double_ml_data_from_data_frame(df,
  y_col = "y",
  d_cols = "d", x_cols = Xnames, z_cols = "z"
)
data_pliv = list(
  df = df,
  dml_data = dml_data
)

set.seed(1282)
df = dgp1_irm(
  setting_irm$theta,
  setting_irm$n,
  setting_irm$p
)
Xnames = names(df)[names(df) %in% c("y", "d", "z") == FALSE]
dml_data = double_ml_data_from_data_frame(df,
  y_col = "y",
  d_cols = "d", x_cols = Xnames
)
data_irm = list(
  df = df,
  dml_data = dml_data
)

set.seed(1282)
df = dgp1_irm_binary(
  setting_irm$theta,
  setting_irm$n,
  setting_irm$p
)
Xnames = names(df)[names(df) %in% c("y", "d", "z") == FALSE]
dml_data = double_ml_data_from_data_frame(df,
  y_col = "y",
  d_cols = "d", x_cols = Xnames
)
data_irm_binary = list(
  df = df,
  dml_data = dml_data
)

set.seed(1282)
df = dgp1_irmiv(
  setting$theta,
  setting$n,
  setting$p
)
Xnames = names(df)[names(df) %in% c("y", "d", "z") == FALSE]
dml_data = double_ml_data_from_data_frame(df,
  y_col = "y",
  d_cols = "d", x_cols = Xnames, z_col = "z"
)
data_iivm = list(
  df = df,
  dml_data = dml_data
)

set.seed(1282)
df = dgp1_irmiv_binary(
  setting$theta,
  setting$n,
  setting$p
)
Xnames = names(df)[names(df) %in% c("y", "d", "z") == FALSE]
dml_data = double_ml_data_from_data_frame(df,
  y_col = "y",
  d_cols = "d", x_cols = Xnames, z_col = "z"
)
data_iivm_binary = list(
  df = df,
  dml_data = dml_data
)

set.seed(1282)
data_plr_multi = dgp1_toeplitz(
  setting$n,
  setting$p
)

set.seed(1282)
dim_z = 150
df = make_pliv_CHS2015(
  setting$n,
  alpha = setting$theta,
  dim_z = dim_z,
  return_type = "data.frame"
)
Xnames = names(df)[names(df) %in% c("y", "d", paste0("Z", 1:dim_z)) == FALSE]
dml_data = double_ml_data_from_data_frame(df,
  y_col = "y",
  d_cols = "d", x_cols = Xnames, z_cols = paste0("Z", 1:dim_z)
)
data_pliv_partialXZ = list(
  df = df,
  dml_data = dml_data
)

set.seed(1282)
dim_z = 5
df = make_pliv_CHS2015(
  setting$n,
  alpha = setting$theta,
  dim_z = dim_z,
  return_type = "data.frame"
)
Xnames = names(df)[names(df) %in% c("y", "d", paste0("Z", 1:dim_z)) == FALSE]
dml_data = double_ml_data_from_data_frame(df,
  y_col = "y",
  d_cols = "d", x_cols = Xnames, z_cols = paste0("Z", 1:dim_z)
)
data_pliv_partialX = list(
  df = df,
  dml_data = dml_data
)

set.seed(1282)
dim_z = 150
df = make_data_pliv_partialZ(
  setting$n,
  alpha = setting$theta,
  dim_x = 5
)
Xnames = names(df)[names(df) %in% c("y", "d", paste0("Z", 1:dim_z)) == FALSE]
dml_data = double_ml_data_from_data_frame(df,
  y_col = "y",
  d_cols = "d", x_cols = Xnames, z_cols = paste0("Z", 1:dim_z)
)
data_pliv_partialZ = list(
  df = df,
  dml_data = dml_data
)
