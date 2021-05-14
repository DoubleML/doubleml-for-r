
# simulate data sets
settings = list(
  list(theta = 0.5, n = 1000, p = 20),
  list(theta = 1.5, n = 1000, p = 50),
  list(theta = -0.75, n = 1000, p = 100))
settings = list(list(theta = 0.5, n = 1000, p = 20))

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  settings_irm = list(list(theta = 0.5, n = 1000, p = 20))
} else {
  settings_irm = list(list(theta = 0.5, n = 5000, p = 20))
}

settings_pliv_partial = list(list(theta=1.0, n = 500))

n_settings = length(settings)

data_plr = vector("list", n_settings)
set.seed(1282)

for (i_setting in 1:n_settings) {
  df = dgp1_plr(
    settings[[i_setting]]$theta,
    settings[[i_setting]]$n,
    settings[[i_setting]]$p)
  Xnames = names(df)[names(df) %in% c("y", "d", "z") == FALSE]
  dml_data = double_ml_data_from_data_frame(df,
                                            y_col = "y",
                                            d_cols = "d", x_cols = Xnames)
  data_plr[[i_setting]] = list(df = df,
                               dml_data = dml_data)
}

data_pliv = vector("list", n_settings)
set.seed(1282)

for (i_setting in 1:n_settings) {
   df = dgp1_iv(
    settings[[i_setting]]$theta,
    settings[[i_setting]]$n,
    settings[[i_setting]]$p)
  Xnames = names(df)[names(df) %in% c("y", "d", "z") == FALSE] # note that Xnames includes z2
  dml_data = double_ml_data_from_data_frame(df,
                                            y_col = "y",
                                            d_cols = "d", x_cols = Xnames, z_cols = "z")
  data_pliv[[i_setting]] = list(df = df,
                                dml_data = dml_data)
}

data_irm = vector("list", n_settings)
set.seed(1282)

for (i_setting in 1:n_settings) {
  df = dgp1_irm(
    settings_irm[[i_setting]]$theta,
    settings_irm[[i_setting]]$n,
    settings_irm[[i_setting]]$p)
  Xnames = names(df)[names(df) %in% c("y", "d", "z") == FALSE]
  dml_data = double_ml_data_from_data_frame(df,
                                            y_col = "y",
                                            d_cols = "d", x_cols = Xnames)
  data_irm[[i_setting]] = list(df = df,
                               dml_data = dml_data)
}

data_iivm = vector("list", n_settings)
set.seed(1282)

for (i_setting in 1:n_settings) {
  df = dgp1_irmiv(
    settings[[i_setting]]$theta,
    settings[[i_setting]]$n,
    settings[[i_setting]]$p)
  Xnames = names(df)[names(df) %in% c("y", "d", "z") == FALSE]
  dml_data = double_ml_data_from_data_frame(df,
                                            y_col = "y",
                                            d_cols = "d", x_cols = Xnames, z_col = "z")
  data_iivm[[i_setting]] = list(df = df,
                                dml_data = dml_data)
}

data_plr_multi = vector("list", n_settings)
set.seed(1282)

for (i_setting in 1:n_settings) {
  data_plr_multi[[i_setting]] = dgp1_toeplitz(
    settings[[i_setting]]$n,
    settings[[i_setting]]$p)
}

data_pliv_partialXZ = vector("list", length(settings_pliv_partial))
set.seed(1282)

dim_z = 150
for (i_setting in 1:length(settings_pliv_partial)) {
  df = make_pliv_CHS2015(
    settings[[i_setting]]$n,
    alpha = settings[[i_setting]]$theta,
    dim_z = dim_z,
    return_type = "data.frame")
  Xnames = names(df)[names(df) %in% c("y", "d", paste0("Z", 1:dim_z)) == FALSE]
  dml_data = double_ml_data_from_data_frame(df,
                                            y_col = "y",
                                            d_cols = "d", x_cols = Xnames, z_cols = paste0("Z", 1:dim_z))
  data_pliv_partialXZ[[i_setting]] = list(df = df,
                                          dml_data = dml_data)
}

data_pliv_partialX = vector("list", length(settings_pliv_partial))
set.seed(1282)

dim_z = 5
for (i_setting in 1:length(settings_pliv_partial)) {
  df = make_pliv_CHS2015(
    settings[[i_setting]]$n,
    alpha = settings[[i_setting]]$theta,
    dim_z = dim_z,
    return_type = "data.frame")
  Xnames = names(df)[names(df) %in% c("y", "d", paste0("Z", 1:dim_z)) == FALSE]
  dml_data = double_ml_data_from_data_frame(df,
                                            y_col = "y",
                                            d_cols = "d", x_cols = Xnames, z_cols = paste0("Z", 1:dim_z))
  data_pliv_partialX[[i_setting]] = list(df = df,
                                         dml_data = dml_data)
}

data_pliv_partialZ = vector("list", length(settings_pliv_partial))
set.seed(1282)

dim_z = 150
for (i_setting in 1:length(settings_pliv_partial)) {
  df = make_data_pliv_partialZ(
    settings[[i_setting]]$n,
    alpha = settings[[i_setting]]$theta,
    dim_x = 5)
  Xnames = names(df)[names(df) %in% c("y", "d", paste0("Z", 1:dim_z)) == FALSE]
  dml_data = double_ml_data_from_data_frame(df,
                                            y_col = "y",
                                            d_cols = "d", x_cols = Xnames, z_cols = paste0("Z", 1:dim_z))
  data_pliv_partialZ[[i_setting]] = list(df = df,
                                         dml_data = dml_data)
}
