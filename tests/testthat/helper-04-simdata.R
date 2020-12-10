
# simulate data sets
settings <- list(list(theta = 0.5, n = 1000, p = 20),
                 list(theta = 1.5, n = 1000, p = 50),
                 list(theta = -0.75, n = 1000, p = 100))
settings <- list(list(theta = 0.5, n = 1000, p = 20))

on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  settings_irm <- list(list(theta = 0.5, n = 1000, p = 20))
} else {
  settings_irm <- list(list(theta = 0.5, n = 5000, p = 20))
}

n_settings <- length(settings)

data_plr <- vector("list", n_settings)
set.seed(1282)

for (i_setting in 1:n_settings) {
   data_plr[[i_setting]] <- dgp1_plr(settings[[i_setting]]$theta,
                                     settings[[i_setting]]$n,
                                     settings[[i_setting]]$p)
}

data_pliv <- vector("list", n_settings)
set.seed(1282)

for (i_setting in 1:n_settings) {
  data_pliv[[i_setting]] <- dgp1_iv(settings[[i_setting]]$theta,
                                    settings[[i_setting]]$n,
                                    settings[[i_setting]]$p)
}

data_irm <- vector("list", n_settings)
set.seed(1282)

for (i_setting in 1:n_settings) {
  data_irm[[i_setting]] <- dgp1_irm(settings_irm[[i_setting]]$theta,
                                    settings_irm[[i_setting]]$n,
                                    settings_irm[[i_setting]]$p)
}

data_iivm <- vector("list", n_settings)
set.seed(1282)

for (i_setting in 1:n_settings) {
  data_iivm[[i_setting]] <- dgp1_irmiv(settings[[i_setting]]$theta,
                                       settings[[i_setting]]$n,
                                       settings[[i_setting]]$p)
}

data_plr_multi <- vector("list", n_settings)
set.seed(1282)

for (i_setting in 1:n_settings) {
  data_plr_multi[[i_setting]] <- dgp1_toeplitz(settings[[i_setting]]$n,
                                               settings[[i_setting]]$p)
}

