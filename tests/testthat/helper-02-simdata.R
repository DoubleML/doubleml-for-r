
# Number of folds
K <- 10

# Number of repetitions for repeated CV
Rcv <- 1

# simulate data sets
settings <- list(list(n = 1000, p = 20),
                 list(n = 1000, p = 50),
                 list(n = 1000, p = 100))

n_settings <- length(settings)
data_plm <- vector("list", n_settings)

set.seed(1282)

for (i_setting in 1:n_settings) {
   # n <- 1000
# #  DGP <- DGP1(n, theta_0 = theta_0, p1 = 80, p2 = 80, cov_toep = 0.2,
# #              betamax = 4, gammamax = 2)
#   DGP <- DGP2(theta = theta_0, N = n, p = 20)
#   data_plm[[i_setting]] <- DGP # DGP$data
   data_plm[[i_setting]] <- DGP <- DGP4(settings[[i_setting]]$n,
                                        settings[[i_setting]]$p, 
                                        betamax = 9, decay = 0.99, 
                                        threshold = 0.75, noisevar = 3)$data
}

