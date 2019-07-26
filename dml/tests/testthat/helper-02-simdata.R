# True effect
theta_0 <- 4

# Number of folds
K <- 10

# Number of repetitions for repeated CV
Rcv <- 1

# simulate data sets
settings <- list(list(n = 100, p = 500),
                 list(n = 100, p = 1000),
                 list(n = 1000, p = 2000))

n_settings <- length(settings)
data_plm <- vector("list", n_settings)

set.seed(1282)
for (i_setting in 1:n_settings) {
  n <- 500
#  DGP <- DGP1(n, theta_0 = theta_0, p1 = 80, p2 = 80, cov_toep = 0.2,
#              betamax = 4, gammamax = 2)
  DGP <- DGP2(theta = theta_0, N = n, p = 20)
  data_plm[[i_setting]] <- DGP # DGP$data
}

get_default_mlmethod <- function(learner) {
  if (learner == 'regr.lm') {
    mlmethod <- list(mlmethod_m = learner,
                     mlmethod_g = learner)
    params <- list(params_g = list(),
                   params_m = list())

  }
  else if (learner == 'regr.ranger') {
    mlmethod <- list(mlmethod_m = learner,
                     mlmethod_g = learner)

    params <- list(params_g = list(num.trees = 100),
                   params_m = list(num.trees = 120))

  }

  return(list(mlmethod=mlmethod, params=params))
}

