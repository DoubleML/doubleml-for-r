# Some DGP

# (Approx.) Sparse DGP
DGP4 = function(n, p, betamax = 4, decay = 0.99, threshold = 0, noisevar = 10, ...) {

  beta = vector("numeric", length = p)

  for (j in 1:p) {
    beta[j] = betamax * (j)^{
      -decay
    }
  }

  beta[beta < threshold] = 0

  covar = stats::toeplitz(0.9^(0:(p - 1)))
  diag(covar) = rep(1, p)
  mu = rep(0, p)

  x = mvtnorm::rmvnorm(n = n, mean = mu, sigma = covar)
  e = stats::rnorm(n, sd = sqrt(noisevar))
  y = x %*% beta + e

  colnames(x) = paste0("Var", seq(from = 1, to = p))
  names(y) = "y"
  return(list(data = data.frame(y, x), beta = beta, covar = covar))
}