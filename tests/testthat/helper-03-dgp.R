
# dgps

g = function(x) {
  res = sin(x)^2
  return(res)
}

m = function(x, nu = 0, gamma = 1) {
  xx = sinh(gamma) / (cosh(gamma) - cos(x - nu))
  res = 0.5 / pi * xx
  return(res)
}

dgp1_plr = function(theta, N, k) {

  b = 1 / (1:k)
  sigma = clusterGeneration::genPositiveDefMat(k, "unifcorrmat")$Sigma

  X = mvtnorm::rmvnorm(N, sigma = sigma)
  G = g(as.vector(X %*% b))
  M = m(as.vector(X %*% b))
  d = M + rnorm(N)
  y = theta * d + G + rnorm(N)

  data = data.frame(y, d, X)
  return(data)
}


dgp1_iv = function(theta, N, k) {

  b = 1 / (1:k)
  sigma = clusterGeneration::genPositiveDefMat(k, "unifcorrmat")$Sigma

  X = mvtnorm::rmvnorm(N, sigma = sigma)
  G = g(as.vector(X %*% b))
  M = m(as.vector(X %*% b))
  z = X[, 1] * b[1] + X[, 2] * b[2] + rnorm(N)
  z2 = X[, 2] * b[2] + X[, 3] * b[3] + rnorm(N)
  U = rnorm(N)
  d = 3 * z + M + rnorm(N) - 4 * U
  y = theta * d + G + 4 * U + rnorm(N)

  data = data.frame(y, d, z, z2, X)
  return(data)
}


dgp1_irm = function(theta, N, k) {

  b = 1 / (1:k)
  sigma = clusterGeneration::genPositiveDefMat(k, "unifcorrmat")$Sigma

  X = mvtnorm::rmvnorm(N, sigma = sigma)
  G = g(as.vector(X %*% b))
  M = m(as.vector(X %*% b))
  pr = 1 / (1 + exp(-(1) * (X[, 1] * (-0.5) + X[, 2] * 0.5 + rnorm(N))))
  d = rbinom(N, 1, pr)

  err = rnorm(N)

  # y1 = theta + G + err
  # y0 = G + err
  # ATTE = mean(y1[d==1]) - mean(y0[d==1])

  y = theta * d + G + err

  data = data.frame(y, d, X)

  return(data)
}

dgp1_irm_binary = function(theta, N, k) {

  b = 1 / (1:k)
  sigma = clusterGeneration::genPositiveDefMat(k, "unifcorrmat")$Sigma

  X = mvtnorm::rmvnorm(N, sigma = sigma)
  G = g(as.vector(X %*% b))
  M = m(as.vector(X %*% b))
  pr = 1 / (1 + exp(-(1) * (X[, 1] * (-0.5) + X[, 2] * 0.5 + rnorm(N))))
  d = rbinom(N, 1, pr)

  err = rnorm(N)

  # y1 = theta + G + err
  # y0 = G + err
  # ATTE = mean(y1[d==1]) - mean(y0[d==1])

  pry = 1 / (1 + exp(-1 * theta * d + G + err))
  y = rbinom(N, 1, pry)

  data = data.frame(y, d, X)

  return(data)
}

dgp1_irmiv = function(theta, N, k) {

  b = 1 / (1:k)
  sigma = clusterGeneration::genPositiveDefMat(k, "unifcorrmat")$Sigma

  X = mvtnorm::rmvnorm(N, sigma = sigma)
  G = g(as.vector(X %*% b))
  M = m(as.vector(X %*% b))

  pr_z = 1 / (1 + exp(-(1) * X[, 1] * b[5] + X[, 2] * b[2] + rnorm(N)))
  z = rbinom(N, 1, pr_z)

  U = rnorm(N)
  pr = 1 / (1 + exp(-(1) * (0.5 * z + X[, 1] * (-0.5) + X[, 2] * 0.25 - 0.5 * U + rnorm(N))))
  d = rbinom(N, 1, pr)
  err = rnorm(N)

  y = theta * d + G + 4 * U + err

  # y1 = theta + G + err
  # y0 = G + err
  # ATTE = mean(y1[d==1]) - mean(y0[d==1])

  data = data.frame(y, d, z, X)

  return(data)
}

dgp1_irmiv_binary = function(theta, N, k) {

  b = 1 / (1:k)
  sigma = clusterGeneration::genPositiveDefMat(k, "unifcorrmat")$Sigma

  X = mvtnorm::rmvnorm(N, sigma = sigma)
  G = g(as.vector(X %*% b))
  M = m(as.vector(X %*% b))

  pr_z = 1 / (1 + exp(-(1) * X[, 1] * b[5] + X[, 2] * b[2] + rnorm(N)))
  z = rbinom(N, 1, pr_z)

  U = rnorm(N)
  pr = 1 / (1 + exp(-(1) * (0.5 * z + X[, 1] * (-0.5) + X[, 2] * 0.25 - 0.5 * U + rnorm(N))))
  d = rbinom(N, 1, pr)
  err = rnorm(N)

  pry = 1 / (1 + exp((-1) * theta * d + G + 4 * U + err))
  y = rbinom(N, 1, pry)

  # y1 = theta + G + err
  # y0 = G + err
  # ATTE = mean(y1[d==1]) - mean(y0[d==1])

  data = data.frame(y, d, z, X)

  return(data)
}

dgp1_toeplitz = function(n, p, betamax = 4, decay = 0.99, threshold = 0, noisevar = 10, ...) {

  beta = vector("numeric", length = p)

  for (j in 1:p) {
    beta[j] = betamax * (j)^{
      -decay
    }
  }

  beta[beta < threshold] = 0

  cols_treatment = c(1, 5, 10)

  covar = toeplitz(0.9^(0:(p - 1)))
  diag(covar) = rep(1, p)
  mu = rep(0, p)

  X = mvtnorm::rmvnorm(n = n, mean = mu, sigma = covar)
  e = rnorm(n, sd = sqrt(noisevar))
  y = X %*% beta + e

  d = X[, cols_treatment]
  X = X[, -cols_treatment]

  # colnames(x) = paste0("X", seq( from = 1, to = dim(x)[1] ))
  colnames(d) = paste0("d", seq(from = 1, to = length(cols_treatment)))
  # names(y) = "y"
  data = data.frame(y, d, X)

  return(data)
}

make_data_pliv_partialZ = function(n_obs, alpha = 1, dim_x = 5, dim_z = 150) {

  sigma_e_u = matrix(c(1, 0.6, 0.6, 1), ncol = 2)
  mu_e_u = rep(0, 2)
  e_u = mvtnorm::rmvnorm(n = n_obs, mean = mu_e_u, sigma = sigma_e_u)
  epsilon = e_u[, 1]
  u = e_u[, 2]

  sigma_x = toeplitz(0.5^(0:(dim_x - 1)))
  mu_x = rep(0, dim_x)
  x = mvtnorm::rmvnorm(n = n_obs, mean = mu_x, sigma = sigma_x)

  I_z = diag(x = 1, ncol = dim_z, nrow = dim_z)
  mu_xi = rep(0, dim_z)
  xi = mvtnorm::rmvnorm(n = n_obs, mean = mu_xi, sigma = 0.25 * I_z)

  beta = 1 / (1:dim_x)^2
  gamma = beta
  delta = 1 / (1:dim_z)^2

  zeros = matrix(0, nrow = dim_x, ncol = (dim_z - dim_x))
  I_x = diag(x = 1, ncol = dim_x, nrow = dim_x)
  Pi = cbind(I_x, zeros)

  z = x %*% Pi + xi
  d = x %*% gamma + z %*% delta + u
  y = alpha * d + x %*% beta + epsilon


  colnames(x) = paste0("X", 1:dim_x)
  colnames(z) = paste0("Z", 1:dim_z)
  colnames(y) = "y"
  colnames(d) = "d"

  data = data.frame(x, y, d, z)
  return(data)
}
