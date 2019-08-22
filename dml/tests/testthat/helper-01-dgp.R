# Some DGP

# (Approx.) Sparse DGP
DGP4 = function(n, p, betamax = 4, decay = 0.99, threshold = 0, noisevar = 10,  ...){
  
  beta = vector("numeric", length = p)
  
  for (j in 1:p){
    beta[j]= betamax*(j)^{-decay}
  }
  
  beta[beta<threshold] = 0
  
  covar = stats::toeplitz(0.9^(0:(p-1)))
  diag(covar) = rep(1,p)
  mu = rep(0,p)
  
  x = mvtnorm::rmvnorm(n=n, mean=mu, sigma=covar)
  e = stats::rnorm(n, sd = sqrt(noisevar))
  y = x%*%beta + e
  
  colnames(x) = paste0("Var", seq( from = 1, to = p ))
  names(y) = "y"
  return(list(data = data.frame(y,x), beta = beta, covar = covar))
}


# 
# g <- function(x){
#   res = sin(x)^2
#   return(res)
# }
# 
# m <- function(x, nu=0, gamma=1){
#   xx = sinh(gamma) / (cosh(gamma) - cos(x-nu))
#   res = 0.5 / pi * xx
#   return(res)
# }
# 
# DGP2 <- function(theta, N, p) {
# 
#   b <- 1/(1:p)
#   sigma <- clusterGeneration::genPositiveDefMat(p,"unifcorrmat")$Sigma
# 
#   X = mvtnorm::rmvnorm(N, sigma=sigma)
#   G = g(as.vector(X%*%b))
#   M = m(as.vector(X%*%b))
#   d = M + stats::rnorm(N)
#   y = theta * d + G + stats::rnorm(N)
# 
#   data <- data.frame(y,d,X)
#   return(data)
# }
# 
# 
# DGP1 <- function(n = 100, theta_0 = 4, p1 = 100, p2 = 100, s1 = 3, s2 = 3,
#                  betamax = 8, gammamax = 8, betamin = 0,
#                  gammamin = 0, decay1 = 0, decay2 = 0, threshold1 = 0,
#                  threshold2 = 0, var_eps1 = 10, var_eps2 = 10, cov_toep = 0.5, ...) {
# 
#   if (!identical(p1, p2)) stop("p1 and p2 do not match, adjust DGP (tbd)...")
# 
#   # else{
# 
#   beta <-  vector("numeric", length = p1)
#   gamma <- vector("numeric", length = p2)
# 
#   # Coefficients (linear model)
#   # if (p1 == p2) {
#     for (j in 1:p1) {
#     beta[j] <- betamax*(j)^(-decay1)
#     gamma[j] <- gammamax*(j)^(-decay2)
#     }
#   # }
# 
#   # else{
#   #   for (j in 1:p1) {
#   #     beta[j] <- betamax*(j)^{-decay1}
#   #   }
#   #   for (j in 1:p2) {
#   #   gamma[j] <- gammamax*(j)^{-decay2}
#   #   }
#   # }
# 
#   beta[(s1+1):p1] <- 0
#   beta[beta < threshold1] <- 0
# 
#   gamma[(s2+1):p2] <- 0
#   gamma[gamma < threshold2] <- 0
# 
#   # if (p1 == p2) {
# 
#   # covariates
#   covar <- stats::toeplitz(cov_toep^(0:(p1 - 1)))
#   diag(covar) <- rep(1, p1)
#   mu <- rep(0, p1)
#   x <- mvtnorm::rmvnorm(n = n, mean = mu, sigma = covar)
# 
#   # error aux reg
#   e2 <- stats::rnorm(n, sd = sqrt(var_eps2))
# 
#   # treatment
#   pr <- exp(x%*%gamma + e2 )/(1+exp(x%*%gamma + e2))
#   d <- stats::rbinom(n,1,pr)
# 
#   # main reg
#   e1 <- stats::rnorm(n, sd = sqrt(var_eps1))
#   y <- d*theta_0 + x%*%beta + e1
# 
# 
#   colnames(x) = paste0("Var", seq( from = 1, to = p1))
#   names(y) = "y"
# 
#   return(list(data = data.frame(y, d, x), beta = beta, covar = covar,
#               gamma = gamma, pr = pr, theta_0 = theta_0) )
#   # }
# }
# 
# 


# #### function for DGP
# summary_dgp1 <- function(dgp) {
#   p <- dim(dgp$data[,])[2] - 1 # don't count y in dimension
#   n <- dim(dgp$data)[1]
#   beta <- dgp$beta
#   gamma <- dgp$gamma
#   theta_0 <- dgp$theta_0
#   s_beta <- sum(beta!=0)
#   s_gamma <- sum(gamma!=0)
#   tab_d <- table(dgp$data$d)
# 
#   params <- list(p = p, n = n, beta = beta, gamma = gamma,
#                  s_beta = s_beta, s_gamma = s_gamma, tab_d = tab_d, theta_0 = theta_0)
# 
#   id <- c(1:length(beta))
#   df_coef <- data.frame(beta, gamma)
#   df_coef<- tidyr::gather(df_coef, key = "cf_id")
#   df_coef$id <- rep(id, 2)
# 
#   p_beta <- ggplot2::ggplot(df_coef, aes(x = id, y = value, col = cf_id))  + geom_point() +
#             xlab("Coefficient")
# 
#   pr_d1 <- data.frame(dgp$pr)
#   d <- dgp$data$d
#   pr_d1 <- data.frame(dgp$pr, d)
#   g_d1 <- ggplot2::ggplot(pr_d1, aes(x=dgp.pr)) + geom_density(alpha=.3) +
#                           geom_vline(xintercept = 0.5, linetype = "dashed", color = "black", alpha = 0.4) + xlab( "P(d=1)")
# #  pr_d1 <- gather(pr_d1, d, dgp.pr)
# #  g <- ggplot2::ggplot(pr_d1, aes(x=dgp.pr, col = d)) + geom_density(alpha=.3) + xlab( "P(d=1)")
# 
#   df_y1 <- cbind(dplyr::filter(dgp$data, d==1)$y, 1)
#   df_y0 <- cbind(dplyr::filter(dgp$data, d==0)$y, 0)
#   df_y <- data.frame(rbind(df_y1, df_y0))
#   names(df_y) <-  c("y", "d")
#   df_y$d <- as.factor(df_y$d)
# 
#   g_y <- ggplot2::ggplot(df_y, aes(x = d, y= y)) + geom_boxplot() + xlab("Treatment status, d")
# 
# 
#   return(list(params = params, p_beta = p_beta, g_d1 = g_d1, g_y = g_y))
# 
# }
# 
# 
# # Some DGP: Instrumental Variable
# 
# DGP1_IV <- function(n = 100, theta_0 = 4, p1 = 100, p2 = 100, p3 = 100, s1 = 3, s2 = 3, s3 = 4,
#                  betamax = 8, gammamax = 8, gamma2max = 8, gammaz = 6, betamin = 0,
#                  gammamin = 0, gamma2min = 0, decay1 = 0, decay2 = 0, decay3 = 0, threshold1 = 0,
#                  threshold2 = 0, threshold3 = 0, var_eps1 = 10, var_eps2 = 10, var_eps3 = 10, cov_toep = 0.5, ...) {
# 
#   if (!identical(p1, p2, p3)) stop("p1, p2, and p3 do not match, adjust DGP (tbd)...")
# 
#   # else{
# 
#   beta <-  vector("numeric", length = p1)
#   gamma <- vector("numeric", length = p2)
#   gamma2 <- vector("numeric", length = p3)
# 
#   gamma2indx <- base::sample(p3, size = s3, replace = FALSE)
# 
#   # Coefficients (linear model)
#   # if (p1 == p2) {
#   for (j in 1:p1) {
#     beta[j] <- betamax*(j)^(-decay1)
#     gamma[j] <- gammamax*(j)^(-decay2)
#   }
# 
#   for (j in 1:s3) {
#     gamma2[gamma2indx][j] <- gamma2max*(j)^(-decay3)
#   }
#   # }
# 
#   # else{
#   #   for (j in 1:p1) {
#   #     beta[j] <- betamax*(j)^{-decay1}
#   #   }
#   #   for (j in 1:p2) {
#   #   gamma[j] <- gammamax*(j)^{-decay2}
#   #   }
#   # }
# 
#   beta[(s1+1):p1] <- 0
#   beta[beta < threshold1] <- 0
# 
#   gamma[(s2+1):p2] <- 0
#   gamma[gamma < threshold2] <- 0
# 
#   gamma2[gamma2 < threshold3] <- 0
# 
#   # if (p1 == p2) {
# 
#   # covariates
#   covar <- stats::toeplitz(cov_toep^(0:(p1 - 1)))
#   diag(covar) <- rep(1, p1)
#   mu <- rep(0, p1)
#   x <- mvtnorm::rmvnorm(n = n, mean = mu, sigma = covar)
# 
#   # error aux reg
#   e2 <- stats::rnorm(n, sd = sqrt(var_eps2))
# 
#   # error aux reg
#   e3 <- stats::rnorm(n, sd = sqrt(var_eps3))
# 
#   # instrument
#   pr2 <- exp(x%*%gamma2 + e3 )/(1+exp(x%*%gamma2 + e3))
#   z <- stats::rbinom(n,1,pr2)
# 
#   # treatment
#   pr <- exp(gammaz*z + x%*%gamma + e2 )/(1+exp(gammaz*z + x%*%gamma + e2))
#   d <- stats::rbinom(n,1,pr)
# 
#   # main reg
#   e1 <- stats::rnorm(n, sd = sqrt(var_eps1))
#   y <- d*theta_0 + x%*%beta + e1
# 
# 
#   colnames(x) = paste0("Var", seq( from = 1, to = p1))
#   names(y) = "y"
# 
#   return(list(data = data.frame(y, d, z, x), beta = beta, covar = covar,
#               gamma = gamma, gamma2 = gamma2, gammaz = gammaz, pr = pr, pr2 = pr2) )
#   # }
# }
# 
# 
# #### function for DGP
# summary_dgp1_IV <- function(dgp) {
#   p <- dim(dgp$data[,])[2] - 1 # don't count y in dimension
#   n <- dim(dgp$data)[1]
#   beta <- dgp$beta
#   gamma <- dgp$gamma
#   gamma2 <- dgp$gamma2
#   gammaz <- dgp$gammaz
#   s_beta <- sum(beta!=0)
#   s_gamma <- sum(gamma!=0) # excluding z
#   s_gamma2 <- sum(gamma2!=0)
#   tab_d <- table(dgp$data$d)
#   tab_z <- table(dgp$data$z)
#   tab_dz <- table(dgp$data$d, dgp$data$z)
# 
#   params <- list(p = p, n = n, beta = beta, gamma = gamma, gamma_2 = gamma2,
#                  s_beta = s_beta, s_gamma = s_gamma,
#                  s_gamma2 = s_gamma2, tab_d = tab_d, tab_z = tab_z, tab_dz = tab_dz)
# 
#   id <- c(1:length(beta))
#   df_coef <- data.frame(beta, gamma, gamma2)
#   df_coef <- tidyr::gather(df_coef, key = "cf_id")
#   df_coef$id <- rep(id, 3)
# 
#   p_beta <- ggplot2::ggplot(df_coef, aes(x = id, y = value, col = cf_id, shape = cf_id))  + geom_point() +
#     xlab("Coefficient")
# 
#   pr_d1 <- data.frame(dgp$pr)
#   d <- dgp$data$d
#   z <- dgp$data$z
#   pr_d1 <- data.frame(dgp$pr, d, dgp$pr2, z)
# 
#   pr_df <- tidyr::gather(pr_d1, key = "id")
#   pr_df <- dplyr::filter(pr_df, id %in% c("d","z"))
# 
#   g_d1 <- ggplot2::ggplot(pr_df, aes(x=value, col = id)) + geom_density(alpha=.3) +
#     geom_vline(xintercept = 0.5, linetype = "dashed", color = "black", alpha = 0.4) + xlab( "P(d=1)")
#   #  pr_d1 <- gather(pr_d1, d, dgp.pr)
#   #  g <- ggplot2::ggplot(pr_d1, aes(x=dgp.pr, col = d)) + geom_density(alpha=.3) + xlab( "P(d=1)")
# 
#   df_y1 <- cbind(dplyr::filter(dgp$data, d==1)$y, 1)
#   df_y0 <- cbind(dplyr::filter(dgp$data, d==0)$y, 0)
#   df_y <- data.frame(rbind(df_y1, df_y0))
#   names(df_y) <-  c("y", "d")
#   df_y$d <- as.factor(df_y$d)
# 
#   g_y <- ggplot2::ggplot(df_y, aes(x = d, y= y)) + geom_boxplot() + xlab("Treatment status, d")
# 
# 
#   return(list(params = params, p_beta = p_beta, g_d1 = g_d1, g_y = g_y))
# 
# }

