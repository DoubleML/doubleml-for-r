# Function to load 401k data set
#' @export
load_401k = function() {
  url = 'https://github.com/VC2015/DMLonGitHub/raw/master/sipp1991.dta'
  data = foreign::read.dta(url)
  return(data)
}

# Set up example causal PLR model with 401k example

# quickstart_example = function() {
#   data = load_401k()
  # DoubleML_PLR_401k = double_ml_data_from_data_frame(data,
  #                                         x_cols = c("age", "inc", "educ",
  #                                                     "fsize", "marr", "twoearn",
  #                                                     "db", "pira", "hown"),
  #                                         y_col = "net_tfa",
  #                                         d_cols = "e401")
#   return(DoubleML_PLR_401k)
# }


#' @export
DGP_pliv_CHS2015 = function(n_samples, alpha = 1, dim_x = 200, dim_z = 150){
  # see https://assets.aeaweb.org/asset-server/articles-attachments/aer/app/10505/P2015_1022_app.pdf
  if (dim_x < dim_z) {
    stop("Dimension of X should be greater than dimension of Z.")
  }
  
  sigma_e_u = matrix(c(1, 0.6, 0.6, 1), ncol = 2)
  mu_e_u = rep(0, 2)
  e_u = mvtnorm::rmvnorm(n = n_samples, mean = mu_e_u, sigma = sigma_e_u)
  epsilon = e_u[,1]  
  u = e_u[,2]
  
  sigma_x = stats::toeplitz(0.5^(0:(dim_x - 1)))
  mu_x = rep(0, dim_x)
  X = mvtnorm::rmvnorm(n = n_samples, mean = mu_x, sigma = sigma_x)
  
  I_z = diag(x = 1, ncol = dim_z, nrow = dim_z)
  mu_xi = rep(0, dim_z)
  xi = mvtnorm::rmvnorm(n = n_samples, mean = mu_xi, sigma = 0.25*I_z)
  
  beta = 1/(1:dim_x)^2 
  gamma = beta
  delta = 1/(1:dim_z)^2
  
  zeros = matrix(0, nrow = dim_z, ncol = (dim_x - dim_z))
  Pi = cbind(I_z, zeros)
  
  Z = X%*%t(Pi) + xi
  D = X%*%gamma + Z%*% delta + u
  Y = alpha * D + X%*%beta + epsilon
  
  data = data.table("X" = X, "Y" = Y, "D" = D, "Z" = Z)
  
  return(data)
}

m = function(x, nu = 0, gamma = 1){
  y = 0.5/pi *sinh(gamma)/(cosh(gamma)-cos(x-nu))
  return(y)
}

g = function(x){
  y = sin(x)^2
}
  
  
#' @export
make_plr_data = function(n_samples = 100, n_features = 20, theta = 0.5, return_X_y_d = FALSE){

  b = 1/(1:n_features)
  sigma = clusterGeneration::genPositiveDefMat(n_features)
  X = mvtnorm::rmvnorm(n = n_samples, mean = rep(0, n_features), sigma = sigma$Sigma)
  G = g(X%*%b)
  M = m(X%*%b) 
  d = M + stats::rnorm(n_samples)
  y = theta*d + G + stats::rnorm(n_samples)
  
  colnames(X) = paste0("X", 1:n_features)
  colnames(y) = "y"
  colnames(d) = "d"
  
  if (return_X_y_d) {
    return(list("X" = X, "y" = y, "d" = d))
  } else {

    data = data.table::data.table(X, y, d)
    return(data)
  }
}


# To generate data_plr.csv
# set.seed(1234)
# data_plr = make_plr_data()
# data.table::fwrite(data_plr, "data/data_plr.csv")














