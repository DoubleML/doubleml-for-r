# Function to load 401k data set
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
  
  sigma_x = toeplitz(0.5^(0:(dim_x - 1)))
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
  
  data = data.table(X, Y, D, Z)
  
  return(data)
}
