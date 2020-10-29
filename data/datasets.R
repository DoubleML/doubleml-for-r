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
make_pliv_CHS2015 = function(n_obs, alpha = 1, dim_x = 200, dim_z = 150, return_type = "DoubleMLData"){
  # see https://assets.aeaweb.org/asset-server/articles-attachments/aer/app/10505/P2015_1022_app.pdf
  checkmate::check_choice(return_type, c("data.table", "matrix", "data.frame", "DoubleMLData"))
  if (dim_x < dim_z) {
    stop("Dimension of X should be greater than dimension of Z.")
  }
  sigma_e_u = matrix(c(1, 0.6, 0.6, 1), ncol = 2)
  mu_e_u = rep(0, 2)
  e_u = mvtnorm::rmvnorm(n = n_obs, mean = mu_e_u, sigma = sigma_e_u)
  epsilon = e_u[,1]  
  u = e_u[,2]
  
  sigma_x = stats::toeplitz(0.5^(0:(dim_x - 1)))
  mu_x = rep(0, dim_x)
  X = mvtnorm::rmvnorm(n = n_obs, mean = mu_x, sigma = sigma_x)
  
  I_z = diag(x = 1, ncol = dim_z, nrow = dim_z)
  mu_xi = rep(0, dim_z)
  xi = mvtnorm::rmvnorm(n = n_obs, mean = mu_xi, sigma = 0.25*I_z)
  
  beta = 1/(1:dim_x)^2 
  gamma = beta
  delta = 1/(1:dim_z)^2
  
  zeros = matrix(0, nrow = dim_z, ncol = (dim_x - dim_z))
  Pi = cbind(I_z, zeros)
  
  Z = X%*%t(Pi) + xi
  D = X%*%gamma + Z%*% delta + u
  Y = alpha * D + X%*%beta + epsilon

 if (return_type == "matrix") {
    return(list("X" = X, "y" = Y, "d" = D, "z" = Z))
  } else {
    colnames(X) = paste0("x", 1:dim_x)
    colnames(Z) = paste0("z", 1:dim_z)
    colnames(Y) = "y"
    colnames(D) = "d"
    
    if (return_type == "data.frame") {
      data = data.frame(X, Y, D, Z)
      return(data) 
    } else if (return_type == "data.table") {
     data = data.table::data.table(X, Y, D, Z)
     return(data)
    } else if (return_type == "DoubleMLData") {
     dt = data.table::data.table(X, Y, D, Z)
     data = DoubleML::DoubleMLData$new(dt, y_col = "y", d_cols = "d", 
                                           x_cols = colnames(X), 
                                           z_cols = colnames(Z))
     return(data)
    }
  }
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
make_plr_data = function(n_obs = 100, n_features = 20, theta = 0.5, return_X_y_d = FALSE){
  
  b = 1/(1:n_features)
  sigma = clusterGeneration::genPositiveDefMat(n_features)
  X = mvtnorm::rmvnorm(n = n_obs, mean = rep(0, n_features), sigma = sigma$Sigma)
  G = g(X%*%b)
  M = m(X%*%b) 
  d = M + stats::rnorm(n_obs)
  y = theta*d + G + stats::rnorm(n_obs)
  
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
# save(data_plr, file = "data/data_plr.rda")


#' @export
make_plr_CCDDHNR2018 = function(n_obs = 500, n_features = 20, alpha = 0.5,
                                return_type = "DoubleMLData"){

  checkmate::check_choice(return_type, c("data.table", "matrix", "data.frame", "DoubleMLData"))
  cov_mat = stats::toeplitz(0.7^(0:(n_features - 1)))
  a_1 = 0.25
  b_1 = 0.25
  x = mvtnorm::rmvnorm(n = n_obs, mean = rep(0, n_features), sigma = cov_mat)
  
  d = as.matrix( x[,1] + a_1*(exp(x[,2])/(1+exp(x[,2]))) + stats::rnorm(n_obs))
  y = as.matrix( alpha * d + exp(x[,2])/(1+exp(x[,2])) + b_1 * x[,2] + stats::rnorm(n_obs))
  
  colnames(x) = paste0("X", 1:n_features)
  colnames(y) = "y"
  colnames(d) = "d"
  if (return_type == "matrix") {
    return(list("X" = x, "y" = y, "d" = d))
  } else if (return_type == "data.frame") {
    data = data.frame(x,y,d)
    return(data) 
  } else if (return_type == "data.table") {
     data = data.table::data.table(x, y, d)
     return(data)
  } else if (return_type == "DoubleMLData") {
     dt = data.table::data.table(x, y, d)
     data = DoubleML::DoubleMLData$new(dt, y_col = "y", d_cols = "d")
     return(data)
  }
}

make_irm_data = function(n_obs = 500, dim_x = 20, theta = 0, R2_d = 0.5, R2_y = 0.5, 
              return_type = "DoubleMLData") {
  # inspired by https://onlinelibrary.wiley.com/doi/abs/10.3982/ECTA12723, see suplement
  checkmate::check_choice(return_type, c("data.table", "matrix", "data.frame", "DoubleMLData"))
  v = stats::runif(n_obs)
  zeta = stats::rnorm(n_obs)
  cov_mat = stats::toeplitz(0.5^(0:(dim_x - 1)))
  x = mvtnorm::rmvnorm(n = n_obs, mean = rep(0, dim_x), sigma = cov_mat)
  
  beta = 1/(1:dim_x)^2 
  b_sigma_b = beta %*% cov_mat %*% beta
  c_y = c(R2_y/((1-R2_y) * b_sigma_b))
  c_d = c(pi^2/3 * R2_d/((1-R2_d)*b_sigma_b))
  
  xx = exp(x %*% (beta*c_d))
  d = 1*((xx/(1+xx)) > v)
  
  y = d*theta + d* x%*% (beta * c_y) + zeta
  
  colnames(x) = paste0("X", 1:dim_x)
  colnames(y) = "y"
  colnames(d) = "d"
  
  if (return_type == "matrix") {
    return(list("X" = x, "y" = y, "d" = d))
  } else if (return_type == "data.frame") {
    data = data.frame(x,y,d)
    return(data) 
  } else if (return_type == "data.table") {
     data = data.table::data.table(x, y, d)
     return(data)
  } else if (return_type == "DoubleMLData") {
     dt = data.table::data.table(x, y, d)
     data = DoubleML::DoubleMLData$new(dt, y_col = "y", d_cols = "d")
     return(data)
  }
}


make_iivm_data = function(n_obs = 500, dim_x = 20, theta = 1, alpha_x = 0.2, 
                          return_type = "DoubleMLData") {
  # inspired by https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3619201&download=yes
  checkmate::check_choice(return_type, c("data.table", "matrix", "data.frame", "DoubleMLData"))
  xx = mvtnorm::rmvnorm(n = n_obs, mean = rep(0, 2), 
                        sigma = matrix(c(1, 0.3, 0.3, 1), ncol = 2, nrow = 2))
  u = xx[, 1]
  v = xx[, 2]
  
  cov_mat = stats::toeplitz(0.5^(0:(dim_x - 1)))
  x = mvtnorm::rmvnorm(n = n_obs, mean = rep(0, dim_x), sigma = cov_mat)

  beta = 1/(1:dim_x)^2
  z = sample(c(0,1), size = n_obs, prob = c(0.5, 0.5), replace = TRUE)
  d = 1*(alpha_x*z + v > 0)
  
  y = d*theta + x %*% beta + u
  
  if (return_type == "matrix") {
    return(list("X" = x, "y" = y, "d" = d, "z" = z))
  } else {
    colnames(x) = paste0("x", 1:dim_x)
    colnames(y) = "y"
    if (return_type == "data.frame") {
      data = data.frame(x, y, d, z)
      return(data) 
    } else if (return_type == "data.table") {
     data = data.table::data.table(x, y, d, z)
     return(data)
    } else if (return_type == "DoubleMLData") {
     dt = data.table::data.table(x, y, d, z)
     data = DoubleML::DoubleMLData$new(dt, y_col = "y", d_cols = "d", 
                                           x_cols = colnames(x), 
                                           z_cols = "z")
     return(data)
    }
  }
}


