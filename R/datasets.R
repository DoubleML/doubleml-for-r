#' @title Data set on financial wealth and 401(k) plan participation.
#' 
#' @description 
#' Preprocessed data set on financial wealth and 401(k) plan participation. The raw data files are preprocessed to reproduce the examples in Chernozhukov et al. (2020). An internet connection is required to sucessfully download the data set.
#' 
#' 
#' @details 
#' Variable description, based on the supplementary material of Chernozhukov et al. (2020):
#' 
#' * net_tfa: net total financial assets
#' * e401: = 1 if employer offers 401(k)
#' * p401: = 1 if individual participates in a 401(k) plan
#' * age: age
#' * inc: income
#' * fsize: family size
#' * educ: years of education
#' * db: = 1 if individual has defined benefit pension
#' * marr: = 1 if married
#' * twoearn: = 1 if two-earner household
#' * pira: = 1 if individual participates in IRA plan
#' * hown: = 1 if home owner
#' 
#' The supplementary data of the study by Chernozhukov et al. (2018) is available at [https://academic.oup.com/ectj/article/21/1/C1/5056401#supplementary-data](https://academic.oup.com/ectj/article/21/1/C1/5056401#supplementary-data). 
#' @references Abadie, A. (2003), Semiparametric instrumental variable estimation of treatment response models. Journal of Econometrics, 113(2): 231-263.
#' 
#' @references Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C., Newey, W. and Robins, J. (2018), Double/debiased machine learning for treatment and structural parameters. The Econometrics Journal, 21: C1-C68. \doi{10.1111/ectj.12097}.
#' 
#' @param return_type (`character(1)`) \cr
#' If `"DoubleMLData"`, returns a `DoubleMLData` object. If `"data.frame"` returns a `data.frame()`. If `"data.table"` returns a `data.table()`. Default is `"DoubleMLData"`.  
#' 
#' @param polynomial_features (`logical(1)`) \cr
#' If `TRUE` polynomial freatures are added (see replication file of Chernozhukov et al. (2018)).
#' 
#' @param instrument (`logical(1)`) \cr
#' If `TRUE`, the returned data object contains the variables `e401` and `p401`. If `return_type = "DoubleMLData"`, the variable `e401` is used as an instrument for the endogenous treatment variable `p401`. If `FALSE`, `p401` is removed from the data set.  
#' 
#' @return A data object according to the choice of `return_type`.
#' 
#' 
#' @export
fetch_401k = function(return_type = "DoubleMLData", polynomial_features = FALSE, 
                      instrument = FALSE) {
  checkmate::check_choice(return_type, c("data.table", "data.frame", "DoubleMLData"))
  checkmate::check_logical(polynomial_features)
  checkmate::check_logical(instrument)
  url = "https://github.com/VC2015/DMLonGitHub/raw/master/sipp1991.dta"
  data = readstata13::read.dta13(url)
  
  x_cols = NULL
  z_cols = NULL
  d_cols = NULL
  y_col = "net_tfa"
  
  if (polynomial_features) {
    formula_x = stats::formula(" ~ -1 + (poly(age, 6, raw=TRUE) + poly(inc, 8, raw=TRUE) + 
                                    poly(educ, 4, raw=TRUE) + poly(fsize, 2, raw=TRUE) +
                                    marr + twoearn + db + pira + hown)^2")
  } else {
    formula_x = stats::formula(" ~ -1 + age + inc + educ + fsize + marr + twoearn + db + pira + hown")
  }
  
  if (instrument) {
    # https://github.com/VC2015/DMLonGitHub/blob/b91cbf96c01eccd73367fbd6601ecdd7aa78403b/401K-LATE.R#L60-L71
    data = data.frame("net_tfa" = data$net_tfa, 
                      stats::model.matrix(formula_x, data), 
                      "p401" = data$p401, "e401" = data$e401)
    d_cols = "p401"
    z_cols = "e401"
  } else {
    # see https://github.com/VC2015/DMLonGitHub/blob/b91cbf96c01eccd73367fbd6601ecdd7aa78403b/401K.R#L67  
    data = data.frame("net_tfa" = data$net_tfa, stats::model.matrix(formula_x, data), 
                      "e401" = data$e401)
    d_cols = "e401"
  }
  if (return_type == "data.frame") {
      return(data)
  } else if (return_type == "data.table") {
    data = as.data.table(data)
    return(data)
  } else if (return_type == "DoubleMLData") {
    dt = as.data.table(data)
    data = DoubleMLData$new(dt, y_col = y_col, d_cols = d_cols, x_cols = x_cols, 
                              z_cols = z_cols)
    return(data)
  }
}


#' @title Data set on the Pennsylvania Reemployment Bonus experiment.
#' 
#' @description 
#' Preprocessed data set on the Pennsylvania Reemploymnent Bonus experiment. The raw data files are preprocessed to reproduce the examples in Chernozhukov et al. (2020). An internet connection is required to sucessfully download the data set.
#' 
#' @details 
#' Variable description, based on the supplementary material of Chernozhukov et al. (2020):
#' 
#' * abdt:  chronological time of enrollment of each claimant in the Pennsylvania reemployment bonus experiment.
#' * tg:  indicates the treatment group (bonus amount - qualification period) of each claimant. 
#' * inuidur1:  a measure of length (in weeks) of the first spell of unemployment
#' * inuidur2:  a second measure for the length (in weeks) of 
#' * female:  dummy variable; it indicates if the claimant's sex is female (=1) or male (=0).
#' * black: dummy variable; it indicates a person of black race (=1).
#' * hispanic:  dummy variable; it indicates a person of hispanic race (=1).
#' * othrace: dummy variable; it indicates a non-white, non-black, not-hispanic person (=1).
#' * dep1: dummy variable; indicates if the number of dependents of each claimant is equal to 1 (=1). 
#' * dep2: dummy variable; indicates if the number of dependents of each claimant is equal to 2 (=1).
#' * q1-q6: six dummy variables indicating the quarter of experiment during which each claimant enrolled.
#' * recall:  takes the value of 1 if the claimant answered ``yes'' when was asked if he/she had any expectation to be recalled
#' * agelt35: takes the value of 1 if the claimant's age is less than 35 and 0 otherwise.
#' * agegt54: takes the value of 1 if the claimant's age is more than 54 and 0 otherwise.
#' * durable: it takes the value of 1 if the occupation of the claimant was in the sector of durable manufacturing and 0 otherwise.
#' * nondurable:  it takes the value of 1 if the occupation of the claimant was in the sector of nondurable manufacturing and 0 otherwise.
#' * lusd:  it takes the value of 1 if the claimant filed in Coatesville, Reading, or Lancaster and 0 otherwise.
#' * These three sites were considered to be located in areas characterized by low unemployment rate and short duration of unemployment.
#' * husd:  it takes the value of 1 if the claimant filed in Lewistown, Pittston, or Scranton and 0 otherwise.
#' * These three sites were considered to be located in areas characterized by high unemployment rate and short duration of unemployment.
#' * muld:  it takes the value of 1 if the claimant filed in Philadelphia-North, Philadelphia-Uptown, McKeesport, Erie, or Butler and 0 otherwise.
#' * These three sites were considered to be located in areas characterized by moderate unemployment rate and long duration of unemployment."
#' 
#' The supplementary data of the study by Chernozhukov et al. (2018) is available at [https://academic.oup.com/ectj/article/21/1/C1/5056401#supplementary-data](https://academic.oup.com/ectj/article/21/1/C1/5056401#supplementary-data). 
#' 
#' The supplementary data of the study by Bilias (2000) is available at [http://qed.econ.queensu.ca/jae/2000-v15.6/bilias/](http://qed.econ.queensu.ca/jae/2000-v15.6/bilias/). 
#' 
#' 
#' @references Bilias Y. (2000), Sequential Testing of Duration Data: The Case of Pennsylvania ‘Reemployment Bonus’ Experiment. Journal of Applied Econometrics, 15(6): 575-594.
#' 
#' @references Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C., Newey, W. and Robins, J. (2018), Double/debiased machine learning for treatment and structural parameters. The Econometrics Journal, 21: C1-C68. \doi{10.1111/ectj.12097}.
#' 
#' @param return_type (`character(1)`) \cr
#' If `"DoubleMLData"`, returns a `DoubleMLData` object. If `"data.frame"` returns a `data.frame()`. If `"data.table"` returns a `data.table()`. Default is `"DoubleMLData"`.
#' 
#' @param polynomial_features (`logical(1)`) \cr
#' If `TRUE` polynomial freatures are added (see replication file of Chernozhukov et al. (2018)).
#' 
#' @return A data object according to the choice of `return_type`.
#' 
#' @examples 
#' library(DoubleML)
#' df_bonus = fetch_bonus(return_type = "data.table")
#' obj_dml_data_bonus = DoubleMLData$new(df_bonus, 
#'                                      y_col = "inuidur1", 
#'                                      d_cols = "tg", 
#'                                      x_cols = c("female", "black", "othrace", "dep1", "dep2",
#'                                                 "q2", "q3", "q4", "q5", "q6", "agelt35", "agegt54",
#'                                                   "durable", "lusd", "husd"))
#' obj_dml_data_bonus
#' 
#' @export
fetch_bonus = function(return_type = "DoubleMLData", polynomial_features = FALSE) {
  checkmate::check_choice(return_type, c("data.table", "data.frame", "DoubleMLData"))
  url = "https://raw.githubusercontent.com/VC2015/DMLonGitHub/master/penn_jae.dat"
  raw_data = utils::read.table(url, header = TRUE)
  
  ind = (raw_data$tg == 0 | raw_data$tg == 4)
  data = raw_data[ind,]
  data$tg[(data$tg==4)] = 1
  data$inuidur1 = log(data$inuidur1)
  
  data$dep1 = as.integer(data$dep == 1)
  data$dep2 = as.integer(data$dep == 2)
  col_indx = names(data) != "dep"
  data = data[, col_indx]
  # data$dep = as.factor(data$dep)
  
  y_col = "inuidur1"
  d_cols = "tg"
  x_cols = NULL
  
  if (polynomial_features) {
    #https://github.com/VC2015/DMLonGitHub/blob/b91cbf96c01eccd73367fbd6601ecdd7aa78403b/Bonus.R#L84
    formula_x = stats::formula(" ~ -1 + (female + black + othrace + dep1 +
                                   dep2 + q2 + q3 + q4 + q5 + q6 + agelt35 + agegt54 + 
                                   durable + lusd + husd)^2")
  } else {
    formula_x = stats::formula(" ~ -1 + female + black + othrace + dep1 +
                                   dep2 + q2 + q3 + q4 + q5 + q6 + agelt35 + agegt54 + 
                                   durable + lusd + husd")
  }
  data = data.frame("inuidur1" = data$inuidur1, stats::model.matrix(formula_x, data), 
                      "tg" = data$tg)
  if (return_type == "data.frame") {
    return(data) 
  } else if (return_type == "data.table") {
     data = as.data.table(data)
     return(data)
  } else if (return_type == "DoubleMLData") {
     dt = as.data.table(data)
     data = DoubleMLData$new(dt, y_col = y_col, d_cols = d_cols, x_cols = x_cols)
     return(data)
  }
}


m = function(x, nu = 0, gamma = 1){
  y = 0.5/pi *sinh(gamma)/(cosh(gamma)-cos(x-nu))
  return(y)
}


g = function(x){
  y = sin(x)^2
}


#' @title Generates data from a partially linear regression model used in Chernozhukov et al. (2018)
#' 
#' @description 
#'  Generates data from a partially linear regression model used in Chernozhukov et al. (2018) for Figure 1.
#'  The data generating process is defined as
#' 
#' \eqn{d_i = m_0(x_i) + s_1 v_i,}
#' 
#' \eqn{y_i = \alpha d_i + g_0(x_i) + s_2 \zeta_i,}
#' 
#' with \eqn{v_i \sim \mathcal{N}(0,1)} and \eqn{\zeta_i \sim \mathcal{N}(0,1),}. The covariates are distributed as \eqn{x_i \sim \mathcal{N}(0, \Sigma)}, where  \eqn{\Sigma} is a matrix with entries  \eqn{\Sigma_{kj} = 0.7^{|j-k|}}. The nuisance functions are given by
#' 
#' \eqn{m_0(x_i) = a_0 x_{i,1} + a_1 \frac{\exp(x_{i,3})}{1+\exp(x_{i,3})},}
#' 
#' \eqn{g_0(x_i) = b_0 \frac{\exp(x_{i,1})}{1+\exp(x_{i,1})} + b_1 x_{i,3},}
#' 
#' with \eqn{a_0=1}, \eqn{a_1=0.25}, \eqn{s_1=1}, \eqn{b_0=1}, \eqn{b_1=0.25}, \eqn{s_2=1}.
#' 
#' @references Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C., Newey, W. and Robins, J. (2018), Double/debiased machine learning for treatment and structural parameters. The Econometrics Journal, 21: C1-C68. \doi{10.1111/ectj.12097}.
#' 
#' @param n_obs (`integer(1)`) \cr
#' The number of observations to simulate. 
#' 
#' @param alpha (`numeric(1)`) \cr
#' The value of the causal parameter. 
#' 
#' @param dim_x (`integer(1)`) \cr
#' The number of covariates. 
#' 
#' @param return_type (`character(1)`) \cr
#' If `"DoubleMLData"`, returns a `DoubleMLData` object. If `"data.frame"` returns a `data.frame()`. If `"data.table"` returns a `data.table()`. If `"matrix"` a named `list()` with entries `X`, `y` and `d` is returned. Every entry in the list is a `matrix()` object.  Default is `"DoubleMLData"`.
#' 
#' @return A data object according to the choice of `return_type`.
#' 
#' @export
make_plr_CCDDHNR2018 = function(n_obs = 500, dim_x = 20, alpha = 0.5,
                                return_type = "DoubleMLData"){
  checkmate::check_choice(return_type, c("data.table", "matrix", "data.frame", "DoubleMLData"))
  cov_mat = stats::toeplitz(0.7^(0:(dim_x - 1)))
  a_0 = 1
  a_1 = 0.25
  s_1 = 1
  b_0 = 1
  b_1 = 0.25
  s_2 = 1
  x = mvtnorm::rmvnorm(n = n_obs, mean = rep(0, dim_x), sigma = cov_mat)
  
  d = as.matrix(a_0*x[,1] + a_1*(exp(x[,3])/(1+exp(x[,3]))) + s_1*stats::rnorm(n_obs))
  y = as.matrix( alpha * d + b_0* exp(x[,1])/(1+exp(x[,1])) + b_1 * x[,3] + s_2*stats::rnorm(n_obs))
  
  colnames(x) = paste0("X", 1:dim_x)
  colnames(y) = "y"
  colnames(d) = "d"
  if (return_type == "matrix") {
    return(list("X" = x, "y" = y, "d" = d))
  } else if (return_type == "data.frame") {
    data = data.frame(x,y,d)
    return(data) 
  } else if (return_type == "data.table") {
     data = data.table(x, y, d)
     return(data)
  } else if (return_type == "DoubleMLData") {
     dt = data.table(x, y, d)
     data = DoubleMLData$new(dt, y_col = "y", d_cols = "d")
     return(data)
  }
}

#' @title Generates data from a partially linear regression model used in a blog article by Turrell (2018).
#' 
#' @description 
#' Generates data from a partially linear regression model used in a blog article by Turrell (2018). The data generating process is defined as
#' 
#' \eqn{d_i = m_0(x_i' b) + v_i,}
#' 
#' \eqn{y_i = \theta d_i + g_0(x_i' b) + u_i,}
#' 
#' with \eqn{v_i \sim \mathcal{N}(0,1)}, \eqn{u_i \sim \mathcal{N}(0,1)}, and covariates \eqn{x_i \sim \mathcal{N}(0, \Sigma)}, where  \eqn{\Sigma} is a random symmetric, positive-definite matrix generated with [clusterGeneration::genPositiveDefMat()]. \eqn{b} is a vector with entries \eqn{b_j=\frac{1}{j}} and the nuisance functions are given by
#' 
#' \eqn{m_0(x_i) = \frac{1}{2 \pi} \frac{\sinh(\gamma)}{\cosh(\gamma) - \cos(x_i-\nu)},}
#' 
#' \eqn{g_0(x_i) = \sin(x_i)^2.}
#' 
#' 
#' 
#' @references Turrell, A. (2018), Econometrics in Python part I - Double machine learning, Markov Wanderer: A blog on economics, science, coding and data. [http://aeturrell.com/2018/02/10/econometrics-in-python-partI-ML/](http://aeturrell.com/2018/02/10/econometrics-in-python-partI-ML/).
#' 
#' @param n_obs (`integer(1)`) \cr
#' The number of observations to simulate. 
#' 
#' @param dim_x (`integer(1)`) \cr
#' The number of covariates. 
#' 
#' @param theta (`numeric(1)`) \cr
#' The value of the causal parameter. 
#' 
#' @param nu (`numeric(1)`) \cr
#' The value of the parameter \eqn{\nu}. Default is `0`. 
#' 
#' @param gamma (`numeric(1)`) \cr
#' The value of the parameter \eqn{\gamma}. Default is `1`.  
#' 
#' @param return_type (`character(1)`) \cr
#' If `"DoubleMLData"`, returns a `DoubleMLData` object. If `"data.frame"` returns a `data.frame()`. If `"data.table"` returns a `data.table()`. If `"matrix"` a named `list()` with entries `X`, `y` and `d` is returned. Every entry in the list is a `matrix()` object.  Default is `"DoubleMLData"`.
#' 
#' @return A data object according to the choice of `return_type`.
#' 
#' @export
make_plr_turrell2018 = function(n_obs = 100, dim_x = 20, theta = 0.5, return_type = "DoubleMLData", nu = 0, gamma = 1) {
  b = 1/(1:dim_x)
  sigma = clusterGeneration::genPositiveDefMat(dim_x)
  X = mvtnorm::rmvnorm(n = n_obs, mean = rep(0, dim_x), sigma = sigma$Sigma)
  G = g(X%*%b)
  M = m(X%*%b, nu = nu, gamma = gamma) 
  d = M + stats::rnorm(n_obs)
  y = theta*d + G + stats::rnorm(n_obs)
  
  colnames(X) = paste0("X", 1:dim_x)
  colnames(y) = "y"
  colnames(d) = "d"
  
  if (return_type == "matrix") {
    return(list("X" = X, "y" = y, "d" = d))
  } else if (return_type == "data.frame") {
    data = data.frame(X,y,d)
    return(data) 
  } else if (return_type == "data.table") {
     data = data.table(X, y, d)
     return(data)
  } else if (return_type == "DoubleMLData") {
     dt = data.table(X, y, d)
     data = DoubleMLData$new(dt, y_col = "y", d_cols = "d")
     return(data)
  }
}


#' @title Generates data from a partially linear IV regression model used in Chernozhukov, Hansen and Spindler (2015).
#' 
#' @description 
#' Generates data from a partially linear IV regression model used in Chernozhukov, Hansen and Spindler (2015). The data generating process is defined as 
#' 
#' \eqn{z_i = \Pi x_i + \zeta_i,}
#' 
#' \eqn{d_i = x_i'\gamma + z_i'\delta + u_i,}
#' 
#' \eqn{y_i = \alpha d_i + x_i'\beta + \epsilon_i,}
#' 
#' with 
#' 
#' \eqn{\left(\begin{array}{c} \varepsilon_i \\ u_i \\ \zeta_i \\ x_i \end{array} \right) \sim \mathcal{N}\left(0, \left(\begin{array}{cccc} 1 & 0.6 & 0 & 0 \\ 0.6 & 1 & 0 & 0 \\ 0 & 0 & 0.25 I_{p_n^z} & 0 \\ 0 & 0 & 0 & \Sigma \end{array} \right) \right)}
#' 
#' where \eqn{\Sigma} is a \eqn{p_n^x \times p_n^x} matrix with entries \eqn{\Sigma_{kj} = 0.5^{|j-k|}} and \eqn{I_{p_n^z}} is the \eqn{p^z_n \times p^z_n} identity matrix. \eqn{\beta=\gamma} iis a \eqn{p^x_n}-vector with entries \eqn{\beta_j = \frac{1}{j^2}}, \eqn{\delta} is a \eqn{p^z_n}-vector with entries \eqn{\delta_j = \frac{1}{j^2}} and \eqn{\Pi = (I_{p_n^z}, O_{p_n^z \times (p_n^x - p_n^z)})}.
#' 
#' @references Chernozhukov, V., Hansen, C. and Spindler, M. (2015), Post-Selection and Post-Regularization Inference in Linear Models with Many Controls and Instruments. American Economic Review: Papers and Proceedings, 105 (5): 486-90.
#' 
#' @param n_obs (`integer(1)`) \cr
#' The number of observations to simulate. 
#' 
#' @param alpha (`numeric(1)`) \cr
#' The value of the causal parameter. 
#' 
#' @param dim_x (`integer(1)`) \cr
#' The number of covariates. 
#' 
#' @param dim_z (`integer(1)`) \cr
#' The number of instruments. 
#' 
#' @param return_type (`character(1)`) \cr
#' If `"DoubleMLData"`, returns a `DoubleMLData` object. If `"data.frame"` returns a `data.frame()`. If `"data.table"` returns a `data.table()`. If `"matrix"` a named `list()` with entries `X`, `y`, `d` and `z` is returned. Every entry in the list is a `matrix()` object.  Default is `"DoubleMLData"`.
#' 
#' @return A data object according to the choice of `return_type`.
#' 
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
    colnames(X) = paste0("X", 1:dim_x)
    colnames(Z) = paste0("Z", 1:dim_z)
    colnames(Y) = "y"
    colnames(D) = "d"
    
    if (return_type == "data.frame") {
      data = data.frame(X, Y, D, Z)
      return(data) 
    } else if (return_type == "data.table") {
     data = data.table(X, Y, D, Z)
     return(data)
    } else if (return_type == "DoubleMLData") {
     dt = data.table(X, Y, D, Z)
     data = DoubleMLData$new(dt, y_col = "y", d_cols = "d", 
                                           x_cols = colnames(X), 
                                           z_cols = colnames(Z))
     return(data)
    }
  }
  return(data)
}


#' @title Generates data from a interactive regression (IRM) model.
#' 
#' @description
#' Generates data from a interactive regression (IRM) model. The data generating process is defined as
#' 
#' \eqn{d_i = 1\left\lbrace \frac{\exp(c_d x_i' \beta)}{1+\exp(c_d x_i' \beta)} > v_i \right\rbrace,}
#' 
#' \eqn{ y_i = \theta d_i + c_y x_i' \beta d_i + \zeta_i,}
#' 
#' with \eqn{v_i \sim \mathcal{U}(0,1)}, \eqn{\zeta_i \sim \mathcal{N}(0,1)} and covariates \eqn{x_i \sim \mathcal{N}(0, \Sigma)}, where \eqn{\Sigma} is a matrix with entries \eqn{\Sigma_{kj} = 0.5^{|j-k|}}. \eqn{\beta} is a `dim_x`-vector with entries \eqn{\beta_j = \frac{1}{j^2}} and the constancts \eqn{c_y} and \eqn{c_d} are given by 
#' 
#' \eqn{ c_y = \sqrt{\frac{R_y^2}{(1-R_y^2) \beta' \Sigma \beta}},}
#' 
#' \eqn{c_d = \sqrt{\frac{(\pi^2 /3) R_d^2}{(1-R_d^2) \beta' \Sigma \beta}}.}
#' 
#' The data generating process is inspired by a process used in the simulation experiment (see Appendix P) of Belloni et al. (2017).
#' 
#' @references Belloni, A., Chernozhukov, V., Fernández-Val, I. and Hansen, C. (2017). Program Evaluation and Causal Inference With High-Dimensional Data. Econometrica, 85: 233-298.
#' 
#' @param n_obs (`integer(1)`) \cr
#' The number of observations to simulate. 
#' 
#' @param dim_x (`integer(1)`) \cr
#' The number of covariates. 
#' 
#' @param theta (`numeric(1)`) \cr
#' The value of the causal parameter.
#' 
#' @param R2_d (`numeric(1)`) \cr
#' The value of the parameter \eqn{R_d^2}.
#' 
#' @param R2_y (`numeric(1)`) \cr
#' The value of the parameter \eqn{R_y^2}.
#' 
#' @param return_type (`character(1)`) \cr
#' If `"DoubleMLData"`, returns a `DoubleMLData` object. If `"data.frame"` returns a `data.frame()`. If `"data.table"` returns a `data.table()`. If `"matrix"` a named `list()` with entries `X`, `y`, `d` and `z` is returned. Every entry in the list is a `matrix()` object.  Default is `"DoubleMLData"`.
#' 
#' @export
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
     data = data.table(x, y, d)
     return(data)
  } else if (return_type == "DoubleMLData") {
     dt = data.table(x, y, d)
     data = DoubleMLData$new(dt, y_col = "y", d_cols = "d")
     return(data)
  }
}


#' @title Generates data from a interactive IV regression (IIVM) model.
#' 
#' @description
#' Generates data from a interactive IV regression (IIVM) model. The data generating process is defined as
#' 
#' 
#' \eqn{d_i = 1\left\lbrace \alpha_x Z + v_i > 0 \right\rbrace,}
#'
#' \eqn{y_i = \theta d_i + x_i' \beta + u_i,}
#' 
#' \eqn{Z \sim \textstyle{Bernoulli} (0.5)} and 
#' 
#' \eqn{\left(\begin{array}{c} u_i \\ v_i \end{array} \right) \sim \mathcal{N}\left(0, \left(\begin{array}{cc} 1 & 0.3 \\ 0.3 & 1 \end{array} \right) \right).}
#' 
#' The covariates :\eqn{x_i \sim \mathcal{N}(0, \Sigma)}, where  \eqn{\Sigma} is a matrix with entries
#' \eqn{\Sigma_{kj} = 0.5^{|j-k|}} and \eqn{\beta} is a `dim_x`-vector with entries \eqn{\beta_j=\frac{1}{j^2}}.
#' 
#' The data generating process is inspired by a process used in the simulation experiment of Farbmacher, Gruber and Klaaßen (2020).
#' 
#' @references Farbmacher, H., Guber, R. and Klaaßen, S. (2020). Instrument Validity Tests with Causal Forests. MEA Discussion Paper No. 13-2020. Available at SSRN: \doi{10.2139/ssrn.3619201}.
#' 
#' @param n_obs (`integer(1)`) \cr
#' The number of observations to simulate. 
#' 
#' @param dim_x (`integer(1)`) \cr
#' The number of covariates. 
#' 
#' @param theta (`numeric(1)`) \cr
#' The value of the causal parameter.
#' 
#' @param alpha_x (`numeric(1)`) \cr
#' The value of the parameter \eqn{\alpha_x}.
#' 
#' @param return_type (`character(1)`) \cr
#' If `"DoubleMLData"`, returns a `DoubleMLData` object. If `"data.frame"` returns a `data.frame()`. If `"data.table"` returns a `data.table()`. If `"matrix"` a named `list()` with entries `X`, `y`, `d` and `z` is returned. Every entry in the list is a `matrix()` object.  Default is `"DoubleMLData"`.
#' 
#' @export
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
  z = matrix(sample(c(0,1), size = n_obs, prob = c(0.5, 0.5), replace = TRUE))
  d = matrix(1*(alpha_x*z + v > 0))
  
  y = d*theta + x %*% beta + u
  
  colnames(x) = paste0("X", 1:dim_x)
  colnames(y) = "y"
  colnames(d) = "d"
  colnames(z) = "z"
  
  if (return_type == "matrix") {
    return(list("X" = x, "y" = y, "d" = d, "z" = z))
  } else {
    colnames(x) = paste0("x", 1:dim_x)
    colnames(y) = "y"
    if (return_type == "data.frame") {
      data = data.frame(x, y, d, z)
      return(data) 
    } else if (return_type == "data.table") {
     data = data.table(x, y, d, z)
     return(data)
    } else if (return_type == "DoubleMLData") {
     dt = data.table(x, y, d, z)
     data = DoubleMLData$new(dt, y_col = "y", d_cols = "d", 
                                           x_cols = colnames(x), 
                                           z_cols = "z")
     return(data)
    }
  }
}


