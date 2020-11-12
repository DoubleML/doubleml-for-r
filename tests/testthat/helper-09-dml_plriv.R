#' Double Machine Learning for Partially Linear Instrumental Variable Regression.
#'
#' @param data Data frame.
#' @param y Name of outcome variable. The variable must be included in \code{data}.
#' @param d Name of treatment variables for which inference should be performed.
#' @param z Name of instrumental variables. 
#' @inheritParams DML
#' @param resampling Resampling scheme for cross-fitting of class \code{\link[mlr3]{ResamplingCV}}.
#' @param dml_procedure Double machine learning algorithm to be used, either \code{"dml1"} or \code{"dml2"} (default).
#' @param mlmethod List with classification or regression methods according to naming convention of the \code{mlr} package. Set \code{mlmethod_g} for classification or regression method according to naming convention of the \code{mlr} package for regression of y on X (nuisance part g). Set \code{mlmethod_m} for  classification or regression method for regression of z on X (nuisance part m). Set \code{mlmethod_m} for  classification or regression method for regression of d on X (nuisance part r). 
#' @param params Hyperparameters to be passed to classification or regression method. Set hyperparameters \code{params_g} for predictions of nuisance part g, \code{params_m} for nuisance m, and \code{params_r} for nuisance r. 
#' @param score Inference model for final estimation, default \code{"partialling out"}. Alternatively, specify \code{"ivreg"}. 
#' @param se_type Method to estimate standard errors. Default \code{"partialling out"}. Alternatively, specify \code{"ivreg"}. The options chosen for \code{score} and \code{se_type} are required to match. 
#' @param bootstrap Choice for implementation of multiplier bootstrap, can be set to \code{"normal"} (by default), \code{"none"}, \code{"Bayes"}, \code{"wild"}.
#' @param nRep Number of repetitions for multiplier bootstrap, by default \code{nRep=500}.
#' @param ... further options passed to underlying functions.
#' @return Result object with estimated coefficient and standard errors.
#' @export

dml_plriv <- function(data, y, d, z, k = 2, smpls = NULL, mlmethod, 
                      params = list(params_m = list(), params_r = list(),
                                    params_g = list()),
                    dml_procedure = "dml2",
                    score = "partialling out", se_type = "partialling out",
                    bootstrap = "normal",  nRep = 500, ...) {
  
  if (is.null(smpls)) {
    smpls = sample_splitting(k, data)
  }
  train_ids = smpls$train_ids
  test_ids = smpls$test_ids

  if (is.null(z)){
    stop("No instrument in z specified.")
  }
  
  checkmate::checkDataFrame(data)

  # tbd: ml_method handling: default mlmethod_g = mlmethod_m
  # tbd: parameter passing
  n <- nrow(data)
  theta <- se <- te <- pval <- boot_se <- NA
  boot_theta <- matrix(NA, nrow = 1, ncol = nRep)

  if (se_type != "partialling out" & se_type != "ivreg") {
    stop("Value for se_type is not valid.")
  }
  
  if (score != "partialling out" & score != "ivreg") {
    stop("Value for score is not valid.")
  }
 

  # nuisance g: E[Y|X]
  g_indx <- names(data) != d & names(data) != z
  data_g <- data[ , g_indx, drop = FALSE]
  task_g <- mlr3::TaskRegr$new(id = paste0("nuis_g_", d), backend = data_g, target = y)
  
  resampling_g <- mlr3::rsmp("custom")
  resampling_g$instantiate(task_g, train_ids, test_ids)
  n_iters <- resampling_g$iters
  
  # tbd: handling learners from mlr3 base and mlr3learners package
  # ml_g <- mlr3::mlr_learners$get(mlmethod$mlmethod_g)
  ml_g <- mlr3::lrn(mlmethod$mlmethod_g)
  ml_g$param_set$values <- params$params_g # tbd: check if parameter passing really works
    
   # ml_g <-  mlr:makeLearner(mlmethod$mlmethod_g, id = "nuis_g", par.vals = params$params_g)
  r_g <- mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
  
  # # r_g <- mlr::resample(learner = ml_g, task = task_g, resampling = rin)
  # g_hat_list <- r_g$data$prediction
  # # g_hat_list <- mlr::getRRPredictionList(r_g)
  # #g_hat_list <- lapply(g_hat_list$test, extract_test_pred)
  # g_hat_list <- lapply(g_hat_list, function(x) x$response)
  # g_hat_list <- lapply(r_g$data$prediction, function(x) x$test$response)
  g_hat_list <- lapply(r_g$data$predictions(), function(x) x$response)

  # nuisance m: E[Z|X]
  m_indx <- names(data) != y & names(data) != d
  data_m <- data[, m_indx, drop = FALSE]
  task_m <- mlr3::TaskRegr$new(id = paste0("nuis_m_", z), backend = data_m, target = z)
  ml_m <- mlr3::lrn(mlmethod$mlmethod_m)
  ml_m$param_set$values <- params$params_m # tbd: check if parameter passing really works

  # ml_m <- mlr::makeLearner(mlmethod$mlmethod_m, id = "nuis_m", par.vals = params$params_m)
  resampling_m <- mlr3::rsmp("custom")
  resampling_m$instantiate(task_m, train_ids, test_ids)
  
  train_ids_m <- lapply(1:n_iters, function(x) resampling_m$train_set(x))
  test_ids_m <- lapply(1:n_iters, function(x) resampling_m$test_set(x))

  r_m <- mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
  
 # r_m <- mlr::resample(learner = ml_m, task = task_m, resampling = rin)
  # m_hat_list <- r_m$data$prediction # alternatively, r_m$prediction (not listed)
  # # m_hat_list <- mlr::getRRPredictionList(r_m)
  # m_hat_list <- lapply(m_hat_list, function(x) x$response)
  # # m_hat_list <-lapply(m_hat_list$test,  extract_test_pred)
  # m_hat_list <- lapply(r_m$data$prediction, function(x) x$test$response)
  m_hat_list <- lapply(r_m$data$predictions(), function(x) x$response)


  # nuisance r: E[D|X]
  r_indx <- names(data) != y & names(data) != z
  data_r <- data[, r_indx, drop = FALSE]
  task_r <- mlr3::TaskRegr$new(id = paste0("nuis_r_", d), backend = data_r, target = d)
  ml_r <- mlr3::lrn(mlmethod$mlmethod_r)
  ml_r$param_set$values <- params$params_r # tbd: check if parameter passing really works

  # ml_m <- mlr::makeLearner(mlmethod$mlmethod_m, id = "nuis_m", par.vals = params$params_m)
  resampling_r <- mlr3::rsmp("custom")
  resampling_r$instantiate(task_r, train_ids, test_ids)
  
  train_ids_r <- lapply(1:n_iters, function(x) resampling_r$train_set(x))
  test_ids_r <- lapply(1:n_iters, function(x) resampling_r$test_set(x))

  r_r <- mlr3::resample(task_r, ml_r, resampling_r, store_models = TRUE)
  
 # r_m <- mlr::resample(learner = ml_m, task = task_m, resampling = rin)
  # r_hat_list <- r_r$data$prediction # alternatively, r_r$prediction (not listed)
  # # m_hat_list <- mlr::getRRPredictionList(r_m)
  # r_hat_list <- lapply(r_hat_list, function(x) x$response)
  # # m_hat_list <-lapply(m_hat_list$test,  extract_test_pred)
  # r_hat_list <- lapply(r_r$data$prediction, function(x) x$test$response)
  r_hat_list <- lapply(r_r$data$predictions(), function(x) x$response)



  # if ((rin$desc$iters != r_g$pred$instance$desc$iters) ||
  #     (rin$desc$iters != r_m$pred$instance$desc$iters) ||
  #     !identical(rin$train.inds, r_g$pred$instance$train.inds) ||
  #     !identical(rin$train.inds, r_m$pred$instance$train.inds)) {
  #   stop('Resampling instances not equal')
  # }
  if ( (resampling_g$iters != resampling_m$iters) ||
       (resampling_g$iters != resampling_r$iters) ||
       (resampling_g$iters != n_iters) ||
       (resampling_m$iters != n_iters) ||
       (resampling_r$iters != n_iters) ||
       (!identical(train_ids, train_ids_m)) ||
       (!identical(train_ids, train_ids_r)) ||
       (!identical(test_ids, test_ids_m)) ||
       (!identical(test_ids, test_ids_r))) {
    stop('Resampling instances not equal')
  }

  # test_index_list <- rin$test.inds
 #  n_k <- vapply(test_index_list, length, double(1))
  n_k <- vapply(test_ids, length, double(1L))

  D <- data[ , d]
  Y <- data[ , y]
  Z <- data[ , z]

  # DML 1
  if ( dml_procedure == "dml1") {
    thetas <- vars <- boot_vars <-  rep(NA, n_iters)
    boot_thetas <- matrix(NA, ncol = nRep, nrow = n_iters)
    se_i <- NA
    
    v_hat <- u_hat <- w_hat <- matrix(NA, nrow = max(n_k), ncol = n_iters)
    v_hat_se <- u_hat_se <- w_hat_se <- matrix(NA, nrow = max(n), ncol = 1)

    
    for (i in 1:n_iters) {
        # test_index = test_index_list[[i]]
        test_index = test_ids[[i]]

        m_hat <- m_hat_list[[i]]
        g_hat <- g_hat_list[[i]]
        r_hat <- r_hat_list[[i]]

        v_hat[, i] <- v_hat_se[test_index, ] <- D[test_index] - r_hat
        u_hat[, i]  <- u_hat_se[test_index, ] <- Y[test_index] - g_hat
        w_hat[, i] <- w_hat_se[test_index, ] <- Z[test_index] - m_hat

        orth_est <- orth_plriv_dml(u_hat = u_hat[, i] , v_hat = v_hat[, i] , 
                                   w_hat = w_hat[, i], 
                                   score = score) #, se_type)
        thetas[i] <- orth_est$theta
    
    }
    
    theta <- mean(thetas, na.rm = TRUE)
    
    se <- sqrt(var_plriv(theta = theta, u_hat = u_hat_se, v_hat = v_hat_se,
                        w_hat = w_hat_se, score = score,
                        dml_procedure = dml_procedure))
    
    
    t <- theta/se 
    
    pval <-  2 * stats::pnorm(-abs(t))
    
    if (bootstrap != "none") {
      
      boot <- bootstrap_plriv(theta = theta, u_hat = u_hat, v_hat = v_hat, 
                              w_hat = w_hat, score = score, se = se,
                              bootstrap = bootstrap, nRep = nRep)
  #    boot_se <- sqrt(boot$boot_var)
      boot_theta <- boot$boot_theta
    }
    
  }
  
  if ( dml_procedure == "dml2") {

    v_hat <- u_hat <- w_hat <- matrix(NA, nrow = n, ncol = 1)
    
    for (i in 1:n_iters){

       # test_index = test_index_list[[i]]
       test_index = test_ids[[i]]

       m_hat = m_hat_list[[i]]
       g_hat = g_hat_list[[i]]
       r_hat = r_hat_list[[i]]

       v_hat[test_index, 1] <- D[test_index] - r_hat
       u_hat[test_index, 1] <- Y[test_index] - g_hat
       w_hat[test_index, 1] <- Z[test_index] - m_hat
    }

    orth_est <- orth_plriv_dml(u_hat = u_hat, v_hat = v_hat, w_hat = w_hat, 
                               score = score)
    
    theta <- orth_est$theta
    se <- sqrt(var_plriv(theta = theta, u_hat = u_hat, v_hat = v_hat,
                        w_hat = w_hat, score = score, 
                        dml_procedure = dml_procedure))
    
    t <- theta/se 
    
    pval <-  2 * stats::pnorm(-abs(t))
    
    if (bootstrap != "none") {
      
      boot <- bootstrap_plriv(theta = theta, u_hat = u_hat, v_hat = v_hat, 
                              w_hat = w_hat, score = score, se = se,
                              bootstrap = bootstrap, nRep = nRep)
  #    boot_se <- sqrt(boot$boot_var)
      boot_theta <- boot$boot_theta
    }
  }
  
  all_preds = list(m_hat_list = m_hat_list,
                   g_hat_list = g_hat_list,
                   r_hat_list = r_hat_list)

  names(theta) <- names(se) <- names(boot_se) <- d
  res <- list( coefficients = theta, se = se, t = t, pval = pval,
               boot_se = boot_se, boot_theta = boot_theta,
               all_preds)
  
  class(res) <- "DML"
  return(res)
}



#' Orthogonalized Estimation of Coefficient in PLR
#'
#' Function to estimate the structural parameter in a partially linear regression model with instrumental variables (PLRIV).
#'
#' @inheritParams var_plriv
#' @return List with estimate (\code{theta}).
#' @export
orth_plriv_dml <- function(u_hat, v_hat, w_hat, score) { #, se_type) {

  theta <-  NA

  if (score == "ivreg") {
    res_fit <- AER::ivreg(u_hat ~ 0 + v_hat | 0 + w_hat)
    theta <- stats::coef(res_fit)
  }

   else if (score == 'partialling out') {
     theta <- mean(u_hat*w_hat)/mean(v_hat*w_hat)
  }

  else {
    stop("Inference framework for orthogonal estimation unknown")
  }

  res <- list(theta = theta)
  return(res)
}


#' Variance estimation for DML estimator in the partially linear regression model
#'
#' Variance estimation for the structural parameter estimator in a partially linear regression model (PLR) with double machine learning.
#' @inheritParams dml_plriv
#' @param theta final dml estimator for the partially linear model.
#' @param v_hat Residuals from \eqn{d-r(x)}.
#' @param u_hat Residuals from \eqn{y-g(x)}.
#' @param w_hat Residuals from \eqn{z-m(x)}.
#' @return Variance estimator (\code{var}).
var_plriv <- function(theta, u_hat, v_hat, w_hat, score, dml_procedure) {
  
  var <- NA
  
  if (score == "partialling out") {
  
    var <- mean( 1/length(u_hat) * 1/(colMeans(v_hat * w_hat, na.rm = TRUE))^2  *
            colMeans( ( (u_hat - v_hat*theta)*w_hat)^2), na.rm = TRUE)
  }
   
  # Q: only for "dml2"?
  else if (score == "ivreg" & dml_procedure == "dml2") {
      res_fit <- AER::ivreg(u_hat ~ 0 + v_hat | 0 + w_hat)
      var <- sandwich::vcovHC(res_fit)
  }
  
  else {
    stop("Inference framework for variance estimation unknown")
  }
  
  return(c(var))
}



#' Bootstrap Implementation for Partially Linear Regression Model 
#'
#' Multiplier bootstrap to construct simultaneous confidence bands for multiple target coefficients in a partially linear regression model (PLR) with double machine learning.
#'
#' @inheritParams var_plriv
#' @inheritParams dml_plriv
#' @inheritParams DMLIV
#' @param se Estimated standard error from DML procedure.
#' @return List with bootstrapped standard errors (\code{boot_se}) and bootstrapped coefficients.
bootstrap_plriv <- function(theta, u_hat, v_hat, w_hat, score, se, bootstrap, nRep) {
  
  boot_var <- NA
  
  # implement multiplier bootstrap for score = "partialling out" by default
  score <- (u_hat - v_hat*theta)*w_hat
  J <-  -colMeans(v_hat*w_hat, na.rm = TRUE)
  
  n <- length(u_hat)
  pertub <- matrix(NA, nrow = 1, ncol = nRep)
  
  if (!is.vector(score)) {
    J <- matrix(rep(J, each=nrow(score)), nrow=nrow(score))
  }
  
  for (i in seq(nRep)) {
    
    if (bootstrap == "Bayes") {
        weights <- stats::rexp(n, rate = 1) - 1
    }
    
      if (bootstrap == "normal") {
        weights <- stats::rnorm(n)
      }
    
      if (bootstrap == "wild") {
        weights <- stats::rnorm(n)/sqrt(2) + (stats::rnorm(n)^2 - 1)/2
      }
      
     pertub[1,i] <- mean( colMeans(weights * 1/se * 1/J * score, na.rm = TRUE))
    
  }
  
  res = list(boot_theta = pertub)
  return(c(res))
}















