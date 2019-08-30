#' Double Machine Learning for Partially Linear Regression.
#'
#' @param data Data frame.
#' @param y Name of outcome variable. The variable must be included in \code{data}.
#' @param d Name of treatment variables for which inference should be performed.
#' @inheritParams DML
#' @param resampling Resampling scheme for cross-fitting of class \code{\link[mlr3]{ResamplingCV}}.
#' @param dml_procedure Double machine learning algorithm to be used, either \code{"dml1"} or \code{"dml2"} (default).
#' @param inf_model Inference model for final estimation, default \code{"IV-type"} (...)
#' @param se_type Method to estimate standard errors. Default \code{"ls"} to estimate usual standard error from least squares regression of residuals. Alternatively, specify \code{"IV-type"} or \code{"DML2018"} to obtain standard errors that correspond to the specified \code{inf_model}. The options chosen for \code{inf_model} and \code{se_type} are required to match. 
#' @param bootstrap Choice for implementation of multiplier bootstrap, can be set to \code{"normal"} (by default), \code{"none"}, \code{"Bayes"}, \code{"wild"}.
#' @param nRep Number of repetitions for multiplier bootstrap, by default \code{nRep=500}.
#' @param ... further options passed to underlying functions.
#' @return Result object with estimated coefficient and standard errors.
#' @export

dml_plr <- function(data, y, d, resampling = NULL, mlmethod, params = list(params_m = list(),
                    params_g = list()),
                    dml_procedure = "dml2",
                    inf_model = "IV-type", se_type = "ls",
                    bootstrap = "normal",  nRep = 500, ...) {

  # function not yet fully implemented (test)
  if (!is.null(resampling)) {
    checkmate::check_class(resampling, "ResamplingCV")
  }
  # tbd. if (is.null(resampling))
  checkmate::checkDataFrame(data)

  # tbd: ml_method handling: default mlmethod_g = mlmethod_m
  # tbd: parameter passing
  n <- nrow(data)
  theta <- se <- te <- pval <- boot_se <- NA
  boot_theta <- matrix(NA, nrow = 1, ncol = nRep)
  
  # if (is.null(ResampleInstance)) {
  #   n_iters <- resampling$iters
  #   rin <- mlr::makeResampleInstance(resampling, size = nrow(data))
  #   }
  # 
  # else {
  # 
  #   if (!is.null(resampling)) {
  #     message("Options in 'resampling' are overwritten by options specified for 'ResampleInstance'")
  #   }
  # 
  #   rin <- ResampleInstance
  #   resampling <- rin$desc
  #   n_iters <- resampling$iters
  # 
  #   }

   if (se_type != "ls") {
    se_type <- inf_model
  }
  
  if (se_type == "ls" & dml_procedure == "dml1"){
    se_type <- inf_model
  }

  # nuisance g
  g_indx <- names(data) != d 
  data_g <- data[ , g_indx, drop = FALSE]
  task_g <- mlr3::TaskRegr$new(id = paste0("nuis_g_", d), backend = data_g, target = y)
  
  if (is.null(resampling)) {
    resampling <- mlr3::ResamplingCV$new()
    resampling$param_set$values$folds = k
  }
  
  # tbd: handling of resampling 
  if (!resampling$is_instantiated) {
    resampling <- resampling$instantiate(task_g)
  } # tbd: else 
  
  n_iters <- resampling$iters
  train_ids <- lapply(1:n_iters, function(x) resampling$train_set(x))
  test_ids <- lapply(1:n_iters, function(x) resampling$test_set(x))

  # tbd: handling learners from mlr3 base and mlr3learners package
  # ml_g <- mlr3::mlr_learners$get(mlmethod$mlmethod_g)
  ml_g <- mlr3::lrn(mlmethod$mlmethod_g)
 # ml_g$param_set$values <- params$params_g # tbd: check if parameter passing really works
    
   # ml_g <-  mlr:makeLearner(mlmethod$mlmethod_g, id = "nuis_g", par.vals = params$params_g)
  r_g <- mlr3::resample(task_g, ml_g, resampling, store_models = TRUE)
  
  # r_g <- mlr::resample(learner = ml_g, task = task_g, resampling = rin)
  g_hat_list <- r_g$data$prediction
  # g_hat_list <- mlr::getRRPredictionList(r_g)
  #g_hat_list <- lapply(g_hat_list$test, extract_test_pred)
  g_hat_list <- lapply(g_hat_list, function(x) x$response)
  
  # nuisance m
  m_indx <- names(data) != y
  data_m <- data[, m_indx, drop = FALSE]
  task_m <- mlr3::TaskRegr$new(id = paste0("nuis_m_", d), backend = data_m, target = d)
  ml_m <- mlr3::lrn(mlmethod$mlmethod_m)

  # ml_m <- mlr::makeLearner(mlmethod$mlmethod_m, id = "nuis_m", par.vals = params$params_m)
  resampling_m <- mlr3::rsmp("custom")
  resampling_m$instantiate(task_m, train_ids, test_ids)
  
  train_ids_m <- lapply(1:n_iters, function(x) resampling_m$train_set(x))
  test_ids_m <- lapply(1:n_iters, function(x) resampling_m$test_set(x))

  
  r_m <- mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
  
 # r_m <- mlr::resample(learner = ml_m, task = task_m, resampling = rin)
  m_hat_list <- r_m$data$prediction # alternatively, r_m$prediction (not listed)
  # m_hat_list <- mlr::getRRPredictionList(r_m)
  m_hat_list <- lapply(m_hat_list, function(x) x$response)
  # m_hat_list <-lapply(m_hat_list$test,  extract_test_pred)


  # if ((rin$desc$iters != r_g$pred$instance$desc$iters) ||
  #     (rin$desc$iters != r_m$pred$instance$desc$iters) ||
  #     !identical(rin$train.inds, r_g$pred$instance$train.inds) ||
  #     !identical(rin$train.inds, r_m$pred$instance$train.inds)) {
  #   stop('Resampling instances not equal')
  # }
  if ( (resampling$iters != resampling_m$iters) ||
       (resampling$iters != n_iters) ||
       (resampling_m$iters != n_iters) ||
         (!identical(train_ids, train_ids_m)) ||
         (!identical(test_ids, test_ids_m))) {
    stop('Resampling instances not equal')
  }

  # test_index_list <- rin$test.inds
 #  n_k <- vapply(test_index_list, length, double(1))
  n_k <- vapply(test_ids, length, double(1L))

  D <- data[ , d]
  Y <- data[ , y]

  # DML 1
  if ( dml_procedure == "dml1") {
    thetas <- vars <- boot_vars <-  rep(NA, n_iters)
    boot_thetas <- matrix(NA, ncol = nRep, nrow = n_iters)
    se_i <- NA
    
    v_hat <- u_hat <- v_hatd <- d_k <- matrix(NA, nrow = max(n_k), ncol = n_iters)
    
    for (i in 1:n_iters) {
        # test_index = test_index_list[[i]]
        test_index = test_ids[[i]]

        m_hat <- m_hat_list[[i]]
        g_hat <- g_hat_list[[i]]

        d_k[, i] <- D[test_index]
        v_hat[, i] <- D[test_index] - m_hat
        u_hat[, i]  <- Y[test_index] - g_hat
        v_hatd[, i]  <- v_hat[, i]*D[test_index]

        orth_est <- orth_plr_dml(u_hat = u_hat[, i] , v_hat = v_hat[, i] ,
                                 v_hatd = v_hatd[, i], 
                                 inf_model = inf_model) #, se_type)
        thetas[i] <- orth_est$theta
    
    }
    
    theta <- mean(thetas, na.rm = TRUE)
    
    se <- sqrt(var_plr(theta = theta, d = d_k, u_hat = u_hat, v_hat = v_hat,
                        v_hatd = v_hatd, inf_model = inf_model, se_type = se_type,
                        dml_procedure = dml_procedure))
    
    
    t <- theta/se 
    
    pval <-  2 * stats::pnorm(-abs(t))
    
    if (bootstrap != "none") {
      
      boot <- bootstrap_plr(theta = theta, d = d_k, u_hat = u_hat, v_hat = v_hat, 
                              v_hatd = v_hatd, inf_model = inf_model, se = se,
                              bootstrap = bootstrap, nRep = nRep)
  #    boot_se <- sqrt(boot$boot_var)
      boot_theta <- boot$boot_theta
    }
    
  }
  
  if ( dml_procedure == "dml2") {

    v_hat <- u_hat <- v_hatd <- matrix(NA, nrow = n, ncol = 1)
    
    for (i in 1:n_iters){

       # test_index = test_index_list[[i]]
       test_index = test_ids[[i]]

       m_hat = m_hat_list[[i]]
       g_hat = g_hat_list[[i]]

       v_hat[test_index, 1] <- D[test_index] - m_hat
       u_hat[test_index, 1] <- Y[test_index] - g_hat
       v_hatd[test_index, 1] <- v_hat[test_index]*D[test_index]

    }

    orth_est <- orth_plr_dml(u_hat = u_hat, v_hat = v_hat, v_hatd = v_hatd, inf_model = inf_model)
    
    theta <- orth_est$theta
    se <- sqrt(var_plr(theta = theta, d = D, u_hat = u_hat, v_hat = v_hat,
                        v_hatd = v_hatd, inf_model = inf_model, se_type = se_type, 
                        dml_procedure = dml_procedure))
    
    t <- theta/se 
    
    pval <-  2 * stats::pnorm(-abs(t))
    
    if (bootstrap != "none") {
      
      boot <- bootstrap_plr(theta = theta, d = D, u_hat = u_hat, v_hat = v_hat, 
                              v_hatd = v_hatd, inf_model = inf_model, se = se,
                              bootstrap = bootstrap, nRep = nRep)
  #    boot_se <- sqrt(boot$boot_var)
      boot_theta <- boot$boot_theta
    }
  }


  names(theta) <- names(se) <- names(boot_se) <- d
  res <- list( coefficients = theta, se = se, t = t, pval = pval,
               boot_se = boot_se, boot_theta = boot_theta)
  
  class(res) <- "DML"
  return(res)
}



#' Orthogonalized Estimation of Coefficient in PLR
#'
#' Function to estimate the structural parameter in a partially linear regression model (PLR).
#'
#' @inheritParams var_plr
#' @return List with estimate (\code{theta}).
#' @export
orth_plr_dml <- function(u_hat, v_hat, v_hatd, inf_model) { #, se_type) {

  theta <-  NA

  if (inf_model == "DML2018") {
    res_fit <- stats::lm(u_hat ~ 0 + v_hat)
    theta <- stats::coef(res_fit)
  }

   else if (inf_model == 'IV-type') {
     theta <- mean(v_hat*u_hat)/mean(v_hatd)
    # se <- 1/(mean(u_hat)^2) * mean((v_hat - theta*u_hat)*u_hat)^2
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
#' @inheritParams dml_plr
#' @param theta final dml estimator for the partially linear model.
#' @param d treatment variable.
#' @param v_hat Residuals from \eqn{d-m(x)}.
#' @param u_hat Residuals from \eqn{y-g(x)}.
#' @param v_hatd Product of \code{v_hat} with \code{d}.
#' @return Variance estimator (\code{var}).
var_plr <- function(theta, d, u_hat, v_hat, v_hatd, inf_model, se_type, dml_procedure) {
  
  var <- NA
  
  if (se_type == inf_model){ 
    
    if (inf_model == "DML2018") {
  
    var <- mean( 1/length(u_hat) * 1/(colMeans(v_hat^2, na.rm = TRUE))^2  *
            colMeans( ( (u_hat - v_hat*theta)*v_hat)^2), na.rm = TRUE)
    }
    
     else if (inf_model == 'IV-type') {
     var <- mean( 1/length(u_hat) * (1/colMeans(v_hatd, na.rm = TRUE))^2  * 
            colMeans( ( (u_hat - d*theta)*v_hat)^2, na.rm = TRUE) )
    }
  
  }
   
  # Q: only for "dml2"?
  if (se_type == "ls" & inf_model == "DML2018" & dml_procedure == "dml2") {
      res_fit <- stats::lm(u_hat ~ 0 + v_hat)
      var <- sandwich::vcovHC(res_fit)
  }
  
  
  return(c(var))
}



#' Bootstrap Implementation for Partially Linear Regression Model 
#'
#' Multiplier bootstrap to construct simultaneous confidence bands for multiple target coefficients in a partially linear regression model (PLR) with double machine learning.
#'
#' @inheritParams var_plr
#' @inheritParams dml_plr
#' @inheritParams DML
#' @param se Estimated standard error from DML procedure.
#' @return List with bootstrapped standard errors (\code{boot_se}) and bootstrapped coefficients.
bootstrap_plr <- function(theta, d, u_hat, v_hat, v_hatd, inf_model, se, bootstrap, nRep) {
  
  boot_var <- NA
  
 if (inf_model == "DML2018") {

    score <- (u_hat - v_hat*theta)*v_hat
    J <-  colMeans(v_hat*v_hat, na.rm = TRUE)
    
  }

  else if (inf_model == 'IV-type') {
    score <- (u_hat - d*theta)*v_hat
    J <- colMeans(v_hatd, na.rm = TRUE)
  }
  
  n <- length(d)
  pertub <- matrix(NA, nrow = 1, ncol = nRep)
  
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
















