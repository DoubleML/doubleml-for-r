#' Double Machine Learning for Interactive Regression Model.
#'
#' @param data Data frame.
#' @param y Name of outcome variable. The variable must be included in \code{data}.
#' @param d Name of treatment variables for which inference should be performed.
#' @inheritParams DML
#' @param resampling Resampling scheme for cross-fitting of class \code{\link[mlr3]{ResamplingCV}}.
#' @param dml_procedure Double machine learning algorithm to be used, either \code{"dml1"} or \code{"dml2"} (default).
#' @param mlmethod List with classification or regression methods according to naming convention of the \code{mlr} package. Set \code{mlmethod_g} for classification or regression method according to naming convention of the \code{mlr} package for regression of y on X (nuisance part g). Set \code{mlmethod_m} for  classification or regression method for regression of d on X (nuisance part m). 
#' @param params Hyperparameters to be passed to classification or regression method. Set hyperparameters \code{params_g1} for predictions of nuisance part g1, \code{params_g0} for nuisance part g0, and \code{params_m} for nuisance m.
#' @param inf_model Estimator for final estimation, default average treatment effect \code{"ATE"}. Alternatively switch to \code{"ATET"} for average treatment effect on the treated.
#' @param se_type Method to estimate standard errors (redundant / identical to inf_model).
#' @param bootstrap Choice for implementation of multiplier bootstrap, can be set to \code{"normal"} (by default), \code{"none"}, \code{"Bayes"}, \code{"wild"}.
#' @param nRep Number of repetitions for multiplier bootstrap, by default \code{nRep=500}.
#' @param ... further options passed to underlying functions.
#' @return Result object with estimated coefficient and standard errors.
#' @export

dml_irm <- function(data, y, d, k = 2, resampling = NULL, mlmethod, params = list(params_m = list(),
                    params_g = list()),
                    dml_procedure = "dml2",
                    inf_model = "ATE", se_type = "ATE",
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

  if (se_type != inf_model){
    se_type <- inf_model
    message("Options se_type and inf_model do not match. Set se_type to value of inf_model")
  }

  # Set up task_m first to get resampling (test and train ids) scheme based on full sample
  # nuisance m
  m_indx <- names(data) != y
  data_m <- data[, m_indx, drop = FALSE]
  
    # tbd: handle case with classif vs. regr. for task_p
  data_m[, d] <- factor(data_m[, d])
  task_m <- mlr3::TaskClassif$new(id = paste0("nuis_p_", d), backend = data_m,
                                    target = d, positive = "1")

  if (is.null(resampling)) {
    resampling_scheme <- mlr3::ResamplingCV$new()
    resampling_scheme$param_set$values$folds <- k
    resampling_scheme <- resampling_scheme$instantiate(task_m)
  }
  
  # tbd: handling of resampling 
  if (!resampling$is_instantiated) {
    resampling_scheme <- resampling$clone()
    resampling_scheme <- resampling_scheme$instantiate(task_m)
  }
  
  if (!is.null(resampling) & resampling$is_instantiated) {
    resampling_scheme <- mlr3::ResamplingCV$new()
    resampling_scheme$param_set$values$folds <- resampling$iters
    message("Specified 'resampling' was instantiated. New resampling scheme was instantiated internally.")
    resampling_scheme <- resampling_scheme$instantiate(task_m)
  } # tbd: else 
  
  
  n_iters <- resampling_scheme$iters
  # tbd: ensure that train_ids and test_ids are integers
  train_ids <- lapply(1:n_iters, function(x) resampling_scheme$train_set(x))
  test_ids <- lapply(1:n_iters, function(x) resampling_scheme$test_set(x))
  
  # train and test ids according to status of d
  # in each fold, select those with d = 0
  train_ids_0 <- lapply(1:n_iters, function(x) 
                    resampling_scheme$train_set(x)[data[resampling_scheme$train_set(x), d] == 0])
  # in each fold, select those with d = 0
  train_ids_1 <- lapply(1:n_iters, function(x) 
                    resampling_scheme$train_set(x)[data[resampling_scheme$train_set(x), d] == 1])
  
  ml_m <- mlr3::lrn(mlmethod$mlmethod_m, predict_type = "prob")
  ml_m$param_set$values <- params$params_m # tbd: check if parameter passing really works
  # ml_m <- mlr::makeLearner(mlmethod$mlmethod_m, id = "nuis_m", par.vals = params$params_m)
  r_m <- mlr3::resample(task_m, ml_m, resampling_scheme, store_models = TRUE)
  # r_m <- mlr::resample(learner = ml_m, task = task_m, resampling = rin)
  # m_hat_list <- r_m$data$prediction # alternatively, r_m$prediction (not listed)
  # # m_hat_list <- mlr::getRRPredictionList(r_m)
  # m_hat_list <- lapply(m_hat_list, function(x) x$response)
  # # m_hat_list <-lapply(m_hat_list$test,  extract_test_pred)
  m_hat_list <- lapply(r_m$data$prediction, function(x) x$test$prob[, "1"])

   
  # nuisance g0: E[Y|D=0, X]
  g_indx <- names(data) != d
  data_g <- data[ , g_indx, drop = FALSE]
  task_g0 <- mlr3::TaskRegr$new(id = paste0("nuis_g0_", d), backend = data_g, target = y)
  # tbd: handling learners from mlr3 base and mlr3learners package
  # ml_g <- mlr3::mlr_learners$get(mlmethod$mlmethod_g)
  ml_g0 <- mlr3::lrn(mlmethod$mlmethod_g)
  ml_g0$param_set$values <- params$params_g 
  resampling_g0 <- mlr3::rsmp("custom")
  # Train on subset with d == 0 (in each fold) only, predict for all test obs
  resampling_g0$instantiate(task_g0, train_ids_0, test_ids)
  train_ids_g0 <- lapply(1:n_iters, function(x) resampling_g0$train_set(x))
  test_ids_g0 <- lapply(1:n_iters, function(x) resampling_g0$test_set(x))

  # tbd: check if parameter passing really works
  # ml_g <-  mlr:makeLearner(mlmethod$mlmethod_g, id = "nuis_g", par.vals = params$params_g)
  r_g0 <- mlr3::resample(task_g0, ml_g0, resampling_g0, store_models = TRUE)
  # r_g <- mlr::resample(learner = ml_g, task = task_g, resampling = rin)
  # g0_hat_list <- r_g0$data$prediction
  # # g_hat_list <- mlr::getRRPredictionList(r_g)
  # #g_hat_list <- lapply(g_hat_list$test, extract_test_pred)
  # g0_hat_list <- lapply(g0_hat_list, function(x) x$response)
  g0_hat_list <- lapply(r_g0$data$prediction, function(x) x$test$response)

  # 
    # nuisance g1: E[Y|D=1, X]
  task_g1 <- mlr3::TaskRegr$new(id = paste0("nuis_g1_", d), backend = data_g, target = y)
  ml_g1 <- mlr3::lrn(mlmethod$mlmethod_g)
  ml_g1$param_set$values <- params$params_g # tbd: check if parameter passing really works
  resampling_g1 <- mlr3::rsmp("custom")
  resampling_g1$instantiate(task_g1, train_ids_1, test_ids)
  train_ids_g1 <- lapply(1:n_iters, function(x) resampling_g1$train_set(x))
  test_ids_g1 <- lapply(1:n_iters, function(x) resampling_g1$test_set(x))
  
  # tbd: option to omit estimation of g1_hat_list for ATET (omit computations)
  # Estimation only required for ATE, not for ATET
  #if (inf_model == "ATE") {
    r_g1 <- mlr3::resample(task_g1, ml_g1, resampling_g1, store_models = TRUE)
  #   # r_g <- mlr::resample(learner = ml_g, task = task_g, resampling = rin)
  #   g1_hat_list <- r_g1$data$prediction
  #   # g_hat_list <- mlr::getRRPredictionList(r_g)
  #   #g_hat_list <- lapply(g_hat_list$test, extract_test_pred)
  #   g1_hat_list <- lapply(g1_hat_list, function(x) x$response)
  # # }
  g1_hat_list <- lapply(r_g1$data$prediction, function(x) x$test$response)

  
  if ( (resampling_scheme$iters != resampling_g0$iters) ||
       (resampling_scheme$iters != resampling_g1$iters) ||
       (resampling_scheme$iters != n_iters) ||
       (resampling_g1$iters != n_iters) ||
       (resampling_g0$iters != n_iters) ||
       (!identical(train_ids_0, train_ids_g0)) ||
       (!identical(train_ids_1, train_ids_g1)) ||
       (!identical(test_ids, test_ids_g0)) ||
         (!identical(test_ids, test_ids_g1))) {
    stop('Resampling instances not equal')
  }
  
  # tbd: handling case: (is.null(rownames(data)))
  if (any(vapply(train_ids, function(x) is.character(x), logical(1L))) ) {
    train_ids <- lapply(train_ids, function(x) as.integer(x))
  }
    
  if (any(vapply(test_ids, function(x) is.character(x), logical(1L)))) {
    test_ids <- lapply(test_ids, function(x) as.integer(x))
  }

  # test_index_list <- rin$test.inds
 #  n_k <- vapply(test_index_list, length, double(1))
  n_k <- vapply(test_ids, length, double(1L))

  D <- data[, d]
  Y <- data[, y]

  # DML 1
  if ( dml_procedure == "dml1") {
    thetas <- vars <- boot_vars <-  rep(NA, n_iters)
    boot_thetas <- matrix(NA, ncol = nRep, nrow = n_iters)
    se_i <- NA
    
    g0_hat <- g1_hat <- u0_hat <- u1_hat <- m_hat_k <- d_k <- p_hat_k <- y_k <- matrix(NA, nrow = max(n_k), ncol = n_iters)

    for (i in 1:n_iters) {
        # test_index = test_index_list[[i]]
        test_index = test_ids[[i]]

        g0_hat[, i] <- g0_hat_list[[i]]
        g1_hat[, i] <- g1_hat_list[[i]]
        
        m_hat_k[, i] <- m_hat_list[[i]]
        d_k[, i] <- D[test_index]
        p_hat_k[, i] <- mean(D[test_index])
        y_k[, i] <- Y[test_index]
        u0_hat[, i]  <- Y[test_index] - g0_hat[, i]
        u1_hat[, i] <- Y[test_index] - g1_hat[, i]
    
        orth_est <- orth_irm_dml(g0_hat = g0_hat[, i], g1_hat = g1_hat[, i], 
                                 u0_hat = u0_hat[, i], u1_hat = u1_hat[, i],
                                 d = d_k[, i], p_hat = p_hat_k[, i], m = m_hat_k[, i], 
                                  y = y_k[, i],
                                 inf_model = inf_model) #, se_type)
        thetas[i] <- orth_est$theta

    }
    
    theta <- mean(thetas, na.rm = TRUE)
    
    se <- sqrt(var_irm(theta = theta, g0_hat = g0_hat, g1_hat = g1_hat, 
                            u0_hat = u0_hat, u1_hat = u1_hat, 
                            d = d_k, p_hat = p_hat_k, m = m_hat_k, y = y_k, inf_model = inf_model))

    t <- theta/se 
    
    pval <-  2 * stats::pnorm(-abs(t))
    
    if (bootstrap != "none") {
      
      boot <- bootstrap_irm(theta = theta, g0_hat = g0_hat, g1_hat = g1_hat, 
                            u0_hat = u0_hat, u1_hat = u1_hat, 
                            d = d_k, p_hat = p_hat_k, m = m_hat_k, y = y_k, 
                            inf_model = inf_model, se = se,
                              bootstrap = bootstrap, nRep = nRep)
  #    boot_se <- sqrt(boot$boot_var)
      boot_theta <- boot$boot_theta
    }
    
  }
  
  if ( dml_procedure == "dml2") {

    g0_hat <- g1_hat <- u0_hat <- u1_hat <- m_hat <- p_hat <- matrix(NA, nrow = n, ncol = 1)
    
    for (i in 1:n_iters){

       # test_index = test_index_list[[i]]
       test_index = test_ids[[i]]

       m_hat[test_index, 1] <- m_hat_list[[i]]
       p_hat[test_index, 1] <- mean(D[test_index])
       g0_hat[test_index, 1] <- g0_hat_list[[i]]
       g1_hat[test_index, 1] <- g1_hat_list[[i]]

       u0_hat[test_index, 1] <- Y[test_index] - g0_hat[test_index, 1]
       u1_hat[test_index, 1] <- Y[test_index] - g1_hat[test_index, 1]
 
    }

    orth_est <- orth_irm_dml(g0_hat = g0_hat, g1_hat = g1_hat, 
                            u0_hat = u0_hat, u1_hat = u1_hat, d = D, p_hat = p_hat,
                            y = Y, m = m_hat, inf_model = inf_model)
    
    theta <- orth_est$theta
    se <- sqrt(var_irm(theta = theta, g0_hat = g0_hat, g1_hat = g1_hat, 
                            u0_hat = u0_hat, u1_hat = u1_hat, 
                            d = D, p_hat = p_hat, m = m_hat, y = Y, inf_model = inf_model))
    
    t <- theta/se 
    
    pval <-  2 * stats::pnorm(-abs(t))
    
    if (bootstrap != "none") {
      
      boot <- bootstrap_irm(theta = theta, 
                            g0_hat = g0_hat, g1_hat = g1_hat, 
                            u0_hat = u0_hat, u1_hat = u1_hat, 
                            d = D, p_hat = p_hat, m = m_hat, y = Y, 
                            inf_model = inf_model, se = se,
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



#' Orthogonalized Estimation of Coefficient in irm
#'
#' Function to estimate the structural parameter in an interactive regression model (irm).
#'
#' @inheritParams var_irm
#' @return List with estimate (\code{theta}).
#' @export
orth_irm_dml <- function(g0_hat, g1_hat, u0_hat, u1_hat, d, p_hat, m, y, inf_model) { #, se_type) {

  theta <- NA

  if (inf_model == "ATE") {
     theta <- mean(g1_hat - g0_hat + d*(u1_hat)/m - (1-d)*u0_hat/(1-m))
    
  }

   else if (inf_model == "ATET") {
     
     theta <- mean( d*(y - g0_hat)/p_hat - m*(1-d)*u0_hat/(p_hat*(1-m))) / mean(d/p_hat)
   }
  
  else {
    stop("Inference framework for orthogonal estimation unknown")
  }

  res <- list(theta = theta)
  return(res)
}


#' Variance estimation for DML estimator in the interactive regression model
#'
#' Variance estimation for the structural parameter estimator in an interactive regression model (irm) with double machine learning.
#' @inheritParams dml_irm
#' @param theta Final dml estimator for interactive regression model.
#' @param d Treatment variable.
#' @param y Outcome variable.
#' @param m Predictions from \eqn{d-m(x)}.
#' @param g0_hat Predictions from \eqn{g(0,X)}.
#' @param g1_hat Predictions from \eqn{g(1,X)}
#' @param u0_hat Residuals from \eqn{y-g(0,x)}.
#' @param u1_hat Residuals from \eqn{y-g(1,x)}.
#' @return Variance estimator (\code{var}).
var_irm <- function(theta, g0_hat, g1_hat, u0_hat, u1_hat, d, p_hat, m, y, inf_model) {
  
  var <- NA
  
  if (inf_model == "ATE") {
  
  # var <- mean( 1/length(d) * colMeans(((g1_hat - g0_hat + d*(u1_hat)/m + (1-d)*u0_hat/(1-m))^2), na.rm = TRUE))
  
    var <- mean( 1/length(d) * colMeans(((g1_hat - g0_hat + d*(u1_hat)/m - (1-d)*u0_hat/(1-m) - theta)^2), na.rm = TRUE))
    
  }
    
   else if (inf_model == "ATET") {
  
     var <- mean( 1/length(d) * colMeans( (d*(y - g0_hat)/p_hat - m*(1-d)*u0_hat/(p_hat*(1-m)) - d/p_hat * theta)^2, na.rm = TRUE))
     
  }
  
  return(c(var))
}



#' Bootstrap Implementation for Interactive Regression Model 
#'
#' Multiplier bootstrap to construct simultaneous confidence bands for multiple target coefficients in a interactive regression model (irm) with double machine learning.
#'
#' @inheritParams var_irm
#' @inheritParams dml_irm
#' @inheritParams DML
#' @param se Estimated standard error from DML procedure.
#' @return List with bootstrapped standard errors (\code{boot_se}) and bootstrapped coefficients.
bootstrap_irm <- function(theta, g0_hat, g1_hat, u0_hat, u1_hat, d, p_hat, m, y, inf_model, se, bootstrap, nRep) {
  
  boot_var <- NA
  
 if (inf_model == "ATE") {

    score <- g1_hat - g0_hat + d*(u1_hat)/m + (1-d)*u0_hat/(1-m) - theta
 }
  
 if (inf_model == "ATET") {

    score <- d*(y - g0_hat)/p_hat- m*(1-d)*u0_hat/(p_hat*(1-m)) - d/p_hat * theta
  
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
      
     pertub[1,i] <- mean( colMeans(weights * 1/se * score, na.rm = TRUE))
    
  }
  
  res = list(boot_theta = pertub)
  return(c(res))
}
















