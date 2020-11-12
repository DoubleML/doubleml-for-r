#' Double Machine Learning for Interactive Instrumental Variable Regression Model.
#'
#' @param data Data frame.
#' @param y Name of outcome variable. The variable must be included in \code{data}.
#' @param d Name of treatment variables for which inference should be performed.
#' @param z Name of instrumental variables. 
#' @inheritParams DML
#' @param resampling Resampling scheme for cross-fitting of class \code{\link[mlr3]{ResamplingCV}}.
#' @param dml_procedure Double machine learning algorithm to be used, either \code{"dml1"} or \code{"dml2"} (default).
#' @param mlmethod List with classification or regression methods according to naming convention of the \code{mlr} package. Set \code{mlmethod_mu1} for classification or regression method according to naming convention of the \code{mlr} package for regression of y on X for individuals with Z = 1  (nuisance part mu1) and \code{mlmethod_mu0} for individuals with Z = 0 (nuisance part mu0). Set \code{mlmethod_m1} for  classification method for regression of d on X for individuals with Z = 1 (nuisance part m1) and \code{mlmethod_m0} for observations with Z = 0 (nuisance part m0). Set \code{mlmethod_p} for classification method for propensity score (nuisance part p).
#' @param params Hyperparameters to be passed to classification or regression method. Set hyperparameters \code{params_mu1} for predictions of nuisance part g1, \code{params_mu0} for nuisance part mu0, and so forth (names must match those in \code{mlmethod}).
#' @param score Estimator for final estimation, default average treatment effect \code{"LATE"}. Alternatively switch to \code{"LATTE"} for average treatment effect on the treated.
#' @param se_type Method to estimate standard errors (redundant / identical to score).
#' @param always_takers option to adapt to cases with (default) and without always-takers. If \code{FALSE}, the estimator is adapted to a setting without always-takers.
#' @param never_takers option to adapt to cases with (default) and without never-takers. If \code{FALSE}, the estimator is adapted to a setting without never-takers.
#' @param bootstrap Choice for implementation of multiplier bootstrap, can be set to \code{"normal"} (by default), \code{"none"}, \code{"Bayes"}, \code{"wild"}.
#' @param nRep Number of repetitions for multiplier bootstrap, by default \code{nRep=500}.
#' @param ... further options passed to underlying functions.
#' @return Result object with estimated coefficient and standard errors.
#' @export

dml_irmiv <- function(data, y, d, z, k = 2, smpls = NULL, mlmethod, params = list(params_mu = list(), params_m = list(), params_p = list()), 
                    dml_procedure = "dml2", always_takers = TRUE, never_takers = TRUE,
                    score = "LATE", se_type = "LATE",
                    bootstrap = "normal",  nRep = 500, ...) {
  
  if (is.null(smpls)) {
    smpls = sample_splitting(k, data)
  }
  train_ids = smpls$train_ids
  test_ids = smpls$test_ids
  
  checkmate::checkDataFrame(data)

  # tbd: ml_method handling: default mlmethod_g = mlmethod_m
  # tbd: parameter passing
  n <- nrow(data)
  theta <- se <- te <- pval <- boot_se <- NA
  boot_theta <- matrix(NA, nrow = 1, ncol = nRep)

  if (se_type != score){
    se_type <- score
    message("Options se_type and score do not match. Set se_type to value of score")
  }

  # Set up task_m first to get resampling (test and train ids) scheme based on full sample
  # nuisance m
  p_indx <- names(data) != y & names(data) != d
  data_p <- data[, p_indx, drop = FALSE]
  
  # tbd: handle case with classif vs. regr. for task_p
  # if (grepl("regr.", mlmethod$mlmethod_p )) {
  #  # task_p <- mlr3::TaskRegr$new(id = paste0("nuis_p_", z), backend = data_p, target = z)
  #   task_p <- mlr3::TaskRegr$new(id = paste0("nuis_p_", z), backend = data_p, target = z)
  #   # task_p <- mlr3::tsk(id = paste0("nuis_p_", z), backend )
  # }
  
  # if (grepl("classif.", mlmethod$mlmethod_p )) {
  data_p[, z] <- factor(data_p[, z])
  task_p <- mlr3::TaskClassif$new(id = paste0("nuis_p_", z), backend = data_p,
                                    target = z, positive = "1")
  # }
  
  resampling_p <- mlr3::rsmp("custom")
  resampling_p$instantiate(task_p, train_ids, test_ids)
  n_iters <- resampling_p$iters
  
  # # train and test ids according to status of d
  # # in each fold, select those with d = 0
  # train_ids_d0 <- lapply(1:n_iters, function(x) 
  #                   resampling_p$train_set(x)[data[resampling_p$train_set(x), d] == 0])
  # # in each fold, select those with d = 0
  # train_ids_d1 <- lapply(1:n_iters, function(x) 
  #                   resampling_p$train_set(x)[data[resampling_p$train_set(x), d] == 1])
  # 
   # train and test ids according to status of z
  # in each fold, select those with z = 0
  train_ids_0 <- lapply(1:n_iters, function(x) 
                    resampling_p$train_set(x)[data[resampling_p$train_set(x), z] == 0])
  # in each fold, select those with d = 0
  train_ids_1 <- lapply(1:n_iters, function(x) 
                    resampling_p$train_set(x)[data[resampling_p$train_set(x), z] == 1])

  # check ids
  # vapply(as.list(1:n_iters), function(x) setequal(union(train_ids_z0[[x]], train_ids_z1[[x]]), train_ids[[x]]), double(1L))
  
  ml_p <- mlr3::lrn(mlmethod$mlmethod_p, predict_type = "prob")
  ml_p$param_set$values <- params$params_p # tbd: check if parameter passing really works
  
  # ml_m <- mlr::makeLearner(mlmethod$mlmethod_m, id = "nuis_m", par.vals = params$params_m)
  r_p <- mlr3::resample(task_p, ml_p, resampling_p, store_models = TRUE)
  # # r_m <- mlr::resample(learner = ml_m, task = task_m, resampling = rin)
  # p_hat_list <- r_p$data$prediction$test # alternatively, r_m$prediction (not listed)
  # # m_hat_list <- mlr::getRRPredictionList(r_m)
  # p_hat_list <- lapply(p_hat_list, function(x) x$response)
  # # m_hat_list <-lapply(m_hat_list$test,  extract_test_pred)
  # 
  # p_hat_list <- lapply(r_p$data$prediction, function(x) x$test$prob[, "1"])
  p_hat_list = lapply(r_p$data$predictions(), function(x) x$prob[, "1"])

  # nuisance mu0: E[Y|Z=0, X]
  mu_indx <- names(data) != d & names(data) != z
  data_mu <- data[ , mu_indx, drop = FALSE]
  task_mu0 <- mlr3::TaskRegr$new(id = paste0("nuis_mu0_", z), backend = data_mu, target = y)
  # tbd: handling learners from mlr3 base and mlr3learners package
  # ml_g <- mlr3::mlr_learners$get(mlmethod$mlmethod_g)
  ml_mu0 <- mlr3::lrn(mlmethod$mlmethod_mu)
  ml_mu0$param_set$values <- params$params_mu 
  resampling_mu0 <- mlr3::rsmp("custom")
  # Train on subset with z == 0 (in each fold) only, predict for all test obs
  resampling_mu0$instantiate(task_mu0, train_ids_0, test_ids)
  train_ids_mu0 <- lapply(1:n_iters, function(x) resampling_mu0$train_set(x))
  test_ids_mu0 <- lapply(1:n_iters, function(x) resampling_mu0$test_set(x))

  # tbd: check if parameter passing really works
  # ml_g <-  mlr:makeLearner(mlmethod$mlmethod_g, id = "nuis_g", par.vals = params$params_g)
  r_mu0 <- mlr3::resample(task_mu0, ml_mu0, resampling_mu0, store_models = TRUE)
  # r_g <- mlr::resample(learner = ml_g, task = task_g, resampling = rin)
  # g0_hat_list <- r_mu0$data$prediction
  # # g_hat_list <- mlr::getRRPredictionList(r_g)
  # #g_hat_list <- lapply(g_hat_list$test, extract_test_pred)
  # g0_hat_list <- lapply(g0_hat_list, function(x) x$response)
  # 
  # mu0_hat_list <- lapply(r_mu0$data$prediction, function(x) x$test$response)
  mu0_hat_list <- lapply(r_mu0$data$predictions(), function(x) x$response)

  # nuisance g1: E[Y|Z=1, X]
  task_mu1 <- mlr3::TaskRegr$new(id = paste0("nuis_mu1_", z), backend = data_mu, target = y)
  ml_mu1 <- mlr3::lrn(mlmethod$mlmethod_mu)
  ml_mu1$param_set$values <- params$params_mu
  resampling_mu1 <- mlr3::rsmp("custom")
  # Train on subset with z == 1 (in each fold) only, predict for all test obs
  resampling_mu1$instantiate(task_mu1, train_ids_1, test_ids)
  train_ids_mu1 <- lapply(1:n_iters, function(x) resampling_mu1$train_set(x))
  test_ids_mu1 <- lapply(1:n_iters, function(x) resampling_mu1$test_set(x))

  r_mu1 <- mlr3::resample(task_mu1, ml_mu1, resampling_mu1, store_models = TRUE)
  # mu1_hat_list <- lapply(r_mu1$data$prediction, function(x) x$test$response)
  mu1_hat_list <- lapply(r_mu1$data$predictions(), function(x) x$response)

  
  # nuisance m0: E[D|Z=0, X]
  m_indx <- names(data) != y & names(data) != z
  data_m <- data[, m_indx, drop = FALSE]
  data_m[, d] <- factor(data_m[, d])
  
  if (always_takers == FALSE & never_takers == FALSE) {

        message("If there are no always-takers and no never-takers, ATE is estimated")
  }
  
  if (always_takers == FALSE) {
    lengths <- lapply(test_ids, length)
    m0_hat_list <- lapply(lengths, function(x) rep(0, x))
  } 

  if (always_takers == TRUE) {
    task_m0 <- mlr3::TaskClassif$new(id = paste0("nuis_m0_", d), backend = data_m,
                                      target = d, positive = "1")
    ml_m0 <- mlr3::lrn(mlmethod$mlmethod_m, predict_type = "prob")
    ml_m0$param_set$values <- params$params_m # tbd: check if parameter passing really works
   
    resampling_m0 <- mlr3::rsmp("custom")
    # Train on subset with z == 0 (in each fold) only, predict for all test obs
    resampling_m0$instantiate(task_m0, train_ids_0, test_ids)
    train_ids_m0 <- lapply(1:n_iters, function(x) resampling_m0$train_set(x))
    test_ids_m0 <- lapply(1:n_iters, function(x) resampling_m0$test_set(x))
    r_m0 <- mlr3::resample(task_m0, ml_m0, resampling_m0, store_models = TRUE)
    # m0_hat_list <- lapply(r_m0$data$prediction, function(x) x$test$prob[, "1"])
    m0_hat_list <- lapply(r_m0$data$predictions(), function(x) x$prob[, "1"])
  }
  
  if (never_takers == FALSE) {
    lengths <- lapply(test_ids, length)
    m1_hat_list <- lapply(lengths, function(x) rep(1, x))
  }
  
  if (never_takers == TRUE) {
    # nuisance m1: E[E|Z=1, 0]
    task_m1 <- mlr3::TaskClassif$new(id = paste0("nuis_m1_", d), backend = data_m,
                                      target = d, positive = "1")
    ml_m1 <- mlr3::lrn(mlmethod$mlmethod_m, predict_type = "prob")
    ml_m1$param_set$values <- params$params_m # tbd: check if parameter passing really works
   
    resampling_m1 <- mlr3::rsmp("custom")
    # Train on subset with z == 0 (in each fold) only, predict for all test obs
    resampling_m1$instantiate(task_m1, train_ids_1, test_ids)
    train_ids_m1 <- lapply(1:n_iters, function(x) resampling_m1$train_set(x))
    test_ids_m1 <- lapply(1:n_iters, function(x) resampling_m1$test_set(x))
    r_m1 <- mlr3::resample(task_m1, ml_m1, resampling_m1, store_models = TRUE)
    # m1_hat_list <- lapply(r_m1$data$prediction, function(x) x$test$prob[, "1"])
    m1_hat_list <- lapply(r_m1$data$predictions(), function(x) x$prob[, "1"])
  }
  
  if ( (resampling_p$iters != resampling_mu0$iters) ||
       (resampling_p$iters != resampling_mu1$iters) ||
       # (resampling_p$iters != resampling_m0$iters) ||
       # (resampling_p$iters != resampling_m1$iters) ||
       (resampling_p$iters != n_iters) ||
       (!identical(train_ids_0, train_ids_mu0)) ||
       (!identical(train_ids_1, train_ids_mu1)) ||
       # (!identical(train_ids_0, train_ids_m0)) ||
       # (!identical(train_ids_1, train_ids_m1)) ||      
       (!identical(test_ids, test_ids_mu0)) ||
       # (!identical(test_ids, test_ids_mu1)) ||
       # (!identical(test_ids, test_ids_m0)) ||
       # (!identical(test_ids, test_ids_m1))) {
      (!identical(test_ids, test_ids_mu1))) {
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
  Z <- data[, z]

  # DML 1
  if ( dml_procedure == "dml1") {
    thetas <- vars <- boot_vars <-  rep(NA, n_iters)
    boot_thetas <- matrix(NA, ncol = nRep, nrow = n_iters)
    se_i <- NA
    
    p_hat <- mu0_hat <- mu1_hat <- m0_hat <- m1_hat <- d_k <- y_k <- z_k <- matrix(NA, nrow = max(n_k), ncol = n_iters)
    p_hat_se <- mu0_hat_se <- mu1_hat_se <- m0_hat_se <- m1_hat_se <- matrix(NA, nrow = n, ncol = 1)
    
    for (i in 1:n_iters) {
        # test_index = test_index_list[[i]]
        test_index = test_ids[[i]]

        p_hat[, i] <- p_hat_se[test_index, 1] <- p_hat_list[[i]]
        mu0_hat[, i] <- mu0_hat_se[test_index, 1] <- mu0_hat_list[[i]]
        mu1_hat[, i] <- mu1_hat_se[test_index, 1] <- mu1_hat_list[[i]]
        m0_hat[, i] <- m0_hat_se[test_index, 1] <- m0_hat_list[[i]]
        m1_hat[, i] <- m1_hat_se[test_index, 1] <- m1_hat_list[[i]]
        d_k[, i] <- D[test_index]
        y_k[, i] <- Y[test_index]
        z_k[, i] <- Z[test_index]

        orth_est <- orth_irmiv_dml(p_hat = p_hat[, i], mu0_hat = mu0_hat[, i],
                                   mu1_hat = mu1_hat[, i], 
                                   m0_hat = m0_hat[, i], m1_hat = m1_hat[, i],
                                   d = d_k[, i], y = y_k[, i], z = z_k[, i],
                                   score = score) #, se_type)
        thetas[i] <- orth_est$theta
    
    }
    
    theta <- mean(thetas, na.rm = TRUE)
    
    se <- sqrt(var_irmiv(theta = theta, p_hat = p_hat_se, mu0_hat = mu0_hat_se, 
                          mu1_hat = mu1_hat_se, m0_hat = m0_hat_se, m1_hat = m1_hat_se, 
                          d = D, y = Y, z = Z, score = score))
    
    t <- theta/se 
    
    pval <-  2 * stats::pnorm(-abs(t))
    
    if (bootstrap != "none") {
      
      boot <- bootstrap_irmiv(theta = theta, p_hat = p_hat, mu0_hat = mu0_hat, 
                          mu1_hat = mu1_hat, m0_hat = m0_hat, m1_hat = m1_hat, 
                          d = d_k, y = y_k, z = z_k, score = score, se = se,
                              bootstrap = bootstrap, nRep = nRep)
  #    boot_se <- sqrt(boot$boot_var)
      boot_theta <- boot$boot_theta
    }
    
  }
  
  if ( dml_procedure == "dml2") {

    p_hat <- mu0_hat <- mu1_hat <- m0_hat <- m1_hat <- matrix(NA, nrow = n, ncol = 1)
    
    for (i in 1:n_iters){

       # test_index = test_index_list[[i]]
       test_index = test_ids[[i]]
      
       p_hat[test_index, 1] <- p_hat_list[[i]]
       mu0_hat[test_index, 1] <- mu0_hat_list[[i]]
       mu1_hat[test_index, 1] <- mu1_hat_list[[i]]
       m0_hat[test_index, 1] <- m0_hat_list[[i]]
       m1_hat[test_index, 1] <- m1_hat_list[[i]]
 
    }

    orth_est <- orth_irmiv_dml(p_hat = p_hat, mu0_hat = mu0_hat,
                                   mu1_hat = mu1_hat, 
                                   m0_hat = m0_hat, m1_hat = m1_hat,
                                   d = D, y = Y, z = Z,
                                   score = score)
    
    theta <- orth_est$theta
    se <- sqrt(var_irmiv(theta = theta, p_hat = p_hat, mu0_hat = mu0_hat, 
                          mu1_hat = mu1_hat, m0_hat = m0_hat, m1_hat = m1_hat, 
                          d = D, y = Y, z = Z, score = score))
    
    t <- theta/se 
    
    pval <-  2 * stats::pnorm(-abs(t))
    
    if (bootstrap != "none") {
      
      boot <- bootstrap_irmiv(theta = theta, p_hat = p_hat, mu0_hat = mu0_hat, 
                          mu1_hat = mu1_hat, m0_hat = m0_hat, m1_hat = m1_hat, 
                          d = D, y = Y, z = Z, score = score, se = se,
                              bootstrap = bootstrap, nRep = nRep)
  #    boot_se <- sqrt(boot$boot_var)
      boot_theta <- boot$boot_theta
    }
  }
  
  
  all_preds = list(p_hat_list = p_hat_list,
                   mu0_hat_list = mu0_hat_list,
                   mu1_hat_list = mu1_hat_list,
                   m0_hat = m0_hat,
                   m1_hat = m1_hat)

  names(theta) <- names(se) <- names(boot_se) <- d
  res <- list( coefficients = theta, se = se, t = t, pval = pval,
               boot_se = boot_se, boot_theta = boot_theta,
               all_preds = all_preds)
  
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
orth_irmiv_dml <- function(p_hat, mu0_hat, mu1_hat, m0_hat, m1_hat, d, y, z, score) { #, se_type) {

  theta <- NA

  if (score == "LATE" | score == "partialling out") {
     theta <- 1/mean( m1_hat - m0_hat + z*(d-m1_hat)/p_hat - ((1-z)*(d-m0_hat)/(1-p_hat)))*
              mean(mu1_hat - mu0_hat + z*(y - mu1_hat)/p_hat - ((1-z)*(y - mu0_hat)/(1-p_hat)))
  }
 
   else if (score == "LATTE") { 
     
     # tbd: LATTE
     
     # Ep <- mean(d) 
     
     # theta <- mean( d*(y - g0_hat)/Ep - m*(1-d)*u0_hat/(Ep*(1-m))) / mean(d/Ep)
   }
  
  else {
    stop("Inference framework for orthogonal estimation unknown")
  }

  res <- list(theta = theta)
  return(res)
}


#' Variance estimation for DML estimator in the Interactive Instrumental Variable Regression Model 
#'
#' Variance estimation for the structural parameter estimator in an interactive regression model (irm) with double machine learning.
#' @inheritParams dml_irmiv
#' @param theta Final dml estimator for interactive regression model.
#' @param d Treatment variable.
#' @param y Outcome variable.
#' @param z Instrumental variable.
#' @param m Predictions from \eqn{d-m(x)}.
#' @param g0_hat Predictions from \eqn{g(0,X)}.
#' @param g1_hat Predictions from \eqn{g(1,X)}
#' @param u0_hat Residuals from \eqn{y-g(0,x)}.
#' @param u1_hat Residuals from \eqn{y-g(1,x)}.
#' @return Variance estimator (\code{var}).
var_irmiv <-  function(theta, p_hat, mu0_hat, mu1_hat, m0_hat, m1_hat, d, y, z, score) {

  var <- NA

  if (score == "LATE") {
    
    var <- mean( 1/length(d) * 1/(colMeans((m1_hat - m0_hat + z*(d-m1_hat)/p_hat - (1-z)*(d-m0_hat)/(1-p_hat)), na.rm = TRUE))^2  *
                   colMeans( ( mu1_hat - mu0_hat + z*(y - mu1_hat)/p_hat - (1-z)*(y - mu0_hat)/(1-p_hat) - 
                                 (m1_hat - m0_hat + z*(d-m1_hat)/p_hat - (1-z)*(d-m0_hat)/(1-p_hat))*theta )^2, na.rm = TRUE) )
    
  } 
  
  # else if (score == "partialling out") {
  # 
  #      score_mat <- 1/(m1_hat - m0_hat + z*(d-m1_hat)/p_hat - ((1-z)*(d-m0_hat)/(1-p_hat)))*
  #                     (mu1_hat - mu0_hat + z*(y - mu1_hat)/p_hat - ((1-z)*(y - mu0_hat)/(1-p_hat)))
  #               
  #      var <- 1/length(d) * apply(score_mat, 2, function(x) var(x, na.rm = TRUE))
  #   }
  #   
    else if (score == "LATTE") {
     
     # tbd: LATTE
  
     #    if (is.numeric(d)) {
     #      d <- as.matrix(d)
     #    }
     #
     #  Ep <- colMeans(d)
     #
     # var <- mean( 1/length(d) * colMeans( (d*(y - g0_hat)/Ep - m*(1-d)*u0_hat/(Ep*(1-m)) - d/Ep * theta)^2, na.rm = TRUE))
     #

      message ("LATTE not yet implemented.")
      }  else {
    
    stop("Inference framework for variance estimation unknown")
  }
  return(c(var))
}



#' Bootstrap Implementation for Interactive Instrumental Variable Regression Model 
#'
#' Multiplier bootstrap to construct simultaneous confidence bands for multiple target coefficients in a interactive regression model (irm) with double machine learning.
#'
#' @inheritParams var_irmiv
#' @inheritParams dml_irmiv
#' @inheritParams DML
#' @param se Estimated standard error from DML procedure.
#' @return List with bootstrapped standard errors (\code{boot_se}) and bootstrapped coefficients.
bootstrap_irmiv <- function(theta, p_hat, mu0_hat, mu1_hat, m0_hat, m1_hat, 
                            d, y, z, score, se, bootstrap, nRep) {

  boot_var <- NA

 if (score == "LATE") {

    score <-  mu1_hat - mu0_hat + z*(y - mu1_hat)/p_hat - (1-z)*(y - mu0_hat)/(1-p_hat) - 
            (m1_hat - m0_hat + z*(d-m1_hat)/p_hat - (1-z)*(d-m0_hat)/(1-p_hat))*theta 
    
    J <-  -colMeans( m1_hat - m0_hat + z*(d-m1_hat)/p_hat 
                                  - (1-z)*(d-m0_hat)/(1-p_hat) , na.rm = TRUE)
 }

 else if (score == "LATTE") {

    # if (is.numeric(d)) {
    #     d <- as.matrix(d)
    #   }
    # 
    # Ep <- colMeans(d)
    # 
    # score <- d*(y - g0_hat)/Ep - m*(1-d)*u0_hat/(Ep*(1-m)) - d/Ep * theta

 } else {
    
    stop("Inference framework for multiplier bootstrap unknown")
 }
  
  n <- length(d)
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
















