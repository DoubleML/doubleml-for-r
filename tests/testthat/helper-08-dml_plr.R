#' Double Machine Learning for Partially Linear Regression.
#'
#' @param data Data frame.
#' @param y Name of outcome variable. The variable must be included in \code{data}.
#' @param d Name of treatment variables for which inference should be performed.
#' @inheritParams DML
#' @param resampling Resampling scheme for cross-fitting of class \code{\link[mlr3]{ResamplingCV}}.
#' @param dml_procedure Double machine learning algorithm to be used, either \code{"dml1"} or \code{"dml2"} (default).
#' @param mlmethod List with classification or regression methods according to naming convention of the \code{mlr} package. Set \code{mlmethod_g} for classification or regression method according to naming convention of the \code{mlr} package for regression of y on X (nuisance part g). Set \code{mlmethod_m} for  classification or regression method for regression of d on X (nuisance part m). 
#' @param params Hyperparameters to be passed to classification or regression method. Set hyperparameters \code{params_g} for predictions of nuisance part g and \code{params_m} for nuisance m.
#' @param score Inference model for final estimation, default \code{"IV-type"} (...)
#' @param se_type Method to estimate standard errors. Default \code{"ls"} to estimate usual standard error from least squares regression of residuals. Alternatively, specify \code{"IV-type"} or \code{"partialling out"} to obtain standard errors that correspond to the specified \code{score}. The options chosen for \code{score} and \code{se_type} are required to match. 
#' @param bootstrap Choice for implementation of multiplier bootstrap, can be set to \code{"normal"} (by default), \code{"none"}, \code{"Bayes"}, \code{"wild"}.
#' @param nRep Number of repetitions for multiplier bootstrap, by default \code{nRep=500}.
#' @param ... further options passed to underlying functions.
#' @return Result object with estimated coefficient and standard errors.
#' @export

dml_plr <- function(data, y, d, k = 2, smpls = NULL, mlmethod, params = list(params_m = list(),
                    params_g = list()),
                    dml_procedure = "dml2",
                    score = "IV-type", se_type = "ls", ...) {
  
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

   if (se_type != "ls") {
    se_type <- score
  }
  
  if (se_type == "ls" & dml_procedure == "dml1"){
    se_type <- score
  }

  # nuisance g
  g_indx <- names(data) != d 
  data_g <- data[ , g_indx, drop = FALSE]
  task_g <- mlr3::TaskRegr$new(id = paste0("nuis_g_", d), backend = data_g, target = y)
  
  resampling_g <- mlr3::rsmp("custom")
  resampling_g$instantiate(task_g, train_ids, test_ids)
  n_iters = resampling_g$iters
  
  # tbd: handling learners from mlr3 base and mlr3learners package
  # ml_g <- mlr3::mlr_learners$get(mlmethod$mlmethod_g)
  ml_g <- mlr3::lrn(mlmethod$mlmethod_g)
  ml_g$param_set$values <- params$params_g # tbd: check if parameter passing really works
    
   # ml_g <-  mlr:makeLearner(mlmethod$mlmethod_g, id = "nuis_g", par.vals = params$params_g)
  r_g <- mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
  
  # r_g <- mlr::resample(learner = ml_g, task = task_g, resampling = rin)
  # g_hat_list <- r_g$data$prediction
  # # g_hat_list <- mlr::getRRPredictionList(r_g)
  # #g_hat_list <- lapply(g_hat_list$test, extract_test_pred)
  # g_hat_list <- lapply(g_hat_list, function(x) x$response)
  g_hat_list <- lapply(r_g$data$predictions(), function(x) x$response)
  # nuisance m
  m_indx <- names(data) != y
  data_m <- data[, m_indx, drop = FALSE]
  task_m <- mlr3::TaskRegr$new(id = paste0("nuis_m_", d), backend = data_m, target = d)
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
  m_hat_list <- lapply(r_m$data$predictions(), function(x) x$response)


  # if ((rin$desc$iters != r_g$pred$instance$desc$iters) ||
  #     (rin$desc$iters != r_m$pred$instance$desc$iters) ||
  #     !identical(rin$train.inds, r_g$pred$instance$train.inds) ||
  #     !identical(rin$train.inds, r_m$pred$instance$train.inds)) {
  #   stop('Resampling instances not equal')
  # }
  if ( (resampling_g$iters != resampling_m$iters) ||
       (resampling_g$iters != n_iters) ||
       (resampling_m$iters != n_iters) ||
         (!identical(train_ids, train_ids_m)) ||
         (!identical(test_ids, test_ids_m))) {
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

  D <- data[ , d]
  Y <- data[ , y]

  # DML 1
  if ( dml_procedure == "dml1") {
    thetas <- vars <-  rep(NA, n_iters)
    se_i <- NA
    
    v_hat <- u_hat <- v_hatd <- d_k <- matrix(NA, nrow = max(n_k), ncol = n_iters)
    v_hat_se <- u_hat_se <- v_hatd_se <- matrix(NA, nrow = max(n), ncol = 1)
    
    for (i in 1:n_iters) {
        # test_index = test_index_list[[i]]
        test_index = test_ids[[i]]

        m_hat <- m_hat_list[[i]]
        g_hat <- g_hat_list[[i]]

        d_k[, i] <- D[test_index]
        v_hat[, i] <- v_hat_se[test_index, ] <- D[test_index] - m_hat
        u_hat[, i]  <- u_hat_se[test_index, ] <- Y[test_index] - g_hat
        v_hatd[, i]  <- v_hatd_se[test_index, ] <- v_hat[, i]*D[test_index]

        orth_est <- orth_plr_dml(u_hat = u_hat[, i] , v_hat = v_hat[, i] ,
                                 v_hatd = v_hatd[, i], 
                                 score = score) #, se_type)
        thetas[i] <- orth_est$theta
    }
    
    theta <- mean(thetas, na.rm = TRUE)
    se <- sqrt(var_plr(theta = theta, d = D, u_hat = u_hat_se, v_hat = v_hat_se,
                        v_hatd = v_hatd_se, score = score, se_type = se_type,
                        dml_procedure = dml_procedure))
    
    t <- theta/se
    pval <-  2 * stats::pnorm(-abs(t))
    
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

    orth_est <- orth_plr_dml(u_hat = u_hat, v_hat = v_hat, v_hatd = v_hatd, score = score)
    
    theta <- orth_est$theta
    se <- sqrt(var_plr(theta = theta, d = D, u_hat = u_hat, v_hat = v_hat,
                        v_hatd = v_hatd, score = score, se_type = se_type, 
                        dml_procedure = dml_procedure))
    
    t <- theta/se
    pval <-  2 * stats::pnorm(-abs(t))
  }

  all_preds = list(m_hat_list = m_hat_list,
                   g_hat_list = g_hat_list,
                   smpls = smpls)

  names(theta) <- names(se) <- d
  res <- list( coefficients = theta, se = se, t = t, pval = pval,
               all_preds = all_preds)
  
  class(res) <- "DML"
  return(res)
}

#' @export
dml_plr_boot <- function(data, y, d, theta, se, all_preds, dml_procedure = "dml2",
                         score = "IV-type", se_type = "ls",
                         weights = weights,  nRep = 500) {
  
  m_hat_list <- all_preds$m_hat_list
  g_hat_list <- all_preds$g_hat_list
  
  smpls <- all_preds$smpls
  train_ids <- smpls$train_ids
  test_ids <- smpls$test_ids
  
  n_iters = length(test_ids)
  n_k <- vapply(test_ids, length, double(1L))
  
  n <- nrow(data)
  D <- data[ , d]
  Y <- data[ , y]
  
  # DML 1
  if ( dml_procedure == "dml1") {
    v_hat <- u_hat <- v_hatd <- d_k <- matrix(NA, nrow = max(n_k), ncol = n_iters)
    
    for (i in 1:n_iters) {
      test_index = test_ids[[i]]
      
      m_hat <- m_hat_list[[i]]
      g_hat <- g_hat_list[[i]]
      
      d_k[, i] <- D[test_index]
      v_hat[, i] <- D[test_index] - m_hat
      u_hat[, i]  <- Y[test_index] - g_hat
      v_hatd[, i]  <- v_hat[, i]*D[test_index]
    }
    
    boot <- bootstrap_plr(theta = theta, d = d_k, u_hat = u_hat, v_hat = v_hat, 
                            v_hatd = v_hatd, score = score, se = se,
                            weights = weights, nRep = nRep)
    boot_theta <- boot$boot_theta
  }
  
  if ( dml_procedure == "dml2") {
    
    v_hat <- u_hat <- v_hatd <- matrix(NA, nrow = n, ncol = 1)
    
    for (i in 1:n_iters){
      test_index = test_ids[[i]]
      
      m_hat = m_hat_list[[i]]
      g_hat = g_hat_list[[i]]
      
      v_hat[test_index, 1] <- D[test_index] - m_hat
      u_hat[test_index, 1] <- Y[test_index] - g_hat
      v_hatd[test_index, 1] <- v_hat[test_index]*D[test_index]
      
    }
    
    boot <- bootstrap_plr(theta = theta, d = D, u_hat = u_hat, v_hat = v_hat, 
                            v_hatd = v_hatd, score = score, se = se,
                          weights = weights, nRep = nRep)
    boot_theta <- boot$boot_theta
  }
  
  return(boot_theta)
}


#' Orthogonalized Estimation of Coefficient in PLR
#'
#' Function to estimate the structural parameter in a partially linear regression model (PLR).
#'
#' @inheritParams var_plr
#' @return List with estimate (\code{theta}).
#' @export
orth_plr_dml <- function(u_hat, v_hat, v_hatd, score) { #, se_type) {

  theta <-  NA

  if (score == "partialling out") {
    res_fit <- stats::lm(u_hat ~ 0 + v_hat)
    theta <- stats::coef(res_fit)
  }

   else if (score == 'IV-type') {
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
var_plr <- function(theta, d, u_hat, v_hat, v_hatd, score, se_type, dml_procedure) {
  
  var <- NA
  
  if (se_type == score){ 
    
    if (score == "partialling out") {
  
    var <- mean( 1/length(u_hat) * 1/(colMeans(v_hat^2, na.rm = TRUE))^2  *
            colMeans( ( (u_hat - v_hat*theta)*v_hat)^2, na.rm = TRUE) )
    }
    
     else if (score == 'IV-type') {
     var <- mean( 1/length(u_hat) * (1/colMeans(v_hatd, na.rm = TRUE))^2  * 
            colMeans( ( (u_hat - d*theta)*v_hat)^2 , na.rm = TRUE))
    }
  
  }
   
  # Q: only for "dml2"?
  if (se_type == "ls" & score == "partialling out" & dml_procedure == "dml2") {
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
bootstrap_plr <- function(theta, d, u_hat, v_hat, v_hatd, score, se, weights, nRep) {
  
  boot_var <- NA
  
 if (score == "partialling out") {

    score <- (u_hat - v_hat*theta)*v_hat
    J <-  -colMeans(v_hat*v_hat, na.rm = TRUE)
    
  }

  else if (score == 'IV-type') {
    score <- (u_hat - d*theta)*v_hat
    J <- -colMeans(v_hatd, na.rm = TRUE)
  }
  
  n <- length(d)
  pertub <- matrix(NA, nrow = 1, ncol = nRep)
  
  if (!is.vector(score)) {
    J <- matrix(rep(J, each=nrow(score)), nrow=nrow(score))
  }
  for (i in seq(nRep)) {
     pertub[1,i] <- mean( colMeans(weights[i,] * 1/se * 1/J * score, na.rm = TRUE))
    
  }
  
  res = list(boot_theta = pertub)
  return(c(res))
}
















