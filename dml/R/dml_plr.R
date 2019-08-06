#' Double Machine Learning for Partially Linear Regression.
#'
#' @param data Data frame.
#' @param y Name of outcome variable. The variable must be included in \code{data}.
#' @param d Name of treatment variables for which inference should be performed.
#' @param resampling Resampling scheme for cross-fitting of class \code{"ResampleDesc"}.
#' @param ResampleInstance (Optional) \code{ResampleInstance} that can be passed through in order to obtain replicable sample splits. By default, \code{ResampleInstance} is set \code{NULL} and \code{resampling} is instantiated internally. Note that \code{ResampleInstance} will override the information in \code{resampling}.
#' @param mlmethod List with classification or regression methods according to naming convention of the \code{mlr} package. Set \code{mlmethod_g} for classification or regression method according to naming convention of the \code{mlr} package for regression of y on X (nuisance part g). Set \code{mlmethod_m} for  classification or regression method for regression of d on X (nuisance part m). A list of available methods is available at \url{https://mlr.mlr-org.com/articles/tutorial/integrated_learners.html}.
#' @param params Hyperparameters to be passed to classification or regression method. Set hyperparameters \code{params_g} for predictions of nuisance part g and \code{params_m} for nuisance m.
#' @param dml_procedure Double machine learning algorithm to be used, either \code{"dml1"} or \code{"dml2"} (default).
#' @param inf_model Inference model for final estimation, default \code{"IV-type"} (...)
#' @param se_type Method to estimate standard errors. Default \code{"ls"} to estimate usual standard error from least squares regression of residuals. Alternatively, specify \code{"IV-type"} or \code{"DML2018"} to obtain standard errors that correspond to the specified \code{inf_model}. The options chosen for \code{inf_model} and \code{se_type} are required to match. 
#' @param bootstrap Choice for implementation of multplier bootstrap, can be set to \code{"none"} (by default), \code{"Bayes"}, \code{"normal"}, \code{"wild"}.
#' @param nRep Number of repetitions for multiplier bootstrap, by default \code{nRep=500}.
#' @param ... further options passed to underlying functions.
#' @return Result object with estimated coefficient and standard errors.
#' @export

dml_plr <- function(data, y, d, resampling = NULL, ResampleInstance = NULL, mlmethod, params = list(params_m = list(),
                    params_g = list()),
                    dml_procedure = "dml2",
                    inf_model = "IV-type", se_type = "ls",
                    bootstrap = "none",  nRep = 500, ...) {

  # function not yet fully implemented (test)
  checkmate::check_class(resampling, "ResampleDesc")
  checkmate::checkDataFrame(data)

  # tbd: ml_method handling: default mlmethod_g = mlmethod_m
  # tbd: parameter passing

  n <- nrow(data)
  theta <- se <- boot_se <- NA

  if (is.null(ResampleInstance)) {
    n_iters <- resampling$iters
    rin <- mlr::makeResampleInstance(resampling, size = nrow(data))
    }

  else {

    if (!is.null(resampling)) {
      message("Options in 'resampling' are overwritten by options specified for 'ResampleInstance'")
    }

    rin <- ResampleInstance
    resampling <- rin$desc
    n_iters <- resampling$iters

    }

  if (se_type != "ls") {
    se_type <- inf_model
  }

  # nuisance g
  g_indx <-  grepl(d, names(data)) == FALSE
  task_g <- mlr::makeRegrTask(data = data[ , g_indx], target = y)
  ml_g <- mlr::makeLearner(mlmethod$mlmethod_g, id = "nuis_g", par.vals = params$params_g)
  r_g <- mlr::resample(learner = ml_g, task = task_g, resampling = rin)
  g_hat_list <- mlr::getRRPredictionList(r_g)
  g_hat_list <- lapply(g_hat_list$test, extract_test_pred)

  # nuisance m
  m_indx <-  grepl(y, names(data)) == FALSE
  task_m  <- mlr::makeRegrTask(data = data[ , m_indx], target = d)
  ml_m <- mlr::makeLearner(mlmethod$mlmethod_m, id = "nuis_m", par.vals = params$params_m)
  r_m <- mlr::resample(learner = ml_m, task = task_m, resampling = rin)
  m_hat_list <- mlr::getRRPredictionList(r_m)
  m_hat_list <-lapply(m_hat_list$test,  extract_test_pred)


  if ((rin$desc$iters != r_g$pred$instance$desc$iters) ||
      (rin$desc$iters != r_m$pred$instance$desc$iters) ||
      !identical(rin$train.inds, r_g$pred$instance$train.inds) ||
      !identical(rin$train.inds, r_m$pred$instance$train.inds)) {
    stop('Resampling instances not equal')
  }

  test_index_list <- rin$test.inds
  n_k <- vapply(test_index_list, length, double(1))

  D <- data[ , d]
  Y <- data[ , y]

  # DML 1
  if ( dml_procedure == "dml1") {
    thetas <- vars <- rep(NA, n_iters)
    
    for (i in 1:n_iters) {
        test_index = test_index_list[[i]]
        m_hat <- m_hat_list[[i]]
        g_hat <- g_hat_list[[i]]

        v_hat <- D[test_index] - m_hat
        u_hat <- Y[test_index] - g_hat
        v_hatd <- v_hat*D[test_index]

        orth_est <- orth_plr_dml(u_hat = u_hat, v_hat = v_hat, v_hatd = v_hatd, inf_model = inf_model) #, se_type)
        thetas[i] <- orth_est$theta
        vars[i] <- n_k[i]/n * var_plr(theta = orth_est$theta, d = D[test_index], u_hat = u_hat, v_hat = v_hat, v_hatd = v_hatd, 
               inf_model = inf_model, se_type = se_type)
    }
    
    theta <- mean(thetas, na.rm = TRUE)
    se <- sqrt(mean(vars, na.rm=T))
  }

  if ( dml_procedure == "dml2") {

    # tbd: what if sample cannot be split in equal sized folds
    # maxlen <- vapply(test_index_list, length, double(1)) # test predictions for each fold k

    # v_hat_mat <- matrix(NA, nrow = maxlen, ncol = n_iters)
    # u_hat_mat <- matrix(NA, nrow = maxlen, ncol = n_iters)
    # v_hatd_mat <- matrix(NA, nrow = maxlen, ncol = n_iters)

    v_hat <- u_hat <- v_hatd <- rep(NA, n)

    for (i in 1:n_iters){

       test_index = test_index_list[[i]]
       m_hat = m_hat_list[[i]]
       g_hat = g_hat_list[[i]]

       v_hat[test_index] <- D[test_index] - m_hat
       u_hat[test_index] <- Y[test_index] - g_hat
       v_hatd[test_index] <- v_hat[test_index]*D[test_index]

    }

    orth_est <- orth_plr_dml(u_hat = u_hat, v_hat = v_hat, v_hatd = v_hatd, inf_model = inf_model)
    theta <- orth_est$theta
  #  se <- sqrt(orth_est$var)
    se <- sqrt(var_plr(theta = theta, d = D, u_hat = u_hat, v_hat = v_hat, v_hatd = v_hatd, 
               inf_model = inf_model, se_type = se_type))
    # boot_se <- multiplier_plr(theta = theta, d = D, u_hat = u_hat, v_hat = v_hat, 
                              # v_hatd = v_hatd, inf_model = inf_model, se = se, 
                              # bootstrap = bootstrap, nRep = nRep)
  }


  names(theta) <- names(se) <- names(boot_se) <- d
  res <- list( theta = theta, se = se, boot_se = boot_se)
  return(res)
}



#' Orthogonalized Estimation of Coefficient in PLR
#'
#' Function to estimate the structural parameter in a partially linear regression model (PLR).
#'
#' @param v_hat Residuals from \eqn{d-m(x)}.
#' @param u_hat Residuals from \eqn{y-g(x)}.
#' @param v_hatd Product \code{v_hat * d}.
#' @param inf_model Method to estimate structural parameter.
#' 
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
#'
#' @param theta final dml estimator for the partially linear model.
#' @param d treatment variable.
#' @param v_hat Residuals from \eqn{d-m(x)}.
#' @param u_hat Residuals from \eqn{y-g(x)}.
#' @param v_hatd Product of \code{v_hat} with \code{d}.
#' @param inf_model Method to estimate structural parameter.
#' @param se_type Method to estimate standard errors. Default \code{"ls"} to estimate usual standard error from least squares regression of residuals. Alternatively, specify \code{"IV-type"} or \code{"DML2018"} to obtain standard errors that correspond to the specified \code{inf_model}. The options chosen for \code{inf_model} and \code{se_type} are required to match. 
#' @return Variance estimator (\code{var}).
var_plr <- function(theta, d, u_hat, v_hat, v_hatd, inf_model, se_type) {
  
  var <- NA
  
  if (se_type == inf_model){ 
    
    if (inf_model == "DML2018") {
  
    var <- 1/length(u_hat) * 1/(mean(v_hat*v_hat))^2  * mean( ( (u_hat - v_hat*theta)*v_hat)^2)
    }
    
     else if (inf_model == 'IV-type') {
     var <-  1/length(u_hat) * (1/mean(v_hatd))^2  * mean( ( (u_hat - d*theta)*v_hat)^2)
    }
  
  }
   
  # Q: only for "dml2"?
  if (se_type == "ls" & inf_model == "DML2018") {
      res_fit <- stats::lm(u_hat ~ 0 + v_hat)
      var <- sandwich::vcovHC(res_fit)
  }
  
  
  return(c(var))
}















