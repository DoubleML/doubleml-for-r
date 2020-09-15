#' Wrapper Function for Double Machine Learning with Cross-Fitting
#'
#' Implements double machine learning inference with \code{k}-fold cross-fitting (default).
#' @param data Data frame.
#' @param y Name of outcome variable. The variable must be included in \code{data}.
#' @param d Name of treatment variables for which inference should be performed.
#' @param z List with names of instrumental variables. For each treatment variable in d, one instrument must be specified.
#' @param model Inference model to be implemented, e.g. partially linear regression (\code{plr}), partially linear instrumental variable regression (\code{plriv}) (default).
#' @param k Number of folds for \code{k}-fold cross-fitting (default \code{k}=2).
#' @param S number of repetitions (default \code{S=1}. 
#' @param aggreg_median logical indicating how target estimators and standard errors should be aggregated for repeated cross-fitting. By default (\code{TRUE}), the median over all estimators as taken as the final estimator. If \code{FALSE}, the mean is calculated over all estimators. 
#' @param params If multiple target coefficients are provided and different mlmethods chosen for each coefficient, the names of the methods are required to match.
#' @param mlmethod If multiple target coefficients are provided and different mlmethods chosen for each coefficient, the names of the methods are required to match. A list of available methods is available at \url{https://mlr.mlr-org.com/articles/tutorial/integrated_learners.html}.
#' @inheritParams dml_plr
#' @return Result object of class \code{DML} with estimated coefficient and standard errors.
#' @export

# Preliminary implementation of Inference task (basic input + output, ignore OOP first)

DMLIV <- function(data, y, d, z = list(), model = "plr", k = 2, S = 1, resampling = NULL,
                          mlmethod,
                          dml_procedure = "dml2", params = list(params_m = list(),
                          params_g = list()), score = "IV-type", se_type = "ls", 
                          bootstrap = "normal", nRep = 500, aggreg_median = TRUE,
                          ...){

  
  # if (is.null(ResampleInstance)) {
  # 
  #   if (is.null(resampling)) {
  #   resampling <-  mlr::makeResampleDesc("CV", iters = k)
  #   }
  # }
  if (is.null(resampling)) {
    resampling <- mlr3::ResamplingCV$new()
    resampling$param_set$values$folds = k
  }
    
  # if(S > 1 & !is.null(ResampleInstance)) {
  #   
  #   message("ResampleInstance is not passed for repeated cross-fitting! Resampling is based on ResampleIncstance$desc.")
  #   resampling <- ResampleInstance$desc
  #   n_iters <- resampling$iters
  #   
  #   ResampleInstance <- NULL
  #   }
  
  p1 <- length(d)
  p2 <- length(z)
  n <- nrow(data)
  
  res <- rep(list(list()), S)
  theta_s <- se_s <- matrix(NA, nrow = p1, ncol = S)
   
  coefficients <- se <- t <- pval <- boot_se <- rep(NA, p1)
  boot_theta_s <- matrix(NA, nrow = p1, ncol = nRep)
  boot_theta <- matrix(NA, nrow = p1, ncol = nRep * S)
  boot_list <- rep(list(), S)
  
  # 
  # if ( length(mlmethod$mlmethod_m) == 1 & p1 > 1) {
  #   mlmethod$mlmethod_m <- rep(mlmethod$mlmethod_m, p1)
  #   message("Only one mlmethod_m provided, assumed to be identical for all coefficients")
  # }
  # 
  
  if ( length(params$params_g) == 1 & p1 > 1) {
    
    params$params_g <- rep(list(params$params_g), p1) 
    message("Only one set of parameters params_g provided, assumed to be identical for all coefficients")
  }
  
    if ( length(params$params_r) == 1 & p1 > 1) {
    
    params$params_r <- rep(list(params$params_r), p1) 
    message("Only one set of parameters params_r provided, assumed to be identical for all coefficients")
    }
    
   if ( length(params$params_m) == 1 & p2 > 1) {
    
    params$params_m <- rep(params$params_m, p2)
    message("Only one set of parameters params_m provided, assumed to be identical for all coefficients")
  }
  
  if ( length(params$params_r) == p1 & is.null(names(params$params_r)) ) {
    names(params$params_r) <- d
  }
  
    if ( length(params$params_g) == p1 & is.null(names(params$params_g)) ) {
    names(params$params_g) <- d
    }
  
    if ( length(params$params_m) == p2 & is.null(names(params$params_m)) ) {
    names(params$params_m) <- z
  }
  
  if ( length(params$params_g) == 0)  {
  
  params$params_g <- rep(list(params$params_g), p1) 
  
  }
  
  if ( length(params$params_r) == 0)  {
  
  params$params_r <- rep(list(params$params_r), p1) 
  
  }
  
  if ( length(params$params_m) == 0)  {
  
  params$params_m <- rep(list(params$params_m), p2) 
  
  }  
  
 # stopifnot(length(params$params_m) == p1)
      
  # 
  #   if ( length(mlmethod$mlmethod_m) == p1 & is.null(names(mlmethod$mlmethod_m))) {
  #   names(mlmethod$mlmethod_m) <- d
  # }
  # 
  
  for (s in seq(S)){
    

    for (j in seq(p1)) {
      
      # task <- list(data, y, d, z, resampling, mlmethod, params, dml_procedure,
                    # score, ...)
      # class(task) <- model

      # tbd: implementation of object orientation -> from here jump to plr, ...
      
      res_j <- dml_plriv(data = data, y = y, d = d[j], z = z[[j]],
                    resampling = resampling,  
                    mlmethod = mlmethod, dml_procedure = dml_procedure,
                    params = list(params_m = params$params_m[[j]], params_g = params$params_g[[j]], 
                                  params_r = params$params_r[[j]]),
                    score = score, se_type = se_type,
                    bootstrap = bootstrap, nRep = nRep, ...)
      
      coefficients[j] <- res_j$coefficients
      se[j] <- res_j$se
      t[j] <- res_j$t
      pval[j] <- res_j$pval
      boot_se[j] <- res_j$boot_se
      boot_theta_s[j,] <- res_j$boot_theta
      
    }
    
  names(coefficients) <- names(se) <- names(t) <- names(pval) <- 
    names(boot_se) <- rownames(boot_theta_s) <- d

  res[[s]] <- list( coefficients = coefficients, se = se, t = t, pval = pval, 
               boot_se = boot_se, boot_theta = boot_theta_s, samplesize = n)
  
  }
  
  
  theta_s <- vapply(res, function(x) x$coefficients, double(p1))
  se_s <- vapply(res, function(x) x$se, double(p1))
  
  if (S > 1) {
    if (aggreg_median) {
      
      coefficients <- apply(theta_s, 1, stats::median) 
      
    }
    
    if (!aggreg_median) {
      
      coefficients <- rowMeans(theta_s)
      
    }
    
    se <- apply(se_s, 1, function(x) se_repeated(x, coefficients, theta_s, aggreg_median))
  }
  
  t <- coefficients/se
  pval <- 2 * stats::pnorm(-abs(t))
  
  boot_list <- lapply(res, function(x) x$boot_theta)
  boot_theta <- matrix(unlist(boot_list), ncol = nRep * S)
    
  names(coefficients) <- names(se) <- names(t) <- names(pval) <- names(boot_se) <- rownames(boot_theta) <- d
  res <- list( coefficients = coefficients, se = se, t = t, pval = pval, 
               boot_se = boot_se, boot_theta = boot_theta, samplesize = n,
               theta_s = theta_s, se_s = se_s)
  
  class(res) <- "DML"
  
  return(res)

}

# Methods are the same as for wrapper DML

#' 
#' #' Methods for Inference Task
#' #'
#' #' Methods for S3 class \code{DML}
#' #'
#' #' @param object Object of class \code{DML}.
#' #' @param parm a specification of which parameters are to be given confidence intervals among the variables for which inference was done, either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' #' @param level confidence level required.
#' #' @param joint logical, if \code{TRUE} joint confidence intervals are calculated.
#' #' @param ... arguments passed to print function and other methods.
#' #' @rdname confint.DML
#' #' @export
#' confint.DML <- function(object, parm, level = 0.95, joint = FALSE, ...){
#'   cf <- stats::coef(object)
#'   
#'   # if (is.na(object$boot_se)) {
#'     ses <- object$se
#'     # }
#'   
#'   # if (!is.na(object$boot_se)){
#'   #   ses <- object$boot_se
#'   # }
#'   
#'   pnames <- names(cf)
#' 
#'   if (missing(parm))
#'     parm <- pnames else if (is.numeric(parm))
#'       parm <- pnames[parm]
#' 
#'   # pnames <- names(ses)
#'   # if (is.matrix(cf))
#'   #   cf <- setNames(as.vector(cf), pnames)
#'   # if (missing(parm))
#'   #   parm <- pnames
#'   # else if (is.numeric(parm))
#'   #   parm <- pnames[parm]
#'  
#'   if (joint == FALSE) {
#'     a <- (1 - level)/2
#'     a <- c(a, 1 - a)
#'     fac <- stats::qnorm(a)
#'     pct <- format.perc(a, 3)
#'     ci <- array(NA_real_, dim = c(length(parm), 2L), dimnames = list(parm,
#'                                                                      pct))
#'     ci[] <- cf[parm] + ses %o% fac
#'     
#'   }
#'   
#'   if (joint == TRUE) {
#'     
#'     a <- (1 - level) 
#'     ab <- c(a/2, 1 - a/2)
#'     pct <- format.perc(ab, 3) 
#'     ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
#'     
#'     sim <- apply(abs(object$boot_theta), 2, max)
#'       
#'     hatc <- stats::quantile(sim, probs = 1 - a)
#'     
#'     ci[, 1] <- cf[parm] - hatc * ses
#'     ci[, 2] <- cf[parm] + hatc * ses
#'    }
#'   return(ci)
#' }
#' 
#' #' Methods for Inference Task
#' #'
#' #' Methods for S3 class \code{DML}
#' #'
#' #' @param x Object of class \code{DML}.
#' #' @param ... arguments passed to print function and other methods.
#' #' @rdname print.DML
#' #' @export 
#' print.DML <- function(x, ...) {
#'   
#'   if (all(is.na(x$boot_se))) {
#'    return(list(coefficients = x$coefficients , se = x$se))}
#'   
#'   if (!all(is.na(x$boot_se))) {
#'     return(list(coefficients = x$coefficients, se = x$se, boot_se = x$boot_se))
#'   }
#' }
#' 
#' #' Methods for Inference Task
#' #'
#' #' Methods for S3 class \code{DML}
#' #'
#' #' @param object an object of class \code{DML}.
#' #' @inheritParams print.DML
#' #' @rdname coef.DML
#' #' @export
#' coef.DML <- function(object, ...) return(object$coefficients)
#' 
#' 
#' #' Summarizing Inference Task
#' #' 
#' #' Summary method for class \code{DML}. 
#' #' 
#' #' @inheritParams confint.DML
#' #' @rdname confint.DML
#' #' @export 
#' summary.DML <- function(object, ...) {
#'   ans <- NULL
#'   k <- length(object$coefficients)
#'   table <- matrix(NA, ncol = 4, nrow = k)
#'   rownames(table) <- names(object$coefficients)
#'   colnames(table) <- c("Estimate.", "Std. Error", "t value", "Pr(>|t|)")
#'   table[, 1] <- object$coefficients
#'   table[, 2] <- object$se
#'   table[, 3] <- object$t
#'   table[, 4] <- object$pval
#'   ans$coefficients <- table
#'   ans$object <- object
#'   class(ans) <- "summary.DML"
#'   return(ans)
#' }
#' 
#' #' Summarizing Inference Task
#' #' 
#' #' Summary method for class \code{DML}. 
#' #' 
#' #' @param x an object of class \code{summary.DML}, usually a result of a call or \code{summary.DML}
#' #' @param digits the number of significant digits to use when printing.
#' #' @param ... arguments passed to print function and other methods.
#' #' @method print summary.DML
#' #' @rdname summary.DML
#' #' @export
#' print.summary.DML <- function(x, digits = max(3L, getOption("digits") - 
#'                                                           3L), ...) {
#'   if (length(coef.DML(x$object))) {
#'     k <- dim(x$coefficients)[1]
#'     table <- x$coefficients
#'     print("Estimates and significance testing of the effect of target variables")
#'     stats::printCoefmat(table, digits = digits, P.values = TRUE, has.Pvalue = TRUE)
#'     cat("\n")
#'   } else {
#'     cat("No coefficients\n")
#'   }
#'   cat("\n")
#'   invisible(table)
#' }
#' 

