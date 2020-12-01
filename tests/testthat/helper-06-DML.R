#' Wrapper Function for Double Machine Learning with Cross-Fitting
#'
#' Implements double machine learning inference with \code{k}-fold cross-fitting (default).
#' @param data Data frame.
#' @param y Name of outcome variable. The variable must be included in \code{data}.
#' @param d Name of treatment variables for which inference should be performed.
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

DML <- function(data, y, d, model = "plr", k = 2, S = 1, smpls = NULL,
                          mlmethod,
                          dml_procedure = "dml2", params = list(params_m = list(),
                          params_g = list()), score = "IV-type", se_type = "ls", 
                          aggreg_median = TRUE,
                          ...){
  
  p1 <- length(d)
  n <- nrow(data)
  
  res <- rep(list(list()), S)
  theta_s <- se_s <- matrix(NA, nrow = p1, ncol = S)
   
  coefficients <- se <- t <- pval <- boot_se <- rep(NA, p1)
  
  all_preds <- list()
  
  # 
  # if ( length(mlmethod$mlmethod_m) == 1 & p1 > 1) {
  #   mlmethod$mlmethod_m <- rep(mlmethod$mlmethod_m, p1)
  #   message("Only one mlmethod_m provided, assumed to be identical for all coefficients")
  # }
  # 
  
  if ( length(params$params_m) == 1 & p1 > 1) {
    
    params$params_m <- rep(params$params_m, p1)
    message("Only one set of parameters params_m provided, assumed to be identical for all coefficients")
  }
  
  if ( length(params$params_g) == 1 & p1 > 1) {
    
    params$params_g <- rep(list(params$params_g), p1) 
    message("Only one set of parameters params_g provided, assumed to be identical for all coefficients")
  
    }
  
  if ( length(params$params_m) == p1 & is.null(names(params$params_m)) ) {
    names(params$params_m) <- d
  }
  
    if ( length(params$params_g) == p1 & is.null(names(params$params_g)) ) {
    names(params$params_g) <- d
    }
  
  
  if ( length(params$params_g) == 0)  {
  
  params$params_g <- rep(list(params$params_g), p1) 
  
  }
  
  if ( length(params$params_m) == 0)  {
  
  params$params_m <- rep(list(params$params_m), p1) 
  
  }  
  
 # stopifnot(length(params$params_m) == p1)
      
  # 
  #   if ( length(mlmethod$mlmethod_m) == p1 & is.null(names(mlmethod$mlmethod_m))) {
  #   names(mlmethod$mlmethod_m) <- d
  # }
  # 
  
  if (is.null(smpls)) {
    smpls <- lapply(1:S, function(x) sample_splitting(k, data))
  }
  
  for (s in seq(S)){
    this_smpls = smpls[[s]]

    for (j in seq(p1)) {
      
      # task <- list(data, y, d, z, resampling, mlmethod, params, dml_procedure,
                    # score, ...)
      # class(task) <- model

      # tbd: implementation of object orientation -> from here jump to plr, ...
      
      res_j <- dml_plr(data = data, y = y, d = d[j],
                       smpls = this_smpls,  
                    mlmethod = mlmethod, dml_procedure = dml_procedure,
                    params = list(params_m = params$params_m[[j]], params_g = params$params_g[[j]]),
                    score = score, se_type = se_type, ...)
      
      coefficients[j] <- res_j$coefficients
      se[j] <- res_j$se
      t[j] <- res_j$t
      pval[j] <- res_j$pval
      
      all_preds[[j]] <- res_j$all_preds
      
    }
    
  names(coefficients) <- names(se) <- names(t) <- names(pval) <- d

  res[[s]] <- list( coefficients = coefficients, se = se, t = t, pval = pval, 
               samplesize = n, all_preds = all_preds)
  
  }
  
  
  theta_s <- vapply(res, function(x) x$coefficients, double(p1))
  se_s <- vapply(res, function(x) x$se, double(p1))
  
  if (p1 == 1) {
    # make sure to have matrices
    theta_s = matrix(theta_s, nrow = 1)
    se_s = matrix(se_s, nrow = 1)
  }
  
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
  
  all_preds <- lapply(res,function(x) x$all_preds)
    
  names(coefficients) <- names(se) <- names(t) <- names(pval) <- names(boot_se) <- d
  res <- list( coefficients = coefficients, se = se, t = t, pval = pval, 
               samplesize = n, theta_s = theta_s, se_s = se_s,
               all_preds = all_preds)
  
  class(res) <- "DML"
  
  return(res)

}




#' Methods for Inference Task
#'
#' Methods for S3 class \code{DML}
#'
#' @param object Object of class \code{DML}.
#' @param parm a specification of which parameters are to be given confidence intervals among the variables for which inference was done, either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' @param level confidence level required.
#' @param joint logical, if \code{TRUE} joint confidence intervals are calculated.
#' @param ... arguments passed to print function and other methods.
#' @rdname confint.DML
#' @export
confint.DML <- function(object, parm, level = 0.95, joint = FALSE, ...){
  cf <- stats::coef(object)
  
  # if (is.na(object$boot_se)) {
    ses <- object$se
    # }
  
  # if (!is.na(object$boot_se)){
  #   ses <- object$boot_se
  # }
  
  pnames <- names(cf)

  if (missing(parm))
    parm <- pnames else if (is.numeric(parm))
      parm <- pnames[parm]

  # pnames <- names(ses)
  # if (is.matrix(cf))
  #   cf <- setNames(as.vector(cf), pnames)
  # if (missing(parm))
  #   parm <- pnames
  # else if (is.numeric(parm))
  #   parm <- pnames[parm]
 
  if (joint == FALSE) {
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    fac <- stats::qnorm(a)
    pct <- format.perc(a, 3)
    ci <- array(NA_real_, dim = c(length(parm), 2L), dimnames = list(parm,
                                                                     pct))
    ci[] <- cf[parm] + ses %o% fac
    
  }
  
  if (joint == TRUE) {
    
    a <- (1 - level) 
    ab <- c(a/2, 1 - a/2)
    pct <- format.perc(ab, 3) 
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
    
    sim <- apply(abs(object$boot_theta), 2, max)
      
    hatc <- stats::quantile(sim, probs = 1 - a)
    
    ci[, 1] <- cf[parm] - hatc * ses
    ci[, 2] <- cf[parm] + hatc * ses
   }
  return(ci)
}

#' @export
bootstrap.DML <- function(object, data, y, d,
                          dml_procedure, score, se_type,
                          bootstrap = "normal", nRep = 500) {
  
  xx = dim(object$theta_s)
  n_rep = xx[2]
  p1 = length(d)
  
  boot_theta = matrix(NA, nrow = p1, ncol = nRep * n_rep)
  n_obs <- nrow(data)
  for (s in seq(n_rep)){
    if (bootstrap == "Bayes") {
      weights = stats::rexp(nRep * n_obs, rate = 1) - 1
    } else if (bootstrap == "normal") {
      weights = stats::rnorm(nRep * n_obs)
    } else if (bootstrap == "wild") {
      weights = stats::rnorm(nRep * n_obs)/sqrt(2) + (stats::rnorm(nRep * n_obs)^2 - 1)/2
    } else {
      stop("invalid boot method")
    }
    weights = matrix(weights, nrow = nRep, ncol = n_obs, byrow=TRUE)
    for (j in seq(p1)) {
      ind_start <- ((s-1)*nRep+1)
      ind_end <- (s*nRep)
      boot_theta[j, ind_start:ind_end] <- dml_plr_boot(data, y = y, d = d[j],
                                                       theta = object$theta_s[j, s], se = object$se_s[j, s],
                                                       all_preds = object$all_preds[[s]][[j]],
                                                       dml_procedure = dml_procedure,
                                                       score = score, se_type = score,
                                                       weights = weights,  nRep = nRep)
    }
  }
  
  return(boot_theta)
  
}


#' Methods for Inference Task
#'
#' Methods for S3 class \code{DML}
#'
#' @param x Object of class \code{DML}.
#' @param ... arguments passed to print function and other methods.
#' @rdname print.DML
#' @export 
print.DML <- function(x, ...) {
  
  if (all(is.na(x$boot_se))) {
   return(list(coefficients = x$coefficients , se = x$se))}
  
  if (!all(is.na(x$boot_se))) {
    return(list(coefficients = x$coefficients, se = x$se, boot_se = x$boot_se))
  }
}

#' Methods for Inference Task
#'
#' Methods for S3 class \code{DML}
#'
#' @param object an object of class \code{DML}.
#' @inheritParams print.DML
#' @rdname coef.DML
#' @export
coef.DML <- function(object, ...) return(object$coefficients)


#' Summarizing Inference Task
#' 
#' Summary method for class \code{DML}. 
#' 
#' @inheritParams confint.DML
#' @rdname confint.DML
#' @export 
summary.DML <- function(object, ...) {
  ans <- NULL
  k <- length(object$coefficients)
  table <- matrix(NA, ncol = 4, nrow = k)
  rownames(table) <- names(object$coefficients)
  colnames(table) <- c("Estimate.", "Std. Error", "t value", "Pr(>|t|)")
  table[, 1] <- object$coefficients
  table[, 2] <- object$se
  table[, 3] <- object$t
  table[, 4] <- object$pval
  ans$coefficients <- table
  ans$object <- object
  class(ans) <- "summary.DML"
  return(ans)
}

#' Summarizing Inference Task
#' 
#' Summary method for class \code{DML}. 
#' 
#' @param x an object of class \code{summary.DML}, usually a result of a call or \code{summary.DML}
#' @param digits the number of significant digits to use when printing.
#' @param ... arguments passed to print function and other methods.
#' @method print summary.DML
#' @rdname summary.DML
#' @export
print.summary.DML <- function(x, digits = max(3L, getOption("digits") - 
                                                          3L), ...) {
  if (length(coef.DML(x$object))) {
    k <- dim(x$coefficients)[1]
    table <- x$coefficients
    print("Estimates and significance testing of the effect of target variables")
    stats::printCoefmat(table, digits = digits, P.values = TRUE, has.Pvalue = TRUE)
    cat("\n")
  } else {
    cat("No coefficients\n")
  }
  cat("\n")
  invisible(table)
}


