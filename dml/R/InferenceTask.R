#' Wrapper Function for Double Machine Learning with Cross-Fitting
#'
#' Implements double machine learning inference with \code{k}-fold cross-fitting (default).
#' @param data Data frame.
#' @param y Name of outcome variable. The variable must be included in \code{data}.
#' @param d Name of treatment variables for which inference should be performed.
#' @param z Name of instrument variable.
#' @param model Inference model to be implemented, e.g. partially linear regression (\code{plr}) (default).
#' @param k Number of folds for \code{k}-fold cross-fitting (default \code{k}=2).
#' @param mlmethod List with classification or regression methods according to naming convention of the \code{mlr} package. Set \code{mlmethod_g} for classification or regression method according to naming convention of the \code{mlr} package for regression of y on X (nuisance part g). Set \code{mlmethod_m} for  classification or regression method for regression of d on X (nuisance part m). If multiple target coefficients are provided and different mlmethods chosen for each coefficient, the names of the methods are required to match. A list of available methods is available at \url{https://mlr.mlr-org.com/articles/tutorial/integrated_learners.html}.
#' @param params Hyperparameters to be passed to classification or regression method. Set hyperparameters \code{params_g} for predictions of nuisance part g and \code{params_m} for nuisance m. If multiple target coefficients are provided the names of the lists with hyperparameters and the target coefficients must match.
#' @inheritParams dml_plr
#' @return Result object of class \code{InfTask} with estimated coefficient and standard errors.
#' @export

# Preliminary implementation of Inference task (basic input + output, ignore OOP first)

InferenceTask <- function(data, y, d, z = NULL, model = "plr", k = 2, resampling = NULL,
                          ResampleInstance = NULL, mlmethod,
                          dml_procedure = "dml2", params = list(params_m = list(),
                          params_g = list()), inf_model = "IV-type", se_type = "ls", 
                          bootstrap = "normal", nRep = 500, ...){

  if (is.null(ResampleInstance)) {

    if (is.null(resampling)) {
    resampling <-  mlr::makeResampleDesc("CV", iters = k)
    }

  }

  # tbd: Thoroughly implement repeated CV (check that predictions are extracted correctly, ...)
  # rdesc <- mlr_resamplings$get("repeated_cv", param_vals = list(repeats = CV_reps,
  #                                                               folds = k))
  #
  # task <- list(data, y, d, z, resampling, mlmethod, params, dml_procedure,
                    # inf_model, ...)
  # class(task) <- model

  # tbd: implementation of object orientation -> from here jump to plr, ...

  p1 <- length(d)
  n <- nrow(data)
  
  coefficients <- se <- t <- pval <- boot_se <- rep(NA, p1)
  boot_theta <- matrix(NA, nrow = p1, ncol = nRep)
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
    
    params$params_g <- rep(params$params_g, p1)
    message("Only one set of parameters params_g provided, assumed to be identical for all coefficients")
  
    }
  
  if ( length(params$params_m) == p1 & is.null(names(params$params_m)) ) {
    names(params$params_m) <- d
  }
  
  stopifnot(length(params$params_m) == p1)
      
  # 
  #   if ( length(mlmethod$mlmethod_m) == p1 & is.null(names(mlmethod$mlmethod_m))) {
  #   names(mlmethod$mlmethod_m) <- d
  # }
  # 
  
  for (j in seq(p1)) {
    d_j <- d[j]
  
    res_j <- dml_plr(data = data, y = y, d = d_j, z = z,
                  resampling = resampling, ResampleInstance = ResampleInstance, 
                  mlmethod = mlmethod, 
                  params = list(params_m = params$params_m[[j]], params_g = params$params_g[j]),
                  inf_model = inf_model, se_type = se_type,
                  bootstrap = bootstrap, nRep = nRep, ...)
    
    coefficients[j] <- res_j$theta
    se[j] <- res_j$se
    t[j] <- res_j$t
    pval[j] <- res_j$pval
    boot_se[j] <- res_j$boot_se
    boot_theta[j,] <- res_j$boot_theta
  }
  
  names(coefficients) <- names(se) <- names(t) <- names(pval) <- names(boot_se) <- rownames(boot_theta) <- d
  res <- list( coefficients = coefficients, se = se, t = t, pval = pval, 
               boot_se = boot_se, boot_theta = boot_theta, samplesize = n)
  
  class(res) <- "InfTask"

  return(res)

}

#' Methods for Inference Task
#'
#' Methods for S3 class \code{InfTask}
#'
#' @param object Object of class \code{InfTask}.
#' @param parm a specification of which parameters are to be given confidence intervals among the variables for which inference was done, either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' @param level confidence level required.
#' @param joint logical, if \code{TRUE} joint confidence intervals are calculated.
#' @param ... arguments passed to print function and other methods.
#' @rdname confint.InfTask
#' @export
confint.InfTask <- function(object, parm, level = 0.95, joint = FALSE, ...){
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

#' Methods for Inference Task
#'
#' Methods for S3 class \code{InfTask}
#'
#' @param x Object of class \code{InfTask}.
#' @param ... arguments passed to print function and other methods.
#' @rdname print.InfTask
#' @export 
print.InfTask <- function(x, ...) {
  
  if (all(is.na(x$boot_se))) {
   return(list(coefficients = x$coefficients , se = x$se))}
  
  if (!all(is.na(x$boot_se))) {
    return(list(coefficients = x$coefficients, se = x$se, boot_se = x$boot_se))
  }
}

#' Methods for Inference Task
#'
#' Methods for S3 class \code{InfTask}
#'
#' @param object an object of class \code{InfTask}.
#' @inheritParams print.InfTask
#' @rdname coef.InfTask
#' @export
coef.InfTask <- function(object, ...) return(object$coefficients)


#' Summarizing Inference Task
#' 
#' Summary method for class \code{InfTask}. 
#' 
#' @inheritParams confint.InfTask
#' @rdname confint.InfTask
#' @export 
summary.InfTask <- function(object, ...) {
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
  class(ans) <- "summary.InfTask"
  return(ans)
}

#' Summarizing Inference Task
#' 
#' Summary method for class \code{InfTask}. 
#' 
#' @param x an object of class \code{summary.InfTask}, usually a result of a call or \code{summary.InfTask}
#' @param digits the number of significant digits to use when printing.
#' @param ... arguments passed to print function and other methods.
#' @method print summary.InfTask
#' @rdname summary.InfTask
#' @export
print.summary.InfTask <- function(x, digits = max(3L, getOption("digits") - 
                                                          3L), ...) {
  if (length(coef.InfTask(x$object))) {
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


