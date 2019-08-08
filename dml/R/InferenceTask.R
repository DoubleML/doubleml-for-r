#' Wrapper Function for Double Machine Learning with Cross-Fitting
#'
#' Implements double machine learning inference with \code{k}-fold cross-fitting (default).
#' @param data Data frame.
#' @param y Name of outcome variable. The variable must be included in \code{data}.
#' @param d Name of treatment variables for which inference should be performed.
#' @param z Name of instrument variable.
#' @param model Inference model to be implemented, e.g. partially linear regression (\code{plr}) (default).
#' @param k Number of folds for \code{k}-fold cross-fitting (default \code{k}=2).
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

  res <- dml_plr(data = data, y = y, d = d, z = z,
                  resampling = resampling, ResampleInstance = ResampleInstance, mlmethod = mlmethod,
                  dml_procedure = dml_procedure, params = params,
                  inf_model = inf_model, se_type = se_type,
                  bootstrap = bootstrap, nRep = nRep, ...)

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
#' @export
confint.InfTask <- function(object, parm, level = 0.95, joint = FALSE){
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
      
    hatc <- quantile(sim, probs = 1 - a)
    
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
print.InfTask <- function(x) {
  
  if (is.na(x$boot_se)) {
   return(list(theta = x$theta , se = x$se))}
  if (!is.na(x$boot_se)) {
    return(list(theta = x$theta, se = x$se, boot_se = x$boot_se))
  }
}

#' Methods for Inference Task
#'
#' Methods for S3 class \code{InfTask}
#'
#' @param x Object of class \code{InfTask}.
coef.InfTask <- function(x) return(x$theta)


