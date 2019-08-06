#' Wrapper Function for Double Machine Learning with Cross-Fitting
#'
#' Implements double machine learning inference with \code{k}-fold cross-fitting (default).
#' @param data Data frame.
#' @param y Name of outcome variable. The variable must be included in \code{data}.
#' @param d Name of treatment variables for which inference should be performed.
#' @param z Name of instrument variable.
#' @param model Inference model to be implemented, e.g. partially linear regression (\code{plr}) (default).
#' @param k Number of folds for \code{k}-fold cross-fitting (default \code{k}=2).
#' @param resampling (Optional) resampling scheme for cross-fitting of class \code{"ResampleDesc"}, default 2-fold cross-fitting.
#' @param ResampleInstance (Optional) \code{ResampleInstance} that can be passed through in order to obtain replicable sample splits. By default, \code{ResampleInstance} is set \code{NULL} and \code{resampling} is instantiated internally. Note that \code{ResampleInstance} will override the information in \code{resampling}.
#' @param mlmethod List with classification or regression methods according to naming convention of the \code{mlr} package. Set \code{mlmethod_g} for classification or regression method according to naming convention of the \code{mlr} package for regression of y on X (nuisance part g). Set \code{mlmethod_m} for  classification or regression method for regression of d on X (nuisance part m). A list of available methods is available at \url{https://mlr.mlr-org.com/articles/tutorial/integrated_learners.html}.
#' @param params Hyperparameters to be passed to classification or regression method. Set hyperparameters \code{params_g} for predictions of nuisance part g and \code{params_m} for nuisance m.
#' @param dml_procedure Double machine learning algorithm to be used, either \code{"dml1"} or \code{"dml2"} (default).
#' @param inf_model Inference model for final estimation, default \code{"IV-type"} (...)
#' @param se_type Method to estimate standard errors. Default \code{"ls"} to estimate usual standard error from least squares regression of residuals. Alternatively, specify \code{"IV-type"} or \code{"DML2018"} to obtain standard errors that correspond to the specified \code{inf_model}. The options chosen for \code{inf_model} and \code{se_type} are required to match.
#' @param bootstrap Choice for implementation of multplier bootstrap, can be set to \code{"none"} (by default), \code{"Bayes"}, \code{"normal"}, \code{"wild"}.
#' @param nRep Number of repetitions for multiplier bootstrap, by default \code{nRep=500}.
#' @param ... further options passed to underlying functions.
#' @return Result object of class \code{InfTask} with estimated coefficient and standard errors.
#' @export

# Preliminary implementation of Inference task (basic input + output, ignore OOP first)

InferenceTask <- function(data, y, d, z = NULL, model = "plr", k = 2, resampling = NULL,
                          ResampleInstance = NULL, mlmethod,
                          dml_procedure = "dml2", params = list(params_m = list(),
                          params_g = list()), inf_model = "IV-type", se_type = "ls", 
                          bootstrap = "none", nRep = 500, ...){

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
                  bootstrap == bootstrap, nRep = nRep, ...)

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
  ses <- object$se
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

  a <- (1 - level)/2
  a <- c(a, 1 - a)
  fac <- stats::qnorm(a)
  pct <- format.perc(a, 3)
  ci <- array(NA_real_, dim = c(length(parm), 2L), dimnames = list(parm,
                                                                   pct))
  ci[] <- cf[parm] + ses %o% fac

  return(ci)
}

#' Methods for Inference Task
#'
#' Methods for S3 class \code{InfTask}
#'
#' @param x Object of class \code{InfTask}.
print.InfTask <- function(x) return(list(theta = x$theta , se = x$se))

#' Methods for Inference Task
#'
#' Methods for S3 class \code{InfTask}
#'
#' @param x Object of class \code{InfTask}.
coef.InfTask <- function(x) return(x$theta)


