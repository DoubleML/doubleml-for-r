#' Helper Functions
#'
#' Functions needed in package code.
#'
#' @param x some object.
#' @name helpers
NULL


#' @rdname helpers
extract_test_pred <- function(x) {
  pred <- x$data$response
  return(pred)
}

#' @param probs probability.
#' @param digits number of digits.
#' @rdname helpers 
format.perc <- function (probs, digits) {
  paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
        "%" ) }

#' @param se_s Standard errors from repeated cross-fitting.
#' @param coefficients Final estimators aggregated over all repetitions. 
#' @param theta_s Estimators from all repetitions.
#' @inheritParams DML
#' @rdname helpers 
se_repeated <- function(se_s, coefficients, theta_s, aggreg_median) {
  
  if (aggreg_median) {
      se <- sqrt(stats::median(se_s^2  - (theta_s - coefficients)^2))
  }
  
  if (!aggreg_median) {
      se <- sqrt(mean(se_s^2  - (theta_s - coefficients)^2))
  }
    
  return(se)
}


sample_splitting <- function(k, resampling, data) {
  
  # function not yet fully implemented (test)
  if (!is.null(resampling)) {
    checkmate::check_class(resampling, "ResamplingCV")
  }
  
  if (is.null(resampling)) {
    resampling <- mlr3::ResamplingCV$new()
    resampling$param_set$values$folds <- k
  }
  
  # tbd: handling of resampling 
  if (!resampling$is_instantiated) {
    resampling_scheme <- resampling$clone()
    dummy_task = Task$new('dummy_resampling', 'regr', data)
    resampling_scheme <- resampling_scheme$instantiate(dummy_task)
  }
  
  if (!is.null(resampling) & resampling$is_instantiated) {
    # skip re-instantiation in case of a ResamplingCustom object that was already instatiated (see also multi-treatment unit test)
    if (resampling$id == 'custom'){
      resampling_scheme = resampling
    } else {
      resampling_scheme <- mlr3::ResamplingCV$new()
      resampling_scheme$param_set$values$folds <- resampling$iters
      message("Specified 'resampling' was instantiated. New resampling scheme was instantiated internally.")
    }
  } # tbd: else 
  
  n_iters <- resampling_scheme$iters
  train_ids <- lapply(1:n_iters, function(x) resampling_scheme$train_set(x))
  test_ids <- lapply(1:n_iters, function(x) resampling_scheme$test_set(x))
  
  return(list(train_ids = train_ids, test_ids = test_ids))
}


  