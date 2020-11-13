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
# format.perc <- function (probs, digits) {
#   paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
#         "%" ) }

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


sample_splitting <- function(k, data) {
  
  resampling <- mlr3::ResamplingCV$new()
  resampling$param_set$values$folds <- k
  
  dummy_task = mlr3::Task$new('dummy_resampling', 'regr', data)
  resampling <- resampling$instantiate(dummy_task)
  
  n_iters <- resampling$iters
  train_ids <- lapply(1:n_iters, function(x) resampling$train_set(x))
  test_ids <- lapply(1:n_iters, function(x) resampling$test_set(x))
  
  return(list(train_ids = train_ids, test_ids = test_ids))
}


  