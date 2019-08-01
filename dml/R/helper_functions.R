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
