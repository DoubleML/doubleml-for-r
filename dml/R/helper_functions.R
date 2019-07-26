#' Helper Functions
#'
#' Functions needed in package code.
#'
#' @param x some object.
#' @param probs probability.
#' @param digits number of digits.
# Function to extract test predictions
extract_test_pred <- function(x) {
  pred <- x$data$response
  return(pred)
}

#' @describeIn extract_test_pred  Formating required in confint.
format.perc <- function (probs, digits) {
  paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
        "%" ) }
