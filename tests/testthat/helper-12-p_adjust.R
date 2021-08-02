#' Multiple Testing Adjustment of p-values for S3 objects \code{DML}
#'
#' Multiple hypotheses testing adjustment of p-values for double machine learning.
#'
#' Multiple testing adjustment is performed for S3 objects of class
#' \code{DML}. Implemented methods for multiple testing
#' adjustment are Romano-Wolf stepdown '\code{RW}' (default) and the adjustment
#' methods available in the \code{p.adjust} function of the \code{stats} package,
#' including the Bonferroni, Bonferroni-Holm, and Benjamini-Hochberg corrections,
#' see \code{\link{p.adjust.methods}}.
#'
#' Objects of class \code{DML} are constructed by
#' \code{\link{DML}}.
#'
#' @param x an object of S3 class \code{DML}.
#' @param method the method of p-value adjustment for multiple testing.
#'  Romano-Wolf stepdown ('\code{RW}') is chosen by default.
#' @param ... further arguments passed on to methods.
#' @rdname p_adjust
#' @aliases p_adjust.DML
#' @return A matrix with the estimated coefficients and the p-values that are
#'  adjusted according to the specified method.
#' @references J.P. Romano, M. Wolf (2005). Exact and approximate stepdown
#'  methods for multiple hypothesis testing. Journal of the American Statistical
#'  Association, 100(469), 94-108.
#' @references J.P. Romano, M. Wolf (2016). Efficient computation of adjusted
#'  p-values for resampling-based stepdown multiple testing. Statistics and
#'  Probability Letters, (113), 38-40.
#'
#' @export

p_adjust = function(x, ...) {
  UseMethod("p_adjust")
}


#' @describeIn p_adjust
#' @export
#'
p_adjust.DML = function(x, method = "RW", ...) {

  checkmate::checkClass(x, "DML")
  checkmate::checkChoice(method, c("RW", stats::p.adjust.methods))

  if (all(is.na(x$boot_theta))) {
    message("Note: Multiplier bootstrap is not active in DML and cannot be used
    for p-value adjustment.")
  }

  # n = x$samplesize
  B = ncol(x$boot_theta)
  k = length(x$coefficients)
  cf = x$coefficients
  se = x$se
  n = x$samplesize

  pinit = corr.padj = pval = vector(mode = "numeric", length = k)

  if (is.element(method, stats::p.adjust.methods)) {
    pval = stats::p.adjust(x$pval, method = method, n = k)
  }

  if (method == "RW") {


    # e = x$residuals$e
    # v = x$residuals$v
    # ev = e * v
    # Ev2 = colMeans(v^2)
    # Omegahat = matrix(NA_real_, ncol = k, nrow = k)
    # for (j in 1:k) {
    #   for (l in 1:k) {
    #     Omegahat[j, l] = Omegahat[l, j] = 1/(Ev2[j] * Ev2[l]) * mean(ev[, j] * ev[, l])
    #   }
    # }
    # se = sqrt(diag(Omegahat))
    #
    # Beta_i = matrix(NA_real_, ncol = k, nrow = B)
    # for (i in 1:B) {
    #   Beta_i[i, ] = MASS::mvrnorm(mu = rep(0, k), Sigma = Omegahat/n)
    # }

    tstats = cf / se
    stepdown.index = order(abs(tstats), decreasing = TRUE)
    ro = order(stepdown.index)
    Beta_i = x$boot_theta

    for (s in 1:k) {
      if (s == 1) {
        sim = apply(abs(Beta_i), 2, max)
        pinit[s] = pmin(1, (sum(sim >= abs(tstats[stepdown.index][s]))) / B)
      }
      if (s > 1) {
        sim = apply(abs(Beta_i[-stepdown.index[1:(s - 1)], , drop = F]), 2, max)

        pinit[s] = pmin(1, (sum(sim >= abs(tstats[stepdown.index][s]))) / B)
      }

      for (j in 1:k) {
        if (j == 1) {
          corr.padj[j] = pinit[j]
        }

        if (j > 1) {
          corr.padj[j] = max(pinit[j], corr.padj[j - 1])
        }
      }
      pval = corr.padj[ro]

    }
  }

  res = as.matrix(cbind(cf, pval))
  colnames(res) = c("Estimate.", "pval")

  return(res)
}
