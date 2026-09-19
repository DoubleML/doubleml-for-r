# Survival Analysis Inference Functions
#' @importFrom foreach foreach %dopar%
NULL

# Influence functions centered at the estimate and scaled by its standard
# error on grid columns tl:tu, the input of the multiplier bootstrap
scaled_ueif = function(ueif, estimate, se, tl, tu) {
  k = tu - tl + 1
  (ueif[, tl:tu] - matrix(estimate[tl:tu], nrow = nrow(ueif), ncol = k, byrow = TRUE)) /
    matrix(se[tl:tu], nrow = nrow(ueif), ncol = k, byrow = TRUE)
}

# These functions handle confidence intervals and confidence bands for survival outcomes
# Based on the inference patterns from the original implementation

#' Survival Probability Inference
#'
#' Calculate confidence intervals and simultaneous confidence bands for survival probability
#'
#' @param npath Number of bootstrap paths for confidence bands
#' @param ueif.a1.list List of K UEIF matrices for treatment a=1 (one per fold)
#' @param ueif.a0.list List of K UEIF matrices for treatment a=0 (one per fold)
#' @param time.list List of K time grid vectors (one per fold)
#' @return Data frame with point estimates, confidence intervals, and confidence bands
#' @noRd
survival_prob_inference = function(npath, ueif.a1.list, ueif.a0.list, time.list) {
  ## merge K-fold UEIFs onto union time grid
  res.a0 = merge_kfold_ueif(ueif.a0.list, time.list)
  ueif.a0 = res.a0$ueif
  union.time = res.a0$union.time

  res.a1 = merge_kfold_ueif(ueif.a1.list, time.list)
  ueif.a1 = res.a1$ueif

  n = nrow(ueif.a0)

  ## point estimate
  estimate.a0.cummin = data.frame(t = union.time, estimate.a0 = cummin(colMeans(ueif.a0)))
  ueif.df = estimate.a0.cummin

  estimate.a1.cummin = data.frame(t = union.time, estimate.a1 = cummin(colMeans(ueif.a1)))
  ueif.df = merge(ueif.df, estimate.a1.cummin, all = TRUE, by = "t")

  # diff
  ueif.df = approxTime(ueif.df, xout = ueif.df$t, rule = 2)
  ueif.df$estimate.diff = ueif.df$estimate.a1 - ueif.df$estimate.a0

  ## standard error
  ueif.df$estimate.a0.se = ueif_se(ueif.a0, ueif.df$estimate.a0)
  ueif.df$estimate.a1.se = ueif_se(ueif.a1, ueif.df$estimate.a1)
  ueif.df$estimate.diff.se = ueif_se(ueif.a1 - ueif.a0, ueif.df$estimate.diff)

  ueif.df$estimate.a0.lower.ci = ueif.df$estimate.a0 + qnorm(0.025) * ueif.df$estimate.a0.se
  ueif.df$estimate.a0.upper.ci = ueif.df$estimate.a0 + qnorm(0.975) * ueif.df$estimate.a0.se
  ueif.df$estimate.a1.lower.ci = ueif.df$estimate.a1 + qnorm(0.025) * ueif.df$estimate.a1.se
  ueif.df$estimate.a1.upper.ci = ueif.df$estimate.a1 + qnorm(0.975) * ueif.df$estimate.a1.se
  ueif.df$estimate.diff.lower.ci = ueif.df$estimate.diff + qnorm(0.025) * ueif.df$estimate.diff.se
  ueif.df$estimate.diff.upper.ci = ueif.df$estimate.diff + qnorm(0.975) * ueif.df$estimate.diff.se

  ## confidence bands
  tu = round(0.99 * nrow(ueif.df)) # row index
  # a=0
  tl = ifelse(sum(ueif.df$estimate.a0.se == 0), max(which(ueif.df$estimate.a0.se == 0)) + 1, 1)
  ceif.a0 = scaled_ueif(ueif.a0, ueif.df$estimate.a0, ueif.df$estimate.a0.se, tl, tu)
  c.a0 = quantile(foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(rnorm(n) / sqrt(n)) %*% ceif.a0))
  }, probs = 0.95)
  ueif.df$estimate.a0.lower.cb = ueif.df$estimate.a0 - ueif.df$estimate.a0.se * c.a0 / sqrt(n)
  ueif.df$estimate.a0.upper.cb = ueif.df$estimate.a0 + ueif.df$estimate.a0.se * c.a0 / sqrt(n)

  # a=1
  tl = ifelse(sum(ueif.df$estimate.a1.se == 0), max(which(ueif.df$estimate.a1.se == 0)) + 1, 1)
  ceif.a1 = scaled_ueif(ueif.a1, ueif.df$estimate.a1, ueif.df$estimate.a1.se, tl, tu)
  c.a1 = quantile(foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(rnorm(n) / sqrt(n)) %*% ceif.a1))
  }, probs = 0.95)
  ueif.df$estimate.a1.lower.cb = ueif.df$estimate.a1 - ueif.df$estimate.a1.se * c.a1 / sqrt(n)
  ueif.df$estimate.a1.upper.cb = ueif.df$estimate.a1 + ueif.df$estimate.a1.se * c.a1 / sqrt(n)

  # diff
  tl = ifelse(sum(ueif.df$estimate.diff.se == 0), max(which(ueif.df$estimate.diff.se == 0)) + 1, 1)
  ueif.diff = ueif.a1 - ueif.a0
  ceif.diff = scaled_ueif(ueif.diff, ueif.df$estimate.diff, ueif.df$estimate.diff.se, tl, tu)
  c.diff = quantile(foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(rnorm(n) / sqrt(n)) %*% ceif.diff))
  }, probs = 0.95)
  ueif.df$estimate.diff.lower.cb = ueif.df$estimate.diff -
    ueif.df$estimate.diff.se * c.diff / sqrt(n)
  ueif.df$estimate.diff.upper.cb = ueif.df$estimate.diff +
    ueif.df$estimate.diff.se * c.diff / sqrt(n)
  return(ueif.df)
}

#' Restricted Mean Time Lost Inference
#'
#' Calculate confidence intervals and simultaneous confidence bands for RMTL
#'
#' @param npath Number of bootstrap paths for confidence bands
#' @param ueif.a1.list List of K UEIF matrices for treatment a=1 (one per fold)
#' @param ueif.a0.list List of K UEIF matrices for treatment a=0 (one per fold)
#' @param time.list List of K time grid vectors (one per fold)
#' @return Data frame with point estimates, confidence intervals, and confidence bands
#' @noRd
rmtlj_inference = function(npath, ueif.a1.list, ueif.a0.list, time.list) {
  ## merge K-fold UEIFs onto union time grid
  res.a0 = merge_kfold_ueif(ueif.a0.list, time.list)
  ueif.a0 = res.a0$ueif
  union.time = res.a0$union.time

  res.a1 = merge_kfold_ueif(ueif.a1.list, time.list)
  ueif.a1 = res.a1$ueif

  n = nrow(ueif.a0)

  ## point estimate
  # a=0
  estimate.a0.gcm = monotone_hull(ueif.a0, union.time, "gcm")
  names(estimate.a0.gcm) = c("t", "estimate.a0")
  ueif.df = merge(data.frame(t = union.time), estimate.a0.gcm, all = TRUE, by = "t")

  # a=1
  estimate.a1.gcm = monotone_hull(ueif.a1, union.time, "gcm")
  names(estimate.a1.gcm) = c("t", "estimate.a1")
  ueif.df = merge(ueif.df, estimate.a1.gcm, all = TRUE, by = "t")

  # diff
  ueif.df = approxTime(ueif.df, xout = ueif.df$t, rule = 2)
  ueif.df$estimate.diff = ueif.df$estimate.a1 - ueif.df$estimate.a0

  ## standard error
  ueif.df$estimate.a0.se = ueif_se(ueif.a0, ueif.df$estimate.a0)
  ueif.df$estimate.a1.se = ueif_se(ueif.a1, ueif.df$estimate.a1)
  ueif.df$estimate.diff.se = ueif_se(ueif.a1 - ueif.a0, ueif.df$estimate.diff)

  ueif.df$estimate.a0.lower.ci = ueif.df$estimate.a0 + qnorm(0.025) * ueif.df$estimate.a0.se
  ueif.df$estimate.a0.upper.ci = ueif.df$estimate.a0 + qnorm(0.975) * ueif.df$estimate.a0.se
  ueif.df$estimate.a1.lower.ci = ueif.df$estimate.a1 + qnorm(0.025) * ueif.df$estimate.a1.se
  ueif.df$estimate.a1.upper.ci = ueif.df$estimate.a1 + qnorm(0.975) * ueif.df$estimate.a1.se
  ueif.df$estimate.diff.lower.ci = ueif.df$estimate.diff + qnorm(0.025) * ueif.df$estimate.diff.se
  ueif.df$estimate.diff.upper.ci = ueif.df$estimate.diff + qnorm(0.975) * ueif.df$estimate.diff.se

  ## confidence bands
  tu = round(0.99 * nrow(ueif.df)) # row index
  # a=0
  tl = ifelse(sum(ueif.df$estimate.a0.se == 0), max(which(ueif.df$estimate.a0.se == 0)) + 1, 1)
  ceif.a0 = scaled_ueif(ueif.a0, ueif.df$estimate.a0, ueif.df$estimate.a0.se, tl, tu)
  c.a0 = quantile(foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(rnorm(n) / sqrt(n)) %*% ceif.a0))
  }, probs = 0.95)
  ueif.df$estimate.a0.lower.cb = ueif.df$estimate.a0 - ueif.df$estimate.a0.se * c.a0 / sqrt(n)
  ueif.df$estimate.a0.upper.cb = ueif.df$estimate.a0 + ueif.df$estimate.a0.se * c.a0 / sqrt(n)

  # a=1
  tl = ifelse(sum(ueif.df$estimate.a1.se == 0), max(which(ueif.df$estimate.a1.se == 0)) + 1, 1)
  ceif.a1 = scaled_ueif(ueif.a1, ueif.df$estimate.a1, ueif.df$estimate.a1.se, tl, tu)
  c.a1 = quantile(foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(rnorm(n) / sqrt(n)) %*% ceif.a1))
  }, probs = 0.95)
  ueif.df$estimate.a1.lower.cb = ueif.df$estimate.a1 - ueif.df$estimate.a1.se * c.a1 / sqrt(n)
  ueif.df$estimate.a1.upper.cb = ueif.df$estimate.a1 + ueif.df$estimate.a1.se * c.a1 / sqrt(n)

  # diff
  tl = ifelse(sum(ueif.df$estimate.diff.se == 0), max(which(ueif.df$estimate.diff.se == 0)) + 1, 1)
  ueif.diff = ueif.a1 - ueif.a0
  ceif.diff = scaled_ueif(ueif.diff, ueif.df$estimate.diff, ueif.df$estimate.diff.se, tl, tu)
  c.diff = quantile(foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(rnorm(n) / sqrt(n)) %*% ceif.diff))
  }, probs = 0.95)
  ueif.df$estimate.diff.lower.cb = ueif.df$estimate.diff -
    ueif.df$estimate.diff.se * c.diff / sqrt(n)
  ueif.df$estimate.diff.upper.cb = ueif.df$estimate.diff +
    ueif.df$estimate.diff.se * c.diff / sqrt(n)
  return(ueif.df)
}

#' Cumulative Incidence Function Inference
#'
#' Calculate confidence intervals and simultaneous confidence bands for CIF
#'
#' @param npath Number of bootstrap paths for confidence bands
#' @param ueif.a1.list List of K UEIF matrices for treatment a=1 (one per fold)
#' @param ueif.a0.list List of K UEIF matrices for treatment a=0 (one per fold)
#' @param time.list List of K time grid vectors (one per fold)
#' @return Data frame with point estimates, confidence intervals, and confidence bands
#' @noRd
cifj_inference = function(npath, ueif.a1.list, ueif.a0.list, time.list) {
  ## merge K-fold UEIFs onto union time grid
  res.a0 = merge_kfold_ueif(ueif.a0.list, time.list)
  ueif.a0 = res.a0$ueif
  union.time = res.a0$union.time

  res.a1 = merge_kfold_ueif(ueif.a1.list, time.list)
  ueif.a1 = res.a1$ueif

  n = nrow(ueif.a0)

  ## point estimate
  estimate.a0.cummax = data.frame(t = union.time, estimate.a0 = cummax(colMeans(ueif.a0)))
  ueif.df = estimate.a0.cummax

  estimate.a1.cummax = data.frame(t = union.time, estimate.a1 = cummax(colMeans(ueif.a1)))
  ueif.df = merge(ueif.df, estimate.a1.cummax, all = TRUE, by = "t")

  # diff
  ueif.df = approxTime(ueif.df, xout = ueif.df$t, rule = 2)
  ueif.df$estimate.diff = ueif.df$estimate.a1 - ueif.df$estimate.a0

  ## standard error
  ueif.df$estimate.a0.se = ueif_se(ueif.a0, ueif.df$estimate.a0)
  ueif.df$estimate.a1.se = ueif_se(ueif.a1, ueif.df$estimate.a1)
  ueif.df$estimate.diff.se = ueif_se(ueif.a1 - ueif.a0, ueif.df$estimate.diff)

  ueif.df$estimate.a0.lower.ci = ueif.df$estimate.a0 + qnorm(0.025) * ueif.df$estimate.a0.se
  ueif.df$estimate.a0.upper.ci = ueif.df$estimate.a0 + qnorm(0.975) * ueif.df$estimate.a0.se
  ueif.df$estimate.a1.lower.ci = ueif.df$estimate.a1 + qnorm(0.025) * ueif.df$estimate.a1.se
  ueif.df$estimate.a1.upper.ci = ueif.df$estimate.a1 + qnorm(0.975) * ueif.df$estimate.a1.se
  ueif.df$estimate.diff.lower.ci = ueif.df$estimate.diff + qnorm(0.025) * ueif.df$estimate.diff.se
  ueif.df$estimate.diff.upper.ci = ueif.df$estimate.diff + qnorm(0.975) * ueif.df$estimate.diff.se

  ## confidence bands
  tu = round(0.99 * nrow(ueif.df)) # row index
  # a=0
  tl = ifelse(sum(ueif.df$estimate.a0.se == 0), max(which(ueif.df$estimate.a0.se == 0)) + 1, 1)
  ceif.a0 = scaled_ueif(ueif.a0, ueif.df$estimate.a0, ueif.df$estimate.a0.se, tl, tu)
  c.a0 = quantile(foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(rnorm(n) / sqrt(n)) %*% ceif.a0))
  }, probs = 0.95)
  ueif.df$estimate.a0.lower.cb = ueif.df$estimate.a0 - ueif.df$estimate.a0.se * c.a0 / sqrt(n)
  ueif.df$estimate.a0.upper.cb = ueif.df$estimate.a0 + ueif.df$estimate.a0.se * c.a0 / sqrt(n)

  # a=1
  tl = ifelse(sum(ueif.df$estimate.a1.se == 0), max(which(ueif.df$estimate.a1.se == 0)) + 1, 1)
  ceif.a1 = scaled_ueif(ueif.a1, ueif.df$estimate.a1, ueif.df$estimate.a1.se, tl, tu)
  c.a1 = quantile(foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(rnorm(n) / sqrt(n)) %*% ceif.a1))
  }, probs = 0.95)
  ueif.df$estimate.a1.lower.cb = ueif.df$estimate.a1 - ueif.df$estimate.a1.se * c.a1 / sqrt(n)
  ueif.df$estimate.a1.upper.cb = ueif.df$estimate.a1 + ueif.df$estimate.a1.se * c.a1 / sqrt(n)

  # diff
  tl = ifelse(sum(ueif.df$estimate.diff.se == 0), max(which(ueif.df$estimate.diff.se == 0)) + 1, 1)
  ueif.diff = ueif.a1 - ueif.a0
  ceif.diff = scaled_ueif(ueif.diff, ueif.df$estimate.diff, ueif.df$estimate.diff.se, tl, tu)
  c.diff = quantile(foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(rnorm(n) / sqrt(n)) %*% ceif.diff))
  }, probs = 0.95)
  ueif.df$estimate.diff.lower.cb = ueif.df$estimate.diff -
    ueif.df$estimate.diff.se * c.diff / sqrt(n)
  ueif.df$estimate.diff.upper.cb = ueif.df$estimate.diff +
    ueif.df$estimate.diff.se * c.diff / sqrt(n)
  return(ueif.df)
}

#' RMST Inference
#'
#' Calculate confidence intervals and simultaneous confidence bands for RMST
#'
#' @param npath Number of bootstrap paths for confidence bands
#' @param ueif.a1.list List of K UEIF matrices for treatment a=1 (one per fold)
#' @param ueif.a0.list List of K UEIF matrices for treatment a=0 (one per fold)
#' @param time.list List of K time grid vectors (one per fold)
#' @return Data frame with point estimates, confidence intervals, and confidence bands
#' @noRd
rmst_inference = function(npath, ueif.a1.list, ueif.a0.list, time.list) {
  ## merge K-fold UEIFs onto union time grid
  res.a0 = merge_kfold_ueif(ueif.a0.list, time.list)
  ueif.a0 = res.a0$ueif
  union.time = res.a0$union.time

  res.a1 = merge_kfold_ueif(ueif.a1.list, time.list)
  ueif.a1 = res.a1$ueif

  n = nrow(ueif.a0)

  ## point estimate
  # a=0
  estimate.a0.lcm = monotone_hull(ueif.a0, union.time, "lcm")
  names(estimate.a0.lcm) = c("t", "estimate.a0")
  ueif.df = merge(data.frame(t = union.time), estimate.a0.lcm, all = TRUE, by = "t")

  # a=1
  estimate.a1.lcm = monotone_hull(ueif.a1, union.time, "lcm")
  names(estimate.a1.lcm) = c("t", "estimate.a1")
  ueif.df = merge(ueif.df, estimate.a1.lcm, all = TRUE, by = "t")

  # diff
  ueif.df = approxTime(ueif.df, xout = ueif.df$t, rule = 2)
  ueif.df$estimate.diff = ueif.df$estimate.a1 - ueif.df$estimate.a0

  ## standard error
  ueif.df$estimate.a0.se = ueif_se(ueif.a0, ueif.df$estimate.a0)
  ueif.df$estimate.a1.se = ueif_se(ueif.a1, ueif.df$estimate.a1)
  ueif.df$estimate.diff.se = ueif_se(ueif.a1 - ueif.a0, ueif.df$estimate.diff)

  ueif.df$estimate.a0.lower.ci = ueif.df$estimate.a0 + qnorm(0.025) * ueif.df$estimate.a0.se
  ueif.df$estimate.a0.upper.ci = ueif.df$estimate.a0 + qnorm(0.975) * ueif.df$estimate.a0.se
  ueif.df$estimate.a1.lower.ci = ueif.df$estimate.a1 + qnorm(0.025) * ueif.df$estimate.a1.se
  ueif.df$estimate.a1.upper.ci = ueif.df$estimate.a1 + qnorm(0.975) * ueif.df$estimate.a1.se
  ueif.df$estimate.diff.lower.ci = ueif.df$estimate.diff + qnorm(0.025) * ueif.df$estimate.diff.se
  ueif.df$estimate.diff.upper.ci = ueif.df$estimate.diff + qnorm(0.975) * ueif.df$estimate.diff.se

  ## confidence bands
  tu = round(0.99 * nrow(ueif.df)) # row index
  # a=0
  tl = ifelse(sum(ueif.df$estimate.a0.se == 0), max(which(ueif.df$estimate.a0.se == 0)) + 1, 1)
  ceif.a0 = scaled_ueif(ueif.a0, ueif.df$estimate.a0, ueif.df$estimate.a0.se, tl, tu)
  c.a0 = quantile(foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(rnorm(n) / sqrt(n)) %*% ceif.a0))
  }, probs = 0.95)
  ueif.df$estimate.a0.lower.cb = ueif.df$estimate.a0 - ueif.df$estimate.a0.se * c.a0 / sqrt(n)
  ueif.df$estimate.a0.upper.cb = ueif.df$estimate.a0 + ueif.df$estimate.a0.se * c.a0 / sqrt(n)

  # a=1
  tl = ifelse(sum(ueif.df$estimate.a1.se == 0), max(which(ueif.df$estimate.a1.se == 0)) + 1, 1)
  ceif.a1 = scaled_ueif(ueif.a1, ueif.df$estimate.a1, ueif.df$estimate.a1.se, tl, tu)
  c.a1 = quantile(foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(rnorm(n) / sqrt(n)) %*% ceif.a1))
  }, probs = 0.95)
  ueif.df$estimate.a1.lower.cb = ueif.df$estimate.a1 - ueif.df$estimate.a1.se * c.a1 / sqrt(n)
  ueif.df$estimate.a1.upper.cb = ueif.df$estimate.a1 + ueif.df$estimate.a1.se * c.a1 / sqrt(n)

  # diff
  tl = ifelse(sum(ueif.df$estimate.diff.se == 0), max(which(ueif.df$estimate.diff.se == 0)) + 1, 1)
  ueif.diff = ueif.a1 - ueif.a0
  ceif.diff = scaled_ueif(ueif.diff, ueif.df$estimate.diff, ueif.df$estimate.diff.se, tl, tu)
  c.diff = quantile(foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(rnorm(n) / sqrt(n)) %*% ceif.diff))
  }, probs = 0.95)
  ueif.df$estimate.diff.lower.cb = ueif.df$estimate.diff -
    ueif.df$estimate.diff.se * c.diff / sqrt(n)
  ueif.df$estimate.diff.upper.cb = ueif.df$estimate.diff +
    ueif.df$estimate.diff.se * c.diff / sqrt(n)
  return(ueif.df)
}
