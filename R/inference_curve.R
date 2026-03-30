# Survival Analysis Inference Functions
#' @importFrom foreach foreach %dopar%
#' @importFrom magrittr %>%
NULL

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
  ueif.df$estimate.a0.se = cummax(sqrt(colMeans((ueif.a0 - matrix(ueif.df$estimate.a0, nrow = nrow(ueif.a0), ncol = ncol(ueif.a0), byrow = TRUE))^2)) / sqrt(n))
  ueif.df$estimate.a1.se = cummax(sqrt(colMeans((ueif.a1 - matrix(ueif.df$estimate.a1, nrow = nrow(ueif.a1), ncol = ncol(ueif.a1), byrow = TRUE))^2)) / sqrt(n))
  ueif.df$estimate.diff.se = cummax(sqrt(colMeans(((ueif.a1 - ueif.a0) - matrix(ueif.df$estimate.diff, nrow = nrow(ueif.a0), ncol = ncol(ueif.a0), byrow = TRUE))^2)) / sqrt(n))

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
  ceif.a0 = (ueif.a0[, tl:tu] - matrix(ueif.df$estimate.a0[tl:tu], nrow = nrow(ueif.a0), ncol = (tu - tl + 1), byrow = TRUE)) / matrix(ueif.df$estimate.a0.se[tl:tu], nrow = nrow(ueif.a0), ncol = (tu - tl + 1), byrow = TRUE)
  c.a0 = foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(Rfast::Rnorm(n) / sqrt(n)) %*% ceif.a0))
  } %>% quantile(., probs = 0.95)
  ueif.df$estimate.a0.lower.cb = ueif.df$estimate.a0 - ueif.df$estimate.a0.se * c.a0 / sqrt(n)
  ueif.df$estimate.a0.upper.cb = ueif.df$estimate.a0 + ueif.df$estimate.a0.se * c.a0 / sqrt(n)

  # a=1
  tl = ifelse(sum(ueif.df$estimate.a1.se == 0), max(which(ueif.df$estimate.a1.se == 0)) + 1, 1)
  ceif.a1 = (ueif.a1[, tl:tu] - matrix(ueif.df$estimate.a1[tl:tu], nrow = nrow(ueif.a1), ncol = (tu - tl + 1), byrow = TRUE)) / matrix(ueif.df$estimate.a1.se[tl:tu], nrow = nrow(ueif.a1), ncol = (tu - tl + 1), byrow = TRUE)
  c.a1 = foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(Rfast::Rnorm(n) / sqrt(n)) %*% ceif.a1))
  } %>% quantile(., probs = 0.95)
  ueif.df$estimate.a1.lower.cb = ueif.df$estimate.a1 - ueif.df$estimate.a1.se * c.a1 / sqrt(n)
  ueif.df$estimate.a1.upper.cb = ueif.df$estimate.a1 + ueif.df$estimate.a1.se * c.a1 / sqrt(n)

  # diff
  tl = ifelse(sum(ueif.df$estimate.diff.se == 0), max(which(ueif.df$estimate.diff.se == 0)) + 1, 1)
  ueif.diff = ueif.a1 - ueif.a0
  ceif.diff = (ueif.diff[, tl:tu] - matrix(ueif.df$estimate.diff[tl:tu], nrow = nrow(ueif.diff), ncol = (tu - tl + 1), byrow = TRUE)) / matrix(ueif.df$estimate.diff.se[tl:tu], nrow = nrow(ueif.diff), ncol = (tu - tl + 1), byrow = TRUE)
  c.diff = foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(Rfast::Rnorm(n) / sqrt(n)) %*% ceif.diff))
  } %>% quantile(., probs = 0.95)
  ueif.df$estimate.diff.lower.cb = ueif.df$estimate.diff - ueif.df$estimate.diff.se * c.diff / sqrt(n)
  ueif.df$estimate.diff.upper.cb = ueif.df$estimate.diff + ueif.df$estimate.diff.se * c.diff / sqrt(n)
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
  estimate.a0.gcm = gcmlcm(x = union.time[1:max(which(!is.na(colMeans(ueif.a0))))], y = colMeans(ueif.a0)[1:max(which(!is.na(colMeans(ueif.a0))))], type = "gcm")[c("x.knots", "y.knots")]
  names(estimate.a0.gcm) = c("t", "estimate.a0")
  ueif.df = merge(data.frame(t = union.time), estimate.a0.gcm, all = TRUE, by = "t")

  # a=1
  estimate.a1.gcm = gcmlcm(x = union.time[1:max(which(!is.na(colMeans(ueif.a1))))], y = colMeans(ueif.a1)[1:max(which(!is.na(colMeans(ueif.a1))))], type = "gcm")[c("x.knots", "y.knots")]
  names(estimate.a1.gcm) = c("t", "estimate.a1")
  ueif.df = merge(ueif.df, estimate.a1.gcm, all = TRUE, by = "t")

  # diff
  ueif.df = approxTime(ueif.df, xout = ueif.df$t, rule = 2)
  ueif.df$estimate.diff = ueif.df$estimate.a1 - ueif.df$estimate.a0

  ## standard error
  ueif.df$estimate.a0.se = cummax(sqrt(colMeans((ueif.a0 - matrix(ueif.df$estimate.a0, nrow = nrow(ueif.a0), ncol = ncol(ueif.a0), byrow = TRUE))^2)) / sqrt(n))
  ueif.df$estimate.a1.se = cummax(sqrt(colMeans((ueif.a1 - matrix(ueif.df$estimate.a1, nrow = nrow(ueif.a1), ncol = ncol(ueif.a1), byrow = TRUE))^2)) / sqrt(n))
  ueif.df$estimate.diff.se = cummax(sqrt(colMeans(((ueif.a1 - ueif.a0) - matrix(ueif.df$estimate.diff, nrow = nrow(ueif.a0), ncol = ncol(ueif.a0), byrow = TRUE))^2)) / sqrt(n))

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
  ceif.a0 = (ueif.a0[, tl:tu] - matrix(ueif.df$estimate.a0[tl:tu], nrow = nrow(ueif.a0), ncol = (tu - tl + 1), byrow = TRUE)) / matrix(ueif.df$estimate.a0.se[tl:tu], nrow = nrow(ueif.a0), ncol = (tu - tl + 1), byrow = TRUE)
  c.a0 = foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(Rfast::Rnorm(n) / sqrt(n)) %*% ceif.a0))
  } %>% quantile(., probs = 0.95)
  ueif.df$estimate.a0.lower.cb = ueif.df$estimate.a0 - ueif.df$estimate.a0.se * c.a0 / sqrt(n)
  ueif.df$estimate.a0.upper.cb = ueif.df$estimate.a0 + ueif.df$estimate.a0.se * c.a0 / sqrt(n)

  # a=1
  tl = ifelse(sum(ueif.df$estimate.a1.se == 0), max(which(ueif.df$estimate.a1.se == 0)) + 1, 1)
  ceif.a1 = (ueif.a1[, tl:tu] - matrix(ueif.df$estimate.a1[tl:tu], nrow = nrow(ueif.a1), ncol = (tu - tl + 1), byrow = TRUE)) / matrix(ueif.df$estimate.a1.se[tl:tu], nrow = nrow(ueif.a1), ncol = (tu - tl + 1), byrow = TRUE)
  c.a1 = foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(Rfast::Rnorm(n) / sqrt(n)) %*% ceif.a1))
  } %>% quantile(., probs = 0.95)
  ueif.df$estimate.a1.lower.cb = ueif.df$estimate.a1 - ueif.df$estimate.a1.se * c.a1 / sqrt(n)
  ueif.df$estimate.a1.upper.cb = ueif.df$estimate.a1 + ueif.df$estimate.a1.se * c.a1 / sqrt(n)

  # diff
  tl = ifelse(sum(ueif.df$estimate.diff.se == 0), max(which(ueif.df$estimate.diff.se == 0)) + 1, 1)
  ueif.diff = ueif.a1 - ueif.a0
  ceif.diff = (ueif.diff[, tl:tu] - matrix(ueif.df$estimate.diff[tl:tu], nrow = nrow(ueif.diff), ncol = (tu - tl + 1), byrow = TRUE)) / matrix(ueif.df$estimate.diff.se[tl:tu], nrow = nrow(ueif.diff), ncol = (tu - tl + 1), byrow = TRUE)
  c.diff = foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(Rfast::Rnorm(n) / sqrt(n)) %*% ceif.diff))
  } %>% quantile(., probs = 0.95)
  ueif.df$estimate.diff.lower.cb = ueif.df$estimate.diff - ueif.df$estimate.diff.se * c.diff / sqrt(n)
  ueif.df$estimate.diff.upper.cb = ueif.df$estimate.diff + ueif.df$estimate.diff.se * c.diff / sqrt(n)
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
  ueif.df$estimate.a0.se = cummax(sqrt(colMeans((ueif.a0 - matrix(ueif.df$estimate.a0, nrow = nrow(ueif.a0), ncol = ncol(ueif.a0), byrow = TRUE))^2)) / sqrt(n))
  ueif.df$estimate.a1.se = cummax(sqrt(colMeans((ueif.a1 - matrix(ueif.df$estimate.a1, nrow = nrow(ueif.a1), ncol = ncol(ueif.a1), byrow = TRUE))^2)) / sqrt(n))
  ueif.df$estimate.diff.se = cummax(sqrt(colMeans(((ueif.a1 - ueif.a0) - matrix(ueif.df$estimate.diff, nrow = nrow(ueif.a0), ncol = ncol(ueif.a0), byrow = TRUE))^2)) / sqrt(n))

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
  ceif.a0 = (ueif.a0[, tl:tu] - matrix(ueif.df$estimate.a0[tl:tu], nrow = nrow(ueif.a0), ncol = (tu - tl + 1), byrow = TRUE)) / matrix(ueif.df$estimate.a0.se[tl:tu], nrow = nrow(ueif.a0), ncol = (tu - tl + 1), byrow = TRUE)
  c.a0 = foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(Rfast::Rnorm(n) / sqrt(n)) %*% ceif.a0))
  } %>% quantile(., probs = 0.95)
  ueif.df$estimate.a0.lower.cb = ueif.df$estimate.a0 - ueif.df$estimate.a0.se * c.a0 / sqrt(n)
  ueif.df$estimate.a0.upper.cb = ueif.df$estimate.a0 + ueif.df$estimate.a0.se * c.a0 / sqrt(n)

  # a=1
  tl = ifelse(sum(ueif.df$estimate.a1.se == 0), max(which(ueif.df$estimate.a1.se == 0)) + 1, 1)
  ceif.a1 = (ueif.a1[, tl:tu] - matrix(ueif.df$estimate.a1[tl:tu], nrow = nrow(ueif.a1), ncol = (tu - tl + 1), byrow = TRUE)) / matrix(ueif.df$estimate.a1.se[tl:tu], nrow = nrow(ueif.a1), ncol = (tu - tl + 1), byrow = TRUE)
  c.a1 = foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(Rfast::Rnorm(n) / sqrt(n)) %*% ceif.a1))
  } %>% quantile(., probs = 0.95)
  ueif.df$estimate.a1.lower.cb = ueif.df$estimate.a1 - ueif.df$estimate.a1.se * c.a1 / sqrt(n)
  ueif.df$estimate.a1.upper.cb = ueif.df$estimate.a1 + ueif.df$estimate.a1.se * c.a1 / sqrt(n)

  # diff
  tl = ifelse(sum(ueif.df$estimate.diff.se == 0), max(which(ueif.df$estimate.diff.se == 0)) + 1, 1)
  ueif.diff = ueif.a1 - ueif.a0
  ceif.diff = (ueif.diff[, tl:tu] - matrix(ueif.df$estimate.diff[tl:tu], nrow = nrow(ueif.diff), ncol = (tu - tl + 1), byrow = TRUE)) / matrix(ueif.df$estimate.diff.se[tl:tu], nrow = nrow(ueif.diff), ncol = (tu - tl + 1), byrow = TRUE)
  c.diff = foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(Rfast::Rnorm(n) / sqrt(n)) %*% ceif.diff))
  } %>% quantile(., probs = 0.95)
  ueif.df$estimate.diff.lower.cb = ueif.df$estimate.diff - ueif.df$estimate.diff.se * c.diff / sqrt(n)
  ueif.df$estimate.diff.upper.cb = ueif.df$estimate.diff + ueif.df$estimate.diff.se * c.diff / sqrt(n)
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
  estimate.a0.lcm = gcmlcm(x = union.time[1:max(which(!is.na(colMeans(ueif.a0))))], y = colMeans(ueif.a0)[1:max(which(!is.na(colMeans(ueif.a0))))], type = "lcm")[c("x.knots", "y.knots")]
  names(estimate.a0.lcm) = c("t", "estimate.a0")
  ueif.df = merge(data.frame(t = union.time), estimate.a0.lcm, all = TRUE, by = "t")

  # a=1
  estimate.a1.lcm = gcmlcm(x = union.time[1:max(which(!is.na(colMeans(ueif.a1))))], y = colMeans(ueif.a1)[1:max(which(!is.na(colMeans(ueif.a1))))], type = "lcm")[c("x.knots", "y.knots")]
  names(estimate.a1.lcm) = c("t", "estimate.a1")
  ueif.df = merge(ueif.df, estimate.a1.lcm, all = TRUE, by = "t")

  # diff
  ueif.df = approxTime(ueif.df, xout = ueif.df$t, rule = 2)
  ueif.df$estimate.diff = ueif.df$estimate.a1 - ueif.df$estimate.a0

  ## standard error
  ueif.df$estimate.a0.se = cummax(sqrt(colMeans((ueif.a0 - matrix(ueif.df$estimate.a0, nrow = nrow(ueif.a0), ncol = ncol(ueif.a0), byrow = TRUE))^2)) / sqrt(n))
  ueif.df$estimate.a1.se = cummax(sqrt(colMeans((ueif.a1 - matrix(ueif.df$estimate.a1, nrow = nrow(ueif.a1), ncol = ncol(ueif.a1), byrow = TRUE))^2)) / sqrt(n))
  ueif.df$estimate.diff.se = cummax(sqrt(colMeans(((ueif.a1 - ueif.a0) - matrix(ueif.df$estimate.diff, nrow = nrow(ueif.a0), ncol = ncol(ueif.a0), byrow = TRUE))^2)) / sqrt(n))

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
  ceif.a0 = (ueif.a0[, tl:tu] - matrix(ueif.df$estimate.a0[tl:tu], nrow = nrow(ueif.a0), ncol = (tu - tl + 1), byrow = TRUE)) / matrix(ueif.df$estimate.a0.se[tl:tu], nrow = nrow(ueif.a0), ncol = (tu - tl + 1), byrow = TRUE)
  c.a0 = foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(Rfast::Rnorm(n) / sqrt(n)) %*% ceif.a0))
  } %>% quantile(., probs = 0.95)
  ueif.df$estimate.a0.lower.cb = ueif.df$estimate.a0 - ueif.df$estimate.a0.se * c.a0 / sqrt(n)
  ueif.df$estimate.a0.upper.cb = ueif.df$estimate.a0 + ueif.df$estimate.a0.se * c.a0 / sqrt(n)

  # a=1
  tl = ifelse(sum(ueif.df$estimate.a1.se == 0), max(which(ueif.df$estimate.a1.se == 0)) + 1, 1)
  ceif.a1 = (ueif.a1[, tl:tu] - matrix(ueif.df$estimate.a1[tl:tu], nrow = nrow(ueif.a1), ncol = (tu - tl + 1), byrow = TRUE)) / matrix(ueif.df$estimate.a1.se[tl:tu], nrow = nrow(ueif.a1), ncol = (tu - tl + 1), byrow = TRUE)
  c.a1 = foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(Rfast::Rnorm(n) / sqrt(n)) %*% ceif.a1))
  } %>% quantile(., probs = 0.95)
  ueif.df$estimate.a1.lower.cb = ueif.df$estimate.a1 - ueif.df$estimate.a1.se * c.a1 / sqrt(n)
  ueif.df$estimate.a1.upper.cb = ueif.df$estimate.a1 + ueif.df$estimate.a1.se * c.a1 / sqrt(n)

  # diff
  tl = ifelse(sum(ueif.df$estimate.diff.se == 0), max(which(ueif.df$estimate.diff.se == 0)) + 1, 1)
  ueif.diff = ueif.a1 - ueif.a0
  ceif.diff = (ueif.diff[, tl:tu] - matrix(ueif.df$estimate.diff[tl:tu], nrow = nrow(ueif.diff), ncol = (tu - tl + 1), byrow = TRUE)) / matrix(ueif.df$estimate.diff.se[tl:tu], nrow = nrow(ueif.diff), ncol = (tu - tl + 1), byrow = TRUE)
  c.diff = foreach(i = 1:npath, .combine = c) %dopar% {
    max(abs(rbind(Rfast::Rnorm(n) / sqrt(n)) %*% ceif.diff))
  } %>% quantile(., probs = 0.95)
  ueif.df$estimate.diff.lower.cb = ueif.df$estimate.diff - ueif.df$estimate.diff.se * c.diff / sqrt(n)
  ueif.df$estimate.diff.upper.cb = ueif.df$estimate.diff + ueif.df$estimate.diff.se * c.diff / sqrt(n)
  return(ueif.df)
}
