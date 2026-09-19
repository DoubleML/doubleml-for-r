# Helper Functions for Survival Analysis Inference
#' @importFrom fdrtool gcmlcm
NULL

# These functions support both survival analysis and competing risks methods

#' Last Observation Carried Forward
#'
#' Simple implementation of na.locf (Last Observation Carried Forward)
#' Similar to zoo::na.locf functionality
#'
#' @param x Vector or matrix to fill missing values
#' @return Object with missing values filled using last observation
#' @noRd
na.locf = function(x) {
  if (is.vector(x)) {
    for (i in 2:length(x)) {
      if (is.na(x[i])) {
        x[i] = x[i - 1]
      }
    }
  } else if (is.matrix(x)) {
    for (j in seq_len(ncol(x))) {
      for (i in 2:nrow(x)) {
        if (is.na(x[i, j])) {
          x[i, j] = x[i - 1, j]
        }
      }
    }
  }
  return(x)
}

#' Time-based Approximation for Survival Functions
#'
#' Performs linear interpolation for time-dependent survival functions
#' Similar to approx() but with specific rules for survival data
#'
#' @param object Data frame with time column and value columns
#' @param xout Output time points for interpolation
#' @param rule Integer specifying how to handle out-of-bounds values
#' @return Data frame with interpolated values
#' @noRd
approxTime = function(object, xout, rule = 2) {
  time_col = object[, 1] # First column is time
  result = data.frame(t = xout)

  # For each non-time column, perform interpolation
  for (j in 2:ncol(object)) {
    col_values = object[, j]
    # Use approx for linear interpolation
    interpolated = approx(x = time_col, y = col_values, xout = xout, rule = rule)
    result[, j] = interpolated$y
  }

  # Preserve column names
  names(result) = names(object)
  return(result)
}

#' Merge K-fold UEIF matrices onto a union time grid
#'
#' Takes a list of UEIF matrices (one per fold, each with its own time grid)
#' and merges them onto a common union time grid via interpolation.
#'
#' @param ueif.list List of K UEIF matrices, each n_k x length(time.list)
#' @param time.list List of K time grid vectors
#' @return List with: merged (n_total x n_union_times matrix), union.time (vector)
#' @noRd
merge_kfold_ueif = function(ueif.list, time.list) {
  union.time = sort(unique(unlist(time.list)))
  merged = data.frame(t = union.time)
  for (k in seq_along(ueif.list)) {
    fold.df = merge(
      data.frame(t = time.list[[k]], t(ueif.list[[k]])),
      data.frame(t = union.time),
      all = TRUE, by = "t"
    )
    merged = merge(merged, fold.df, all = TRUE, by = "t")
  }
  merged = approxTime(merged, xout = merged$t)
  if (sum(is.na(merged)) > 0) {
    merged = approxTime(rbind(0, merged), xout = c(0, union.time))[-1, ]
  }
  ueif = t(merged[, -1])
  return(list(ueif = ueif, union.time = union.time))
}

# Pointwise standard error of a mean influence-function curve, made monotone
# in time with cummax (variance can only accumulate along the time grid)
ueif_se = function(ueif, estimate) {
  centered = ueif - matrix(estimate, nrow = nrow(ueif), ncol = ncol(ueif), byrow = TRUE)
  cummax(sqrt(colMeans(centered^2)) / sqrt(nrow(ueif)))
}

# Least concave majorant ("lcm") or greatest convex minorant ("gcm") of the
# mean curve over the time points with a finite mean; returns the knots
monotone_hull = function(ueif, time, type) {
  m = colMeans(ueif)
  k = max(which(!is.na(m)))
  gcmlcm(x = time[1:k], y = m[1:k], type = type)[c("x.knots", "y.knots")]
}

#' Extract Average Treatment Effect for Survival Probability at Tau
#'
#' Extracts point estimates, standard errors, and treatment effects for survival
#' probability at a specific time point tau using cumulative minimum for
#' monotonic decreasing survival probabilities.
#'
#' @param ueif.a1.list List of K UEIF matrices for treatment a=1 (one per fold)
#' @param ueif.a0.list List of K UEIF matrices for treatment a=0 (one per fold)
#' @param time.list List of K time grid vectors (one per fold)
#' @param tau Time point at which to extract the treatment effect
#' @return Data frame with point estimates and standard errors at tau
#' @noRd
survival_prob_extract_average = function(ueif.a1.list, ueif.a0.list, time.list, tau) {
  ## merge K-fold UEIFs onto union time grid
  res.a0 = merge_kfold_ueif(ueif.a0.list, time.list)
  ueif.a0 = res.a0$ueif
  union.time = res.a0$union.time

  res.a1 = merge_kfold_ueif(ueif.a1.list, time.list)
  ueif.a1 = res.a1$ueif

  ## point estimate
  # a=0
  estimate.a0.raw = colMeans(ueif.a0, na.rm = TRUE)
  estimate.a0 = cummin(estimate.a0.raw)
  ueif.df = data.frame(t = union.time[seq_along(estimate.a0)], estimate.a0 = estimate.a0)

  # a=1
  estimate.a1.raw = colMeans(ueif.a1, na.rm = TRUE)
  estimate.a1 = cummin(estimate.a1.raw)
  ueif.df$estimate.a1 = estimate.a1[seq_len(nrow(ueif.df))]

  ueif.df = approxTime(ueif.df, xout = ueif.df$t)
  ueif.df$estimate.diff = ueif.df$estimate.a1 - ueif.df$estimate.a0

  ## standard error - using cummax for monotonic increasing variance over time
  ueif.df$estimate.a0.se = ueif_se(ueif.a0, ueif.df$estimate.a0)
  ueif.df$estimate.a1.se = ueif_se(ueif.a1, ueif.df$estimate.a1)
  ueif.df$estimate.diff.se = ueif_se(ueif.a1 - ueif.a0, ueif.df$estimate.diff)

  # Use <= for right-continuous step function (cadlag)
  tau.ind = max(which(ueif.df$t <= tau))
  ueif.df = ueif.df[tau.ind, ]
  return(ueif.df[, -1])
}

#' Extract Average Treatment Effect for Cumulative Incidence at Tau
#'
#' Extracts point estimates, standard errors, and treatment effects for cumulative
#' incidence function (CIF) at a specific time point tau using cumulative maximum
#' for monotonic increasing cumulative incidence probabilities.
#'
#' @param ueif.a1.list List of K UEIF matrices for treatment a=1 (one per fold)
#' @param ueif.a0.list List of K UEIF matrices for treatment a=0 (one per fold)
#' @param time.list List of K time grid vectors (one per fold)
#' @param tau Time point at which to extract the treatment effect
#' @return Data frame with point estimates and standard errors at tau
#' @noRd
cifj_extract_average = function(ueif.a1.list, ueif.a0.list, time.list, tau) {
  ## merge K-fold UEIFs onto union time grid
  res.a0 = merge_kfold_ueif(ueif.a0.list, time.list)
  ueif.a0 = res.a0$ueif
  union.time = res.a0$union.time

  res.a1 = merge_kfold_ueif(ueif.a1.list, time.list)
  ueif.a1 = res.a1$ueif

  ## point estimate
  # a=0
  estimate.a0.raw = colMeans(ueif.a0, na.rm = TRUE)
  estimate.a0 = cummax(estimate.a0.raw)
  ueif.df = data.frame(t = union.time[seq_along(estimate.a0)], estimate.a0 = estimate.a0)

  # a=1
  estimate.a1.raw = colMeans(ueif.a1, na.rm = TRUE)
  estimate.a1 = cummax(estimate.a1.raw)
  ueif.df$estimate.a1 = estimate.a1[seq_len(nrow(ueif.df))]

  ueif.df = approxTime(ueif.df, xout = ueif.df$t)
  ueif.df$estimate.diff = ueif.df$estimate.a1 - ueif.df$estimate.a0

  ## standard error - using cummax for monotonic increasing variance over time
  ueif.df$estimate.a0.se = ueif_se(ueif.a0, ueif.df$estimate.a0)
  ueif.df$estimate.a1.se = ueif_se(ueif.a1, ueif.df$estimate.a1)
  ueif.df$estimate.diff.se = ueif_se(ueif.a1 - ueif.a0, ueif.df$estimate.diff)

  # Use < for left-continuous behavior, no interpolation
  tau.ind = max(which(ueif.df$t < tau))
  ueif.df = ueif.df[tau.ind, ]
  return(ueif.df[, -1])
}

#' Extract Average Treatment Effect for RMST at Tau
#'
#' Extracts point estimates, standard errors, and treatment effects for restricted
#' mean survival time (RMST) at a specific time point tau using least concave majorant
#' for monotonic increasing RMST values.
#'
#' @param ueif.a1.list List of K UEIF matrices for treatment a=1 (one per fold)
#' @param ueif.a0.list List of K UEIF matrices for treatment a=0 (one per fold)
#' @param time.list List of K time grid vectors (one per fold)
#' @param tau Time point at which to extract the treatment effect
#' @return Data frame with point estimates and standard errors at tau
#' @noRd
rmst_extract_average = function(ueif.a1.list, ueif.a0.list, time.list, tau) {
  ## merge K-fold UEIFs onto union time grid
  res.a0 = merge_kfold_ueif(ueif.a0.list, time.list)
  ueif.a0 = res.a0$ueif
  union.time = res.a0$union.time

  res.a1 = merge_kfold_ueif(ueif.a1.list, time.list)
  ueif.a1 = res.a1$ueif

  ## point estimate
  # a=0
  estimate.a0.lcm = monotone_hull(ueif.a0, union.time, "lcm")
  names(estimate.a0.lcm) = c("t", "estimate.a0")
  ueif.df = merge(data.frame(t = union.time), estimate.a0.lcm, all = TRUE, by = "t")

  # a=1
  estimate.a1.lcm = monotone_hull(ueif.a1, union.time, "lcm")
  names(estimate.a1.lcm) = c("t", "estimate.a1")
  ueif.df = merge(ueif.df, estimate.a1.lcm, all = TRUE, by = "t")

  ueif.df = approxTime(ueif.df, xout = ueif.df$t)
  ueif.df$estimate.diff = ueif.df$estimate.a1 - ueif.df$estimate.a0

  ## standard error
  ueif.df$estimate.a0.se = ueif_se(ueif.a0, ueif.df$estimate.a0)
  ueif.df$estimate.a1.se = ueif_se(ueif.a1, ueif.df$estimate.a1)
  ueif.df$estimate.diff.se = ueif_se(ueif.a1 - ueif.a0, ueif.df$estimate.diff)

  tau.ind = max(which((tau - ueif.df$t) > 0))
  ueif.df = ueif.df[tau.ind, ] +
    (tau - ueif.df$t[tau.ind]) * (ueif.df[tau.ind + 1, ] - ueif.df[tau.ind, ]) /
      (ueif.df$t[tau.ind + 1] - ueif.df$t[tau.ind])
  return(ueif.df[, -1])
}

#' Extract Average Treatment Effect for RMTL at Tau
#'
#' Extracts point estimates, standard errors, and treatment effects for restricted
#' mean time lost (RMTL) at a specific time point tau using greatest convex minorant
#' for monotonic increasing RMTL values.
#'
#' @param ueif.a1.list List of K UEIF matrices for treatment a=1 (one per fold)
#' @param ueif.a0.list List of K UEIF matrices for treatment a=0 (one per fold)
#' @param time.list List of K time grid vectors (one per fold)
#' @param tau Time point at which to extract the treatment effect
#' @return Data frame with point estimates and standard errors at tau
#' @noRd
rmtlj_extract_average = function(ueif.a1.list, ueif.a0.list, time.list, tau) {
  ## merge K-fold UEIFs onto union time grid
  res.a0 = merge_kfold_ueif(ueif.a0.list, time.list)
  ueif.a0 = res.a0$ueif
  union.time = res.a0$union.time

  res.a1 = merge_kfold_ueif(ueif.a1.list, time.list)
  ueif.a1 = res.a1$ueif

  ## point estimate
  # a=0
  estimate.a0.lcm = monotone_hull(ueif.a0, union.time, "gcm")
  names(estimate.a0.lcm) = c("t", "estimate.a0")
  ueif.df = merge(data.frame(t = union.time), estimate.a0.lcm, all = TRUE, by = "t")

  # a=1
  estimate.a1.lcm = monotone_hull(ueif.a1, union.time, "gcm")
  names(estimate.a1.lcm) = c("t", "estimate.a1")
  ueif.df = merge(ueif.df, estimate.a1.lcm, all = TRUE, by = "t")

  ueif.df = approxTime(ueif.df, xout = ueif.df$t)
  ueif.df$estimate.diff = ueif.df$estimate.a1 - ueif.df$estimate.a0

  ## standard error
  ueif.df$estimate.a0.se = ueif_se(ueif.a0, ueif.df$estimate.a0)
  ueif.df$estimate.a1.se = ueif_se(ueif.a1, ueif.df$estimate.a1)
  ueif.df$estimate.diff.se = ueif_se(ueif.a1 - ueif.a0, ueif.df$estimate.diff)

  tau.ind = max(which((tau - ueif.df$t) > 0))
  ueif.df = ueif.df[tau.ind, ] +
    (tau - ueif.df$t[tau.ind]) * (ueif.df[tau.ind + 1, ] - ueif.df[tau.ind, ]) /
      (ueif.df$t[tau.ind + 1] - ueif.df$t[tau.ind])
  return(ueif.df[, -1])
}
