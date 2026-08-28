#' @title Restricted Mean Time Lost for Competing Risks (RMTLJ)
#'
#' @description
#' Computes efficient influence function for restricted mean time lost to cause j
#' before time tau in competing risks settings.
#'
#' @param id (`numeric`) \\cr
#' Individual identifiers for observations.
#'
#' @param a (`numeric`) \\cr
#' Binary treatment indicator (0 or 1).
#'
#' @param time (`numeric`) \\cr
#' Observed time (minimum of event time and censoring time).
#'
#' @param event (`numeric`) \\cr
#' Event indicator (0 = censored, j = cause j event).
#'
#' @param bw (`numeric`) \\cr
#' Balancing weights (inverse propensity weights).
#'
#' @param tilt (`numeric`) \\cr
#' Tilting function for stabilization.
#'
#' @param G.a0, G.a1 (`matrix`) \\cr
#' Censoring survival function G(t|X,A=0) and G(t|X,A=1).
#'
#' @param S.a0, S.a1 (`matrix`) \\cr
#' Overall event-free survival function S(t|X,A=0) and S(t|X,A=1).
#'
#' @param Sj.a0, Sj.a1 (`matrix`) \\cr
#' Cause-specific survival function for cause j: Sj(t|X,A=0) and Sj(t|X,A=1).
#'
#' @param freq.time (`numeric(1)`) \\cr
#' Time grid frequency. If NULL, uses unique event times.
#'
#' @param admin.cens (`numeric(1)`) \\cr
#' Administrative censoring time (maximum follow-up).
#'
#' @return List with elements:
#' \describe{
#'   \item{ueif.a1}{Efficient influence function for A=1}
#'   \item{ueif.a0}{Efficient influence function for A=0}
#' }
#'
#' @details
#' This function implements the censoring martingale based efficient influence
#' function for restricted mean time lost (RMTL) to cause j in competing risks.
#' The RMTL measures the expected time lost to a specific cause before time tau.
#'
#' The efficient influence function has 5 terms:
#' \enumerate{
#'   \item Inverse probability weighted observed RMTL
#'   \item Predicted RMTL under treatment level
#'   \item Censoring correction for cumulative incidence
#'   \item Survival correction for RMTL
#'   \item Combined censoring and survival correction
#' }
#'
#' @keywords internal
#' @noRd
double_ml_rmtlj = function(id, a, time, event, bw, tilt, G.a0, G.a1, S.a0, S.a1, Sj.a0, Sj.a1, freq.time = NULL, admin.cens)
{
  n = length(id)
  cause = 1
  if (is.null(freq.time)) {
    s = sort(unique(time))
  } else {
    s = seq(freq.time, admin.cens, freq.time)
  }
  ns = length(s)
  ds = diff(c(0, s))

  # Winsorization of survival functions
  S.a0 = t(na.locf(t(ifelse(S.a0 < 1e-3, 1e-3, S.a0))))
  S.a1 = t(na.locf(t(ifelse(S.a1 < 1e-3, 1e-3, S.a1))))
  G.a0 = t(na.locf(t(ifelse(G.a0 < 1e-3, 1e-3, G.a0))))
  G.a1 = t(na.locf(t(ifelse(G.a1 < 1e-3, 1e-3, G.a1))))
  Sj.a0 = t(na.locf(t(ifelse(Sj.a0 < 1e-3, 1e-3, Sj.a0))))
  Sj.a1 = t(na.locf(t(ifelse(Sj.a1 < 1e-3, 1e-3, Sj.a1))))

  # Compute hazards
  G.dHazard.a0 = t(apply(cbind(0, -log(G.a0)), 1, diff))
  G.dHazard.a1 = t(apply(cbind(0, -log(G.a1)), 1, diff))
  S.dHazard.a0 = t(apply(cbind(0, -log(S.a0)), 1, diff))
  S.dHazard.a1 = t(apply(cbind(0, -log(S.a1)), 1, diff))
  Fj.dHazard.a0 = t(apply(cbind(0, -log(Sj.a0)), 1, diff))
  Fj.dHazard.a1 = t(apply(cbind(0, -log(Sj.a1)), 1, diff))

  # Cumulative incidence and RMTL
  Fj.a0 = t(apply(cbind(1, S.a0[, 1:(ns - 1)]) * Fj.dHazard.a0, 1, cumsum))
  Fj.a1 = t(apply(cbind(1, S.a1[, 1:(ns - 1)]) * Fj.dHazard.a1, 1, cumsum))
  RMTLj.a0 = t(apply(Fj.a0 * matrix(ds, ncol = ns, nrow = n, byrow = TRUE), 1, cumsum))
  RMTLj.a1 = t(apply(Fj.a1 * matrix(ds, ncol = ns, nrow = n, byrow = TRUE), 1, cumsum))

  # Indicators
  Yt = do.call(cbind, lapply(1:ns, function(u) ifelse(time >= s[u], 1, 0)))
  dNct = do.call(cbind, lapply(1:ns, function(u) (dplyr::near(s[u], time)) * (event == 0)))
  RMTLj.obs = do.call(cbind, lapply(1:ns, function(u) (s[u] - pmin(time, s[u])) * (event == cause)))

  # 5-term UEIF for A=0
  term1.a0 = matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * RMTLj.obs / matrix(rowSums(G.a0 * do.call(cbind, lapply(1:ns, function(u) ifelse(time == s[u], 1, 0)))), ncol = ns, nrow = n, byrow = FALSE)
  term2.a0 = matrix(tilt, ncol = ns, nrow = n, byrow = FALSE) * RMTLj.a0
  term3.a0 = matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Fj.a0 * (dNct - Yt * G.dHazard.a0) / (G.a0 * S.a0), 1, cumsum)) -
    matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Fj.a0 * (dNct - Yt * G.dHazard.a0) / (G.a0 * S.a0), 1, cumsum)))
  term4.a0 = matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * RMTLj.a0 * (t(apply((dNct - Yt * G.dHazard.a0) / (S.a0 * G.a0), 1, cumsum)) - 1)
  term5.a0 = matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * t(apply(RMTLj.a0 * (dNct - Yt * G.dHazard.a0) / (S.a0 * G.a0), 1, cumsum))
  ueif.a0 = 1 / mean(tilt) * (term1.a0 + term2.a0 + term3.a0 + term4.a0 - term5.a0)

  # 5-term UEIF for A=1 (symmetric)
  term1.a1 = matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * RMTLj.obs / matrix(rowSums(G.a1 * do.call(cbind, lapply(1:ns, function(u) ifelse(time == s[u], 1, 0)))), ncol = ns, nrow = n, byrow = FALSE)
  term2.a1 = matrix(tilt, ncol = ns, nrow = n, byrow = FALSE) * RMTLj.a1
  term3.a1 = matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Fj.a1 * (dNct - Yt * G.dHazard.a1) / (G.a1 * S.a1), 1, cumsum)) -
    matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Fj.a1 * (dNct - Yt * G.dHazard.a1) / (G.a1 * S.a1), 1, cumsum)))
  term4.a1 = matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * RMTLj.a1 * (t(apply((dNct - Yt * G.dHazard.a1) / (S.a1 * G.a1), 1, cumsum)) - 1)
  term5.a1 = matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * t(apply(RMTLj.a1 * (dNct - Yt * G.dHazard.a1) / (S.a1 * G.a1), 1, cumsum))
  ueif.a1 = 1 / mean(tilt) * (term1.a1 + term2.a1 + term3.a1 + term4.a1 - term5.a1)

  return(list(ueif.a1 = ueif.a1, ueif.a0 = ueif.a0))
}

#' @title Separable Direct Effect for RMTLJ with A*=1
#'
#' @description
#' Computes efficient influence function for separable direct effect of RMTL
#' with reference treatment A*=1 in competing risks mediation analysis.
#'
#' @param id (`numeric`) \\cr
#' Individual identifiers for observations.
#'
#' @param a (`numeric`) \\cr
#' Binary treatment indicator (0 or 1).
#'
#' @param time (`numeric`) \\cr
#' Observed time (minimum of event time and censoring time).
#'
#' @param event (`numeric`) \\cr
#' Event indicator (0 = censored, j = cause j event).
#'
#' @param bw (`numeric`) \\cr
#' Balancing weights (inverse propensity weights).
#'
#' @param tilt (`numeric`) \\cr
#' Tilting function for stabilization.
#'
#' @param G.a0, G.a1 (`matrix`) \\cr
#' Censoring survival function G(t|X,A=0) and G(t|X,A=1).
#'
#' @param S.a0, S.a1 (`matrix`) \\cr
#' Overall event-free survival function S(t|X,A=0) and S(t|X,A=1).
#'
#' @param Sj.a0, Sj.a1 (`matrix`) \\cr
#' Cause-specific survival for cause j: Sj(t|X,A=0) and Sj(t|X,A=1).
#'
#' @param Sjbar.a0, Sjbar.a1 (`matrix`) \\cr
#' Complementary cause survival (all causes except j): Sjbar(t|X,A=0) and Sjbar(t|X,A=1).
#'
#' @param freq.time (`numeric(1)`) \\cr
#' Time grid frequency. If NULL, uses unique event times.
#'
#' @param admin.cens (`numeric(1)`) \\cr
#' Administrative censoring time (maximum follow-up).
#'
#' @return List with elements:
#' \describe{
#'   \item{ueif.a1}{Efficient influence function for A=1 under separable direct effect}
#'   \item{ueif.a0}{Efficient influence function for A=0 under separable direct effect}
#' }
#'
#' @details
#' This function implements the separable direct effect decomposition where the
#' treatment affects cause j directly while holding the effect on complementary
#' causes at reference level A*=1. The counterfactual RMTL combines Sj under A=0
#' with Sjbar under A=1.
#'
#' The efficient influence function has 6 terms accounting for:
#' \enumerate{
#'   \item Inverse probability weighted observed RMTL
#'   \item Predicted counterfactual RMTL
#'   \item Censoring correction for cumulative incidence
#'   \item Survival correction for RMTL
#'   \item Combined censoring and survival correction
#'   \item Complementary cause correction term
#' }
#'
#' @keywords internal
#' @noRd
double_ml_rmtlj_sep_direct_astar1 = function(id, a, time, event, bw, tilt, G.a0, G.a1, S.a0, S.a1, Sj.a0, Sjbar.a0, Sj.a1, Sjbar.a1, freq.time = NULL, admin.cens)
{
  n = length(id)
  cause = 1
  if (is.null(freq.time)) {
    s = sort(unique(time))
  } else {
    s = seq(freq.time, admin.cens, freq.time)
  }
  ns = length(s)
  ds = diff(c(0, s))

  S.a0 = t(na.locf(t(ifelse(S.a0 < 1e-3, 1e-3, S.a0))))
  S.a1 = t(na.locf(t(ifelse(S.a1 < 1e-3, 1e-3, S.a1))))
  G.a0 = t(na.locf(t(ifelse(G.a0 < 1e-3, 1e-3, G.a0))))
  G.a1 = t(na.locf(t(ifelse(G.a1 < 1e-3, 1e-3, G.a1))))
  Sj.a0 = t(na.locf(t(ifelse(Sj.a0 < 1e-3, 1e-3, Sj.a0))))
  Sj.a1 = t(na.locf(t(ifelse(Sj.a1 < 1e-3, 1e-3, Sj.a1))))
  Sjbar.a0 = t(na.locf(t(ifelse(Sjbar.a0 < 1e-3, 1e-3, Sjbar.a0))))
  Sjbar.a1 = t(na.locf(t(ifelse(Sjbar.a1 < 1e-3, 1e-3, Sjbar.a1))))
  G.dHazard.a0 = t(apply(cbind(0, -log(G.a0)), 1, diff))
  G.dHazard.a1 = t(apply(cbind(0, -log(G.a1)), 1, diff))
  S.dHazard.a0 = t(apply(cbind(0, -log(S.a0)), 1, diff))
  S.dHazard.a1 = t(apply(cbind(0, -log(S.a1)), 1, diff))
  Fj.dHazard.a0 = t(apply(cbind(0, -log(Sj.a0)), 1, diff))
  Fj.dHazard.a1 = t(apply(cbind(0, -log(Sj.a1)), 1, diff))
  Fjbar.dHazard.a0 = t(apply(cbind(0, -log(Sjbar.a0)), 1, diff))
  Fjbar.dHazard.a1 = t(apply(cbind(0, -log(Sjbar.a1)), 1, diff))

  Fj.a0 = t(apply(cbind(1, S.a0[, 1:(ns - 1)]) * Fj.dHazard.a0, 1, cumsum))
  Fj.a1 = t(apply(cbind(1, S.a1[, 1:(ns - 1)]) * Fj.dHazard.a1, 1, cumsum))
  Fj.a0.a1 = t(apply(cbind(1, winsorize_values(Sj.a0 * Sjbar.a1, lower = 1e-3, upper = 1)[, 1:(ns - 1)]) * Fj.dHazard.a0, 1, cumsum)) # not Fj.dHazard.a1!

  RMTLj.a0 = t(apply(Fj.a0 * matrix(ds, ncol = ns, nrow = n, byrow = TRUE), 1, cumsum))
  RMTLj.a1 = t(apply(Fj.a1 * matrix(ds, ncol = ns, nrow = n, byrow = TRUE), 1, cumsum))
  RMTLj.a0.a1 = t(apply(Fj.a0.a1 * matrix(ds, ncol = ns, nrow = n, byrow = TRUE), 1, cumsum))

  Yt = do.call(cbind, lapply(1:ns, function(u) ifelse(time >= s[u], 1, 0)))
  dNct = do.call(cbind, lapply(1:ns, function(u) (dplyr::near(s[u], time)) * (event == 0)))
  dNt = do.call(cbind, lapply(1:ns, function(u) (dplyr::near(s[u], time)) * (event != 0)))
  dNjt = do.call(cbind, lapply(1:ns, function(u) (dplyr::near(s[u], time)) * (event == 1)))
  dNjbart = do.call(cbind, lapply(1:ns, function(u) (dplyr::near(s[u], time)) * (event > 1)))

  # a=0
  term1.a0 = matrix(tilt, ncol = ns, nrow = n, byrow = FALSE) * RMTLj.a0.a1
  term2.a0 = matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Sjbar.a1 * (dNjt - Yt * Fj.dHazard.a0) / (Sjbar.a0 * G.a0), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Sjbar.a1 * (dNjt - Yt * Fj.dHazard.a0) / (Sjbar.a0 * G.a0), 1, cumsum)))
  term3.a0 = -matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (RMTLj.a0 * t(apply(Sjbar.a1 * (dNt - Yt * S.dHazard.a0) / (Sjbar.a0 * S.a0 * G.a0), 1, cumsum))
    - t(apply(RMTLj.a0 * Sjbar.a1 * (dNt - Yt * S.dHazard.a0) / (Sjbar.a0 * S.a0 * G.a0), 1, cumsum)))
  term4.a0 = matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Sjbar.a1 * Fj.a0 * (dNt - Yt * S.dHazard.a0) / (Sjbar.a0 * S.a0 * G.a0), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Sjbar.a1 * Fj.a0 * (dNt - Yt * S.dHazard.a0) / (Sjbar.a0 * S.a0 * G.a0), 1, cumsum)))
  term5.a0 = matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (RMTLj.a0.a1 * t(apply((dNjbart - Yt * Fjbar.dHazard.a0) / (S.a0 * G.a0), 1, cumsum))
    - t(apply(RMTLj.a0.a1 * (dNjbart - Yt * Fjbar.dHazard.a0) / (S.a0 * G.a0), 1, cumsum)))
  term6.a0 = -matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Fj.a0.a1 * (dNjbart - Yt * Fjbar.dHazard.a0) / (S.a0 * G.a0), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Fj.a0.a1 * (dNjbart - Yt * Fjbar.dHazard.a0) / (S.a0 * G.a0), 1, cumsum)))
  ueif.a0 = 1 / mean(bw * (a == 0)) * (term2.a0 + term3.a0 + term4.a0 + term5.a0 + term6.a0) + 1 / mean(tilt) * term1.a0

  # a=1
  term1.a1 = matrix(tilt, ncol = ns, nrow = n, byrow = FALSE) * RMTLj.a1
  term2.a1 = matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Sjbar.a1 * (dNjt - Yt * Fj.dHazard.a1) / (Sjbar.a1 * G.a1), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Sjbar.a1 * (dNjt - Yt * Fj.dHazard.a1) / (Sjbar.a1 * G.a1), 1, cumsum)))
  term3.a1 = -matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (RMTLj.a1 * t(apply(Sjbar.a1 * (dNt - Yt * S.dHazard.a1) / (Sjbar.a1 * S.a1 * G.a1), 1, cumsum))
    - t(apply(RMTLj.a1 * Sjbar.a1 * (dNt - Yt * S.dHazard.a1) / (Sjbar.a1 * S.a1 * G.a1), 1, cumsum)))
  term4.a1 = matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Sjbar.a1 * Fj.a1 * (dNt - Yt * S.dHazard.a1) / (Sjbar.a1 * S.a1 * G.a1), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Sjbar.a1 * Fj.a1 * (dNt - Yt * S.dHazard.a1) / (Sjbar.a1 * S.a1 * G.a1), 1, cumsum)))
  term5.a1 = matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (RMTLj.a0.a1 * t(apply((dNjbart - Yt * Fjbar.dHazard.a1) / (S.a1 * G.a1), 1, cumsum))
    - t(apply(RMTLj.a0.a1 * (dNjbart - Yt * Fjbar.dHazard.a1) / (S.a1 * G.a1), 1, cumsum)))
  term6.a1 = -matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Fj.a0.a1 * (dNjbart - Yt * Fjbar.dHazard.a1) / (S.a1 * G.a1), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Fj.a0.a1 * (dNjbart - Yt * Fjbar.dHazard.a1) / (S.a1 * G.a1), 1, cumsum)))
  ueif.a1 = 1 / mean(bw * (a == 1)) * (term2.a1 + term3.a1 + term4.a1 + term5.a1 + term6.a1) + 1 / mean(tilt) * term1.a1

  return(list(ueif.a1 = ueif.a1, ueif.a0 = ueif.a0))
}

#' @title Separable Indirect Effect for RMTLJ with A*=1
#'
#' @description
#' Computes efficient influence function for separable indirect effect of RMTL
#' with reference treatment A*=1 in competing risks mediation analysis.
#'
#' @param id (`numeric`) \\cr
#' Individual identifiers for observations.
#'
#' @param a (`numeric`) \\cr
#' Binary treatment indicator (0 or 1).
#'
#' @param time (`numeric`) \\cr
#' Observed time (minimum of event time and censoring time).
#'
#' @param event (`numeric`) \\cr
#' Event indicator (0 = censored, j = cause j event).
#'
#' @param bw (`numeric`) \\cr
#' Balancing weights (inverse propensity weights).
#'
#' @param tilt (`numeric`) \\cr
#' Tilting function for stabilization.
#'
#' @param G.a0, G.a1 (`matrix`) \\cr
#' Censoring survival function G(t|X,A=0) and G(t|X,A=1).
#'
#' @param S.a0, S.a1 (`matrix`) \\cr
#' Overall event-free survival function S(t|X,A=0) and S(t|X,A=1).
#'
#' @param Sj.a0, Sj.a1 (`matrix`) \\cr
#' Cause-specific survival for cause j: Sj(t|X,A=0) and Sj(t|X,A=1).
#'
#' @param Sjbar.a0, Sjbar.a1 (`matrix`) \\cr
#' Complementary cause survival (all causes except j): Sjbar(t|X,A=0) and Sjbar(t|X,A=1).
#'
#' @param freq.time (`numeric(1)`) \\cr
#' Time grid frequency. If NULL, uses unique event times.
#'
#' @param admin.cens (`numeric(1)`) \\cr
#' Administrative censoring time (maximum follow-up).
#'
#' @return List with elements:
#' \describe{
#'   \item{ueif.a1}{Efficient influence function for A=1 under separable indirect effect}
#'   \item{ueif.a0}{Efficient influence function for A=0 under separable indirect effect}
#' }
#'
#' @details
#' This function implements the separable indirect effect decomposition where the
#' treatment affects complementary causes while holding the effect on cause j at
#' reference level A*=1. The counterfactual RMTL combines Sj under A=1 with Sjbar
#' under the treatment level.
#'
#' The efficient influence function has 6 terms similar to the direct effect but
#' with different complementary cause weighting.
#'
#' @keywords internal
#' @noRd
double_ml_rmtlj_sep_indirect_astar1 = function(id, a, time, event, bw, tilt, G.a0, G.a1, S.a0, S.a1, Sj.a0, Sjbar.a0, Sj.a1, Sjbar.a1, freq.time = NULL, admin.cens)
{
  n = length(id)
  cause = 1
  if (is.null(freq.time)) {
    s = sort(unique(time))
  } else {
    s = seq(freq.time, admin.cens, freq.time)
  }
  ns = length(s)
  ds = diff(c(0, s))

  S.a0 = t(na.locf(t(ifelse(S.a0 < 1e-3, 1e-3, S.a0))))
  S.a1 = t(na.locf(t(ifelse(S.a1 < 1e-3, 1e-3, S.a1))))
  G.a0 = t(na.locf(t(ifelse(G.a0 < 1e-3, 1e-3, G.a0))))
  G.a1 = t(na.locf(t(ifelse(G.a1 < 1e-3, 1e-3, G.a1))))
  Sj.a0 = t(na.locf(t(ifelse(Sj.a0 < 1e-3, 1e-3, Sj.a0))))
  Sj.a1 = t(na.locf(t(ifelse(Sj.a1 < 1e-3, 1e-3, Sj.a1))))
  Sjbar.a0 = t(na.locf(t(ifelse(Sjbar.a0 < 1e-3, 1e-3, Sjbar.a0))))
  Sjbar.a1 = t(na.locf(t(ifelse(Sjbar.a1 < 1e-3, 1e-3, Sjbar.a1))))
  G.dHazard.a0 = t(apply(cbind(0, -log(G.a0)), 1, diff))
  G.dHazard.a1 = t(apply(cbind(0, -log(G.a1)), 1, diff))
  S.dHazard.a0 = t(apply(cbind(0, -log(S.a0)), 1, diff))
  S.dHazard.a1 = t(apply(cbind(0, -log(S.a1)), 1, diff))
  Fj.dHazard.a0 = t(apply(cbind(0, -log(Sj.a0)), 1, diff))
  Fj.dHazard.a1 = t(apply(cbind(0, -log(Sj.a1)), 1, diff))
  Fjbar.dHazard.a0 = t(apply(cbind(0, -log(Sjbar.a0)), 1, diff))
  Fjbar.dHazard.a1 = t(apply(cbind(0, -log(Sjbar.a1)), 1, diff))

  Fj.a0 = t(apply(cbind(1, S.a0[, 1:(ns - 1)]) * Fj.dHazard.a0, 1, cumsum))
  Fj.a1 = t(apply(cbind(1, S.a1[, 1:(ns - 1)]) * Fj.dHazard.a1, 1, cumsum))
  Fj.a0.a1 = t(apply(cbind(1, winsorize_values(Sj.a0 * Sjbar.a1, lower = 1e-3, upper = 1)[, 1:(ns - 1)]) * Fj.dHazard.a0, 1, cumsum)) # not Fj.dHazard.a1!

  RMTLj.a0 = t(apply(Fj.a0 * matrix(ds, ncol = ns, nrow = n, byrow = TRUE), 1, cumsum))
  RMTLj.a1 = t(apply(Fj.a1 * matrix(ds, ncol = ns, nrow = n, byrow = TRUE), 1, cumsum))
  RMTLj.a0.a1 = t(apply(Fj.a0.a1 * matrix(ds, ncol = ns, nrow = n, byrow = TRUE), 1, cumsum))

  Yt = do.call(cbind, lapply(1:ns, function(u) ifelse(time >= s[u], 1, 0)))
  dNct = do.call(cbind, lapply(1:ns, function(u) (dplyr::near(s[u], time)) * (event == 0)))
  dNt = do.call(cbind, lapply(1:ns, function(u) (dplyr::near(s[u], time)) * (event != 0)))
  dNjt = do.call(cbind, lapply(1:ns, function(u) (dplyr::near(s[u], time)) * (event == 1)))
  dNjbart = do.call(cbind, lapply(1:ns, function(u) (dplyr::near(s[u], time)) * (event > 1)))

  # a=0
  term1.a0 = matrix(tilt, ncol = ns, nrow = n, byrow = FALSE) * RMTLj.a0
  term2.a0 = matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply((dNjt - Yt * Fj.dHazard.a0) / (G.a0) * (1 - Sjbar.a1 / Sjbar.a0), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * (dNjt - Yt * Fj.dHazard.a0) / (G.a0) * (1 - Sjbar.a1 / Sjbar.a0), 1, cumsum)))
  term3.a0 = -matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (RMTLj.a0 * t(apply((dNt - Yt * S.dHazard.a0) / (S.a0 * G.a0) * (1 - Sjbar.a1 / Sjbar.a0), 1, cumsum))
    - t(apply(RMTLj.a0 * (dNt - Yt * S.dHazard.a0) / (S.a0 * G.a0) * (1 - Sjbar.a1 / Sjbar.a0), 1, cumsum)))
  term4.a0 = matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Fj.a0 * (dNt - Yt * S.dHazard.a0) / (S.a0 * G.a0) * (1 - Sjbar.a1 / Sjbar.a0), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Fj.a0 * (dNt - Yt * S.dHazard.a0) / (S.a0 * G.a0) * (1 - Sjbar.a1 / Sjbar.a0), 1, cumsum)))
  term5.a0 = -matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (RMTLj.a0.a1 * t(apply((dNjbart - Yt * Fjbar.dHazard.a0) / (S.a0 * G.a0), 1, cumsum))
    - t(apply(RMTLj.a0.a1 * (dNjbart - Yt * Fjbar.dHazard.a0) / (S.a0 * G.a0), 1, cumsum)))
  term6.a0 = matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Fj.a0.a1 * (dNjbart - Yt * Fjbar.dHazard.a0) / (S.a0 * G.a0), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Fj.a0.a1 * (dNjbart - Yt * Fjbar.dHazard.a0) / (S.a0 * G.a0), 1, cumsum)))
  ueif.a0 = 1 / mean(bw * (a == 0)) * (term2.a0 + term3.a0 + term4.a0 + term5.a0 + term6.a0) + 1 / mean(tilt) * term1.a0

  # a=1
  term1.a1 = matrix(tilt, ncol = ns, nrow = n, byrow = FALSE) * RMTLj.a0.a1
  term2.a1 = matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply((dNjt - Yt * Fj.dHazard.a1) / (G.a1) * (1 - Sjbar.a1 / Sjbar.a1), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * (dNjt - Yt * Fj.dHazard.a1) / (G.a1) * (1 - Sjbar.a1 / Sjbar.a1), 1, cumsum)))
  term3.a1 = -matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (RMTLj.a1 * t(apply((dNt - Yt * S.dHazard.a1) / (S.a1 * G.a1) * (1 - Sjbar.a1 / Sjbar.a1), 1, cumsum))
    - t(apply(RMTLj.a1 * (dNt - Yt * S.dHazard.a1) / (S.a1 * G.a1) * (1 - Sjbar.a1 / Sjbar.a1), 1, cumsum)))
  term4.a1 = matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Fj.a1 * (dNt - Yt * S.dHazard.a1) / (S.a1 * G.a1) * (1 - Sjbar.a1 / Sjbar.a1), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Fj.a1 * (dNt - Yt * S.dHazard.a1) / (S.a1 * G.a1) * (1 - Sjbar.a1 / Sjbar.a1), 1, cumsum)))
  term5.a1 = -matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (RMTLj.a0.a1 * t(apply((dNjbart - Yt * Fjbar.dHazard.a1) / (S.a1 * G.a1), 1, cumsum))
    - t(apply(RMTLj.a0.a1 * (dNjbart - Yt * Fjbar.dHazard.a1) / (S.a1 * G.a1), 1, cumsum)))
  term6.a1 = matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Fj.a0.a1 * (dNjbart - Yt * Fjbar.dHazard.a1) / (S.a1 * G.a1), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Fj.a0.a1 * (dNjbart - Yt * Fjbar.dHazard.a1) / (S.a1 * G.a1), 1, cumsum)))
  ueif.a1 = 1 / mean(bw * (a == 1)) * (term2.a1 + term3.a1 + term4.a1 + term5.a1 + term6.a1) + 1 / mean(tilt) * term1.a1

  return(list(ueif.a1 = ueif.a1, ueif.a0 = ueif.a0))
}

#' @title Separable Direct Effect for RMTLJ with A*=0
#'
#' @description
#' Computes efficient influence function for separable direct effect of RMTL
#' with reference treatment A*=0 in competing risks mediation analysis.
#'
#' @param id (`numeric`) \\cr
#' Individual identifiers for observations.
#'
#' @param a (`numeric`) \\cr
#' Binary treatment indicator (0 or 1).
#'
#' @param time (`numeric`) \\cr
#' Observed time (minimum of event time and censoring time).
#'
#' @param event (`numeric`) \\cr
#' Event indicator (0 = censored, j = cause j event).
#'
#' @param bw (`numeric`) \\cr
#' Balancing weights (inverse propensity weights).
#'
#' @param tilt (`numeric`) \\cr
#' Tilting function for stabilization.
#'
#' @param G.a0, G.a1 (`matrix`) \\cr
#' Censoring survival function G(t|X,A=0) and G(t|X,A=1).
#'
#' @param S.a0, S.a1 (`matrix`) \\cr
#' Overall event-free survival function S(t|X,A=0) and S(t|X,A=1).
#'
#' @param Sj.a0, Sj.a1 (`matrix`) \\cr
#' Cause-specific survival for cause j: Sj(t|X,A=0) and Sj(t|X,A=1).
#'
#' @param Sjbar.a0, Sjbar.a1 (`matrix`) \\cr
#' Complementary cause survival (all causes except j): Sjbar(t|X,A=0) and Sjbar(t|X,A=1).
#'
#' @param freq.time (`numeric(1)`) \\cr
#' Time grid frequency. If NULL, uses unique event times.
#'
#' @param admin.cens (`numeric(1)`) \\cr
#' Administrative censoring time (maximum follow-up).
#'
#' @return List with elements:
#' \describe{
#'   \item{ueif.a1}{Efficient influence function for A=1 under separable direct effect}
#'   \item{ueif.a0}{Efficient influence function for A=0 under separable direct effect}
#' }
#'
#' @details
#' This function implements the separable direct effect decomposition where the
#' treatment affects cause j directly while holding the effect on complementary
#' causes at reference level A*=0. The counterfactual RMTL combines Sj under A=1
#' with Sjbar under A=0.
#'
#' The efficient influence function has 6 terms, symmetric to the A*=1 version but
#' with reference level A*=0.
#'
#' @keywords internal
#' @noRd
double_ml_rmtlj_sep_direct_astar0 = function(id, a, time, event, bw, tilt, G.a0, G.a1, S.a0, S.a1, Sj.a0, Sjbar.a0, Sj.a1, Sjbar.a1, freq.time = NULL, admin.cens)
{
  n = length(id)
  cause = 1
  if (is.null(freq.time)) {
    s = sort(unique(time))
  } else {
    s = seq(freq.time, admin.cens, freq.time)
  }
  ns = length(s)
  ds = diff(c(0, s))

  S.a0 = t(na.locf(t(ifelse(S.a0 < 1e-3, 1e-3, S.a0))))
  S.a1 = t(na.locf(t(ifelse(S.a1 < 1e-3, 1e-3, S.a1))))
  G.a0 = t(na.locf(t(ifelse(G.a0 < 1e-3, 1e-3, G.a0))))
  G.a1 = t(na.locf(t(ifelse(G.a1 < 1e-3, 1e-3, G.a1))))
  Sj.a0 = t(na.locf(t(ifelse(Sj.a0 < 1e-3, 1e-3, Sj.a0))))
  Sj.a1 = t(na.locf(t(ifelse(Sj.a1 < 1e-3, 1e-3, Sj.a1))))
  Sjbar.a0 = t(na.locf(t(ifelse(Sjbar.a0 < 1e-3, 1e-3, Sjbar.a0))))
  Sjbar.a1 = t(na.locf(t(ifelse(Sjbar.a1 < 1e-3, 1e-3, Sjbar.a1))))
  G.dHazard.a0 = t(apply(cbind(0, -log(G.a0)), 1, diff))
  G.dHazard.a1 = t(apply(cbind(0, -log(G.a1)), 1, diff))
  S.dHazard.a0 = t(apply(cbind(0, -log(S.a0)), 1, diff))
  S.dHazard.a1 = t(apply(cbind(0, -log(S.a1)), 1, diff))
  Fj.dHazard.a0 = t(apply(cbind(0, -log(Sj.a0)), 1, diff))
  Fj.dHazard.a1 = t(apply(cbind(0, -log(Sj.a1)), 1, diff))
  Fjbar.dHazard.a0 = t(apply(cbind(0, -log(Sjbar.a0)), 1, diff))
  Fjbar.dHazard.a1 = t(apply(cbind(0, -log(Sjbar.a1)), 1, diff))

  Fj.a0 = t(apply(cbind(1, S.a0[, 1:(ns - 1)]) * Fj.dHazard.a0, 1, cumsum))
  Fj.a1 = t(apply(cbind(1, S.a1[, 1:(ns - 1)]) * Fj.dHazard.a1, 1, cumsum))
  Fj.a1.a0 = t(apply(cbind(1, winsorize_values(Sj.a1 * Sjbar.a0, lower = 1e-3, upper = 1)[, 1:(ns - 1)]) * Fj.dHazard.a1, 1, cumsum)) # not Fj.dHazard.a1!

  RMTLj.a0 = t(apply(Fj.a0 * matrix(ds, ncol = ns, nrow = n, byrow = TRUE), 1, cumsum))
  RMTLj.a1 = t(apply(Fj.a1 * matrix(ds, ncol = ns, nrow = n, byrow = TRUE), 1, cumsum))
  RMTLj.a1.a0 = t(apply(Fj.a1.a0 * matrix(ds, ncol = ns, nrow = n, byrow = TRUE), 1, cumsum))

  Yt = do.call(cbind, lapply(1:ns, function(u) ifelse(time >= s[u], 1, 0)))
  dNct = do.call(cbind, lapply(1:ns, function(u) (dplyr::near(s[u], time)) * (event == 0)))
  dNt = do.call(cbind, lapply(1:ns, function(u) (dplyr::near(s[u], time)) * (event != 0)))
  dNjt = do.call(cbind, lapply(1:ns, function(u) (dplyr::near(s[u], time)) * (event == 1)))
  dNjbart = do.call(cbind, lapply(1:ns, function(u) (dplyr::near(s[u], time)) * (event > 1)))

  # a=0
  term1.a0 = matrix(tilt, ncol = ns, nrow = n, byrow = FALSE) * RMTLj.a0
  term2.a0 = matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Sjbar.a0 * (dNjt - Yt * Fj.dHazard.a0) / (Sjbar.a0 * G.a0), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Sjbar.a0 * (dNjt - Yt * Fj.dHazard.a0) / (Sjbar.a0 * G.a0), 1, cumsum)))
  term3.a0 = -matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (RMTLj.a0 * t(apply(Sjbar.a0 * (dNt - Yt * S.dHazard.a0) / (Sjbar.a0 * S.a0 * G.a0), 1, cumsum))
    - t(apply(RMTLj.a0 * Sjbar.a0 * (dNt - Yt * S.dHazard.a0) / (Sjbar.a0 * S.a0 * G.a0), 1, cumsum)))
  term4.a0 = matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Sjbar.a0 * Fj.a0 * (dNt - Yt * S.dHazard.a0) / (Sjbar.a0 * S.a0 * G.a0), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Sjbar.a0 * Fj.a0 * (dNt - Yt * S.dHazard.a0) / (Sjbar.a0 * S.a0 * G.a0), 1, cumsum)))
  term5.a0 = matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (RMTLj.a1.a0 * t(apply((dNjbart - Yt * Fjbar.dHazard.a0) / (S.a0 * G.a0), 1, cumsum))
    - t(apply(RMTLj.a1.a0 * (dNjbart - Yt * Fjbar.dHazard.a0) / (S.a0 * G.a0), 1, cumsum)))
  term6.a0 = -matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Fj.a1.a0 * (dNjbart - Yt * Fjbar.dHazard.a0) / (S.a0 * G.a0), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Fj.a1.a0 * (dNjbart - Yt * Fjbar.dHazard.a0) / (S.a0 * G.a0), 1, cumsum)))
  ueif.a0 = 1 / mean(bw * (a == 0)) * (term2.a0 + term3.a0 + term4.a0 + term5.a0 + term6.a0) + 1 / mean(tilt) * term1.a0

  # a=1
  term1.a1 = matrix(tilt, ncol = ns, nrow = n, byrow = FALSE) * RMTLj.a1.a0
  term2.a1 = matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Sjbar.a0 * (dNjt - Yt * Fj.dHazard.a1) / (Sjbar.a1 * G.a1), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Sjbar.a0 * (dNjt - Yt * Fj.dHazard.a1) / (Sjbar.a1 * G.a1), 1, cumsum)))
  term3.a1 = -matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (RMTLj.a1 * t(apply(Sjbar.a0 * (dNt - Yt * S.dHazard.a1) / (Sjbar.a1 * S.a1 * G.a1), 1, cumsum))
    - t(apply(RMTLj.a1 * Sjbar.a0 * (dNt - Yt * S.dHazard.a1) / (Sjbar.a1 * S.a1 * G.a1), 1, cumsum)))
  term4.a1 = matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Sjbar.a0 * Fj.a1 * (dNt - Yt * S.dHazard.a1) / (Sjbar.a1 * S.a1 * G.a1), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Sjbar.a0 * Fj.a1 * (dNt - Yt * S.dHazard.a1) / (Sjbar.a1 * S.a1 * G.a1), 1, cumsum)))
  term5.a1 = matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (RMTLj.a1.a0 * t(apply((dNjbart - Yt * Fjbar.dHazard.a1) / (S.a1 * G.a1), 1, cumsum))
    - t(apply(RMTLj.a1.a0 * (dNjbart - Yt * Fjbar.dHazard.a1) / (S.a1 * G.a1), 1, cumsum)))
  term6.a1 = -matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Fj.a1.a0 * (dNjbart - Yt * Fjbar.dHazard.a1) / (S.a1 * G.a1), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Fj.a1.a0 * (dNjbart - Yt * Fjbar.dHazard.a1) / (S.a1 * G.a1), 1, cumsum)))
  ueif.a1 = 1 / mean(bw * (a == 1)) * (term2.a1 + term3.a1 + term4.a1 + term5.a1 + term6.a1) + 1 / mean(tilt) * term1.a1

  return(list(ueif.a1 = ueif.a1, ueif.a0 = ueif.a0))
}

#' @title Separable Indirect Effect for RMTLJ with A*=0
#'
#' @description
#' Computes efficient influence function for separable indirect effect of RMTL
#' with reference treatment A*=0 in competing risks mediation analysis.
#'
#' @param id (`numeric`) \\cr
#' Individual identifiers for observations.
#'
#' @param a (`numeric`) \\cr
#' Binary treatment indicator (0 or 1).
#'
#' @param time (`numeric`) \\cr
#' Observed time (minimum of event time and censoring time).
#'
#' @param event (`numeric`) \\cr
#' Event indicator (0 = censored, j = cause j event).
#'
#' @param bw (`numeric`) \\cr
#' Balancing weights (inverse propensity weights).
#'
#' @param tilt (`numeric`) \\cr
#' Tilting function for stabilization.
#'
#' @param G.a0, G.a1 (`matrix`) \\cr
#' Censoring survival function G(t|X,A=0) and G(t|X,A=1).
#'
#' @param S.a0, S.a1 (`matrix`) \\cr
#' Overall event-free survival function S(t|X,A=0) and S(t|X,A=1).
#'
#' @param Sj.a0, Sj.a1 (`matrix`) \\cr
#' Cause-specific survival for cause j: Sj(t|X,A=0) and Sj(t|X,A=1).
#'
#' @param Sjbar.a0, Sjbar.a1 (`matrix`) \\cr
#' Complementary cause survival (all causes except j): Sjbar(t|X,A=0) and Sjbar(t|X,A=1).
#'
#' @param freq.time (`numeric(1)`) \\cr
#' Time grid frequency. If NULL, uses unique event times.
#'
#' @param admin.cens (`numeric(1)`) \\cr
#' Administrative censoring time (maximum follow-up).
#'
#' @return List with elements:
#' \describe{
#'   \item{ueif.a1}{Efficient influence function for A=1 under separable indirect effect}
#'   \item{ueif.a0}{Efficient influence function for A=0 under separable indirect effect}
#' }
#'
#' @details
#' This function implements the separable indirect effect decomposition where the
#' treatment affects complementary causes while holding the effect on cause j at
#' reference level A*=0. The counterfactual RMTL combines Sj under A=0 with Sjbar
#' under the treatment level.
#'
#' The efficient influence function has 6 terms, symmetric to the A*=1 version but
#' with different complementary cause weighting for reference level A*=0.
#'
#' @keywords internal
#' @noRd
double_ml_rmtlj_sep_indirect_astar0 = function(id, a, time, event, bw, tilt, G.a0, G.a1, S.a0, S.a1, Sj.a0, Sjbar.a0, Sj.a1, Sjbar.a1, freq.time = NULL, admin.cens)
{
  n = length(id)
  cause = 1
  if (is.null(freq.time)) {
    s = sort(unique(time))
  } else {
    s = seq(freq.time, admin.cens, freq.time)
  }
  ns = length(s)
  ds = diff(c(0, s))

  S.a0 = t(na.locf(t(ifelse(S.a0 < 1e-3, 1e-3, S.a0))))
  S.a1 = t(na.locf(t(ifelse(S.a1 < 1e-3, 1e-3, S.a1))))
  G.a0 = t(na.locf(t(ifelse(G.a0 < 1e-3, 1e-3, G.a0))))
  G.a1 = t(na.locf(t(ifelse(G.a1 < 1e-3, 1e-3, G.a1))))
  Sj.a0 = t(na.locf(t(ifelse(Sj.a0 < 1e-3, 1e-3, Sj.a0))))
  Sj.a1 = t(na.locf(t(ifelse(Sj.a1 < 1e-3, 1e-3, Sj.a1))))
  Sjbar.a0 = t(na.locf(t(ifelse(Sjbar.a0 < 1e-3, 1e-3, Sjbar.a0))))
  Sjbar.a1 = t(na.locf(t(ifelse(Sjbar.a1 < 1e-3, 1e-3, Sjbar.a1))))
  G.dHazard.a0 = t(apply(cbind(0, -log(G.a0)), 1, diff))
  G.dHazard.a1 = t(apply(cbind(0, -log(G.a1)), 1, diff))
  S.dHazard.a0 = t(apply(cbind(0, -log(S.a0)), 1, diff))
  S.dHazard.a1 = t(apply(cbind(0, -log(S.a1)), 1, diff))
  Fj.dHazard.a0 = t(apply(cbind(0, -log(Sj.a0)), 1, diff))
  Fj.dHazard.a1 = t(apply(cbind(0, -log(Sj.a1)), 1, diff))
  Fjbar.dHazard.a0 = t(apply(cbind(0, -log(Sjbar.a0)), 1, diff))
  Fjbar.dHazard.a1 = t(apply(cbind(0, -log(Sjbar.a1)), 1, diff))

  Fj.a0 = t(apply(cbind(1, S.a0[, 1:(ns - 1)]) * Fj.dHazard.a0, 1, cumsum))
  Fj.a1 = t(apply(cbind(1, S.a1[, 1:(ns - 1)]) * Fj.dHazard.a1, 1, cumsum))
  Fj.a1.a0 = t(apply(cbind(1, winsorize_values(Sj.a1 * Sjbar.a0, lower = 1e-3, upper = 1)[, 1:(ns - 1)]) * Fj.dHazard.a1, 1, cumsum)) # not Fj.dHazard.a1!

  RMTLj.a0 = t(apply(Fj.a0 * matrix(ds, ncol = ns, nrow = n, byrow = TRUE), 1, cumsum))
  RMTLj.a1 = t(apply(Fj.a1 * matrix(ds, ncol = ns, nrow = n, byrow = TRUE), 1, cumsum))
  RMTLj.a1.a0 = t(apply(Fj.a1.a0 * matrix(ds, ncol = ns, nrow = n, byrow = TRUE), 1, cumsum))

  Yt = do.call(cbind, lapply(1:ns, function(u) ifelse(time >= s[u], 1, 0)))
  dNct = do.call(cbind, lapply(1:ns, function(u) (dplyr::near(s[u], time)) * (event == 0)))
  dNt = do.call(cbind, lapply(1:ns, function(u) (dplyr::near(s[u], time)) * (event != 0)))
  dNjt = do.call(cbind, lapply(1:ns, function(u) (dplyr::near(s[u], time)) * (event == 1)))
  dNjbart = do.call(cbind, lapply(1:ns, function(u) (dplyr::near(s[u], time)) * (event > 1)))

  # a=0
  term1.a0 = matrix(tilt, ncol = ns, nrow = n, byrow = FALSE) * RMTLj.a1.a0
  term2.a0 = matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply((dNjt - Yt * Fj.dHazard.a0) / (G.a0) * (1 - Sjbar.a0 / Sjbar.a0), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * (dNjt - Yt * Fj.dHazard.a0) / (G.a0) * (1 - Sjbar.a0 / Sjbar.a0), 1, cumsum)))
  term3.a0 = -matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (RMTLj.a0 * t(apply((dNt - Yt * S.dHazard.a0) / (S.a0 * G.a0) * (1 - Sjbar.a0 / Sjbar.a0), 1, cumsum))
    - t(apply(RMTLj.a0 * (dNt - Yt * S.dHazard.a0) / (S.a0 * G.a0) * (1 - Sjbar.a0 / Sjbar.a0), 1, cumsum)))
  term4.a0 = matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Fj.a0 * (dNt - Yt * S.dHazard.a0) / (S.a0 * G.a0) * (1 - Sjbar.a0 / Sjbar.a0), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Fj.a0 * (dNt - Yt * S.dHazard.a0) / (S.a0 * G.a0) * (1 - Sjbar.a0 / Sjbar.a0), 1, cumsum)))
  term5.a0 = -matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (RMTLj.a1.a0 * t(apply((dNjbart - Yt * Fjbar.dHazard.a0) / (S.a0 * G.a0), 1, cumsum))
    - t(apply(RMTLj.a1.a0 * (dNjbart - Yt * Fjbar.dHazard.a0) / (S.a0 * G.a0), 1, cumsum)))
  term6.a0 = matrix(bw * (a == 0), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Fj.a1.a0 * (dNjbart - Yt * Fjbar.dHazard.a0) / (S.a0 * G.a0), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Fj.a1.a0 * (dNjbart - Yt * Fjbar.dHazard.a0) / (S.a0 * G.a0), 1, cumsum)))
  ueif.a0 = 1 / mean(bw * (a == 0)) * (term2.a0 + term3.a0 + term4.a0 + term5.a0 + term6.a0) + 1 / mean(tilt) * term1.a0

  # a=1
  term1.a1 = matrix(tilt, ncol = ns, nrow = n, byrow = FALSE) * RMTLj.a1
  term2.a1 = matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply((dNjt - Yt * Fj.dHazard.a1) / (G.a1) * (1 - Sjbar.a0 / Sjbar.a1), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * (dNjt - Yt * Fj.dHazard.a1) / (G.a1) * (1 - Sjbar.a0 / Sjbar.a1), 1, cumsum)))
  term3.a1 = -matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (RMTLj.a1 * t(apply((dNt - Yt * S.dHazard.a1) / (S.a1 * G.a1) * (1 - Sjbar.a0 / Sjbar.a1), 1, cumsum))
    - t(apply(RMTLj.a1 * (dNt - Yt * S.dHazard.a1) / (S.a1 * G.a1) * (1 - Sjbar.a0 / Sjbar.a1), 1, cumsum)))
  term4.a1 = matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Fj.a1 * (dNt - Yt * S.dHazard.a1) / (S.a1 * G.a1) * (1 - Sjbar.a0 / Sjbar.a1), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Fj.a1 * (dNt - Yt * S.dHazard.a1) / (S.a1 * G.a1) * (1 - Sjbar.a0 / Sjbar.a1), 1, cumsum)))
  term5.a1 = -matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (RMTLj.a1.a0 * t(apply((dNjbart - Yt * Fjbar.dHazard.a1) / (S.a1 * G.a1), 1, cumsum))
    - t(apply(RMTLj.a1.a0 * (dNjbart - Yt * Fjbar.dHazard.a1) / (S.a1 * G.a1), 1, cumsum)))
  term6.a1 = matrix(bw * (a == 1), ncol = ns, nrow = n, byrow = FALSE) * (matrix(s, ncol = ns, nrow = n, byrow = TRUE) * t(apply(Fj.a1.a0 * (dNjbart - Yt * Fjbar.dHazard.a1) / (S.a1 * G.a1), 1, cumsum))
    - t(apply(matrix(s, ncol = ns, nrow = n, byrow = TRUE) * Fj.a1.a0 * (dNjbart - Yt * Fjbar.dHazard.a1) / (S.a1 * G.a1), 1, cumsum)))
  ueif.a1 = 1 / mean(bw * (a == 1)) * (term2.a1 + term3.a1 + term4.a1 + term5.a1 + term6.a1) + 1 / mean(tilt) * term1.a1

  return(list(ueif.a1 = ueif.a1, ueif.a0 = ueif.a0))
}

#' @title Double Machine Learning for Restricted Mean Time Lost in Competing Risks
#'
#' @description
#' R6 class for double machine learning estimation of weighted average treatment
#' effects (WATE) on restricted mean time lost (RMTL) to a specific cause in
#' competing risks settings.
#'
#' @details
#' This class implements double machine learning estimation for the restricted
#' mean time lost (RMTL) to cause j before time tau in competing risks analysis.
#' RMTL measures the expected time lost to a specific cause before the restriction
#' time tau.
#'
#' The estimator uses efficient influence functions (UEIF) with cross-fitting to
#' obtain doubly robust estimates of the average treatment effect:
#' \deqn{ATE(\tau) = E[RMTL_j(\tau)|A=1] - E[RMTL_j(\tau)|A=0]}
#'
#' Nuisance functions estimated via machine learning:
#' * Overall event survival: S(t|X,A)
#' * Cause-specific survival: Sj(t|X,A)
#' * Complementary cause survival: Sjbar(t|X,A)
#' * Censoring survival: G(t|X,A)
#' * Treatment propensity: P(A=1|X)
#'
#' @examples
#' \dontrun{
#' library(DoubleML)
#' library(SuperLearner)
#' library(survSuperLearner)
#'
#' # Generate competing risks data
#' set.seed(123)
#' data = make_competing_data(n_obs = 2000, admin_cens = 10)
#'
#' # Define learner libraries
#' ml_g_surv = c("survSL.km", "survSL.coxph")
#' ml_g_cens = c("survSL.km", "survSL.coxph")
#' ml_m = c("SL.mean", "SL.glm")
#'
#' # Estimate RMTL treatment effect
#' dml_rmtlj = DoubleMLRMTLJ$new(
#'   data,
#'   ml_g_surv = ml_g_surv,
#'   ml_g_surv_j = ml_g_surv,
#'   ml_g_surv_jbar = ml_g_surv,
#'   ml_g_cens = ml_g_cens,
#'   ml_m = ml_m,
#'   admin_cens = 10,
#'   time_col = "time",
#'   event_col = "event",
#'   tau = 4,
#'   n_folds = 2
#' )
#'
#' dml_rmtlj$fit()
#' print(dml_rmtlj$coef)
#' print(dml_rmtlj$se)
#' }
#'
#' @export
DoubleMLRMTLJ = R6Class("DoubleMLRMTLJ",
  inherit = DoubleMLCompeting,
  public = list(
    #' @description
    #' Creates a new instance of this R6 class.
    #'
    #' @param data (`DoubleMLData`) \cr
    #' The `DoubleMLData` object providing the competing risks data.
    #'
    #' @param ml_g_surv (character vector) \cr
    #' survSuperLearner library for overall event survival S(t|X,A).
    #'
    #' @param ml_g_surv_j (character vector) \cr
    #' survSuperLearner library for cause-specific survival Sj(t|X,A).
    #'
    #' @param ml_g_surv_jbar (character vector) \cr
    #' survSuperLearner library for complementary cause survival Sjbar(t|X,A).
    #'
    #' @param ml_g_cens (character vector) \cr
    #' survSuperLearner library for censoring survival G(t|X,A).
    #'
    #' @param ml_m (character vector) \cr
    #' SuperLearner library for treatment propensity P(A=1|X).
    #'
    #' @param admin_cens (`numeric(1)`) \cr
    #' Administrative censoring time.
    #'
    #' @param time_col (`character(1)`) \cr
    #' Name of the time column in the data.
    #'
    #' @param event_col (`character(1)`) \cr
    #' Name of the event column (0=censored, 1=cause 1, 2=cause 2, ...).
    #'
    #' @param cause (`integer(1)`) \cr
    #' Cause of interest (default is 1).
    #'
    #' @param tau (`numeric(1)` or `numeric(vector)`) \cr
    #' Evaluation time point(s) for RMTL estimation. Default is admin_cens.
    #'
    #' @param freq_time (`numeric(1)`, optional) \cr
    #' Time grid frequency. If NULL, uses unique event times.
    #'
    #' @param ml_framework (`character(1)`) \cr
    #' Machine learning framework. Only "SuperLearner" supported. Default is "SuperLearner".
    #'
    #' @param n_folds (`integer(1)`) \cr
    #' Number of folds for cross-fitting. Default is 2.
    #'
    #' @param n_rep (`integer(1)`) \cr
    #' Number of repetitions for sample splitting. Default is 1.
    #'
    #' @param score (`character(1)`) \cr
    #' Score function. Default is "ueif".
    #'
    #' @param dml_procedure (`character(1)`) \cr
    #' DML procedure. Default is "dml2".
    #'
    #' @param draw_sample_splitting (`logical(1)`) \cr
    #' Whether to draw sample splitting. Default is TRUE.
    #'
    #' @param apply_cross_fitting (`logical(1)`) \cr
    #' Whether to apply cross-fitting. Default is TRUE.
    initialize = function(data,
      ml_g_surv,
      ml_g_surv_j,
      ml_g_surv_jbar,
      ml_g_cens,
      ml_m,
      admin_cens,
      time_col,
      event_col,
      cause = 1,
      tau = NULL,
      freq_time = NULL,
      ml_framework = "SuperLearner",
      n_folds = 2,
      n_rep = 1,
      score = "ueif",
      dml_procedure = "dml2",
      draw_sample_splitting = TRUE,
      apply_cross_fitting = TRUE) {

      # Set tau to admin_cens if not specified
      private$tau_ = if (is.null(tau)) admin_cens else tau

      # Validate tau values
      private$validate_tau(admin_cens)

      # Initialize parent DoubleMLCompeting class
      super$initialize(
        data = data,
        ml_g_surv = ml_g_surv,
        ml_g_surv_j = ml_g_surv_j,
        ml_g_surv_jbar = ml_g_surv_jbar,
        ml_g_cens = ml_g_cens,
        ml_m = ml_m,
        admin_cens = admin_cens,
        time_col = time_col,
        event_col = event_col,
        cause = cause,
        freq_time = freq_time,
        ml_framework = ml_framework,
        n_folds = n_folds,
        n_rep = n_rep,
        score = score,
        dml_procedure = dml_procedure,
        draw_sample_splitting = draw_sample_splitting,
        apply_cross_fitting = apply_cross_fitting
      )
    },

    #' @description
    #' Compute and store separable direct effect with reference treatment A*=1.
    #' Results accessible via \code{$sep_direct_astar1_estimates}.
    get_separable_direct_astar1 = function() {
      private$compute_separable_direct_astar1()
      invisible(NULL)
    },

    #' @description
    #' Compute and store separable indirect effect with reference treatment A*=1.
    #' Results accessible via \code{$sep_indirect_astar1_estimates}.
    get_separable_indirect_astar1 = function() {
      private$compute_separable_indirect_astar1()
      invisible(NULL)
    },

    #' @description
    #' Compute and store separable direct effect with reference treatment A*=0.
    #' Results accessible via \code{$sep_direct_astar0_estimates}.
    get_separable_direct_astar0 = function() {
      private$compute_separable_direct_astar0()
      invisible(NULL)
    },

    #' @description
    #' Compute and store separable indirect effect with reference treatment A*=0.
    #' Results accessible via \code{$sep_indirect_astar0_estimates}.
    get_separable_indirect_astar0 = function() {
      private$compute_separable_indirect_astar0()
      invisible(NULL)
    },

    #' @description
    #' Get full-curve inference for the restricted mean time lost difference.
    #' Returns pointwise and simultaneous confidence bands across all time points.
    #'
    #' @param npath (`integer(1)`) \cr
    #' Number of bootstrap paths for simultaneous confidence bands. Default is 100.
    #'
    #' @return Data frame with point estimates, standard errors, confidence intervals,
    #' and simultaneous confidence bands at each time point.
    get_inference_curve = function(npath = 100) {
      if (is.null(private$all_ueif_a1_)) {
        stop("Model must be fitted first. Call fit() method.")
      }
      rmtlj_inference(
        npath = npath,
        ueif.a1.list = list(private$all_ueif_a1_),
        ueif.a0.list = list(private$all_ueif_a0_),
        time.list = list(private$time_grid_)
      )
    },

    #' @description
    #' Compute inference curve with simultaneous confidence bands for separable direct effect (A*=1).
    #' Requires \code{get_separable_direct_astar1()} to be called first.
    #' @param npath Number of bootstrap paths for confidence bands.
    #' @return Data frame with point estimates, confidence intervals, and confidence bands.
    get_inference_curve_sep_direct_astar1 = function(npath = 100) {
      if (is.null(private$sep_direct_astar1_ueif_a1_)) {
        stop("Separable direct A*=1 effect must be computed first. Call get_separable_direct_astar1().")
      }
      rmtlj_inference(
        npath = npath,
        ueif.a1.list = list(private$sep_direct_astar1_ueif_a1_),
        ueif.a0.list = list(private$sep_direct_astar1_ueif_a0_),
        time.list = list(private$time_grid_)
      )
    },

    #' @description
    #' Compute inference curve with simultaneous confidence bands for separable indirect effect (A*=1).
    #' Requires \code{get_separable_indirect_astar1()} to be called first.
    #' @param npath Number of bootstrap paths for confidence bands.
    #' @return Data frame with point estimates, confidence intervals, and confidence bands.
    get_inference_curve_sep_indirect_astar1 = function(npath = 100) {
      if (is.null(private$sep_indirect_astar1_ueif_a1_)) {
        stop("Separable indirect A*=1 effect must be computed first. Call get_separable_indirect_astar1().")
      }
      rmtlj_inference(
        npath = npath,
        ueif.a1.list = list(private$sep_indirect_astar1_ueif_a1_),
        ueif.a0.list = list(private$sep_indirect_astar1_ueif_a0_),
        time.list = list(private$time_grid_)
      )
    },

    #' @description
    #' Compute inference curve with simultaneous confidence bands for separable direct effect (A*=0).
    #' Requires \code{get_separable_direct_astar0()} to be called first.
    #' @param npath Number of bootstrap paths for confidence bands.
    #' @return Data frame with point estimates, confidence intervals, and confidence bands.
    get_inference_curve_sep_direct_astar0 = function(npath = 100) {
      if (is.null(private$sep_direct_astar0_ueif_a1_)) {
        stop("Separable direct A*=0 effect must be computed first. Call get_separable_direct_astar0().")
      }
      rmtlj_inference(
        npath = npath,
        ueif.a1.list = list(private$sep_direct_astar0_ueif_a1_),
        ueif.a0.list = list(private$sep_direct_astar0_ueif_a0_),
        time.list = list(private$time_grid_)
      )
    },

    #' @description
    #' Compute inference curve with simultaneous confidence bands for separable indirect effect (A*=0).
    #' Requires \code{get_separable_indirect_astar0()} to be called first.
    #' @param npath Number of bootstrap paths for confidence bands.
    #' @return Data frame with point estimates, confidence intervals, and confidence bands.
    get_inference_curve_sep_indirect_astar0 = function(npath = 100) {
      if (is.null(private$sep_indirect_astar0_ueif_a1_)) {
        stop("Separable indirect A*=0 effect must be computed first. Call get_separable_indirect_astar0().")
      }
      rmtlj_inference(
        npath = npath,
        ueif.a1.list = list(private$sep_indirect_astar0_ueif_a1_),
        ueif.a0.list = list(private$sep_indirect_astar0_ueif_a0_),
        time.list = list(private$time_grid_)
      )
    }
  ),

  active = list(
    #' @field coef Treatment effect estimate (from extract_average).
    #' For multiple tau, returns the first tau's estimate; use rmtlj_estimates for all.
    coef = function() {
      if (!is.null(private$rmtlj_estimates_)) {
        est = private$rmtlj_estimates_$estimate.diff
        if (length(est) > 1) {
          return(est[1])
        }
        return(est)
      }
      return(private$coef_)
    },

    #' @field se Standard error of treatment effect estimate (from extract_average).
    #' For multiple tau, returns the first tau's SE; use rmtlj_estimates for all.
    se = function() {
      if (!is.null(private$rmtlj_estimates_)) {
        se_val = private$rmtlj_estimates_$se.diff
        if (length(se_val) > 1) {
          return(se_val[1])
        }
        return(se_val)
      }
      return(private$se_)
    },

    #' @field tau Evaluation time point(s)
    tau = function(value) {
      if (missing(value)) {
        return(private$tau_)
      } else {
        private$tau_ = value
        private$validate_tau(private$admin_cens_)
      }
    },

    #' @field rmtlj_estimates All RMTL estimates and standard errors
    rmtlj_estimates = function() private$rmtlj_estimates_,

    #' @field sep_direct_astar1_estimates Separable direct effect estimates (A*=1)
    sep_direct_astar1_estimates = function() private$sep_direct_astar1_estimates_,

    #' @field sep_indirect_astar1_estimates Separable indirect effect estimates (A*=1)
    sep_indirect_astar1_estimates = function() private$sep_indirect_astar1_estimates_,

    #' @field sep_direct_astar0_estimates Separable direct effect estimates (A*=0)
    sep_direct_astar0_estimates = function() private$sep_direct_astar0_estimates_,

    #' @field sep_indirect_astar0_estimates Separable indirect effect estimates (A*=0)
    sep_indirect_astar0_estimates = function() private$sep_indirect_astar0_estimates_
  ),

  private = list(
    tau_ = NULL,
    coef_ = NULL,
    se_ = NULL,
    rmtlj_estimates_ = NULL,
    sep_direct_astar1_estimates_ = NULL,
    sep_indirect_astar1_estimates_ = NULL,
    sep_direct_astar0_estimates_ = NULL,
    sep_indirect_astar0_estimates_ = NULL,
    all_ueif_a1_ = NULL,
    all_ueif_a0_ = NULL,
    time_grid_ = NULL,
    all_nuisance_ = NULL,
    sep_direct_astar1_ueif_a1_ = NULL,
    sep_direct_astar1_ueif_a0_ = NULL,
    sep_indirect_astar1_ueif_a1_ = NULL,
    sep_indirect_astar1_ueif_a0_ = NULL,
    sep_direct_astar0_ueif_a1_ = NULL,
    sep_direct_astar0_ueif_a0_ = NULL,
    sep_indirect_astar0_ueif_a1_ = NULL,
    sep_indirect_astar0_ueif_a0_ = NULL,

    validate_tau = function(admin_cens) {
      if (any(private$tau_ <= 0)) {
        stop("tau must be positive")
      }
      if (any(private$tau_ > admin_cens)) {
        stop("tau must be <= admin_cens")
      }
      invisible(NULL)
    },

    compute_separable_direct_astar1 = function() {
      if (is.null(private$all_nuisance_)) {
        stop("Model must be fitted first. Call fit() before computing separable effects.")
      }

      n_obs = self$data$n_obs
      time_grid = self$get_time_grid()
      id = 1:n_obs
      a = private$all_nuisance_$a
      time = private$all_nuisance_$time
      event = private$all_nuisance_$event
      propensity = private$all_nuisance_$propensity
      bw = ifelse(a == 1, 1 / propensity, 1 / (1 - propensity))
      tilt = rep(1, n_obs)

      ueif_result = double_ml_rmtlj_sep_direct_astar1(
        id = id, a = a, time = time, event = event,
        bw = bw, tilt = tilt,
        G.a0 = private$all_nuisance_$G_a0, G.a1 = private$all_nuisance_$G_a1,
        S.a0 = private$all_nuisance_$S_a0, S.a1 = private$all_nuisance_$S_a1,
        Sj.a0 = private$all_nuisance_$Sj_a0, Sj.a1 = private$all_nuisance_$Sj_a1,
        Sjbar.a0 = private$all_nuisance_$Sjbar_a0, Sjbar.a1 = private$all_nuisance_$Sjbar_a1,
        freq.time = NULL, admin.cens = self$admin_cens
      )

      ueif_a1_sep = ueif_result$ueif.a1
      ueif_a0_sep = ueif_result$ueif.a0
      private$sep_direct_astar1_ueif_a1_ = ueif_a1_sep
      private$sep_direct_astar1_ueif_a0_ = ueif_a0_sep

      if (length(private$tau_) == 1) {
        raw_results = rmtlj_extract_average(
          ueif.a1.list = list(ueif_a1_sep),
          ueif.a0.list = list(ueif_a0_sep),
          time.list = list(time_grid),
          tau = private$tau_
        )
        results = list(
          tau = private$tau_,
          estimate.a0 = raw_results$estimate.a0,
          estimate.a1 = raw_results$estimate.a1,
          estimate.diff = raw_results$estimate.diff,
          se.a0 = raw_results$estimate.a0.se,
          se.a1 = raw_results$estimate.a1.se,
          se.diff = raw_results$estimate.diff.se
        )
      } else {
        inference_list = lapply(private$tau_, function(tau_i) {
          rmtlj_extract_average(
            ueif.a1.list = list(ueif_a1_sep),
            ueif.a0.list = list(ueif_a0_sep),
            time.list = list(time_grid),
            tau = tau_i
          )
        })
        results = list(
          tau = private$tau_,
          estimate.a0 = sapply(inference_list, function(x) x$estimate.a0),
          estimate.a1 = sapply(inference_list, function(x) x$estimate.a1),
          estimate.diff = sapply(inference_list, function(x) x$estimate.diff),
          se.a0 = sapply(inference_list, function(x) x$estimate.a0.se),
          se.a1 = sapply(inference_list, function(x) x$estimate.a1.se),
          se.diff = sapply(inference_list, function(x) x$estimate.diff.se)
        )
      }

      private$sep_direct_astar1_estimates_ = results
      invisible(NULL)
    },

    compute_separable_indirect_astar1 = function() {
      if (is.null(private$all_nuisance_)) {
        stop("Model must be fitted first. Call fit() before computing separable effects.")
      }

      n_obs = self$data$n_obs
      time_grid = self$get_time_grid()
      id = 1:n_obs
      a = private$all_nuisance_$a
      time = private$all_nuisance_$time
      event = private$all_nuisance_$event
      propensity = private$all_nuisance_$propensity
      bw = ifelse(a == 1, 1 / propensity, 1 / (1 - propensity))
      tilt = rep(1, n_obs)

      ueif_result = double_ml_rmtlj_sep_indirect_astar1(
        id = id, a = a, time = time, event = event,
        bw = bw, tilt = tilt,
        G.a0 = private$all_nuisance_$G_a0, G.a1 = private$all_nuisance_$G_a1,
        S.a0 = private$all_nuisance_$S_a0, S.a1 = private$all_nuisance_$S_a1,
        Sj.a0 = private$all_nuisance_$Sj_a0, Sj.a1 = private$all_nuisance_$Sj_a1,
        Sjbar.a0 = private$all_nuisance_$Sjbar_a0, Sjbar.a1 = private$all_nuisance_$Sjbar_a1,
        freq.time = NULL, admin.cens = self$admin_cens
      )

      ueif_a1_sep = ueif_result$ueif.a1
      ueif_a0_sep = ueif_result$ueif.a0
      private$sep_indirect_astar1_ueif_a1_ = ueif_a1_sep
      private$sep_indirect_astar1_ueif_a0_ = ueif_a0_sep

      if (length(private$tau_) == 1) {
        raw_results = rmtlj_extract_average(
          ueif.a1.list = list(ueif_a1_sep),
          ueif.a0.list = list(ueif_a0_sep),
          time.list = list(time_grid),
          tau = private$tau_
        )
        results = list(
          tau = private$tau_,
          estimate.a0 = raw_results$estimate.a0,
          estimate.a1 = raw_results$estimate.a1,
          estimate.diff = raw_results$estimate.diff,
          se.a0 = raw_results$estimate.a0.se,
          se.a1 = raw_results$estimate.a1.se,
          se.diff = raw_results$estimate.diff.se
        )
      } else {
        inference_list = lapply(private$tau_, function(tau_i) {
          rmtlj_extract_average(
            ueif.a1.list = list(ueif_a1_sep),
            ueif.a0.list = list(ueif_a0_sep),
            time.list = list(time_grid),
            tau = tau_i
          )
        })
        results = list(
          tau = private$tau_,
          estimate.a0 = sapply(inference_list, function(x) x$estimate.a0),
          estimate.a1 = sapply(inference_list, function(x) x$estimate.a1),
          estimate.diff = sapply(inference_list, function(x) x$estimate.diff),
          se.a0 = sapply(inference_list, function(x) x$estimate.a0.se),
          se.a1 = sapply(inference_list, function(x) x$estimate.a1.se),
          se.diff = sapply(inference_list, function(x) x$estimate.diff.se)
        )
      }

      private$sep_indirect_astar1_estimates_ = results
      invisible(NULL)
    },

    compute_separable_direct_astar0 = function() {
      if (is.null(private$all_nuisance_)) {
        stop("Model must be fitted first. Call fit() before computing separable effects.")
      }

      n_obs = self$data$n_obs
      time_grid = self$get_time_grid()
      id = 1:n_obs
      a = private$all_nuisance_$a
      time = private$all_nuisance_$time
      event = private$all_nuisance_$event
      propensity = private$all_nuisance_$propensity
      bw = ifelse(a == 1, 1 / propensity, 1 / (1 - propensity))
      tilt = rep(1, n_obs)

      ueif_result = double_ml_rmtlj_sep_direct_astar0(
        id = id, a = a, time = time, event = event,
        bw = bw, tilt = tilt,
        G.a0 = private$all_nuisance_$G_a0, G.a1 = private$all_nuisance_$G_a1,
        S.a0 = private$all_nuisance_$S_a0, S.a1 = private$all_nuisance_$S_a1,
        Sj.a0 = private$all_nuisance_$Sj_a0, Sj.a1 = private$all_nuisance_$Sj_a1,
        Sjbar.a0 = private$all_nuisance_$Sjbar_a0, Sjbar.a1 = private$all_nuisance_$Sjbar_a1,
        freq.time = NULL, admin.cens = self$admin_cens
      )

      ueif_a1_sep = ueif_result$ueif.a1
      ueif_a0_sep = ueif_result$ueif.a0
      private$sep_direct_astar0_ueif_a1_ = ueif_a1_sep
      private$sep_direct_astar0_ueif_a0_ = ueif_a0_sep

      if (length(private$tau_) == 1) {
        raw_results = rmtlj_extract_average(
          ueif.a1.list = list(ueif_a1_sep),
          ueif.a0.list = list(ueif_a0_sep),
          time.list = list(time_grid),
          tau = private$tau_
        )
        results = list(
          tau = private$tau_,
          estimate.a0 = raw_results$estimate.a0,
          estimate.a1 = raw_results$estimate.a1,
          estimate.diff = raw_results$estimate.diff,
          se.a0 = raw_results$estimate.a0.se,
          se.a1 = raw_results$estimate.a1.se,
          se.diff = raw_results$estimate.diff.se
        )
      } else {
        inference_list = lapply(private$tau_, function(tau_i) {
          rmtlj_extract_average(
            ueif.a1.list = list(ueif_a1_sep),
            ueif.a0.list = list(ueif_a0_sep),
            time.list = list(time_grid),
            tau = tau_i
          )
        })
        results = list(
          tau = private$tau_,
          estimate.a0 = sapply(inference_list, function(x) x$estimate.a0),
          estimate.a1 = sapply(inference_list, function(x) x$estimate.a1),
          estimate.diff = sapply(inference_list, function(x) x$estimate.diff),
          se.a0 = sapply(inference_list, function(x) x$estimate.a0.se),
          se.a1 = sapply(inference_list, function(x) x$estimate.a1.se),
          se.diff = sapply(inference_list, function(x) x$estimate.diff.se)
        )
      }

      private$sep_direct_astar0_estimates_ = results
      invisible(NULL)
    },

    compute_separable_indirect_astar0 = function() {
      if (is.null(private$all_nuisance_)) {
        stop("Model must be fitted first. Call fit() before computing separable effects.")
      }

      n_obs = self$data$n_obs
      time_grid = self$get_time_grid()
      id = 1:n_obs
      a = private$all_nuisance_$a
      time = private$all_nuisance_$time
      event = private$all_nuisance_$event
      propensity = private$all_nuisance_$propensity
      bw = ifelse(a == 1, 1 / propensity, 1 / (1 - propensity))
      tilt = rep(1, n_obs)

      ueif_result = double_ml_rmtlj_sep_indirect_astar0(
        id = id, a = a, time = time, event = event,
        bw = bw, tilt = tilt,
        G.a0 = private$all_nuisance_$G_a0, G.a1 = private$all_nuisance_$G_a1,
        S.a0 = private$all_nuisance_$S_a0, S.a1 = private$all_nuisance_$S_a1,
        Sj.a0 = private$all_nuisance_$Sj_a0, Sj.a1 = private$all_nuisance_$Sj_a1,
        Sjbar.a0 = private$all_nuisance_$Sjbar_a0, Sjbar.a1 = private$all_nuisance_$Sjbar_a1,
        freq.time = NULL, admin.cens = self$admin_cens
      )

      ueif_a1_sep = ueif_result$ueif.a1
      ueif_a0_sep = ueif_result$ueif.a0
      private$sep_indirect_astar0_ueif_a1_ = ueif_a1_sep
      private$sep_indirect_astar0_ueif_a0_ = ueif_a0_sep

      if (length(private$tau_) == 1) {
        raw_results = rmtlj_extract_average(
          ueif.a1.list = list(ueif_a1_sep),
          ueif.a0.list = list(ueif_a0_sep),
          time.list = list(time_grid),
          tau = private$tau_
        )
        results = list(
          tau = private$tau_,
          estimate.a0 = raw_results$estimate.a0,
          estimate.a1 = raw_results$estimate.a1,
          estimate.diff = raw_results$estimate.diff,
          se.a0 = raw_results$estimate.a0.se,
          se.a1 = raw_results$estimate.a1.se,
          se.diff = raw_results$estimate.diff.se
        )
      } else {
        inference_list = lapply(private$tau_, function(tau_i) {
          rmtlj_extract_average(
            ueif.a1.list = list(ueif_a1_sep),
            ueif.a0.list = list(ueif_a0_sep),
            time.list = list(time_grid),
            tau = tau_i
          )
        })
        results = list(
          tau = private$tau_,
          estimate.a0 = sapply(inference_list, function(x) x$estimate.a0),
          estimate.a1 = sapply(inference_list, function(x) x$estimate.a1),
          estimate.diff = sapply(inference_list, function(x) x$estimate.diff),
          se.a0 = sapply(inference_list, function(x) x$estimate.a0.se),
          se.a1 = sapply(inference_list, function(x) x$estimate.a1.se),
          se.diff = sapply(inference_list, function(x) x$estimate.diff.se)
        )
      }

      private$sep_indirect_astar0_estimates_ = results
      invisible(NULL)
    },

    # Main nuisance estimation method called by base DoubleML class
    nuisance_est = function(smpls, ...) {
      full_data = self$data$data
      n_obs = self$data$n_obs
      K = length(smpls$train_ids)

      # Global time grid
      global_time_grid = self$get_time_grid()
      n_times = length(global_time_grid)

      # Assemble cross-fitted predictions on global time grid
      S_a0_pred = matrix(NA, nrow = n_obs, ncol = n_times)
      S_a1_pred = matrix(NA, nrow = n_obs, ncol = n_times)
      Sj_a0_pred = matrix(NA, nrow = n_obs, ncol = n_times)
      Sj_a1_pred = matrix(NA, nrow = n_obs, ncol = n_times)
      Sjbar_a0_pred = matrix(NA, nrow = n_obs, ncol = n_times)
      Sjbar_a1_pred = matrix(NA, nrow = n_obs, ncol = n_times)
      G_a0_pred = matrix(NA, nrow = n_obs, ncol = n_times)
      G_a1_pred = matrix(NA, nrow = n_obs, ncol = n_times)
      propensity_pred = rep(NA, n_obs)

      for (k in seq_len(K)) {
        train_ids = smpls$train_ids[[k]]
        test_ids = smpls$test_ids[[k]]
        train_data = full_data[train_ids, ]
        test_data = full_data[test_ids, ]

        ml_results = private$ml_nuisance_est_superlearner(
          train_data, test_data, global_time_grid)

        S_a0_pred[test_ids, ] = ml_results$S_a0
        S_a1_pred[test_ids, ] = ml_results$S_a1
        Sj_a0_pred[test_ids, ] = ml_results$Sj_a0
        Sj_a1_pred[test_ids, ] = ml_results$Sj_a1
        Sjbar_a0_pred[test_ids, ] = ml_results$Sjbar_a0
        Sjbar_a1_pred[test_ids, ] = ml_results$Sjbar_a1
        G_a0_pred[test_ids, ] = ml_results$G_a0
        G_a1_pred[test_ids, ] = ml_results$G_a1
        propensity_pred[test_ids] = ml_results$propensity
      }

      # Store assembled nuisance predictions
      private$all_nuisance_ = list(
        S_a0 = S_a0_pred, S_a1 = S_a1_pred,
        Sj_a0 = Sj_a0_pred, Sj_a1 = Sj_a1_pred,
        Sjbar_a0 = Sjbar_a0_pred, Sjbar_a1 = Sjbar_a1_pred,
        G_a0 = G_a0_pred, G_a1 = G_a1_pred,
        propensity = propensity_pred,
        a = full_data[[self$data$d_cols]],
        time = full_data[[self$time_col]],
        event = full_data[[self$event_col]]
      )

      # Compute estimates
      private$compute_total_effect()
      invisible(NULL)
    },

    compute_total_effect = function() {
      n_obs = self$data$n_obs
      time_grid = self$get_time_grid()

      # Compute weights from stored nuisance
      a = private$all_nuisance_$a
      propensity = private$all_nuisance_$propensity
      bw = ifelse(a == 1, 1 / propensity, 1 / (1 - propensity))
      tilt = rep(1, n_obs)

      # Compute UEIF on all obs at once
      ueif_result = double_ml_rmtlj(
        id = 1:n_obs,
        a = a,
        time = private$all_nuisance_$time,
        event = private$all_nuisance_$event,
        bw = bw,
        tilt = tilt,
        G.a0 = private$all_nuisance_$G_a0,
        G.a1 = private$all_nuisance_$G_a1,
        S.a0 = private$all_nuisance_$S_a0,
        S.a1 = private$all_nuisance_$S_a1,
        Sj.a0 = private$all_nuisance_$Sj_a0,
        Sj.a1 = private$all_nuisance_$Sj_a1,
        freq.time = private$freq_time_,
        admin.cens = self$admin_cens
      )

      # Store for inference
      private$all_ueif_a1_ = ueif_result$ueif.a1
      private$all_ueif_a0_ = ueif_result$ueif.a0
      private$time_grid_ = time_grid

      # Extract estimates (single matrix wrapped in list)
      if (length(private$tau_) == 1) {
        raw_results = rmtlj_extract_average(
          ueif.a1.list = list(ueif_result$ueif.a1),
          ueif.a0.list = list(ueif_result$ueif.a0),
          time.list = list(time_grid),
          tau = private$tau_
        )
        private$rmtlj_estimates_ = list(
          tau = private$tau_,
          estimate.a0 = raw_results$estimate.a0,
          estimate.a1 = raw_results$estimate.a1,
          estimate.diff = raw_results$estimate.diff,
          se.a0 = raw_results$estimate.a0.se,
          se.a1 = raw_results$estimate.a1.se,
          se.diff = raw_results$estimate.diff.se
        )
      } else {
        inference_list = lapply(private$tau_, function(tau_i) {
          rmtlj_extract_average(
            ueif.a1.list = list(ueif_result$ueif.a1),
            ueif.a0.list = list(ueif_result$ueif.a0),
            time.list = list(time_grid),
            tau = tau_i
          )
        })
        private$rmtlj_estimates_ = list(
          tau = private$tau_,
          estimate.a0 = sapply(inference_list, function(x) x$estimate.a0),
          estimate.a1 = sapply(inference_list, function(x) x$estimate.a1),
          estimate.diff = sapply(inference_list, function(x) x$estimate.diff),
          se.a0 = sapply(inference_list, function(x) x$estimate.a0.se),
          se.a1 = sapply(inference_list, function(x) x$estimate.a1.se),
          se.diff = sapply(inference_list, function(x) x$estimate.diff.se)
        )
      }

      invisible(NULL)
    }
  )
)
