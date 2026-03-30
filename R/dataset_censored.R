#' @title Generates data from a survival analysis model with right censoring.
#'
#' @description
#' Generates data from a survival analysis model with right censoring used for
#' double machine learning estimation of weighted average treatment effects (WATE)
#' on restricted mean survival time and survival probabilities.
#'
#' @details
#' The data generating process is defined as:
#'
#' Treatment assignment:
#' \deqn{ps_i = \frac{1}{1 + \exp(-(-1 + x_{i1} + 1.5x_{i2} + 1.5x_{i3} - x_{i4} - 1.5x_{i5} - x_{i6}))}}
#' \deqn{a_i \sim \textstyle{Bernoulli}(ps_i)}
#'
#' Event times under treatment and control:
#' \deqn{T^{a=0}_i \sim \textstyle{Exponential}(\lambda = 0.12, \beta)}
#' \deqn{T^{a=1}_i \sim \textstyle{Exponential}(\lambda = 0.15, \beta)}
#'
#' where \eqn{\beta = (0.1, 0.1, -0.2, 0.2, 0.1, 0.8, -0.2)} for \eqn{a=0} and
#' \eqn{\beta = (0.17, 0.2, -0.1, 0.4, 0.2, 0.3, 0.4)} for \eqn{a=1}.
#'
#' Censoring times (treatment-specific):
#' \deqn{C_i \mid a_i=0 \sim \textstyle{Exponential}(\lambda = 0.06, \beta_c^0)}
#' \deqn{C_i \mid a_i=1 \sim \textstyle{Exponential}(\lambda = 0.08, \beta_c^1)}
#'
#' where \eqn{\beta_c^0 = (0.1, 0.4, -0.7, -0.4, -0.5, 0.8, -0.6)} and
#' \eqn{\beta_c^1 = (0, 0.5, -0.6, 0.2, 0.6, 0.9, -0.5)}.
#'
#' Covariates:
#' \deqn{(x_{i1}, x_{i2}, x_{i3}) \sim \mathcal{N}(0, \Sigma)}
#' where \eqn{\Sigma_{jk} = 0.5} for \eqn{j \neq k} and \eqn{\Sigma_{jj} = 1}.
#' \deqn{x_{i4}, x_{i5}, x_{i6} \sim \textstyle{Bernoulli}(0.5)}
#'
#' Observed time and event indicator:
#' \deqn{Y_i = \min(T^{a_i}_i, C_i, \tau_{admin})}
#' \deqn{\delta_i = 1\{T^{a_i}_i \leq C_i\}}
#'
#' Time discretization is applied: \eqn{Y_i = \lceil Y_i / \text{time.grid} \rceil \times \text{time.grid}}.
#'
#' @param n_obs (`integer(1)`) \cr
#' The number of observations to simulate. Default is 2000.
#'
#' @param admin_cens (`numeric(1)`) \cr
#' The administrative censoring time. Default is 10.
#'
#' @param time_grid (`numeric(1)`) \cr
#' The discretization grid for time measurements. Default is 0.01.
#'
#' @param return_type (`character(1)`) \cr
#' If `"DoubleMLData"`, returns a `DoubleMLData` object.
#' If `"data.frame"` returns a `data.frame()`.
#' If `"data.table"` returns a `data.table()`.
#' Default is `"DoubleMLData"`.
#'
#' @return A data object according to the choice of `return_type`. The dataset contains:
#' * time: observed follow-up time (discretized)
#' * event: event indicator (1 = event, 0 = censored)
#' * a: treatment assignment (1 = treated, 0 = control)
#' * x1, x2, x3: continuous covariates
#' * x4, x5, x6: binary covariates
#'
#' @export
make_survival_data = function(n_obs = 2000, admin_cens = 10, time_grid = 0.01,
  return_type = "DoubleMLData") {

  assert_choice(
    return_type,
    c("data.table", "data.frame", "DoubleMLData")
  )
  assert_count(n_obs)
  assert_numeric(admin_cens, len = 1)
  assert_numeric(time_grid, len = 1)

  # Generate correlated continuous covariates
  corr_mat = matrix(0.5, nrow = 3, ncol = 3)
  diag(corr_mat) = 1
  x = rmvnorm(n = n_obs, mean = rep(0, 3), sigma = corr_mat)
  x1 = x[, 1]
  x2 = x[, 2]
  x3 = x[, 3]
  x4 = rbinom(n_obs, 1, 0.5)
  x5 = rbinom(n_obs, 1, 0.5)
  x6 = rbinom(n_obs, 1, 0.5)

  # Treatment assignment
  ps = 1 / (1 + exp(-(0.3 + 0.2 * x1 + 0.3 * x2 + 0.3 * x3 - 0.2 * x4 - 0.3 * x5 - 0.2 * x6)))
  a = rbinom(n_obs, 1, ps)

  # Potential event times
  Ta0 = simsurv::simsurv(
    dist = "exponential", lambdas = 0.12,
    x = data.frame(x0 = 1, x1, x2, x3, x4, x5, x6),
    betas = c(x0 = 0.1, x1 = 0.1, x2 = -0.2, x3 = 0.2, x4 = 0.1, x5 = 0.8, x6 = -0.2)
  )$eventtime

  Ta1 = simsurv::simsurv(
    dist = "exponential", lambdas = 0.15,
    x = data.frame(x0 = 1, x1, x2, x3, x4, x5, x6),
    betas = c(x0 = 0.17, x1 = 0.2, x2 = -0.1, x3 = 0.4, x4 = 0.2, x5 = 0.3, x6 = 0.4)
  )$eventtime

  # Observed event time (based on treatment assignment)
  Ta = (1 - a) * Ta0 + a * Ta1

  # Censoring times (treatment-specific)
  C = rep(NA, n_obs)
  if (sum(a == 0) > 0) {
    C[a == 0] = simsurv::simsurv(
      dist = "exponential", lambdas = 0.06,
      x = data.frame(
        x0 = 1,
        x1 = x1[a == 0],
        x2 = x2[a == 0],
        x3 = x3[a == 0],
        x4 = x4[a == 0],
        x5 = x5[a == 0],
        x6 = x6[a == 0]
      ),
      betas = c(x0 = 0.1, x1 = 0.4, x2 = -0.7, x3 = -0.4, x4 = -0.5, x5 = 0.8, x6 = -0.6)
    )$eventtime
  }

  if (sum(a == 1) > 0) {
    C[a == 1] = simsurv::simsurv(
      dist = "exponential", lambdas = 0.08,
      x = data.frame(
        x0 = 1,
        x1 = x1[a == 1],
        x2 = x2[a == 1],
        x3 = x3[a == 1],
        x4 = x4[a == 1],
        x5 = x5[a == 1],
        x6 = x6[a == 1]
      ),
      betas = c(x0 = 0, x1 = 0.5, x2 = -0.6, x3 = 0.2, x4 = 0.6, x5 = 0.9, x6 = -0.5)
    )$eventtime
  }

  # Apply administrative censoring
  C = pmin(C, admin_cens, na.rm = TRUE)

  # Discretize time and compute observed time and event indicator
  end_time = ceiling(pmin(C, Ta) / time_grid) * time_grid
  event = ifelse(Ta <= C, 1, 0)

  # Create dataset
  data_df = data.frame(
    time = end_time,
    event = event,
    a = a,
    x1 = x1,
    x2 = x2,
    x3 = x3,
    x4 = x4,
    x5 = x5,
    x6 = x6
  )

  # Sort by time and event (events before censoring at same time)
  data_df = data_df[order(data_df$time, -data_df$event), ]

  # Return in requested format
  if (return_type == "data.frame") {
    return(data_df)
  } else if (return_type == "data.table") {
    data_dt = as.data.table(data_df)
    return(data_dt)
  } else if (return_type == "DoubleMLData") {
    data_dt = as.data.table(data_df)
    dml_data = DoubleMLData$new(
      data_dt,
      y_col = "time",
      d_cols = "a",
      x_cols = c("x1", "x2", "x3", "x4", "x5", "x6")
    )
    return(dml_data)
  }
}

#' @title Generates data from a competing risks model with right censoring.
#'
#' @description
#' Generates data from a competing risks model with right censoring used for
#' double machine learning estimation of weighted average treatment effects (WATE)
#' on cumulative incidence functions and restricted mean time lost.
#'
#' @details
#' The data generating process is defined as:
#'
#' Treatment assignment:
#' \deqn{ps_i = \frac{1}{1 + \exp(-(-1 + x_{i1} + 1.5x_{i2} + 1.5x_{i3} - x_{i4} - 1.5x_{i5} - x_{i6}))}}
#' \deqn{a_i \sim \textstyle{Bernoulli}(ps_i)}
#'
#' Cause 1 event times under treatment and control:
#' \deqn{T_1^{a=0}_i \sim \textstyle{Exponential}(\lambda = 0.12, \beta_1^0)}
#' \deqn{T_1^{a=1}_i \sim \textstyle{Exponential}(\lambda = 0.15, \beta_1^1)}
#'
#' where \eqn{\beta_1^0 = (0.1, 0.1, -0.2, 0.2, 0.1, 0.8, -0.2)} for \eqn{a=0} and
#' \eqn{\beta_1^1 = (0.17, 0.2, -0.1, 0.4, 0.2, 0.3, 0.4)} for \eqn{a=1}.
#'
#' Cause 2 event times under treatment and control:
#' \deqn{T_2^{a=0}_i \sim \textstyle{Exponential}(\lambda = 0.1, \beta_2^0)}
#' \deqn{T_2^{a=1}_i \sim \textstyle{Exponential}(\lambda = 0.08, \beta_2^1)}
#'
#' where \eqn{\beta_2^0 = (0.12, -0.1, 0.3, 0.1, 0.2, -0.4, 0.5)} for \eqn{a=0} and
#' \eqn{\beta_2^1 = (0.1, -0.2, -0.1, 0.2, 0.3, 0.3, -0.3)} for \eqn{a=1}.
#'
#' Censoring times (treatment-specific):
#' \deqn{C_i \mid a_i=0 \sim \textstyle{Exponential}(\lambda = 0.12, \beta_c^0)}
#' \deqn{C_i \mid a_i=1 \sim \textstyle{Exponential}(\lambda = 0.14, \beta_c^1)}
#'
#' where \eqn{\beta_c^0 = (0.1, 0.4, -0.7, -0.4, -0.5, 0.8, -0.6)} and
#' \eqn{\beta_c^1 = (0, 0.5, -0.6, 0.2, 0.6, 0.9, -0.5)}.
#'
#' Covariates:
#' \deqn{(x_{i1}, x_{i2}, x_{i3}) \sim \mathcal{N}(0, \Sigma)}
#' where \eqn{\Sigma_{jk} = 0.5} for \eqn{j \neq k} and \eqn{\Sigma_{jj} = 1}.
#' \deqn{x_{i4}, x_{i5}, x_{i6} \sim \textstyle{Bernoulli}(0.5)}
#'
#' Observed time and event indicator:
#' \deqn{Y_i = \min(T_1^{a_i}_i, T_2^{a_i}_i, C_i, \tau_{admin})}
#' \deqn{\delta_i = \begin{cases}
#'   0 & \text{if } C_i < \min(T_1^{a_i}_i, T_2^{a_i}_i) \text{ (censored)} \\
#'   1 & \text{if } T_1^{a_i}_i < \min(T_2^{a_i}_i, C_i) \text{ (cause 1)} \\
#'   2 & \text{if } T_2^{a_i}_i < \min(T_1^{a_i}_i, C_i) \text{ (cause 2)}
#' \end{cases}}
#'
#' Time discretization is applied: \eqn{Y_i = \lceil Y_i / \text{time.grid} \rceil \times \text{time.grid}}.
#'
#' @param n_obs (`integer(1)`) \cr
#' The number of observations to simulate. Default is 2000.
#'
#' @param admin_cens (`numeric(1)`) \cr
#' The administrative censoring time. Default is 10.
#'
#' @param time_grid (`numeric(1)`) \cr
#' The discretization grid for time measurements. Default is 0.01.
#'
#' @param return_type (`character(1)`) \cr
#' If `"DoubleMLData"`, returns a `DoubleMLData` object.
#' If `"data.frame"` returns a `data.frame()`.
#' If `"data.table"` returns a `data.table()`.
#' Default is `"DoubleMLData"`.
#'
#' @return A data object according to the choice of `return_type`. The dataset contains:
#' * time: observed follow-up time (discretized)
#' * event: event indicator (0 = censored, 1 = cause 1, 2 = cause 2)
#' * a: treatment assignment (1 = treated, 0 = control)
#' * x1, x2, x3: continuous covariates
#' * x4, x5, x6: binary covariates
#'
#' @export
make_competing_data = function(n_obs = 2000, admin_cens = 10, time_grid = 0.01,
  return_type = "DoubleMLData") {

  assert_choice(
    return_type,
    c("data.table", "data.frame", "DoubleMLData")
  )
  assert_count(n_obs)
  assert_numeric(admin_cens, len = 1)
  assert_numeric(time_grid, len = 1)

  # Generate correlated continuous covariates
  corr_mat = matrix(0.5, nrow = 3, ncol = 3)
  diag(corr_mat) = 1
  x = rmvnorm(n = n_obs, mean = rep(0, 3), sigma = corr_mat)
  x1 = x[, 1]
  x2 = x[, 2]
  x3 = x[, 3]
  x4 = rbinom(n_obs, 1, 0.5)
  x5 = rbinom(n_obs, 1, 0.5)
  x6 = rbinom(n_obs, 1, 0.5)

  # Treatment assignment
  ps = 1 / (1 + exp(-(0.3 + 0.2 * x1 + 0.3 * x2 + 0.3 * x3 - 0.2 * x4 - 0.3 * x5 - 0.2 * x6)))
  a = rbinom(n_obs, 1, ps)

  # Cause 1 event times under treatment and control
  Tj1a0 = simsurv::simsurv(
    dist = "exponential", lambdas = 0.12,
    x = data.frame(x0 = 1, x1, x2, x3, x4, x5, x6),
    betas = c(x0 = 0.1, x1 = 0.1, x2 = -0.2, x3 = 0.2, x4 = 0.1, x5 = 0.8, x6 = -0.2)
  )$eventtime

  Tj1a1 = simsurv::simsurv(
    dist = "exponential", lambdas = 0.15,
    x = data.frame(x0 = 1, x1, x2, x3, x4, x5, x6),
    betas = c(x0 = 0.17, x1 = 0.2, x2 = -0.1, x3 = 0.4, x4 = 0.2, x5 = 0.3, x6 = 0.4)
  )$eventtime

  # Observed cause 1 event time (based on treatment assignment)
  Tj1 = (1 - a) * Tj1a0 + a * Tj1a1

  # Cause 2 event times under treatment and control
  Tj2a0 = simsurv::simsurv(
    dist = "exponential", lambdas = 0.1,
    x = data.frame(x0 = 1, x1, x2, x3, x4, x5, x6),
    betas = c(x0 = 0.12, x1 = -0.1, x2 = 0.3, x3 = 0.1, x4 = 0.2, x5 = -0.4, x6 = 0.5)
  )$eventtime

  Tj2a1 = simsurv::simsurv(
    dist = "exponential", lambdas = 0.08,
    x = data.frame(x0 = 1, x1, x2, x3, x4, x5, x6),
    betas = c(x0 = 0.1, x1 = -0.2, x2 = -0.1, x3 = 0.2, x4 = 0.3, x5 = 0.3, x6 = -0.3)
  )$eventtime

  # Observed cause 2 event time (based on treatment assignment)
  Tj2 = (1 - a) * Tj2a0 + a * Tj2a1

  # Censoring times (treatment-specific)
  C = rep(NA, n_obs)
  if (sum(a == 0) > 0) {
    C[a == 0] = simsurv::simsurv(
      dist = "exponential", lambdas = 0.12,
      x = data.frame(
        x0 = 1,
        x1 = x1[a == 0],
        x2 = x2[a == 0],
        x3 = x3[a == 0],
        x4 = x4[a == 0],
        x5 = x5[a == 0],
        x6 = x6[a == 0]
      ),
      betas = c(x0 = 0.1, x1 = 0.4, x2 = -0.7, x3 = -0.4, x4 = -0.5, x5 = 0.8, x6 = -0.6)
    )$eventtime
  }

  if (sum(a == 1) > 0) {
    C[a == 1] = simsurv::simsurv(
      dist = "exponential", lambdas = 0.14,
      x = data.frame(
        x0 = 1,
        x1 = x1[a == 1],
        x2 = x2[a == 1],
        x3 = x3[a == 1],
        x4 = x4[a == 1],
        x5 = x5[a == 1],
        x6 = x6[a == 1]
      ),
      betas = c(x0 = 0, x1 = 0.5, x2 = -0.6, x3 = 0.2, x4 = 0.6, x5 = 0.9, x6 = -0.5)
    )$eventtime
  }

  # Apply administrative censoring
  C = pmin(C, admin_cens, na.rm = TRUE)

  # Discretize time and compute observed time and event indicator
  end_time = ceiling(pmin(C, Tj1, Tj2) / time_grid) * time_grid
  event = ifelse(pmin(Tj1, Tj2) > C, 0, ifelse(Tj1 < Tj2, 1, 2))

  # Create dataset
  data_df = data.frame(
    time = end_time,
    event = event,
    a = a,
    x1 = x1,
    x2 = x2,
    x3 = x3,
    x4 = x4,
    x5 = x5,
    x6 = x6
  )

  # Sort by time and event (events before censoring at same time)
  data_df = data_df[order(data_df$time, -data_df$event), ]

  # Return in requested format
  if (return_type == "data.frame") {
    return(data_df)
  } else if (return_type == "data.table") {
    data_dt = as.data.table(data_df)
    return(data_dt)
  } else if (return_type == "DoubleMLData") {
    data_dt = as.data.table(data_df)
    dml_data = DoubleMLData$new(
      data_dt,
      y_col = "time",
      d_cols = "a",
      x_cols = c("x1", "x2", "x3", "x4", "x5", "x6")
    )
    return(dml_data)
  }
}
