#' SuperLearner Wrapper Functions for Survival Analysis
#'
#' This file contains wrapper functions to interface between DoubleML's
#' survival analysis classes and the SuperLearner/survSuperLearner framework.
#' These functions handle the training and prediction of survival models,
#' censoring models, and propensity score models within the DML framework.
#'
#' @name survival_nuisance
#' @importFrom stats approx binomial predict rbinom pnorm qnorm
#' @importFrom SuperLearner SuperLearner
#' @importFrom survSuperLearner survSuperLearner
NULL

#' Fit Survival and Censoring Functions using survSuperLearner
#'
#' Wrapper function to fit both event survival functions S(t|X,A) and censoring
#' survival functions G(t|X,A) using survSuperLearner framework for both treatment groups.
#' This follows the original approach where a single survSuperLearner call produces
#' both event.SL.predict and cens.SL.predict outputs.
#'
#' @param data Training data frame
#' @param time_col Name of time column
#' @param event_col Name of event column
#' @param treatment_col Name of treatment column
#' @param covariate_cols Names of covariate columns
#' @param event_sl_lib survSuperLearner library for event models
#' @param cens_sl_lib survSuperLearner library for censoring models
#' @param new_data Test data for predictions
#' @param new_times Time points for prediction
#' @return List with S_a0, S_a1, G_a0, and G_a1 matrices
fit_survival_functions = function(data, time_col, event_col, treatment_col,
  covariate_cols, event_sl_lib, cens_sl_lib, new_data, new_times) {

  # Fit for treatment group A=0
  data_a0 = data[data[[treatment_col]] == 0, ]
  if (nrow(data_a0) == 0) {
    stop("No observations with treatment A=0 in training data. Check data splitting or treatment assignment.")
  }

  # Use survSuperLearner for both event and censoring survival functions
  # Convert to data.frame to avoid data.table issues
  X_train = as.data.frame(data_a0[, covariate_cols, with = FALSE])
  X_pred = as.data.frame(new_data[, covariate_cols, with = FALSE])

  surv_fit_a0 = survSuperLearner(
    time = data_a0[[time_col]],
    event = as.numeric(data_a0[[event_col]] != 0),
    X = X_train,
    newX = X_pred,
    new.times = new_times,
    event.SL.library = event_sl_lib,
    cens.SL.library = cens_sl_lib,
    verbose = FALSE
  )
  S_a0 = surv_fit_a0$event.SL.predict
  G_a0 = surv_fit_a0$cens.SL.predict

  # Fit for treatment group A=1
  data_a1 = data[data[[treatment_col]] == 1, ]
  if (nrow(data_a1) == 0) {
    stop("No observations with treatment A=1 in training data. Check data splitting or treatment assignment.")
  }

  # Use survSuperLearner for both event and censoring survival functions
  # Convert to data.frame to avoid data.table issues
  X_train = as.data.frame(data_a1[, covariate_cols, with = FALSE])
  X_pred = as.data.frame(new_data[, covariate_cols, with = FALSE])

  surv_fit_a1 = survSuperLearner(
    time = data_a1[[time_col]],
    event = as.numeric(data_a1[[event_col]] != 0),
    X = X_train,
    newX = X_pred,
    new.times = new_times,
    event.SL.library = event_sl_lib,
    cens.SL.library = cens_sl_lib,
    verbose = FALSE
  )
  S_a1 = surv_fit_a1$event.SL.predict
  G_a1 = surv_fit_a1$cens.SL.predict

  return(list(S_a0 = S_a0, S_a1 = S_a1, G_a0 = G_a0, G_a1 = G_a1))
}


#' Fit Propensity Score using SuperLearner
#'
#' Wrapper function to fit treatment propensity P(A=1|X) using SuperLearner
#' framework for binary treatments.
#'
#' @param data Training data frame
#' @param treatment_col Name of treatment column
#' @param covariate_cols Names of covariate columns
#' @param sl_library SuperLearner library for treatment model
#' @param new_data Test data for predictions
#' @return Numeric vector of propensity score predictions
fit_propensity_score = function(data, treatment_col, covariate_cols,
  sl_library, new_data) {

  # Prepare training data
  # Convert to data.frame to avoid data.table issues
  X_train = as.data.frame(data[, covariate_cols, with = FALSE])
  Y_train = data[[treatment_col]]
  X_pred = as.data.frame(new_data[, covariate_cols, with = FALSE])

  # Use SuperLearner for propensity score estimation
  sl_fit = SuperLearner(
    Y = Y_train,
    X = X_train,
    family = binomial(),
    SL.library = sl_library,
    verbose = FALSE
  )

  # Predict propensity scores
  pred = predict(sl_fit, newdata = X_pred)

  # Return predicted probabilities
  return(as.numeric(pred$pred))
}

#' Winsorize Values
#'
#' Helper function to bound values within specified range to avoid
#' numerical issues in downstream calculations.
#'
#' @param x Numeric vector or matrix to winsorize
#' @param lower Lower bound (default 1e-3)
#' @param upper Upper bound (default 1-1e-3)
#' @return Winsorized values
winsorize_values = function(x, lower = 1e-3, upper = 1 - 1e-3) {
  if (is.matrix(x)) {
    return(apply(x, c(1, 2), function(val) pmax(lower, pmin(upper, val))))
  } else {
    return(pmax(lower, pmin(upper, x)))
  }
}

#' Get Time Grid
#'
#' Helper function to create time grid for survival analysis.
#'
#' @param times Observed time vector
#' @param freq_time Time grid frequency (optional)
#' @param admin_cens Administrative censoring time
#' @return Vector of time points
get_time_grid = function(times, freq_time = NULL, admin_cens) {
  if (is.null(freq_time)) {
    return(sort(unique(times[times <= admin_cens])))
  } else {
    return(seq(freq_time, admin_cens, freq_time))
  }
}

#' Validate Survival Data
#'
#' Helper function to validate survival data structure and required columns.
#'
#' @param data Data frame to validate
#' @param time_col Name of time column
#' @param event_col Name of event column
#' @param treatment_col Name of treatment column
#' @param covariate_cols Names of covariate columns
#' @return NULL (throws error if validation fails)
validate_survival_data = function(data, time_col, event_col, treatment_col,
  covariate_cols) {

  # Check required columns exist
  required_cols = c(time_col, event_col, treatment_col, covariate_cols)
  missing_cols = setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Check for missing values in critical columns
  if (any(is.na(data[[time_col]]))) {
    stop("Missing values found in time column")
  }
  if (any(is.na(data[[event_col]]))) {
    stop("Missing values found in event column")
  }
  if (any(is.na(data[[treatment_col]]))) {
    stop("Missing values found in treatment column")
  }

  # Check time values are positive
  if (any(data[[time_col]] <= 0)) {
    stop("Time values must be positive")
  }

  # Check treatment is binary
  unique_treatments = unique(data[[treatment_col]])
  if (!all(unique_treatments %in% c(0, 1))) {
    stop("Treatment must be binary (0, 1)")
  }

  return(invisible(NULL))
}
