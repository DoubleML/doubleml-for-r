#' @title Double machine learning for survival analysis base class
#'
#' @description
#' Base class for double machine learning survival analysis methods.
#' This abstract class provides common functionality for survival analysis
#' with censoring, including nuisance function estimation and cross-fitting.
#'
#' @format [R6::R6Class] object inheriting from [DoubleML].
#'
#' @family DoubleML
#' @details
#' This is an abstract base class that provides common infrastructure for:
#' - Survival probability estimation (DoubleMLSurvivalProb)
#' - Restricted mean survival time (DoubleMLRMST)
#' - Competing risks analysis (DoubleMLCIF, DoubleMLRMTL)
#'
#' All survival analysis models handle:
#' - Right-censored time-to-event data
#' - Treatment effect estimation with confounders
#' - Nuisance function estimation via SuperLearner/survSuperLearner
#' - Efficient influence function based inference
#'
#' @usage NULL
#'
#' @examples
#' \donttest{
#' # This is an abstract class - use specific implementations:
#' # - DoubleMLSurvivalProb for survival probability
#' # - DoubleMLRMST for restricted mean survival time
#' # - DoubleMLCIF for cumulative incidence functions
#' # - DoubleMLRMTL for restricted mean time lost
#' }
#' @export
DoubleMLSurvival = R6Class("DoubleMLSurvival",
  inherit = DoubleML,
  public = list(
    #' @description
    #' Creates a new instance of this R6 class.
    #' Note: This is an abstract class - use specific implementations.
    #'
    #' @param data (`DoubleMLData`) \cr
    #' The `DoubleMLData` object providing the survival data.
    #'
    #' @param ml_g_surv (character vector) \cr
    #' survSuperLearner library for event survival functions S(t|X,A).
    #'
    #' @param ml_g_cens (character vector) \cr
    #' survSuperLearner library for censoring survival functions G(t|X,A).
    #'
    #' @param ml_m (character vector) \cr
    #' SuperLearner library for treatment propensity P(A=1|X).
    #'
    #' @param admin_cens (`numeric(1)`) \cr
    #' Administrative censoring time (restriction time for analysis).
    #'
    #' @param time_col (`character(1)`) \cr
    #' Name of the time column in the data.
    #'
    #' @param event_col (`character(1)`) \cr
    #' Name of the event column in the data (0=censored, 1+=event).
    #'
    #' @param freq_time (`numeric(1)`, optional) \cr
    #' Time grid frequency. If NULL, uses unique event times.
    #'
    #' @param ml_framework (`character(1)`) \cr
    #' Machine learning framework to use. Only "SuperLearner" is supported.
    #'
    #' @param n_folds (`integer(1)`) \cr
    #' Number of folds for cross-fitting. Default is 5.
    #'
    #' @param n_rep (`integer(1)`) \cr
    #' Number of repetitions for sample splitting. Default is 1.
    #'
    #' @param score (`character(1)`) \cr
    #' Score function specification. Default is "ueif".
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
      ml_g_cens,
      ml_m,
      admin_cens,
      time_col,
      event_col,
      freq_time = NULL,
      ml_framework = "SuperLearner",
      n_folds = 5,
      n_rep = 1,
      score = "ueif",
      dml_procedure = "dml2",
      draw_sample_splitting = TRUE,
      apply_cross_fitting = TRUE) {

      # Prevent direct instantiation of abstract base class
      if (identical(class(self)[1], "DoubleMLSurvival")) {
        stop("DoubleMLSurvival is an abstract base class. Use specific implementations like DoubleMLSurvivalProb or DoubleMLRMST.")
      }

      # Store survival-specific parameters
      private$admin_cens_ = admin_cens
      private$time_col_ = time_col
      private$event_col_ = event_col
      private$freq_time_ = freq_time
      private$ml_framework_ = ml_framework

      # Store ML learners
      private$ml_g_surv_ = ml_g_surv
      private$ml_g_cens_ = ml_g_cens
      private$ml_m_ = ml_m

      # Check data structure
      private$check_data(data)

      # Check score function
      private$check_score(score)

      # Validate ML framework
      if (ml_framework != "SuperLearner") {
        stop("Only SuperLearner framework is supported")
      }

      # Initialize parent DoubleML class
      super$initialize(
        data,
        n_folds = n_folds,
        n_rep = n_rep,
        score = score,
        dml_procedure = dml_procedure,
        draw_sample_splitting = draw_sample_splitting,
        apply_cross_fitting = apply_cross_fitting
      )
    },

    #' @description
    #' Get time grid for survival analysis.
    #'
    #' @param data_subset Optional data subset to use for time points.
    #' @return Numeric vector of time points.
    get_time_grid = function(data_subset = NULL) {
      if (is.null(data_subset)) {
        data_subset = self$data$data
      }

      if (is.null(private$freq_time_)) {
        time_grid = sort(unique(data_subset[[private$time_col_]]))
      } else {
        time_grid = seq(private$freq_time_, private$admin_cens_, private$freq_time_)
      }

      # Ensure time grid doesn't exceed admin_cens
      time_grid = time_grid[time_grid <= private$admin_cens_]
      return(time_grid)
    },

    #' @description
    #' Validate survival data structure and values.
    #'
    #' @param detailed (`logical(1)`) \cr
    #' Whether to return detailed validation results. Default is FALSE.
    #'
    #' @return List with validation results if detailed=TRUE, otherwise invisible.
    validate_data = function(detailed = FALSE) {
      data = self$data$data
      results = list()

      # Check for missing values
      results$missing_time = sum(is.na(data[[private$time_col_]]))
      results$missing_event = sum(is.na(data[[private$event_col_]]))
      results$missing_treatment = sum(is.na(data[[self$data$d_cols]]))

      # Check time values
      results$negative_times = sum(data[[private$time_col_]] <= 0, na.rm = TRUE)
      results$times_exceed_admin_cens = sum(data[[private$time_col_]] > private$admin_cens_, na.rm = TRUE)

      # Check event values
      unique_events = unique(data[[private$event_col_]])
      results$event_values = sort(unique_events[!is.na(unique_events)])

      # Check treatment values
      unique_treatments = unique(data[[self$data$d_cols]])
      results$treatment_values = sort(unique_treatments[!is.na(unique_treatments)])

      # Calculate summary statistics
      results$n_total = nrow(data)
      results$n_events = sum(data[[private$event_col_]] != 0, na.rm = TRUE)
      results$n_censored = sum(data[[private$event_col_]] == 0, na.rm = TRUE)
      results$censoring_rate = results$n_censored / results$n_total

      # Treatment group sizes
      results$n_treated = sum(data[[self$data$d_cols]] == 1, na.rm = TRUE)
      results$n_control = sum(data[[self$data$d_cols]] == 0, na.rm = TRUE)

      # Warnings and errors
      warnings = character(0)
      errors = character(0)

      if (results$missing_time > 0) {
        errors = c(errors, paste("Missing values in time column:", results$missing_time))
      }
      if (results$missing_event > 0) {
        errors = c(errors, paste("Missing values in event column:", results$missing_event))
      }
      if (results$missing_treatment > 0) {
        errors = c(errors, paste("Missing values in treatment column:", results$missing_treatment))
      }
      if (results$negative_times > 0) {
        errors = c(errors, paste("Non-positive time values:", results$negative_times))
      }
      if (!all(results$treatment_values %in% c(0, 1))) {
        errors = c(errors, "Treatment values must be 0 or 1")
      }
      if (results$censoring_rate > 0.8) {
        warnings = c(warnings, paste("High censoring rate:", round(results$censoring_rate * 100, 1), "%"))
      }
      if (min(results$n_treated, results$n_control) < 50) {
        warnings = c(warnings, "Small treatment group sizes may affect performance")
      }

      results$warnings = warnings
      results$errors = errors

      # Print warnings
      if (length(warnings) > 0) {
        for (w in warnings) {
          warning(w)
        }
      }

      # Stop on errors
      if (length(errors) > 0) {
        stop(paste("Data validation failed:", paste(errors, collapse = "; ")))
      }

      if (detailed) {
        return(results)
      } else {
        return(invisible(results))
      }
    },

    #' @description
    #' Fit the survival analysis model. Overrides base class fit() to bypass
    #' psi_a/psi_b score decomposition, which does not apply to survival analysis.
    #' Estimation goes directly: UEIF -> extract_average -> coef/SE.
    #'
    #' @return self
    fit = function() {
      for (i_rep in 1:self$n_rep) {
        private$i_rep = i_rep
        for (i_treat in 1:self$data$n_treat) {
          private$i_treat = i_treat
          if (self$data$n_treat > 1) {
            self$data$set_data_model(self$data$d_cols[i_treat])
          }
          private$nuisance_est(private$get__smpls())
        }
      }
      invisible(self)
    }
  ),

  active = list(
    #' @field admin_cens Administrative censoring time
    admin_cens = function() private$admin_cens_,

    #' @field time_col Name of time column
    time_col = function() private$time_col_,

    #' @field event_col Name of event column
    event_col = function() private$event_col_,

    #' @field ml_framework Machine learning framework
    ml_framework = function() private$ml_framework_
  ),

  private = list(
    admin_cens_ = NULL,
    freq_time_ = NULL,
    ml_framework_ = NULL,
    time_col_ = NULL,
    event_col_ = NULL,
    ml_g_surv_ = NULL,
    ml_g_cens_ = NULL,
    ml_m_ = NULL,

    check_data = function(obj_dml_data) {
      # Check that required columns exist in the data
      if (!private$time_col_ %in% names(obj_dml_data$data)) {
        stop(paste("Time column", private$time_col_, "not found in data"))
      }
      if (!private$event_col_ %in% names(obj_dml_data$data)) {
        stop(paste("Event column", private$event_col_, "not found in data"))
      }

      # Basic data validation
      self$validate_data(detailed = FALSE)

      return(invisible(NULL))
    },

    check_score = function(score) {
      valid_scores = "ueif"
      if (!score %in% valid_scores) {
        stop(paste("Invalid score. Valid scores:", paste(valid_scores, collapse = ", ")))
      }
      return(invisible(NULL))
    },

    # Common nuisance estimation method for all survival classes
    ml_nuisance_est_superlearner = function(train_data, test_data, time_grid) {

      # Fit both survival and censoring functions using survSuperLearner
      # Single call returns both event.SL.predict and cens.SL.predict
      surv_results = fit_survival_functions(
        data = train_data,
        time_col = private$time_col_,
        event_col = private$event_col_,
        treatment_col = self$data$d_cols,
        covariate_cols = self$data$x_cols,
        event_sl_lib = private$ml_g_surv_,
        cens_sl_lib = private$ml_g_cens_,
        new_data = test_data,
        new_times = time_grid
      )

      # Fit propensity score using SuperLearner
      propensity = fit_propensity_score(
        data = train_data,
        treatment_col = self$data$d_cols,
        covariate_cols = self$data$x_cols,
        sl_library = private$ml_m_,
        new_data = test_data
      )

      # Winsorize to avoid numerical issues
      propensity = winsorize_values(propensity, lower = 1e-3, upper = 1 - 1e-3)

      return(list(
        S_a0 = winsorize_values(surv_results$S_a0, lower = 1e-3, upper = 1 - 1e-3),
        S_a1 = winsorize_values(surv_results$S_a1, lower = 1e-3, upper = 1 - 1e-3),
        G_a0 = winsorize_values(surv_results$G_a0, lower = 1e-3, upper = 1 - 1e-3),
        G_a1 = winsorize_values(surv_results$G_a1, lower = 1e-3, upper = 1 - 1e-3),
        propensity = propensity,
        time_grid = time_grid
      ))
    }
  )
)
