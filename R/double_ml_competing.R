#' @title Base Class for Competing Risks Double Machine Learning
#'
#' @description
#' Abstract base class for double machine learning estimation of causal effects
#' in competing risks settings. This class extends DoubleML to handle multiple
#' event types where the occurrence of one event precludes the others.
#'
#' @details
#' This is an abstract class that provides common infrastructure for competing
#' risks analysis including:
#' * Nuisance function estimation for cause-specific survival functions
#' * Complementary cause survival function estimation
#' * Censoring and propensity score estimation
#' * Cross-fitting and sample splitting procedures
#'
#' Specific implementations like DoubleMLCIFJ and DoubleMLRMTLJ inherit from
#' this class and implement their own score functions.
#'
#' @seealso [DoubleML], [DoubleMLSurvival]
#'
#' @export
DoubleMLCompeting = R6Class("DoubleMLCompeting",
  inherit = DoubleML,
  public = list(
    #' @description
    #' Creates a new instance of this R6 class.
    #' Note: This is an abstract class - use specific implementations.
    #'
    #' @param data (`DoubleMLData`) \cr
    #' The `DoubleMLData` object providing the competing risks data.
    #'
    #' @param ml_g_surv (character vector) \cr
    #' survSuperLearner library for overall event survival functions S(t|X,A).
    #'
    #' @param ml_g_surv_j (character vector) \cr
    #' survSuperLearner library for cause-specific survival Sj(t|X,A).
    #'
    #' @param ml_g_surv_jbar (character vector) \cr
    #' survSuperLearner library for complementary cause survival Sjbar(t|X,A).
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
    #' Name of the event column in the data (0=censored, 1=cause 1, 2=cause 2, ...).
    #'
    #' @param cause (`integer(1)`) \cr
    #' The cause of interest (default is 1).
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
      ml_g_surv_j,
      ml_g_surv_jbar,
      ml_g_cens,
      ml_m,
      admin_cens,
      time_col,
      event_col,
      cause = 1,
      freq_time = NULL,
      ml_framework = "SuperLearner",
      n_folds = 5,
      n_rep = 1,
      score = "ueif",
      dml_procedure = "dml2",
      draw_sample_splitting = TRUE,
      apply_cross_fitting = TRUE) {

      # Prevent direct instantiation of abstract base class
      if (identical(class(self)[1], "DoubleMLCompeting")) {
        stop("DoubleMLCompeting is an abstract base class. Use specific implementations like DoubleMLCIFJ or DoubleMLRMTLJ.")
      }

      # Store competing risks-specific parameters
      private$admin_cens_ = admin_cens
      private$time_col_ = time_col
      private$event_col_ = event_col
      private$cause_ = cause
      private$freq_time_ = freq_time
      private$ml_framework_ = ml_framework

      # Store ML learners
      private$ml_g_surv_ = ml_g_surv
      private$ml_g_surv_j_ = ml_g_surv_j
      private$ml_g_surv_jbar_ = ml_g_surv_jbar
      private$ml_g_cens_ = ml_g_cens
      private$ml_m_ = ml_m

      # Check data structure (pass data directly, self$data not set yet)
      private$check_data(data)

      # Check score function
      private$check_score(score)

      # Validate ML framework
      if (ml_framework != "SuperLearner") {
        stop("Only SuperLearner framework is supported")
      }

      # Initialize parent DoubleML class using initialize_double_ml
      super$initialize_double_ml(
        data,
        n_folds,
        n_rep,
        score,
        dml_procedure,
        draw_sample_splitting,
        apply_cross_fitting
      )
    },

    #' @description
    #' Get time grid for competing risks analysis.
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

      return(time_grid)
    },

    #' @description
    #' Validates competing risks data structure.
    #'
    #' @param print_messages (`logical(1)`) \cr
    #' Whether to print validation messages. Default is TRUE.
    #' @return List with validation results.
    validate_data = function(print_messages = TRUE) {
      # Use self$data$data after initialization is complete
      private$validate_data_internal(self$data, print_messages)
    },

    #' @description
    #' Fit the competing risks model. Overrides base class fit() to bypass
    #' psi_a/psi_b score decomposition, which does not apply to competing risks.
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
    #' @field admin_cens Administrative censoring time.
    admin_cens = function() {
      private$admin_cens_
    },
    #' @field time_col Name of time column.
    time_col = function() {
      private$time_col_
    },
    #' @field event_col Name of event column.
    event_col = function() {
      private$event_col_
    },
    #' @field cause Cause of interest.
    cause = function() {
      private$cause_
    },
    #' @field ml_framework Machine learning framework.
    ml_framework = function() {
      private$ml_framework_
    }
  ),

  private = list(
    admin_cens_ = NULL,
    freq_time_ = NULL,
    ml_framework_ = NULL,
    time_col_ = NULL,
    event_col_ = NULL,
    cause_ = NULL,
    ml_g_surv_ = NULL,
    ml_g_surv_j_ = NULL,
    ml_g_surv_jbar_ = NULL,
    ml_g_cens_ = NULL,
    ml_m_ = NULL,

    # Check data validity
    check_data = function(data) {
      if (!private$time_col_ %in% names(data$data)) {
        stop(sprintf("Time column '%s' not found in data", private$time_col_))
      }
      if (!private$event_col_ %in% names(data$data)) {
        stop(sprintf("Event column '%s' not found in data", private$event_col_))
      }

      # Validate data structure (pass data directly since self$data not set yet)
      private$validate_data_internal(data, print_messages = FALSE)
    },

    # Check score validity
    check_score = function(score) {
      if (score != "ueif") {
        stop("Only 'ueif' score is supported for competing risks models")
      }
    },

    # Internal validation that works with data parameter
    validate_data_internal = function(data, print_messages = TRUE) {
      dt = data$data
      time_col = private$time_col_
      event_col = private$event_col_
      d_col = data$d_cols

      # Check for missing values
      if (any(is.na(dt[[time_col]]))) {
        stop("Missing values found in time column")
      }
      if (any(is.na(dt[[event_col]]))) {
        stop("Missing values found in event column")
      }

      # Check for negative or zero times
      if (any(dt[[time_col]] <= 0)) {
        stop("Time values must be positive")
      }

      # Check event values (must be 0, 1, 2, ...)
      unique_events = unique(dt[[event_col]])
      if (length(unique_events) == 0) {
        stop("Event column is empty")
      }
      if (!all(unique_events >= 0 & unique_events == floor(unique_events))) {
        stop("Event column must contain non-negative integer values (0=censored, 1=cause 1, 2=cause 2, etc.)")
      }

      # Check treatment values (must be 0 or 1)
      if (!all(dt[[d_col]] %in% c(0, 1))) {
        stop("Treatment column must be binary (0 or 1)")
      }

      # Calculate censoring rates
      n_obs = nrow(dt)
      n_censored = sum(dt[[event_col]] == 0)
      censoring_rate = n_censored / n_obs

      # Count events by cause
      n_cause1 = sum(dt[[event_col]] == 1)
      n_cause2 = sum(dt[[event_col]] == 2)

      # Sample sizes by treatment
      n_treated = sum(dt[[d_col]] == 1)
      n_control = sum(dt[[d_col]] == 0)

      if (print_messages) {
        message(sprintf("Total observations: %d", n_obs))
        message(sprintf("Censored: %d (%.1f%%)", n_censored, censoring_rate * 100))
        message(sprintf("Cause 1 events: %d (%.1f%%)", n_cause1, n_cause1 / n_obs * 100))
        message(sprintf("Cause 2 events: %d (%.1f%%)", n_cause2, n_cause2 / n_obs * 100))
        message(sprintf("Treated (A=1): %d (%.1f%%)", n_treated, n_treated / n_obs * 100))
        message(sprintf("Control (A=0): %d (%.1f%%)", n_control, n_control / n_obs * 100))
      }

      invisible(list(
        n_obs = n_obs,
        n_censored = n_censored,
        censoring_rate = censoring_rate,
        n_cause1 = n_cause1,
        n_cause2 = n_cause2,
        n_treated = n_treated,
        n_control = n_control
      ))
    },

    # Common nuisance estimation for competing risks (SuperLearner framework)
    ml_nuisance_est_superlearner = function(train_data, test_data, time_grid) {
      # Extract column names
      time_col = private$time_col_
      event_col = private$event_col_
      d_col = self$data$d_cols
      x_cols = self$data$x_cols

      # Fit overall event survival functions S(t|X,A) and censoring G(t|X,A)
      # Single call returns both event.SL.predict and cens.SL.predict
      surv_result = fit_survival_functions(
        data = train_data,
        time_col = time_col,
        event_col = event_col,
        treatment_col = d_col,
        covariate_cols = x_cols,
        event_sl_lib = private$ml_g_surv_,
        cens_sl_lib = private$ml_g_cens_,
        new_data = test_data,
        new_times = time_grid
      )

      S_a0 = surv_result$S_a0
      S_a1 = surv_result$S_a1
      G_a0 = surv_result$G_a0
      G_a1 = surv_result$G_a1

      # Fit cause-specific survival functions Sj(t|X,A)
      # Create temporary datasets with modified event indicators
      train_data_j = data.table::copy(train_data)
      train_data_j[[event_col]] = ifelse(train_data[[event_col]] == private$cause_, 1, 0)

      surv_j_result = fit_survival_functions(
        data = train_data_j,
        time_col = time_col,
        event_col = event_col,
        treatment_col = d_col,
        covariate_cols = x_cols,
        event_sl_lib = private$ml_g_surv_j_,
        cens_sl_lib = private$ml_g_cens_,
        new_data = test_data,
        new_times = time_grid
      )

      Sj_a0 = surv_j_result$S_a0
      Sj_a1 = surv_j_result$S_a1

      # Fit complementary cause survival functions Sjbar(t|X,A)
      train_data_jbar = data.table::copy(train_data)
      train_data_jbar[[event_col]] = ifelse(train_data[[event_col]] != 0 & train_data[[event_col]] != private$cause_, 1, 0)

      surv_jbar_result = fit_survival_functions(
        data = train_data_jbar,
        time_col = time_col,
        event_col = event_col,
        treatment_col = d_col,
        covariate_cols = x_cols,
        event_sl_lib = private$ml_g_surv_jbar_,
        cens_sl_lib = private$ml_g_cens_,
        new_data = test_data,
        new_times = time_grid
      )

      Sjbar_a0 = surv_jbar_result$S_a0
      Sjbar_a1 = surv_jbar_result$S_a1

      # Fit propensity score P(A=1|X)
      propensity = fit_propensity_score(
        data = train_data,
        treatment_col = d_col,
        covariate_cols = x_cols,
        sl_library = private$ml_m_,
        new_data = test_data
      )

      # Winsorize survival and censoring functions
      S_a0 = winsorize_values(S_a0, lower = 1e-3, upper = 1 - 1e-3)
      S_a1 = winsorize_values(S_a1, lower = 1e-3, upper = 1 - 1e-3)
      Sj_a0 = winsorize_values(Sj_a0, lower = 1e-3, upper = 1 - 1e-3)
      Sj_a1 = winsorize_values(Sj_a1, lower = 1e-3, upper = 1 - 1e-3)
      Sjbar_a0 = winsorize_values(Sjbar_a0, lower = 1e-3, upper = 1 - 1e-3)
      Sjbar_a1 = winsorize_values(Sjbar_a1, lower = 1e-3, upper = 1 - 1e-3)
      G_a0 = winsorize_values(G_a0, lower = 1e-3, upper = 1 - 1e-3)
      G_a1 = winsorize_values(G_a1, lower = 1e-3, upper = 1 - 1e-3)
      propensity = winsorize_values(propensity, lower = 1e-3, upper = 1 - 1e-3)

      # Return all nuisance estimates
      return(list(
        S_a0 = S_a0,
        S_a1 = S_a1,
        Sj_a0 = Sj_a0,
        Sj_a1 = Sj_a1,
        Sjbar_a0 = Sjbar_a0,
        Sjbar_a1 = Sjbar_a1,
        G_a0 = G_a0,
        G_a1 = G_a1,
        propensity = propensity,
        time_grid = time_grid
      ))
    }
  )
)
