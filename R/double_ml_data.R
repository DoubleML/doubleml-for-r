#' @title Double machine learning data-backend
#'
#' @description
#' Double machine learning data-backend.
#'
#' `DoubleMLData` objects can be initialized from a
#' [data.table][data.table::data.table()]. Alternatively `DoubleML` provides
#' functions to initialize from a collection of `matrix` objects or
#' a `data.frame`. The following functions can be used to create a new
#' instance of `DoubleMLData`.
#' * `DoubleMLData$new()` for initialization from a `data.table`.
#' * [double_ml_data_from_matrix()] for initialization from `matrix` objects,
#' * [double_ml_data_from_data_frame()] for initialization from a `data.frame`.
#'
#'
#' @examples
#' library(DoubleML)
#' df = make_plr_CCDDHNR2018(return_type = "data.table")
#' obj_dml_data = DoubleMLData$new(df,
#'   y_col = "y",
#'   d_cols = "d")
#' @export
DoubleMLData = R6Class("DoubleMLData",
  active = list(
    #' @field all_variables (`character()`)\cr
    #' All variables available in the dataset.
    all_variables = function(value) {
      if (missing(value)) {
        return(names(self$data))
      } else {
        stop("can't set field all_variables")
      }
    },

    #' @field d_cols (`character()`)\cr
    #' The treatment variable(s).
    d_cols = function(value) {
      if (missing(value)) {
        return(private$d_cols_)
      } else {
        d_cols = value # to get more meaningful assert error messages
        reset_value = !is.null(self$data_model)
        assert_character(d_cols, unique = TRUE)
        assert_subset(d_cols, self$all_variables)
        private$d_cols_ = d_cols
        if (reset_value) {
          private$check_disjoint_sets()
          self$set_data_model(self$d_cols[1])
        }
      }
    },

    #' @field data ([`data.table`][data.table::data.table()])\cr
    #' Data object.
    data = function(value) {
      if (missing(value)) {
        return(private$data_)
      } else {
        stop("can't set field data")
      }
    },

    #' @field data_model ([`data.table`][data.table::data.table()])\cr
    #' Internal data object that implements the causal model as specified by
    #' the user via `y_col`, `d_cols`, `x_cols` and `z_cols`.
    data_model = function(value) {
      if (missing(value)) {
        return(private$data_model_)
      } else {
        stop("can't set field data_model")
      }
    },

    #' @field n_instr (`NULL`, `integer(1)`) \cr
    #' The number of instruments.
    n_instr = function(value) {
      if (missing(value)) {
        return(length(self$z_cols))
      } else {
        stop("can't set field n_instr")
      }
    },

    #' @field n_obs (`integer(1)`) \cr
    #' The number of observations.
    n_obs = function(value) {
      if (missing(value)) {
        return(dim(self$data)[1])
      } else {
        stop("can't set field n_obs")
      }
    },

    #' @field n_treat (`integer(1)`) \cr
    #' The number of treatment variables.
    n_treat = function(value) {
      if (missing(value)) {
        return(length(self$d_cols))
      } else {
        stop("can't set field n_treat")
      }
    },

    #' @field other_treat_cols (`NULL`, `character()`) \cr
    #' If `use_other_treat_as_covariate` is `TRUE`, `other_treat_cols` are the
    #' treatment variables that are not "active" in the multiple-treatment case.
    #' These variables then are internally added to the covariates `x_cols` during
    #' the fitting stage. If `use_other_treat_as_covariate` is `FALSE`,
    #' `other_treat_cols` is `NULL`.
    other_treat_cols = function(value) {
      if (missing(value)) {
        return(private$other_treat_cols_)
      } else {
        stop("can't set field other_treat_cols")
      }
    },

    #' @field treat_col (`character(1)`) \cr
    #' "Active" treatment variable in the multiple-treatment case.
    treat_col = function(value) {
      if (missing(value)) {
        return(private$treat_col_)
      } else {
        stop("can't set field treat_col")
      }
    },

    #' @field use_other_treat_as_covariate (`logical(1)`) \cr
    #' Indicates whether in the multiple-treatment case the other treatment
    #' variables should be added as covariates. Default is `TRUE`.
    use_other_treat_as_covariate = function(value) {
      if (missing(value)) {
        return(private$use_other_treat_as_covariate_)
      } else {
        use_other_treat_as_covariate = value # to get more meaningful assert error messages
        reset_value = !is.null(self$data_model)
        assert_logical(use_other_treat_as_covariate, len = 1)
        private$use_other_treat_as_covariate_ = use_other_treat_as_covariate
        if (reset_value) {
          private$check_disjoint_sets()
          self$set_data_model(self$d_cols[1])
        }
      }
    },

    #' @field x_cols (`NULL`, `character()`) \cr
    #' The covariates. If `NULL`, all variables (columns of `data`) which are
    #' neither specified as outcome variable `y_col`, nor as treatment variables
    #' `d_cols`, nor as instrumental variables `z_cols` are used as covariates.
    #' Default is `NULL`.
    x_cols = function(value) {
      if (missing(value)) {
        return(private$x_cols_)
      } else {
        x_cols = value # to get more meaningful assert error messages
        reset_value = !is.null(self$data_model)
        if (!is.null(x_cols)) {
          assert_character(x_cols, unique = TRUE)
        }

        if (!is.null(x_cols)) {
          assert_subset(x_cols, self$all_variables)
          private$x_cols_ = x_cols
        } else {
          if (!is.null(self$z_cols) && is.null(self$s_col)) {
            y_d_z = unique(c(self$y_col, self$d_cols, self$z_cols))
            private$x_cols_ = setdiff(self$all_variables, y_d_z)
          } else {
            if (!is.null(self$s_col)) {
              y_d_z_s = unique(c(self$y_col, self$d_cols, self$z_cols, self$s_col))
              private$x_cols_ = setdiff(self$all_variables, y_d_z_s)
            } else {
              y_d = union(self$y_col, self$d_cols)
              private$x_cols_ = setdiff(self$all_variables, y_d)
            }
          }
        }
        if (reset_value) {
          private$check_disjoint_sets()
          self$set_data_model(self$d_cols[1])
        }
      }
    },

    #' @field y_col (`character(1)`) \cr
    #' The outcome variable.
    y_col = function(value) {
      if (missing(value)) {
        return(private$y_col_)
      } else {
        y_col = value # to get more meaningful assert error messages
        reset_value = !is.null(self$data_model)
        assert_character(y_col, len = 1)
        assert_subset(y_col, self$all_variables)
        private$y_col_ = y_col
        if (reset_value) {
          private$check_disjoint_sets()
          self$set_data_model(self$d_cols[1])
        }
      }
    },

    #' @field z_cols (`NULL`, `character()`) \cr
    #' The instrumental variables. Default is `NULL`.
    z_cols = function(value) {
      if (missing(value)) {
        return(private$z_cols_)
      } else {
        z_cols = value # to get more meaningful assert error messages
        reset_value = !is.null(self$data_model)
        if (!is.null(z_cols)) {
          assert_character(z_cols, unique = TRUE)
        }
        assert_subset(z_cols, self$all_variables)
        private$z_cols_ = z_cols
        if (reset_value) {
          private$check_disjoint_sets()
          self$set_data_model(self$d_cols[1])
        }
      }
    },
    #' @field s_col (`NULL`, `character()`) \cr
    #' The score or selection variable (only relevant/used for SSM Estimators). Default is `NULL`.
    s_col = function(value) {
      if (missing(value)) {
        return(private$s_col_)
      } else {
        s_col = value # to get more meaningful assert error messages
        reset_value = !is.null(self$data_model)

        if (!is.null(s_col)) {
          assert_character(s_col, len = 1)
        }
        assert_subset(s_col, self$all_variables)
        private$s_col_ = s_col
        if (reset_value) {
          private$check_disjoint_sets()
          self$set_data_model(self$d_cols[1])
        }
      }
    }
  ),

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param data ([`data.table`][data.table::data.table()], `data.frame()`)\cr
    #' Data object.
    #'
    #' @param y_col (`character(1)`) \cr
    #' The outcome variable.
    #'
    #' @param d_cols (`character()`) \cr
    #' The treatment variable(s).
    #'
    #' @param x_cols (`NULL`, `character()`) \cr
    #' The covariates. If `NULL`, all variables (columns of `data`) which are
    #' neither specified as outcome variable `y_col`, nor as treatment variables
    #' `d_cols`, nor as instrumental variables `z_cols` are used as covariates.
    #' Default is `NULL`.
    #'
    #' @param z_cols (`NULL`, `character()`) \cr
    #' The instrumental variables. Default is `NULL`.
    #'
    #' @param s_col (`NULL`, `character()`) \cr
    #' The score or selection variable (only relevant/used for SSM Estimators). Default is `NULL`.
    #'
    #' @param use_other_treat_as_covariate (`logical(1)`) \cr
    #' Indicates whether in the multiple-treatment case the other treatment
    #' variables should be added as covariates. Default is `TRUE`.
    initialize = function(data = NULL,
      x_cols = NULL,
      y_col = NULL,
      d_cols = NULL,
      z_cols = NULL,
      s_col = NULL,
      use_other_treat_as_covariate = TRUE) {

      if (all(class(data) == "data.frame")) {
        data = data.table(data)
      }
      assert_class(data, "data.table")
      assert_character(names(data), unique = TRUE)

      private$data_ = data

      self$y_col = y_col
      self$d_cols = d_cols
      self$z_cols = z_cols
      self$s_col = s_col
      self$x_cols = x_cols
      private$check_disjoint_sets()

      self$use_other_treat_as_covariate = use_other_treat_as_covariate

      # by default, we initialize to the first treatment variable
      self$set_data_model(d_cols[1])

      invisible(self)
    },

    #' @description
    #' Print DoubleMLData objects.
    print = function() {
      header = "================= DoubleMLData Object ==================\n"
      data_info = paste0(
        "Outcome variable: ", self$y_col, "\n",
        "Treatment variable(s): ", paste0(self$d_cols, collapse = ", "),
        "\n",
        "Covariates: ", paste0(self$x_cols, collapse = ", "), "\n",
        "Instrument(s): ", paste0(self$z_cols, collapse = ", "), "\n",
        "Selection variable: ", paste0(self$s_col, collapse = ", "), "\n",
        "No. Observations: ", self$n_obs, "\n")
      cat(header, "\n",
        "\n------------------ Data summary      ------------------\n",
        data_info,
        sep = "")

      invisible(self)
    },

    #' @description
    #' Setter function for `data_model`. The function implements the causal
    #' model as specified by the user via `y_col`, `d_cols`, `x_cols` and
    #' `z_cols` and assigns the role for the treatment variables in the
    #' multiple-treatment case.
    #' @param treatment_var (`character()`)\cr
    #' Active treatment variable that will be set to `treat_col`.
    set_data_model = function(treatment_var) {

      assert_character(treatment_var, max.len = 1)
      assert_subset(treatment_var, self$d_cols)

      private$treat_col_ = treatment_var

      if (self$n_treat > 1) {
        if (self$use_other_treat_as_covariate) {
          private$other_treat_cols_ = self$d_cols[self$d_cols != treatment_var]
        } else {
          message("Control variables do not include other treatment variables")
          private$other_treat_cols_ = NULL
        }
      }
      col_indx = c(
        self$x_cols, self$y_col, self$treat_col, self$other_treat_cols,
        self$z_cols, self$s_col)
      private$data_model_ = self$data[, col_indx, with = FALSE]
      stopifnot(nrow(self$data) == nrow(self$data_model))

      # successful assigning treatment variable
      if (self$n_treat > 1) {
        message(paste0("Set treatment variable d to ", treatment_var, "."))
      }
      invisible(self)
    }
  ),
  private = list(
    d_cols_ = NULL,
    data_ = NULL,
    data_model_ = NULL,
    other_treat_cols_ = NULL,
    treat_col_ = NULL,
    use_other_treat_as_covariate_ = NULL,
    x_cols_ = NULL,
    y_col_ = NULL,
    z_cols_ = NULL,
    s_col_ = NULL,
    check_disjoint_sets = function() {
      y_col = self$y_col
      x_cols = self$x_cols
      d_cols = self$d_cols

      if (y_col %in% x_cols) {
        stop(paste(
          y_col,
          "cannot be set as outcome variable 'y_col' and",
          "covariate in 'x_cols'."))
      }
      if (y_col %in% d_cols) {
        stop(paste(
          y_col,
          "cannot be set as outcome variable 'y_col' and",
          "treatment variable in 'd_cols'."))
      }
      if (any(d_cols %in% x_cols)) {
        stop(paste(
          "At least one variable/column is set as treatment",
          "variable ('d_cols') and as a covariate ('x_cols').",
          "Consider using parameter 'use_other_treat_as_covariate'."))
      }

      if (!is.null(self$z_cols)) {
        z_cols = self$z_cols

        if (y_col %in% z_cols) {
          stop(paste(
            y_col,
            "cannot be set as outcome variable 'y_col' and",
            "instrumental variable in 'z_cols'."))
        }
        if (any(z_cols %in% d_cols)) {
          stop(paste(
            "At least one variable/column is set as treatment",
            "variable ('d_cols') and instrumental variable in 'z_cols'."))
        }
        if (any(z_cols %in% x_cols)) {
          stop(paste(
            "At least one variable/column is set as covariate ('x_cols')",
            "and instrumental variable in 'z_cols'."))
        }
      }

      if (!is.null(self$s_col)) {
        s_col = self$s_col

        if (y_col %in% s_col) {
          stop(paste(
            y_col,
            "cannot be set as outcome variable 'y_col' and",
            "selection variable in 's_col'."))
        }
        if (any(s_col %in% d_cols)) {
          stop(paste(
            "At least one variable/column is set as treatment",
            "variable ('d_cols') and selection variable in 's_col'."))
        }
        if (any(s_col %in% x_cols)) {
          stop(paste(
            "At least one variable/column is set as covariate ('x_cols')",
            "and selection variable in 's_col'."))
        }
      }
    }
  )
)
#' @title Double machine learning data-backend for data with cluster variables
#'
#' @description
#' Double machine learning data-backend for data with cluster variables.
#'
#' `DoubleMLClusterData` objects can be initialized from a
#' [data.table][data.table::data.table()]. Alternatively `DoubleML` provides
#' functions to initialize from a collection of `matrix` objects or
#' a `data.frame`. The following functions can be used to create a new
#' instance of `DoubleMLClusterData`.
#' * `DoubleMLClusterData$new()` for initialization from a `data.table`.
#' * [double_ml_data_from_matrix()] for initialization from `matrix` objects,
#' * [double_ml_data_from_data_frame()] for initialization from a `data.frame`.
#'
#'
#' @examples
#' library(DoubleML)
#' dt = make_pliv_multiway_cluster_CKMS2021(return_type = "data.table")
#' obj_dml_data = DoubleMLClusterData$new(dt,
#'   y_col = "Y",
#'   d_cols = "D",
#'   z_cols = "Z",
#'   cluster_cols = c("cluster_var_i", "cluster_var_j"))
#' @export
DoubleMLClusterData = R6Class("DoubleMLClusterData",
  inherit = DoubleMLData,
  active = list(
    #' @field cluster_cols (`character()`)\cr
    #' The cluster variable(s).
    cluster_cols = function(value) {
      if (missing(value)) {
        return(private$cluster_cols_)
      } else {
        cluster_cols = value # to get more meaningful assert error messages
        reset_value = !is.null(self$data_model)
        assert_character(cluster_cols, unique = TRUE)
        assert_subset(cluster_cols, self$all_variables)
        private$cluster_cols_ = cluster_cols
        if (reset_value) {
          private$check_disjoint_sets()
          self$set_data_model(self$d_cols[1])
        }
      }
    },

    #' @field x_cols (`NULL`, `character()`) \cr
    #' The covariates. If `NULL`, all variables (columns of `data`) which are
    #' neither specified as outcome variable `y_col`, nor as treatment variables
    #' `d_cols`, nor as instrumental variables `z_cols`, nor as cluster
    #' variables `cluster_cols` are used as covariates.
    #' Default is `NULL`.
    x_cols = function(value) {
      if (missing(value)) {
        return(private$x_cols_)
      } else {
        if (!is.null(value)) {
          super$x_cols = value
        } else {
          if (!is.null(self$z_cols) && is.null(self$s_col)) {
            y_d_z = unique(c(
              self$y_col, self$d_cols, self$z_cols,
              self$cluster_cols))
            x_cols = setdiff(self$all_variables, y_d_z)
          } else {
            if (!is.null(self$s_col)) {
              y_d_z_s = unique(c(self$y_col, self$d_cols, self$z_cols,
                self$s_col, self$cluster_cols))
              x_cols = setdiff(self$all_variables, y_d_z_s)
            } else {
              y_d = unique(c(self$y_col, self$d_cols, self$cluster_cols))
              x_cols = setdiff(self$all_variables, y_d)
            }
          }
          super$x_cols = x_cols
        }
      }
    },

    #' @field n_cluster_vars (`integer(1)`) \cr
    #' The number of cluster variables.
    n_cluster_vars = function(value) {
      if (missing(value)) {
        return(length(self$cluster_cols))
      } else {
        stop("can't set field n_cluster_vars")
      }
    }
  ),
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param data ([`data.table`][data.table::data.table()], `data.frame()`)\cr
    #' Data object.
    #'
    #' @param y_col (`character(1)`) \cr
    #' The outcome variable.
    #'
    #' @param d_cols (`character()`) \cr
    #' The treatment variable(s).
    #'
    #' @param cluster_cols (`character()`) \cr
    #' The cluster variable(s).
    #'
    #' @param x_cols (`NULL`, `character()`) \cr
    #' The covariates. If `NULL`, all variables (columns of `data`) which are
    #' neither specified as outcome variable `y_col`, nor as treatment variables
    #' `d_cols`, nor as instrumental variables `z_cols` are used as covariates.
    #' Default is `NULL`.
    #'
    #' @param z_cols (`NULL`, `character()`) \cr
    #' The instrumental variables. Default is `NULL`.
    #'
    #' @param s_col (`NULL`, `character()`) \cr
    #' The score or selection variable (only relevant/used for SSM Estimators). Default is `NULL`.
    #'
    #' @param use_other_treat_as_covariate (`logical(1)`) \cr
    #' Indicates whether in the multiple-treatment case the other treatment
    #' variables should be added as covariates. Default is `TRUE`.
    initialize = function(data = NULL,
      x_cols = NULL,
      y_col = NULL,
      d_cols = NULL,
      cluster_cols = NULL,
      z_cols = NULL,
      s_col = NULL,
      use_other_treat_as_covariate = TRUE) {

      # we need to set cluster_cols (needs _data) before call to the super class
      # initialize because of the x_cols active binding

      if (all(class(data) == "data.frame")) {
        data = data.table(data)
      }
      assert_class(data, "data.table")
      assert_character(names(data), unique = TRUE)

      private$data_ = data

      self$cluster_cols = cluster_cols

      super$initialize(
        data,
        x_cols,
        y_col,
        d_cols,
        z_cols,
        s_col,
        use_other_treat_as_covariate)
      invisible(self)
    },

    #' @description
    #' Print DoubleMLClusterData objects.
    print = function() {
      header = "================= DoubleMLClusterData Object ==================\n"
      data_info = paste0(
        "Outcome variable: ", self$y_col, "\n",
        "Treatment variable(s): ", paste0(self$d_cols, collapse = ", "), "\n",
        "Cluster variable(s): ", paste0(self$cluster_cols, collapse = ", "),
        "\n",
        "Covariates: ", paste0(self$x_cols, collapse = ", "), "\n",
        "Instrument(s): ", paste0(self$z_cols, collapse = ", "), "\n",
        "Selection variable: ", paste0(self$s_col, collapse = ", "), "\n",
        "No. Observations: ", self$n_obs, "\n")
      cat(header, "\n",
        "\n------------------ Data summary      ------------------\n",
        data_info,
        sep = "")

      invisible(self)
    },

    #' @description
    #' Setter function for `data_model`. The function implements the causal model
    #' as specified by the user via `y_col`, `d_cols`, `x_cols`, `z_cols` and
    #' `cluster_cols` and assigns the role for the treatment variables in the
    #' multiple-treatment case.
    #' @param treatment_var (`character()`)\cr
    #' Active treatment variable that will be set to `treat_col`.
    set_data_model = function(treatment_var) {
      super$set_data_model(treatment_var)

      # add the cluster_cols to the data_model_
      col_indx = c(
        self$x_cols, self$y_col, self$treat_col, self$other_treat_cols,
        self$z_cols, self$s_col, self$cluster_cols)
      private$data_model_ = self$data[, col_indx, with = FALSE]
      stopifnot(nrow(self$data) == nrow(self$data_model))

      invisible(self)
    }
  ),
  private = list(
    cluster_cols_ = NULL,
    check_disjoint_sets = function() {
      # apply the standard checks from the DoubleMLData class

      super$check_disjoint_sets()

      cluster_cols = self$cluster_cols
      y_col = self$y_col
      x_cols = self$x_cols
      d_cols = self$d_cols

      if (y_col %in% cluster_cols) {
        stop(paste(
          y_col,
          "cannot be set as outcome variable 'y_col' and",
          "cluster variable in 'cluster_cols'."))
      }
      if (any(d_cols %in% cluster_cols)) {
        stop(paste(
          "At least one variable/column is set as treatment",
          "variable ('d_cols') and as a cluster variable ('cluster_cols')."))
      }
      if (any(x_cols %in% cluster_cols)) {
        stop(paste(
          "At least one variable/column is set as covariate ('x_cols')",
          "and as a cluster variable ('cluster_cols')."))
      }

      if (!is.null(self$z_cols)) {
        z_cols = self$z_cols

        if (any(z_cols %in% cluster_cols)) {
          stop(paste(
            "At least one variable/column is set as instrumental variable",
            "('z_cols') and as a cluster variable ('cluster_cols')."))
        }
      }

      if (!is.null(self$s_col)) {
        s_col = self$s_col

        if (any(s_col %in% cluster_cols)) {
          stop(paste(
            "At least one variable/column is set as selection variable",
            "('s_col') and as a cluster variable ('cluster_cols')."))
        }
      }
    }
  )
)

#' @title Wrapper for Double machine learning data-backend initialization from
#' data.frame.
#'
#' @description
#' Initalization of DoubleMLData from `data.frame`.
#'
#' @param df (`data.frame()`)\cr
#' Data object.
#'
#' @param y_col (`character(1)`) \cr
#' The outcome variable.
#'
#' @param d_cols (`character()`) \cr
#' The treatment variable(s).
#'
#' @param x_cols (`NULL`, `character()`) \cr
#' The covariates. If `NULL`, all variables (columns of `data`) which are
#' neither specified as outcome variable `y_col`, nor as treatment variables
#' `d_cols`, nor as instrumental variables `z_cols` are used as covariates.
#' Default is `NULL`.
#'
#' @param z_cols (`NULL`, `character()`) \cr
#' The instrumental variables. Default is `NULL`.
#'
#' @param s_col (`NULL`, `character()`) \cr
#' The score or selection variable (only relevant/used for SSM Estimators). Default is `NULL`.
#'
#' @param cluster_cols (`NULL`, `character()`) \cr
#' The cluster variables. Default is `NULL`.
#'
#' @param use_other_treat_as_covariate (`logical(1)`) \cr
#' Indicates whether in the multiple-treatment case the other treatment
#' variables should be added as covariates. Default is `TRUE`.
#'
#' @return Creates a new instance of class `DoubleMLData`.
#'
#' @examples
#' df = make_plr_CCDDHNR2018(return_type = "data.frame")
#' x_names = names(df)[grepl("X", names(df))]
#' obj_dml_data = double_ml_data_from_data_frame(
#'   df = df, x_cols = x_names,
#'   y_col = "y", d_cols = "d")
#' # Input: Data frame, Output: DoubleMLData object
#' @export
double_ml_data_from_data_frame = function(df, x_cols = NULL, y_col = NULL,
  d_cols = NULL, z_cols = NULL, s_col = NULL, cluster_cols = NULL,
  use_other_treat_as_covariate = TRUE) {
  if (is.null(cluster_cols)) {
    data = DoubleMLData$new(df,
      x_cols = x_cols, y_col = y_col, d_cols = d_cols,
      z_cols = z_cols, s_col = s_col,
      use_other_treat_as_covariate = use_other_treat_as_covariate)
  } else {
    data = DoubleMLClusterData$new(df,
      x_cols = x_cols, y_col = y_col,
      d_cols = d_cols, z_cols = z_cols,
      s_col = s_col, cluster_cols = cluster_cols,
      use_other_treat_as_covariate = use_other_treat_as_covariate)
  }
  return(data)
}

#' @title Wrapper for Double machine learning data-backend initialization
#' from matrix.
#'
#' @description
#'
#' Initalization of DoubleMLData from `matrix()` objects.
#'
#' @param X (`matrix()`) \cr
#' Matrix of covariates.
#'
#' @param y (`numeric()`) \cr
#' Vector of outcome variable.
#'
#' @param d (`matrix()`) \cr
#' Matrix of treatment variables.
#'
#' @param z (`matrix()`) \cr
#' Matrix of instruments.
#'
#' @param s (`numeric()`) \cr
#' Vector of the score or selection variable (only relevant for SSM models).
#'
#' @param cluster_vars (`matrix()`) \cr
#' Matrix of cluster variables.
#'
#' @param data_class (`character(1)`) \cr
#' Class of returned object. By default, an object of class `DoubleMLData` is
#' returned. Setting `data_class = "data.table"` returns an object of class
#' `data.table`.
#'
#' @param use_other_treat_as_covariate (`logical(1)`) \cr
#' Indicates whether in the multiple-treatment case the other treatment
#' variables should be added as covariates. Default is `TRUE`.
#'
#' @return  Creates a new instance of class `DoubleMLData`.
#'
#' @examples
#' matrix_list = make_plr_CCDDHNR2018(return_type = "matrix")
#' obj_dml_data = double_ml_data_from_matrix(
#'   X = matrix_list$X,
#'   y = matrix_list$y,
#'   d = matrix_list$d)
#' @export
double_ml_data_from_matrix = function(X = NULL, y, d, z = NULL,
  s = NULL, cluster_vars = NULL,
  data_class = "DoubleMLData",
  use_other_treat_as_covariate = TRUE) {

  assert_choice(data_class, c(
    "DoubleMLData", "data.table",
    "DoubleMLClusterData"))
  assert_logical(use_other_treat_as_covariate, len = 1)

  y = assure_matrix(y)
  d = assure_matrix(d)
  mat_list = list(y, d)

  if (!is.null(X)) {
    X = assure_matrix(X)
    mat_list[[length(mat_list) + 1]] = X
  }
  if (!is.null(z)) {
    z = assure_matrix(z)
    mat_list[[length(mat_list) + 1]] = z
  }
  if (!is.null(s)) {
    s = assure_matrix(s)
    mat_list[[length(mat_list) + 1]] = s
  }
  if (!is.null(cluster_vars)) {
    cluster_vars = assure_matrix(cluster_vars)
    mat_list[[length(mat_list) + 1]] = cluster_vars
  }

  check_matrix_row(mat_list)
  data = data.table(X, y, d, z, s, cluster_vars)

  if (!is.null(z)) {
    if (ncol(z) == 1) {
      z_cols = "z"
    } else {
      z_cols = paste0("z", seq_len(ncol(z)))
    }
  } else {
    z_cols = NULL
  }
  y_col = "y"
  if (ncol(d) == 1) {
    d_cols = "d"
  } else {
    d_cols = paste0("d", seq_len(ncol(d)))
  }
  if (!is.null(X)) {
    x_cols = paste0("X", seq_len(ncol(X)))
  } else {
    x_cols = NULL
  }
  if (!is.null(s)) {
    s_col = "s"
  } else {
    s_col = NULL
  }
  if (!is.null(cluster_vars)) {
    if (ncol(cluster_vars) == 1) {
      cluster_cols = "cluster_var"
    } else {
      cluster_cols = paste0("cluster_var", seq_len(ncol(z)))
    }
  } else {
    cluster_cols = NULL
  }
  names(data) = c(x_cols, y_col, d_cols, z_cols, s_col, cluster_cols)

  if (data_class %in% c("DoubleMLData", "DoubleMLClusterData")) {
    if (is.null(cluster_vars)) {
      if (data_class == "DoubleMLClusterData") {
        stop(paste(
          "To initialize a DoubleMLClusterData object a matrix of cluster",
          "variables (`cluster_vars`) must be provided."))
      }
      data = DoubleMLData$new(data,
        x_cols = x_cols, y_col = y_col, d_cols = d_cols,
        z_cols = z_cols, s_col = s_col,
        use_other_treat_as_covariate = use_other_treat_as_covariate)
    } else {
      data = DoubleMLClusterData$new(data,
        x_cols = x_cols, y_col = y_col, d_cols = d_cols,
        z_cols = z_cols, s_col = s_col, cluster_cols = cluster_cols,
        use_other_treat_as_covariate = use_other_treat_as_covariate)
    }
  }
  return(data)
}
