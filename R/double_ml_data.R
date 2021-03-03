#' @title Double machine learning data-backend
#'
#' @description
#' Double machine learning data-backend.
#' 
#' `DoubleMLData` objects can be initialized from a [data.table][data.table::data.table()]. Alternatively `DoubleML` provides functions to initialize from a collection of `matrix` objects or a `data.frame`. The following functions can be used to create a new instance of `DoubleMLData`. 
#' * `DoubleMLData$new()` for initialization from a `data.table`. 
#' * [double_ml_data_from_matrix()] for initialization from `matrix` objects,
#' * [double_ml_data_from_data_frame()] for initialization from a `data.frame`. 
#'
#'
#' @examples
#' library(DoubleML)
#' df = make_plr_CCDDHNR2018(return_type = "data.table")
#' obj_dml_data = DoubleMLData$new(df,
#'                                y_col = "y",
#'                                d_cols = "d")
#' @export
DoubleMLData = R6Class("DoubleMLData", public = list(
  #' @field all_variables (`character()`)\cr 
  #' All variables available in the dataset.
  all_variables = NULL,

  #' @field d_cols (`character()`)\cr 
  #' The treatment variable(s). 
  d_cols = NULL,

  #' @field data ([`data.table`][data.table::data.table()])\cr 
  #' Data object.
  data = NULL, 
  
  #' @field data_model ([`data.table`][data.table::data.table()])\cr 
  #' Internal data object that implements the causal model as specified by the user via `y_col`, `d_cols`, `x_cols` and `z_cols`. 
  data_model = NULL,
  
  #' @field n_instr (`NULL`, `integer(1)`) \cr 
  #' The number of instruments. 
  n_instr = NULL,
  
  #' @field n_obs (`integer(1)`) \cr 
  #' The number of observations.
  n_obs = NULL, 
 
  #' @field n_treat (`integer(1)`) \cr 
  #' The umber of treatment variables. 
  n_treat = NULL,

  #' @field other_treat_cols (`NULL`, `character()`) \cr
  #' If `use_other_treat_as_covariate` is `TRUE`, `other_treat_cols` are the treatment variables that are not "active" in the multiple-treatment case. These variables then are internally added to the covariates `x_cols` during the fitting stage. If `use_other_treat_as_covariate` is `FALSE`, `other_treat_cols` is `NULL`. 
  other_treat_cols = NULL,  

  #' @field treat_col (`character(1)`) \cr
  #' "Active" treatment variable in the multiple-treatment case. 
  treat_col = NULL,

  #' @field use_other_treat_as_covariate (`logical(1)`) \cr
  #' Indicates whether in the multiple-treatment case the other treatment variables should be added as covariates. Default is `TRUE`. 
  use_other_treat_as_covariate = TRUE, 

  #' @field x_cols (`NULL`, `character()`) \cr
  #' The covariates. If `NULL`, all variables (columns of `data`) which are neither specified as outcome variable `y_col`, nor as treatment variables `d_cols`, nor as instrumental variables `z_cols` are used as covariates. Default is `NULL`.
  x_cols = NULL, 
  
  #' @field y_col (`character(1)`) \cr
  #' The outcome variable. 
  y_col = NULL,
  
  #' @field z_cols (`NULL`, `character()`) \cr
  #' The instrumental variables. Default is `NULL`. 
  z_cols = NULL, 
  
  #' @description
  #' Creates a new instance of this [R6][R6::R6Class] class.
  #' 
  #' @param data ([`data.table`][data.table::data.table()])\cr 
  #' Data object.
  #' 
  #' @param y_col (`character(1)`) \cr
  #' The outcome variable. 
  #' 
  #' @param d_cols (`character()`) \cr
  #' The treatment variable(s). 
  #' 
  #' @param x_cols (`NULL`, `character()`) \cr
  #' The covariates. If `NULL`, all variables (columns of `data`) which are neither specified as outcome variable `y_col`, nor as treatment variables `d_cols`, nor as instrumental variables `z_cols` are used as covariates. Default is `NULL`.
  #' 
  #' @param z_cols (`NULL`, `character()`) \cr
  #' The instrumental variables. Default is `NULL`. 
  #' 
  #' @param use_other_treat_as_covariate (`logical(1)`) \cr
  #' Indicates whether in the multiple-treatment case the other treatment variables should be added as covariates. Default is `TRUE`. 
  initialize = function(data = NULL, 
                        x_cols = NULL,
                        y_col = NULL, 
                        d_cols = NULL,
                        z_cols = NULL, 
                        use_other_treat_as_covariate = TRUE){
    
    # TBD: Input data.frame
    if (all(class(data)=="data.frame")){
      stop("'data' is a data.frame, use 'double_ml_data_from_data_frame' call to instantiate DoubleMLData.")
    }
    checkmate::assert_class(data, "data.table")
    if (!is.null(x_cols)) {
      checkmate::assert_character(x_cols)
    }
    checkmate::assert_character(y_col)
    checkmate::assert_character(d_cols)
    if (!is.null(z_cols)) {
      checkmate::assert_character(z_cols)
    }
    if (any(d_cols %in% x_cols)){
      stop(paste("Invalid model specification.\n", 
            "Treatment variables must not be elements of the control variables X."))
    }
    if (y_col %in% x_cols){
      stop(paste("Invalid model specification.\n", 
            "Dependent variable must not be an element of the control variables X."))
    }
    if (any(z_cols %in% x_cols)){
      stop(paste("Invalid model specification.\n", 
            "Instrumental variables must not be an element of the control variables X."))
    }
    if (any(z_cols %in% d_cols)){
      stop(paste("Invalid model specification.\n", 
            "Instrumental variables must not be an element of the treatment variables d."))
    }
    
    
    checkmate::assert_logical(use_other_treat_as_covariate, len = 1)
        
    self$data = data
    self$data_model = NULL
    
    self$y_col = y_col
    self$d_cols = d_cols
    self$z_cols = z_cols
    
    if (!is.null(x_cols)) {
      self$x_cols = x_cols
    } else {
        if (!is.null(self$z_cols)) {
          y_d_z = unique(c(self$y_col, self$d_cols, self$z_cols))
          self$x_cols = setdiff(names(data), y_d_z)
        } else {
          y_d = union(self$y_col, self$d_cols)
          self$x_cols = setdiff(names(data), y_d)
        }
    }

    self$treat_col = NULL
    self$other_treat_cols = NULL
    self$use_other_treat_as_covariate = use_other_treat_as_covariate

    # by default, we initialize to the first treatment variable
    self$set_data_model(d_cols[1]) 
    
    self$all_variables = names(self$data)
    self$n_treat = length(self$d_cols)
    self$n_instr = length(self$z_cols)
    self$n_obs = dim(self$data)[1]

    invisible(self)
  },
  
  #' @description
  #' Setter function for `data_model`. The function implements the causal model as specified by the user via `y_col`, `d_cols`, `x_cols` and `z_cols` and assigns the role for the treatment variables in the multiple-treatment case. 
  #' @param treatment_var (`character()`)\cr 
  #' Active treatment variable that will be set to `treat_col`. 
  set_data_model = function(treatment_var){
    checkmate::check_character(treatment_var, max.len = 1)
    checkmate::check_subset(treatment_var, self$d_cols)
    
    if (treatment_var %in% self$x_cols){
      stop("The specified treatment variable must not be an element of the control variables X.")
    }
    
    if (any(self$d_cols %in% self$x_cols)){
      stop("The specified treatment variables must not be an element of the control variables X.")
    }
    
    self$treat_col = treatment_var
    
    if (length(self$d_cols) > 1) {
      self$other_treat_cols = self$d_cols[self$d_cols != treatment_var]
    }
    if (length(self$d_cols) > 1 & self$use_other_treat_as_covariate == FALSE) {
      message("Controls variables do not include other treatment variables")
      self$other_treat_cols = NULL
    }
    col_indx = c(self$x_cols, self$y_col, self$treat_col, self$other_treat_cols, self$z_cols)
    self$data_model = self$data[, col_indx, with = FALSE]
    stopifnot(nrow(self$data) == nrow(self$data_model))

    # successful assigning treatment variable
    message(paste0("Set treatment variable d to ", treatment_var, "."))

    invisible(self)
  }
 )
)



#' @title Wrapper for Double machine learning data-backend initialization from data.frame. 
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
#' The covariates. If `NULL`, all variables (columns of `data`) which are neither specified as outcome variable `y_col`, nor as treatment variables `d_cols`, nor as instrumental variables `z_cols` are used as covariates. Default is `NULL`.
#' 
#' @param z_cols (`NULL`, `character()`) \cr
#' The instrumental variables. Default is `NULL`. 
#' 
#' @param data_class (`character(1)`) \cr
#' Class of returned object. By default, an object of class `DoubleMLData` is returned. Setting `data_class = "data.table"` returns an object of class `data.table`. 
#' 
#' @param use_other_treat_as_covariate (`logical(1)`) \cr
#' Indicates whether in the multiple-treatment case the other treatment variables should be added as covariates. Default is `TRUE`. 
#'
#' @return Creates a new instance of class `DoubleMLData` by default. Class of returned object may change with input provided by option `data_class`. 
#' 
#' @examples
#' df = make_plr_CCDDHNR2018(return_type = "data.frame")
#' x_names = names(df)[grepl("X", names(df))]
#' obj_dml_data = double_ml_data_from_data_frame(df = df, x_cols = x_names, 
#'                                              y_col = "y", d_cols = "d")
# Input: Data frame, Output: DoubleMLData or data.table
#' @export
double_ml_data_from_data_frame = function(df, x_cols = NULL, y_col = NULL,
                                              d_cols = NULL, z_cols = NULL, 
                                              data_class = "DoubleMLData", 
                                              use_other_treat_as_covariate = TRUE){
  
  if (is.null(y_col) | is.null(d_cols)) {
    stop("Column indices y_col and d_cols not specified.")
  }
  checkmate::check_choice(data_class, c("DoubleMLData", "data.table"))
  
  if (!is.null(x_cols)) {
    checkmate::check_character(x_cols)
  }
  checkmate::check_character(y_col)
  checkmate::check_character(d_cols)
  
  if (!is.null(z_cols)){
    checkmate::check_character(z_cols)
  }
  if (any(d_cols %in% x_cols)){
    stop(paste("Invalid model specification.\n", 
          "Treatment variables must not be elements of the control variables X."))
  }
  if (y_col %in% x_cols){
    stop(paste("Invalid model specification.\n", 
          "Dependent variable must not be an element of the control variables X."))
  }
  if (any(z_cols %in% x_cols)){
    stop(paste("Invalid model specification.\n", 
          "Instrumental variables must not be an element of the control variables X."))
  }
  if (any(z_cols %in% d_cols)){
    stop(paste("Invalid model specification.\n", 
          "Instrumental variables must not be an element of the treatment variables d."))
  }
    
  if (!is.null(x_cols)) {
    x_cols = x_cols
  } else {
      if (!is.null(z_cols)) {
        y_d_z = unique(c(y_col, d_cols, z_cols))
        x_cols = setdiff(names(df), y_d_z)
      } else {
        y_d = union(y_col, d_cols)
        x_cols = setdiff(names(df), y_d)
      }
  }
  col_indx =  c(x_cols, y_col, d_cols, z_cols)
  data = data.table(df)[, col_indx, with = FALSE]
  if (data_class == "DoubleMLData") {
    data = DoubleMLData$new(data, x_cols = x_cols, y_col = y_col, d_cols = d_cols, 
                            z_cols = z_cols, 
                            use_other_treat_as_covariate = use_other_treat_as_covariate)
  }
  return(data)
}

#' @title Wrapper for Double machine learning data-backend initialization from matrix.
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
#' @param data_class (`character(1)`) \cr
#' Class of returned object. By default, an object of class `DoubleMLData` is returned. Setting `data_class = "data.table"` returns an object of class `data.table`. 
#' 
#' 
#' @param use_other_treat_as_covariate (`logical(1)`) \cr
#' Indicates whether in the multiple-treatment case the other treatment variables should be added as covariates. Default is `TRUE`. 
#' 
#' @return  Creates a new instance of class `DoubleMLData`. 
#' 
#' @examples
#' matrix_list = make_plr_CCDDHNR2018(return_type = "matrix")
#' obj_dml_data = double_ml_data_from_matrix(X = matrix_list$X, 
#'                                           y = matrix_list$y, 
#'                                           d = matrix_list$d)
#' @export
double_ml_data_from_matrix = function(X = NULL, y, d, z = NULL, data_class = "DoubleMLData", 
                                      use_other_treat_as_covariate = TRUE){
  checkmate::check_choice(data_class, c("DoubleMLData", "data.table"))
  checkmate::check_logical(use_other_treat_as_covariate, len = 1)
  if (!is.null(X)) {
    X = assure_matrix(X)
  }
  y = assure_matrix(y)
  d = assure_matrix(d)
  if (is.null(z)){
    if (!is.null(X)) {
      check_matrix_row(list(X, y, d))
    } else {
      check_matrix_row(list(y, d))
    }
    data = data.table(X, y, d)
  } else {
    z = assure_matrix(z)
    if (!is.null(X)) {
      check_matrix_row(list(X, y, d, z))
    } else {
      check_matrix_row(list(y, d, z))
    }
    data = data.table(X, y, d, z)
  }
  if (!is.null(z)){
    if (ncol(z) == 1) {
      z_cols = "z"
    } else {
      z_cols = paste0("z", 1:ncol(z))
    }
  } else {
    z_cols = NULL
  }
  y_col = "y" 
  if (ncol(d) == 1){
    d_cols = "d"
  } else {
    d_cols = paste0("d", 1:ncol(d))
  }
  if (!is.null(X)) {
    x_cols = paste0("X", 1:ncol(X))
  } else {
    x_cols = NULL
  }
  names(data) = c(x_cols, y_col, d_cols, z_cols)
  
  if (data_class == "DoubleMLData") {
    data = DoubleMLData$new(data, x_cols = x_cols, y_col = y_col, d_cols = d_cols, 
                            z_cols = z_cols, 
                            use_other_treat_as_covariate = use_other_treat_as_covariate)
  }
  return(data)
}