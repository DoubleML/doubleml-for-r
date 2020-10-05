#' @title DoubleMLData
#'
#' @description
#' Data structure for double machine learning. 
#'
#' @export
DoubleMLData <- R6Class("DoubleMLData", public = list(
   
  data = NULL, 
  data_model = NULL,
  x_cols = NULL, 
  y_col = NULL, 
  d_cols = NULL,
  treat_col = NULL,
  other_treat_cols = NULL,
  other_treat = NULL,
  z_cols = NULL, 
  
  use_other_treat_as_covariate = TRUE, 
  
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

    checkmate::check_class(data, "data.table")
    checkmate::check_class(use_other_treat_as_covariate, "logical")
        
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
    self$set__data_model(d_cols[1], use_other_treat_as_covariate = self$use_other_treat_as_covariate) 
    
    invisible(self)

  },

  all_variables = function(){
    return(names(self$data))
  },
  
  n_treat = function(){
    return(length(self$d_cols))
  },
  
  n_instr = function(){
    return(length(self$z_cols))
  },
  
  n_obs = function(){
    return(dim(self$data)[1])
  },
  
  x = function(){
    return(self$data[, self$x_cols, with = FALSE])
  }, 
  
  y = function(){
    return(self$data[, self$y_col, with = FALSE])
  }, 
  
  z = function(){
    return(self$data[, self$z_cols, with = FALSE])
  }, 
  
  d = function(){
    return(self$data[, self$d_cols, with = FALSE])
  },
  
  set__data_model = function(treatment_var, use_other_treat_as_covariate = TRUE){
    
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
      
    if (length(self$d_cols) > 1 & use_other_treat_as_covariate == FALSE) {
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


# Input: Matrices or vectors; Output: DoubleMLData or data.table
#' @export
double_ml_data_from_matrix = function(X, y, d, z = NULL, data_class = "DoubleMLData", 
                                      use_other_treat_as_covariate = TRUE){

  checkmate::check_choice(data_class, c("DoubleMLData", "data.table"))

  X = assure_matrix(X)
  y = assure_matrix(y)
  d = assure_matrix(d)
  
  
  if (is.null(z)){
    check_matrix_row(list(X, y, d))
    data = data.table::data.table(X, y, d)
  }

  else {
    z = assure_matrix(z)
    check_matrix_row(list(X, y, d, z))
    data = data.table::data.table(X, y, d, z)
  }
  
  if (!is.null(z)){
    
    if (ncol(z) == 1) {
      z_cols = "z"
    }
    
    else {
      z_cols = paste0("z", 1:ncol(z))
    }
      
  }
    
  else {
    z_cols = NULL
  }
    
  y_col = "y" 

  if (ncol(d) == 1){
    d_cols = "d"
  }
  
  else {
    d_cols = paste0("d", 1:ncol(d))
  }
  
  x_cols = paste0("X", 1:ncol(X))
  
  names(data) = c(x_cols, y_col, d_cols, z_cols)
  
  if (data_class == "DoubleMLData") {

  data = DoubleMLData$new(data, x_cols = x_cols, y_col = y_col, d_cols = d_cols, 
                          z_cols = z_cols, 
                          use_other_treat_as_covariate = use_other_treat_as_covariate)
  }
  
  return(data)
  
}

# Input: Data frame, Output: DoubleMLData or data.table
#' @export
double_ml_data_from_data_frame = function(df, x_cols = NULL, y_col = NULL,
                                              d_cols = NULL, z_cols = NULL, 
                                              data_class = "DoubleMLData", 
                                              use_other_treat_as_covariate = TRUE){
  
  if ( is.null(x_cols) | is.null(y_col) | is.null(d_cols)){
    stop("Column indices x_cols, y_col and d_cols not specified.")
  }
  
  checkmate::check_choice(data_class, c("DoubleMLData", "data.table"))
  checkmate::check_character(x_cols)
  checkmate::check_character(y_col)
  checkmate::check_character(d_cols)
  
  if (!is.null(z_cols)){
    checkmate::check_character(z_cols)
  }
  
  col_indx =  c(x_cols, y_col, d_cols, z_cols)
  data = data.table::data.table(df)[, col_indx, with = FALSE]

  if (data_class == "DoubleMLData") {
    
    data = DoubleMLData$new(data, x_cols = x_cols, y_col = y_col, d_cols = d_cols, 
                            z_cols = z_cols, 
                            use_other_treat_as_covariate = use_other_treat_as_covariate)
  }
  
  return(data)
}

