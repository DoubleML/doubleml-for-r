#' @title DoubleMLData
#'
#' @description
#' Data structure for double machine learning. 
#
#'
#'
DoubleMLData <- R6Class("DoubleMLData", public = list(
   
  data = NULL, 
  data_model = NULL,
  x_cols = NULL, 
  y_col = NULL, 
  d_cols = NULL,
  treat_col = NULL,
  other_treat_cols = NULL,
  other_treat = NULL,
  z_col = NULL, 
  d = NULL, 
  X = NULL,
  y = NULL,
  z = NULL,
  use_other_treat_as_covariate = TRUE, 
  
  initialize = function(data = NULL, 
                        x_cols = NULL,
                        y_col = NULL, 
                        d_cols = NULL,
                        z_col = NULL, 
                        use_other_treat_as_covariate = TRUE){
    
    # TBD: Input data.frame
    if (all(class(data)=="data.frame")){
      stop("'data' is a data.frame, use 'double_ml_data_from_data_frame' call to instantiate DoubleMLData.")
    }

    checkmate::check_class(data, "data.table")
    checkmate::check_class(use_other_treat_as_covariate, "logical")
        
    col_indx =  c(x_cols, y_col, d_cols, z_col)
    self$data = data[, col_indx, with = FALSE]
    self$data_model = NULL
  
    self$x_cols = x_cols
    self$y_col = y_col
    self$d_cols = d_cols
    self$treat_col = NULL
    self$other_treat_cols = NULL
    self$other_treat = NULL
    self$use_other_treat_as_covariate = use_other_treat_as_covariate
    self$z_col = z_col

    self$set__y_z()

    # by default, we initialize to the first treatment variable
    self$set__x_d(d_cols[1], use_other_treat_as_covariate = self$use_other_treat_as_covariate) 
    
    invisible(self)

  },

  all_variables = function(){
    return(names(self$data))
  },
  
  n_treat = function(){
    return(length(self$d_cols))
  },
  
  n_obs = function(){
    return(dim(self$data)[1])
  },
  
  set__x_d = function(treatment_var = NULL, use_other_treat_as_covariate = TRUE){
  
    checkmate::check_character(treatment_var, max.len = 1)
    checkmate::check_subset(treatment_var, self$d_cols)
    
    if (treatment_var %in% self$x_cols){
      stop("The specified treatment variable must not be an element of the control variables X.")
    }
    
    if (any(self$d_cols %in% self$x_cols)){
      stop("The specified treatment variables must not be an element of the control variables X.")
    }
    self$treat_col = treatment_var
    self$other_treat_cols = self$d_cols[self$d_cols != treatment_var]
    
    self$d = data.table::data.table(self$data[, treatment_var, with = FALSE])
    names(self$d) = treatment_var
    message(paste0("Set treatment variable d to ", treatment_var, "."))
    
    if (length(self$d_cols) > 1 & use_other_treat_as_covariate == TRUE) {
    self$other_treat = data.table::data.table(self$data[, self$other_treat_cols, with = FALSE])
    names(self$other_treat) = self$other_treat_cols
    }
    
    else {
      
      if (length(self$d_cols) > 1) {
        message("Controls variables do not include other treatment variables")
      }
      
      self$other_treat = NULL
      self$other_treat_cols = NULL
    }
      
    self$X = data.table::data.table(self$data[, self$x_cols, with = FALSE])
    self$x_cols = names(self$X)
    
    self$set__data_model()
    
    invisible(self)
  }, 
  
  set__y_z = function(){
    self$y = data.table::data.table(self$data[, self$y_col, with = FALSE])
    names(self$y) = paste0(self$y_col)

    if (is.null(self$z_col)){
      self$z = NULL
    }
    
    else {
      self$z = data.table::data.table(self$data[, self$z_col, with = FALSE])
      names(self$z) = paste0(self$z_col)

    }
    
    self$set__data_model()

    invisible(self)
  },
  
  set__data_model = function(){
    
    col_indx = c(self$x_cols, self$y_col, self$treat_col, self$other_treat_cols, self$z_col)

    self$data_model = data.table::data.table(self$data[, col_indx, with = FALSE])

    invisible(self)
  }
 )
)


# Input: Matrices or vectors; Output: DoubleMLData or data.table
double_ml_data_from_matrix = function(X, y, d, z = NULL, data_class = "DoubleMLData"){

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
      z_col = "z"
    }
    
    else {
      z_col = paste0("z", 1:ncol(z))
    }
      
  }
    
  else {
    z_col = NULL
    }
    
  y_col = "y" 

  if (ncol(d) == 1){
    d_cols = "d"
  }
  
  else {
    d_cols = paste0("d", 1:ncol(d))
  }
  
  x_cols = paste0("X", 1:ncol(X))
  
  names(data) = c(x_cols, y_col, d_cols, z_col)
  
  if (data_class == "DoubleMLData") {

  data = DoubleMLData$new(data, x_cols = x_cols, y_col = y_col, d_cols = d_cols, 
                          z_col = z_col)
  }
  
  return(data)
  
}

# Input: Data frame, Output: DoubleMLData or data.table
  
double_ml_data_from_data_frame = function(df, x_cols = NULL, y_col = NULL,
                                              d_cols = NULL, z_col = NULL, 
                                              data_class = "DoubleMLData"){
  
  if ( is.null(x_cols) | is.null(y_col) | is.null(d_cols)){
    stop("Column indices x_cols, y_col and d_cols not specified.")
  }
  
  checkmate::check_choice(data_class, c("DoubleMLData", "data.table"))
  checkmate::check_character(x_cols)
  checkmate::check_character(y_col)
  checkmate::check_character(d_cols)
  
  if (!is.null(z_col)){
    checkmate::check_character(z_col)
  }
  
  col_indx =  c(x_cols, y_col, d_cols, z_col)
  data = data.table::data.table(df)[, col_indx, with = FALSE]
   
  y_name = "y" 
  x_names = paste0("X", 1:length(x_cols))
  
  if (length(d_cols) == 1){
    d_names = "d"
  }
  
  else {
    d_names = paste0("d", 1:length(d_cols))
  }
  
  if (!is.null(z_col)){
    if (length(z_col) == 1) {
      z_name = "z"
    }
    
    else {
      z_name = paste0("z", 1:length(z_col))
    }
  }
    
  else {
    z_name = NULL
  }
  names(data) = c(x_names, y_name, d_names, z_name)
  
  if (data_class == "DoubleMLData") {
    
    data = DoubleMLData$new(data, x_cols = x_names, y_col = y_name, d_cols = d_names, 
                          z_col = z_name)
  }
  
  return(data)
}






