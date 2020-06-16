#' @title DoubleMLData
#'
#' @description
#' Data structure for double machine learning. 
#
#'
#'
DoubleMLData <- R6Class("DoubleMLData", public = list(
   
  data = NULL, 
  x_cols = NULL, 
  y_col = NULL, 
  d_cols = NULL, 
  z_col = NULL, 
  d = NULL, 
  X = NULL,
  y = NULL,
  z = NULL,
  
  initialize = function(data = NULL, 
                        x_cols = NULL,
                        y_col = NULL, 
                        d_cols = NULL,
                        z_col = NULL){
    
    if (all(class(data) == "data.frame")){
      data = double_ml_data_from_data_frame(data, x_cols = x_cols, y_col = y_col,
                                              d_cols = d_cols, z_col = z_col)
    }
    
    checkmate::check_class(data, "data.table")
      
    self$data = data
    self$x_cols = x_cols
    self$y_col = y_col
    self$d_cols = d_cols
    self$z_col = z_col

    self$set__y_z()

    # by default, we initialize to the first treatment variable
    self$set__x_d(d_cols[1]) 

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
  
  set__x_d = function(treatment_var = NULL){
    checkmate::check_character(treatment_var)
    checkmate::check_subset(treatment_var, self$d_cols)
    xd_list = c(self$x_cols, self$d_cols)
    xd_list = xd_list[xd_list != treatment_var]

    self$d = data.table::data.table(self$data[, treatment_var, with = FALSE])
    names(self$d) = paste0(treatment_var)
    
    self$X = data.table::data.table(self$data[, xd_list, with = FALSE])
    
    invisible(self)
  }, 
  
  set__y_z = function(){
    self$y = data.table::data.table(self$data[, self$y_col, with = FALSE])
  
    if (is.null(self$z)){
      self$z = NULL
    }
    
    else {
      self$z = data.table::data.table(self$data[, self$z_cols, with = FALSE])
    }
    
    invisible(self)
  }
 )
)


# Input: Matrices or vectors; Output: DoubleMLData
double_ml_data_from_matrix = function(X, y, d, z = NULL){

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

  return(data)
  
}
  
double_ml_data_from_data_frame = function(df, x_cols = NULL, y_col = NULL,
                                              d_cols = NULL, z_col = NULL){
  
  if ( is.null(x_cols) | is.null(y_col) | is.null(d_cols)){
    stop("Column indices x_cols, y_col and d_cols not specified.")
  }
  
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
  
  return(data)
}






