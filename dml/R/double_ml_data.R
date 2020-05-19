#' @title DoubleMLData
#'
#' @description
#' Data structure for double machine learning. 
#
#'
DoubleMLData <- R6Class("DoubleMLData", public = list(
  
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class. 
    #' 
    #' @param data (`any`)\cr The format of the input data depends on the specialization. E.g., a R [data.frame], [data.table], [Matrix], ..
  initialize = function(self, 
                        data, 
                        x_cols, 
                        y_col, 
                        d_cols,
                        z_col = NULL){
    
    # assertions? 
    
    self$data = data
    self$x_cols = x_cols
    self$y_col = d_cols
    self$z_col = z_col
    self$set_y_z()
    
    # by default, we initialize to the first treatment variable
    self$set_x_d(d_cols[1])
    },
  
  x = function(self){
    return(self)
  },
  
  set_x_d = function(self, treatment_var){
    checkmate::check_character(treatment_var)
    checkmate::check_subset(treatment_var, self$d_cols)
    xd_list = c(self$x_cols, self$d_cols)
    
  }
  )
)


# Input: Matrices or vectors; Output: DoubleMLData
double_ml_data_from_matrix = function(X, y, d, z = NULL){

  X = assure_matrix(X)
  y = assure_matrix(y)
  d = assure_matrix(d)
  
  
  if (is.null(z)){
    check_matrix_row(list(X,y,d))
    data = data.table::data.table(X, y, d)
  }

  else {
    z = assure_matrix(z)
    check_matrix_row(list(X,y,d,z))
    data = data.table::data.table(X,y,d,z)
  }
  
  if (is.null(z)){
      z_col = NULL
  }
    
  else {
    z_col = paste0("z", 1:ncol(z))
    }
    
  y_col = "y" 
  d_cols = paste0("d", 1:ncol(d))
  x_cols = paste0("X", 1:ncol(X))
  names(data) = c(x_cols, y_col, d_cols, z_col)

  return(data)
  
}
  
vec_to_matrix = function(X){
  
}


# double_ml_data_from_data_frame = function(df, )
