#' DoubleMLPLIV R6 class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export

DoubleMLPLIV <- R6Class("DoubleMLPLIV", inherit = DoubleML, public = list(
  initialize = function(n_folds,
                        ml_learners,
                        params = list(params_m = list(),
                                      params_g = list()),
                        dml_procedure,
                        inf_model,
                        n_rep_cross_fit=1) {
    super$initialize_double_ml(n_folds,
                               ml_learners,
                               params,
                               dml_procedure,
                               inf_model,
                               n_rep_cross_fit)
  }
),
private = list(
  ml_nuisance_and_score_elements = function(data, y, d, z) {
    # nuisance g
    g_indx <- names(data) != d & names(data) != z
    data_g <- data[ , g_indx, drop = FALSE]
    task_g <- mlr3::TaskRegr$new(id = paste0("nuis_g_", d), backend = data_g, target = y)
    
    # instantiate resampling
    resampling_g <- private$instantiate_resampling(task_g)
    
    ml_g <- mlr3::lrn(self$ml_learners$mlmethod_g)
    ml_g$param_set$values <- self$params$params_g
    
    r_g <- mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
    
    g_hat = as.data.table(r_g$prediction())
    setorder(g_hat, 'row_id')
    g_hat = g_hat$response
    
    
    # nuisance m
    m_indx <- names(data) != y & names(data) != d
    data_m <- data[, m_indx, drop = FALSE]
    task_m <- mlr3::TaskRegr$new(id = paste0("nuis_m_", z), backend = data_m, target = z)
    ml_m <- mlr3::lrn(self$ml_learners$mlmethod_m)
    ml_m$param_set$values <- self$params$params_m # tbd: check if parameter passing really works
    
    # instantiate resampling
    resampling_m <- private$instantiate_resampling(task_m)
    
    r_m <- mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
    
    m_hat <- as.data.table(r_m$prediction())
    setorder(m_hat, 'row_id')
    m_hat <- m_hat$response
    
    
    # nuisance r
    r_indx <- names(data) != y & names(data) != z
    data_r <- data[, r_indx, drop = FALSE]
    task_r <- mlr3::TaskRegr$new(id = paste0("nuis_r_", d), backend = data_r, target = d)
    ml_r <- mlr3::lrn(self$ml_learners$mlmethod_r)
    ml_r$param_set$values <- self$params$params_r # tbd: check if parameter passing really works
    
    # instantiate resampling
    resampling_r <- private$instantiate_resampling(task_r)
    
    r_r <- mlr3::resample(task_r, ml_r, resampling_r, store_models = TRUE)
    
    r_hat <- as.data.table(r_r$prediction())
    setorder(r_hat, 'row_id')
    r_hat <- r_hat$response
    
    
    D <- data[ , d]
    Y <- data[ , y]
    Z <- data[ , z]
    w_hat <- Z - m_hat
    u_hat <- Y - g_hat
    v_hat <- D - r_hat
    
    # note that v & w are flipped in python
    if (self$inf_model == 'partialling-out') {
      private$score_a = -v_hat * w_hat
      private$score_b = u_hat * w_hat
    }
    
    invisible(self)
  }
)
)

