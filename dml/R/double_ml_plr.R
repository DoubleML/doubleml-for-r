#' DoubleMLPLR R6 class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export

DoubleMLPLR <- R6Class("DoubleMLPLR", inherit = DoubleML, public = list(
  initialize = function(n_folds,
                        ml_learners,
                        params = list(params_m = list(),
                                      params_g = list()),
                        dml_procedure,
                        se_reestimate,
                        inf_model,
                        n_rep_cross_fit=1) {
    super$initialize_double_ml(n_folds,
                               ml_learners,
                               params,
                               dml_procedure,
                               se_reestimate,
                               inf_model,
                               n_rep_cross_fit)
  }
),
private = list(
  ml_nuisance_and_score_elements = function(data, y, d, ...) {
    # nuisance g
    task_g <- initiate_regr_task(paste0("nuis_g_", y), data,
                                 skip_cols = d, target = y)
    
    ml_g <- initiate_learner(self$ml_learners$mlmethod_g,
                             self$params$params_g)
    
    resampling_g <- private$instantiate_resampling(task_g)
    
    r_g <- mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
    
    g_hat = extract_prediction(r_g)
    
    # nuisance m
    task_m <- initiate_regr_task(paste0("nuis_m_", d), data,
                                 skip_cols = y, target = d)
    
    ml_m <- initiate_learner(self$ml_learners$mlmethod_m,
                             self$params$params_m)
    
    resampling_m <- private$instantiate_resampling(task_m)
    
    r_m <- mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
    
    m_hat = extract_prediction(r_m)
    
    D <- data[ , d]
    Y <- data[ , y]
    v_hat <- D - m_hat
    u_hat <- Y - g_hat
    v_hatd <- v_hat * D
    
    if (self$inf_model == 'IV-type') {
      private$score_a = -v_hatd
    } else if (self$inf_model == 'DML2018') {
      private$score_a = -v_hat * v_hat
    }
    private$score_b = v_hat * u_hat
    
    invisible(self)
  }
)
)

