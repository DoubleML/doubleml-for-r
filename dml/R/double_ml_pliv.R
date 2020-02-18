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
                        se_reestimate=FALSE,
                        n_rep_cross_fit=1,
                        param_set = NULL,
                        tune_settings = list(),
                        param_tuning = NULL) {
    super$initialize_double_ml(n_folds,
                               ml_learners,
                               params,
                               dml_procedure,
                               inf_model,
                               se_reestimate,
                               n_rep_cross_fit,
                               param_set,
                               tune_settings,
                               param_tuning)
  }
),
private = list(
  n_nuisance = 3,
  ml_nuisance_and_score_elements = function(data, smpls, y, d, z, params) {
    # nuisance g
    task_g <- initiate_regr_task(paste0("nuis_g_", y), data,
                                 skip_cols = c(d, z), target = y)
    
    ml_g <- initiate_learner(self$ml_learners$mlmethod_g,
                             self$params$params_g)
    
    resampling_g <- mlr3::rsmp("custom")$instantiate(task_g,
                                                     smpls$train_ids,
                                                     smpls$test_ids)
    
    r_g <- mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
    
    g_hat = extract_prediction(r_g)$response
    
    
    # nuisance m
    task_m <- initiate_regr_task(paste0("nuis_m_", z), data,
                                 skip_cols = c(y, d), target = z)
    
    ml_m <- initiate_learner(self$ml_learners$mlmethod_m,
                             self$params$params_m)
    
    resampling_m <- mlr3::rsmp("custom")$instantiate(task_m,
                                                     smpls$train_ids,
                                                     smpls$test_ids)
    
    r_m <- mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
    
    m_hat = extract_prediction(r_m)$response
    
    
    # nuisance r
    task_r <- initiate_regr_task(paste0("nuis_r_", d), data,
                                 skip_cols = c(y, z), target = d)
    
    ml_r <- initiate_learner(self$ml_learners$mlmethod_r,
                             self$params$params_r)
    
    resampling_r <- mlr3::rsmp("custom")$instantiate(task_r,
                                                     smpls$train_ids,
                                                     smpls$test_ids)
    
    r_r <- mlr3::resample(task_r, ml_r, resampling_r, store_models = TRUE)
    
    r_hat = extract_prediction(r_r)$response
    
    
    D <- data[ , d]
    Y <- data[ , y]
    Z <- data[ , z]
    w_hat <- Z - m_hat
    u_hat <- Y - g_hat
    v_hat <- D - r_hat
    
    # note that v & w are flipped in python
    if (self$inf_model == 'partialling-out') {
      score_a = -v_hat * w_hat
      score_b = u_hat * w_hat
    }
    
    return(list(score_a = score_a,
                score_b = score_b))
  }
)
)

