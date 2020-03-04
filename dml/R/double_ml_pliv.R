#' DoubleMLPLIV R6 class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export

DoubleMLPLIV <- R6Class("DoubleMLPLIV", inherit = DoubleML, public = list(
  initialize = function(n_folds,
                        ml_learners,
                        params = list(params_m = list(),
                                      params_g = list(),
                                      params_r = list()),
                        dml_procedure,
                        inf_model,
                        se_reestimate=FALSE,
                        n_rep_cross_fit=1,
                        param_set = list(params_m = list(),
                                      params_g = list(),
                                      params_r = list()),
                        tune_settings = list(n_folds_tune = 5,
                                        n_rep_tune = 1, 
                                        rsmp_tune = "cv", 
                                        measure_g = "regr.mse", 
                                        measure_m = "regr.mse",
                                        measure_r = "regr.mse",
                                        terminator = mlr3tuning::term("evals", n_evals = 20), 
                                        algorithm = "grid_search",
                                        tuning_instance_g = NULL, 
                                        tuning_instance_m = NULL,
                                        tuning_instance_r = NULL,
                                        tuner = "grid_search",
                                        resolution = 5),
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
    
    # nuisance m
    task_m <- initiate_regr_task(paste0("nuis_m_", z), data,
                                 skip_cols = c(y, d), target = z)
    
    # nuisance r
    task_r <- initiate_regr_task(paste0("nuis_r_", d), data,
                                 skip_cols = c(y, z), target = d)
    
    if (is.null(self$param_tuning)){
      
       if (length(params$params_g)==0){
          message("Parameter of learner for nuisance part g are not tuned, results might not be valid!")
       }
      
       if (length(params$params_m)==0){
          message("Parameter of learner for nuisance part m are not tuned, results might not be valid!")
       }
       
       if (length(params$params_m)==0){
          message("Parameter of learner for nuisance part r are not tuned, results might not be valid!")
       }
    
      ml_g <- initiate_learner(self$ml_learners$mlmethod_g,
                               params$params_g)
      resampling_g <- mlr3::rsmp("custom")$instantiate(task_g,
                                                       smpls$train_ids,
                                                       smpls$test_ids)
      r_g <- mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
      g_hat <- extract_prediction(r_g)$response
      
      ml_m <- initiate_learner(self$ml_learners$mlmethod_m,
                               params$params_m)
      resampling_m <- mlr3::rsmp("custom")$instantiate(task_m,
                                                       smpls$train_ids,
                                                       smpls$test_ids)
      r_m <- mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
      m_hat <- extract_prediction(r_m)$response
    
      ml_r <- initiate_learner(self$ml_learners$mlmethod_r,
                               params$params_r)
      resampling_r <- mlr3::rsmp("custom")$instantiate(task_r,
                                                     smpls$train_ids,
                                                     smpls$test_ids)
      r_r <- mlr3::resample(task_r, ml_r, resampling_r, store_models = TRUE)
      r_hat <- extract_prediction(r_r)$response
    }
    
    else if (!is.null(self$param_tuning)){
      ml_g <- lapply(params$params_g, function(x) initiate_learner(self$ml_learners$mlmethod_g, 
                                                                        x))
      resampling_g <- initiate_resampling(task_g, smpls$train_ids, smpls$test_ids)
      r_g <- resample_dml(task_g, ml_g, resampling_g, store_models = TRUE)
      g_hat <- lapply(r_g, extract_prediction)
      g_hat <- rearrange_prediction(g_hat)
      
      ml_m <- lapply(params$params_m, function(x) initiate_learner(self$ml_learners$mlmethod_m, 
                                                                        x))
      resampling_m = initiate_resampling(task_m, smpls$train_ids, smpls$test_ids)
      r_m = resample_dml(task_m, ml_m, resampling_m, store_models = TRUE)
      m_hat = lapply(r_m, extract_prediction)
      m_hat = rearrange_prediction(m_hat)
      
      ml_m <- lapply(params$params_r, function(x) initiate_learner(self$ml_learners$mlmethod_r, 
                                                                        x))
      resampling_r = initiate_resampling(task_r, smpls$train_ids, smpls$test_ids)
      r_r = resample_dml(task_r, ml_r, resampling_r, store_models = TRUE)
      r_hat = lapply(r_r, extract_prediction)
      r_hat = rearrange_prediction(r_hat)
    }
    
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

