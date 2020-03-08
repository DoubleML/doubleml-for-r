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
                        inf_model,
                        subgroups = NULL,
                        se_reestimate=FALSE,
                        n_rep_cross_fit=1,
                        param_set = list(param_set_m = list(),
                                           param_set_g = list()),
                        tune_settings = list(n_folds_tune = 5,
                                        n_rep_tune = 1, 
                                        rsmp_tune = "cv", 
                                        measure_g = "regr.mse", 
                                        measure_m = "regr.mse",
                                        terminator = mlr3tuning::term("evals", n_evals = 20), 
                                        algorithm = "grid_search",
                                        tuning_instance_g = NULL, 
                                        tuning_instance_m = NULL,
                                        tuner = "grid_search",
                                        resolution = 5), 
                        param_tuning = NULL)  {
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
  n_nuisance = 2,
  ml_nuisance_and_score_elements = function(data, smpls, y, d, z = NULL, params, ...) {
    
    # nuisance g
    task_g <- initiate_regr_task(paste0("nuis_g_", y), data, 
                                  skip_cols = d, target = y)
          
    # nuisance m
    task_m <- initiate_regr_task(paste0("nuis_m_", d), data,
                                   skip_cols = y, target = d)
      
    if (is.null(self$param_tuning)){
      
      if (length(params$params_g)==0){
        message("Parameter of learner for nuisance part g are not tuned, results might not be valid!")
      }
      
      if (length(params$params_m)==0){
        message("Parameter of learner for nuisance part m are not tuned, results might not be valid!")
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
    }
    
    D <- data[ , d]
    Y <- data[ , y]
    v_hat <- D - m_hat
    u_hat <- Y - g_hat
    v_hatd <- v_hat * D
    
    if (self$inf_model == 'IV-type') {
      score_a = -v_hatd
    } else if (self$inf_model == 'DML2018') {
      score_a = -v_hat * v_hat
    }
    score_b = v_hat * u_hat
    
    return(list(score_a = score_a,
                score_b = score_b))
  }, 
  tune_params = function(data, smpls, y, d, param_set, tune_settings, ...){
    
    checkmate::check_class(param_set$param_set_g, "ParamSet")    
    checkmate::check_class(param_set$param_set_m, "ParamSet")
    
    data_tune_list = lapply(smpls$train_ids, function(x) extract_training_data(data, x))

    if (any(class(tune_settings$rsmp_tune) == "Resampling")) {
      CV_tune = tune_settings$rsmp_tune
    } else {
      CV_tune = mlr3::rsmp(tune_settings$rsmp_tune, folds = tune_settings$n_folds_tune)
    }
    
    if (any(class(tune_settings$measure_g) == "Measure")) {
      measure_g = tune_settings$measure_g
    } else {
      measure_g = mlr3::msr(tune_settings$measure_g)
    }
    
    if (any(class(tune_settings$measure_m) == "Measure")) {
      measure_m = tune_settings$measure_m
    } else {
      measure_m = mlr3::msr(tune_settings$measure_m)
    }
    
    terminator = tune_settings$terminator
    
    task_g = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_g_", y), x,
                                                skip_cols = d, target = y))
    
    ml_g <- mlr3::lrn(self$ml_learners$mlmethod_g)
    
    tuning_instance_g = lapply(task_g, function(x) TuningInstance$new(task = x,
                                          learner = ml_g,
                                          resampling = CV_tune,
                                          measures = measure_g,
                                          param_set = param_set$param_set_g,
                                          terminator = terminator))
    
    tuner = mlr3tuning::tnr(tune_settings$algorithm, resolution = tune_settings$resolution)
    tuning_result_g = lapply(tuning_instance_g, function(x) tune_instance(tuner, x))
    
    task_m = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_m_", d), x,
                                                  skip_cols = y, target = d))
    
    ml_m <- mlr3::lrn(self$ml_learners$mlmethod_m)

    tuning_instance_m = lapply(task_m, function(x) TuningInstance$new(task = x,
                                          learner = ml_m,
                                          resampling = CV_tune,
                                          measures = measure_m,
                                          param_set = param_set$param_set_m,
                                          terminator = terminator))
    
    tuning_result_m = lapply(tuning_instance_m, function(x) tune_instance(tuner, x))
    
    tuning_result = list(tuning_result = list(tuning_result_g = tuning_result_g, 
                                              tuning_result_m = tuning_result_m),
                         params = list(params_g = extract_tuned_params(tuning_result_g), 
                                       params_m = extract_tuned_params(tuning_result_m)))
    
    return(tuning_result)
    
  }
)
)

