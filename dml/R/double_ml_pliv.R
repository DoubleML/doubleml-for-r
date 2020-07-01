#' DoubleMLPLIV R6 class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export

DoubleMLPLIV <- R6Class("DoubleMLPLIV", inherit = DoubleML, public = list(
  initialize = function(data, 
                        n_folds,
                        ml_learners,
                        params = list(params_m = list(),
                                      params_g = list(),
                                      params_r = list()),
                        dml_procedure,
                        inf_model,
                        subgroups =  NULL,
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
    super$initialize_double_ml(data, 
                               n_folds,
                               ml_learners,
                               params,
                               dml_procedure,
                               inf_model,
                               subgroups,
                               se_reestimate,
                               n_rep_cross_fit,
                               param_set,
                               tune_settings,
                               param_tuning)
  }
),
private = list(
  n_nuisance = 3,
  ml_nuisance_and_score_elements = function(data, smpls, params) {
    # nuisance g
    task_g <- initiate_regr_task(paste0("nuis_g_", data$y_col), data$data_model,
                                 skip_cols = c("d", "z"), target = "y")
    
    # nuisance m
    task_m <- initiate_regr_task(paste0("nuis_m_", data$z_col), data$data_model,
                                 skip_cols = c("y", "d"), target = "z")
    
    # nuisance r
    task_r <- initiate_regr_task(paste0("nuis_r_", data$treat_col), data$data_model,
                                 skip_cols = c("y", "z"), target = "d")
    
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
      
      ml_r <- lapply(params$params_r, function(x) initiate_learner(self$ml_learners$mlmethod_r, 
                                                                        x))
      resampling_r = initiate_resampling(task_r, smpls$train_ids, smpls$test_ids)
      r_r = resample_dml(task_r, ml_r, resampling_r, store_models = TRUE)
      r_hat = lapply(r_r, extract_prediction)
      r_hat = rearrange_prediction(r_hat)
    }
    
    D <- data$data_model$d
    Y <- data$data_model$y
    Z <- data$data_model$z
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
  },
  tune_params = function(data, smpls, y, d, z, param_set, tune_settings, ...){
    
    checkmate::check_class(param_set$param_set_g, "ParamSet")    
    checkmate::check_class(param_set$param_set_m, "ParamSet")
    checkmate::check_class(param_set$param_set_r, "ParamSet")

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
    
    if (any(class(tune_settings$measure_r) == "Measure")) {
      measure_r = tune_settings$measure_r
    } else {
      measure_r = mlr3::msr(tune_settings$measure_r)
    }
    
    terminator = tune_settings$terminator
    
    task_g = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_g_", y), x,
                                                skip_cols = c("d", "z"), target = "y"))
    
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
                                                  skip_cols = c("y", "z"), target = "d"))
    
    ml_m <- mlr3::lrn(self$ml_learners$mlmethod_m)

    tuning_instance_m = lapply(task_m, function(x) TuningInstance$new(task = x,
                                          learner = ml_m,
                                          resampling = CV_tune,
                                          measures = measure_m,
                                          param_set = param_set$param_set_m,
                                          terminator = terminator))
    
    tuning_result_m = lapply(tuning_instance_m, function(x) tune_instance(tuner, x))
    
    task_r = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_r_",z), x,
                                                  skip_cols = c("y", "d"), target = "z"))
    ml_r <- mlr3::lrn(self$ml_learners$mlmethod_r)

    tuning_instance_r = lapply(task_r, function(x) TuningInstance$new(task = x,
                                          learner = ml_r,
                                          resampling = CV_tune,
                                          measures = measure_r,
                                          param_set = param_set$param_set_r,
                                          terminator = terminator))
    
    tuning_result_r = lapply(tuning_instance_r, function(x) tune_instance(tuner, x))
    
    tuning_result = list(tuning_result = list(tuning_result_g = tuning_result_g, 
                                              tuning_result_m = tuning_result_m, 
                                              tuning_result_r = tuning_result_r),
                         params = list(params_g = extract_tuned_params(tuning_result_g), 
                                       params_m = extract_tuned_params(tuning_result_m),
                                       params_r = extract_tuned_params(tuning_result_r)))
    
    return(tuning_result)
    
  }
)
)

