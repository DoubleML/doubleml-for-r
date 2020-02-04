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
                        se_reestimate=FALSE,
                        n_rep_cross_fit=1, 
                        param_set_g = NULL,
                        param_set_m = NULL,
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
                                        resolution = 5) )
  {
    super$initialize_double_ml(n_folds,
                               ml_learners,
                               params,
                               dml_procedure,
                               inf_model,
                               se_reestimate,
                               n_rep_cross_fit, 
                               param_set_g,
                               param_set_m,
                               tune_settings)
  }
),
private = list(
  ml_nuisance_and_score_elements = function(data, smpls, y, d, ...) {
    # nuisance g
    task_g <- initiate_regr_task(paste0("nuis_g_", y), data,
                                 skip_cols = d, target = y)
    
    ml_g <- initiate_learner(self$ml_learners$mlmethod_g,
                             self$params$params_g)
    
    resampling_g <- mlr3::rsmp("custom")$instantiate(task_g,
                                                     smpls$train_ids,
                                                     smpls$test_ids)
    
    r_g <- mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
    
    g_hat = extract_prediction(r_g)
    
    # nuisance m
    task_m <- initiate_regr_task(paste0("nuis_m_", d), data,
                                 skip_cols = y, target = d)
    
    ml_m <- initiate_learner(self$ml_learners$mlmethod_m,
                             self$params$params_m)
    
    resampling_m <- mlr3::rsmp("custom")$instantiate(task_m,
                                                     smpls$train_ids,
                                                     smpls$test_ids)
    
    r_m <- mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
    
    m_hat = extract_prediction(r_m)
    
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
  tune_params = function(data, smpls, y, d, param_set_g, param_set_m, ...){
    
    checkmate::check_class(param_set_g, "ParamSet")    
    checkmate::check_class(param_set_m, "ParamSet")
    
    data_tune_list = lapply(smpls$train_ids, function(x) extract_training_data(data, x))

    if (any(class(self$tune_settings$CV_tune) == "Resampling")) {
      CV_tune = self$tune_settings$CV_tune
    } else {
      CV_tune = mlr3::rsmp(self$tune_settings$rsmp_tune, folds = self$tune_settings$n_folds_tune)
    }
    
    if (any(class(self$tune_settings$measure_g) == "Measure")) {
      measure_g = self$tune_settings$measure_g
    } else {
      measure_g = mlr3::msr(self$tune_settings$measure_g)
    }
    
    if (any(class(tune_settings$measure_m) == "Measure")) {
      measure_m = self$tune_settings$measure_m
    } else {
      measure_m = mlr3::msr(self$tune_settings$measure_m)
    }
    
    terminator = self$tune_settings$terminator
    
    task_g = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_g_", y), x,
                                                skip_cols = d, target = y))
    
    tuning_instance_g = lapply(task_g, function(x) TuningInstance$new(task = x,
                                          learner = self$ml_learners$mlmethod_g,
                                          resampling = CV_tune,
                                          measures = measure_g,
                                          param_set = param_set_g,
                                          terminator = terminator))
    
    tuner = mlr3tuning::tnr(self$tune_settings$algorithm, resolution = self$tune_settings$resolution)
    tuning_result_g = lapply(tuning_instance_g, function(x) tune_instance(tuner, x))
    
    task_m = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_m_", d), x,
                                                  skip_cols = y, target = d))
    
    tuning_instance_m = lapply(task_m, function(x) TuningInstance$new(task = x,
                                          learner = self$ml_learners$mlmethod_m,
                                          resampling = CV_tune,
                                          measures = measure_m,
                                          param_set = param_set_m,
                                          terminator = terminator))
    
    tuning_result_m = lapply(tuning_instance_m, function(x) tune_instance(tuner, x))
    
    tuning_result = list(tuning_result_g = tuning_result_g, 
                         tuning_result_m = tuning_result_m )
    return(tuning_result)
    
  }
)
)

