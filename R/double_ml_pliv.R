#' DoubleMLPLIV R6 class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export

DoubleMLPLIV <- R6Class("DoubleMLPLIV", inherit = DoubleML, public = list(
  ml_g = NULL, 
  ml_m = NULL, 
  ml_r = NULL, 
  g_params = NULL, 
  m_params = NULL, 
  r_params = NULL, 
  
  initialize = function(data, 
                        ml_g,
                        ml_m, 
                        ml_r, 
                        n_folds = 5,
                        n_rep_cross_fit = 1, 
                        score = "partialling out", 
                        dml_procedure = "dml2",
                        draw_sample_splitting = TRUE, 
                        apply_cross_fitting = TRUE) {
    
    super$initialize_double_ml(data, 
                               n_folds,
                               n_rep_cross_fit, 
                               score, 
                               dml_procedure, 
                               draw_sample_splitting, 
                               apply_cross_fitting)
    
    self$ml_g = ml_g
    self$ml_m = ml_m
    self$ml_r = ml_r
  }
),
private = list(
  n_nuisance = 3,
  ml_nuisance_and_score_elements = function(data, smpls, params) {
    # nuisance g
    task_g <- initiate_regr_task(paste0("nuis_g_", data$y_col), data$data_model,
                                 skip_cols = c(data$treat_col, data$z_col), target = data$y_col)
    
    # nuisance m
    task_m <- initiate_regr_task(paste0("nuis_m_", data$z_col), data$data_model,
                                 skip_cols = c(data$y_col, data$treat_col), target = data$z_col)
    
    # nuisance r
    task_r <- initiate_regr_task(paste0("nuis_r_", data$treat_col), data$data_model,
                                 skip_cols = c(data$y_col, data$z_col), target = data$treat_col)
    
    if (is.null(self$param_tuning)){
      
       if (is.null(params$params_g[[data$treat_col]])){
          message("Parameter of learner for nuisance part g are not tuned, results might not be valid!")
       }
      
       if (is.null(params$params_m[[data$treat_col]])){
          message("Parameter of learner for nuisance part m are not tuned, results might not be valid!")
       }
       
       if (is.null(params$params_m)[[data$treat_col]]){
          message("Parameter of learner for nuisance part r are not tuned, results might not be valid!")
       }
    
      ml_g <- initiate_learner(self$ml_learners$mlmethod_g,
                               params$params_g[[data$treat_col]])
      resampling_g <- mlr3::rsmp("custom")$instantiate(task_g,
                                                       smpls$train_ids,
                                                       smpls$test_ids)
      r_g <- mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
      g_hat <- extract_prediction(r_g)$response
      
      ml_m <- initiate_learner(self$ml_learners$mlmethod_m,
                               params$params_m[[data$treat_col]])
      resampling_m <- mlr3::rsmp("custom")$instantiate(task_m,
                                                       smpls$train_ids,
                                                       smpls$test_ids)
      r_m <- mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
      m_hat <- extract_prediction(r_m)$response
    
      ml_r <- initiate_learner(self$ml_learners$mlmethod_r,
                               params$params_r[[data$treat_col]])
      resampling_r <- mlr3::rsmp("custom")$instantiate(task_r,
                                                     smpls$train_ids,
                                                     smpls$test_ids)
      r_r <- mlr3::resample(task_r, ml_r, resampling_r, store_models = TRUE)
      r_hat <- extract_prediction(r_r)$response
    }
    
    else if (!is.null(self$param_tuning)){
      ml_g <- lapply(params$params_g, function(x) initiate_learner(self$ml_learners$mlmethod_g, 
                                                                        x[[1]]))
      resampling_g <- initiate_resampling(task_g, smpls$train_ids, smpls$test_ids)
      r_g <- resample_dml(task_g, ml_g, resampling_g, store_models = TRUE)
      g_hat <- lapply(r_g, extract_prediction)
      g_hat <- rearrange_prediction(g_hat)
      
      ml_m <- lapply(params$params_m, function(x) initiate_learner(self$ml_learners$mlmethod_m, 
                                                                        x[[1]]))
      resampling_m = initiate_resampling(task_m, smpls$train_ids, smpls$test_ids)
      r_m = resample_dml(task_m, ml_m, resampling_m, store_models = TRUE)
      m_hat = lapply(r_m, extract_prediction)
      m_hat = rearrange_prediction(m_hat)
      
      ml_r <- lapply(params$params_r, function(x) initiate_learner(self$ml_learners$mlmethod_r, 
                                                                        x[[1]]))
      resampling_r = initiate_resampling(task_r, smpls$train_ids, smpls$test_ids)
      r_r = resample_dml(task_r, ml_r, resampling_r, store_models = TRUE)
      r_hat = lapply(r_r, extract_prediction)
      r_hat = rearrange_prediction(r_hat)
    }
    
    D <- data$data_model[, data$treat_col, with = FALSE]
    Y <- data$data_model[, data$y_col, with = FALSE]
    Z <- data$data_model[, data$z_col, with = FALSE]
    w_hat <- Z - m_hat
    u_hat <- Y - g_hat
    v_hat <- D - r_hat
    
    # note that v & w are flipped in python
    if (self$score == 'partialling out') {
      psi_a = -v_hat * w_hat
      psi_b = u_hat * w_hat
    }
    
    return(list(psi_a = psi_a,
                psi_b = psi_b))
  },
  ml_nuisance_tuning  = function(data, smpls, param_set, tune_settings, ...){
    
    checkmate::check_class(param_set$param_set_g, "ParamSet")    
    checkmate::check_class(param_set$param_set_m, "ParamSet")
    checkmate::check_class(param_set$param_set_r, "ParamSet")

    if (!tune_on_folds){
      data_tune_list = list(data$data_model)
    } else {
      data_tune_list = lapply(smpls$train_ids, function(x) extract_training_data(data$data_model, x))
    }

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
    
    task_g = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_g_", data$y_col), x,
                                                skip_cols = c(data$treat_col, data$z_col), target = data$y_col))
    
    ml_g <- mlr3::lrn(self$ml_learners$mlmethod_g)
    
    tuning_instance_g = lapply(task_g, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_g,
                                          resampling = CV_tune,
                                          measure = measure_g,
                                          search_space = param_set$param_set_g,
                                          terminator = terminator))
    
    tuner = mlr3tuning::tnr(tune_settings$algorithm, resolution = tune_settings$resolution)
    tuning_result_g = lapply(tuning_instance_g, function(x) tune_instance(tuner, x))
    
    task_m = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_m_", data$treat_col), x,
                                                  skip_cols = c(data$y_col, data$z_col), target = data$treat_col))
    
    ml_m <- mlr3::lrn(self$ml_learners$mlmethod_m)

    tuning_instance_m = lapply(task_m, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_m,
                                          resampling = CV_tune,
                                          measure = measure_m,
                                          search_space = param_set$param_set_m,
                                          terminator = terminator))
    
    tuning_result_m = lapply(tuning_instance_m, function(x) tune_instance(tuner, x))
    
    task_r = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_r_", data$z_col), x,
                                                  skip_cols = c(data$y_col, data$treat_col), target = data$z_col))
    ml_r <- mlr3::lrn(self$ml_learners$mlmethod_r)

    tuning_instance_r = lapply(task_r, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_r,
                                          resampling = CV_tune,
                                          measure = measure_r,
                                          search_space = param_set$param_set_r,
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

