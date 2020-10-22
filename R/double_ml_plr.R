#' DoubleMLPLR R6 class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export

DoubleMLPLR <- R6Class("DoubleMLPLR", inherit = DoubleML, public = list(
  initialize = function(data, 
                        ml_g,
                        ml_m,
                        n_folds = 5,
                        n_rep = 1,
                        score = "partialling out", 
                        dml_procedure = "dml2",
                        draw_sample_splitting = TRUE,
                        apply_cross_fitting = TRUE)  {
    
    super$initialize_double_ml(data, 
                        n_folds,
                        n_rep,
                        score,
                        dml_procedure,
                        draw_sample_splitting,
                        apply_cross_fitting)
    
    self$learner = list("ml_g" = ml_g, 
                        "ml_m" = ml_m)
    private$initialize_ml_nuisance_params()
    
  } 
),
private = list(
  n_nuisance = 2,
  initialize_ml_nuisance_params = function() {
    
   nuisance = vector("list", self$data$n_treat()) 
   names(nuisance) = self$data$d_cols
   self$params = list("ml_g" = nuisance, 
                      "ml_m" = nuisance)
   invisible(self)
  },
  
  ml_nuisance_and_score_elements = function(data, smpls, ...) {
    
    # nuisance g
    task_g <- initiate_regr_task(paste0("nuis_g_", data$y_col), data$data_model, 
                                  skip_cols = data$treat_col, target = data$y_col)
          
    # nuisance m  
    task_m <- initiate_regr_task(paste0("nuis_m_", data$treat_col), data$data_model, # adjust to multi treatment case
                                   skip_cols = data$y_col, target = data$treat_col)
      
    if (!private$fold_specific_params){
      for (i_nuis in self$params_names()){
        if (is.null(private$get__params(i_nuis))) {
          message(paste("Parameter of learner for nuisance part", i_nuis, "are not tuned, results might not be valid!"))
        }
      }
      ml_g <- initiate_learner(self$learner$ml_g,
                               private$get__params("ml_g"))
  
      resampling_g <- mlr3::rsmp("custom")$instantiate(task_g,
                                                       smpls$train_ids,
                                                       smpls$test_ids)
      r_g <- mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
      g_hat <- extract_prediction(r_g)$response
      

      ml_m <- initiate_learner(self$learner$ml_m,
                               private$get__params("ml_m"))
      resampling_m <- mlr3::rsmp("custom")$instantiate(task_m,
                                                       smpls$train_ids,
                                                       smpls$test_ids)
      r_m <- mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
      m_hat <- extract_prediction(r_m)$response
    } else {
      ml_g <- lapply(private$get__params("ml_g"), function(x) initiate_learner(self$learner$ml_g, 
                                                                 x))
      resampling_g <- initiate_resampling(task_g, smpls$train_ids, smpls$test_ids)
      r_g <- resample_dml(task_g, ml_g, resampling_g, store_models = TRUE)
      g_hat <- lapply(r_g, extract_prediction)
      g_hat <- rearrange_prediction(g_hat, smpls$test_ids)
      
      ml_m <- lapply(private$get__params("ml_m"), function(x) initiate_learner(self$learner$ml_m, 
                                                                        x))
      resampling_m = initiate_resampling(task_m, smpls$train_ids, smpls$test_ids)
      r_m = resample_dml(task_m, ml_m, resampling_m, store_models = TRUE)
      m_hat = lapply(r_m, extract_prediction)
      m_hat = rearrange_prediction(m_hat, smpls$test_ids)
    }
    
    d = data$data_model[, data$treat_col, with = FALSE] # numeric # tbd: optimize
    y = data$data_model[, data$y_col, with=FALSE] # numeric # tbd: optimize
    
    v_hat = d - m_hat
    u_hat = y - g_hat
    v_hatd = v_hat * d # tbd: handle product of numeric and binary in data.table
    
    if (self$score == 'IV-type') {
      psi_a = -v_hatd
    } else if (self$score == 'partialling out') {
      psi_a = -v_hat * v_hat
    }
    psi_b = v_hat * u_hat
    
    return(list(psi_a = psi_a,
                psi_b = psi_b))
  }, 
  ml_nuisance_tuning = function(data, smpls, param_set, tune_on_folds, tune_settings, ...){
    checkmate::check_class(param_set$param_set_g, "ParamSet")    
    checkmate::check_class(param_set$param_set_m, "ParamSet")
    
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
    
    if (any(class(tune_settings$measure$measure_g) == "Measure")) {
      measure_g = tune_settings$measure$measure_g
    } else {
        if (is.null(tune_settings$measure$measure_g)){
          measure_g = mlr3::default_measures("regr")[[1]]
        } else {
          measure_g = mlr3::msr(tune_settings$measure$measure_g)    
        }
    }
    
    if (any(class(tune_settings$measure$measure_m) == "Measure")) {
      measure_m = tune_settings$measure$measure_m
    } else {
        if (is.null(tune_settings$measure$measure_m)){
          measure_m = mlr3::default_measures("regr")[[1]]
        } else {
          measure_m = mlr3::msr(tune_settings$measure$measure_m)
      }
    }
    
    terminator = tune_settings$terminator
    tuner = mlr3tuning::tnr(tune_settings$algorithm, resolution = tune_settings$resolution)

    task_g = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_g_", self$data$y_col), x,
                                                skip_cols = data$treat_col, target = data$y_col))
    
    ml_g <- mlr3::lrn(self$learner$ml_g)
    tuning_instance_g = lapply(task_g, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_g,
                                          resampling = CV_tune,
                                          measure = measure_g,
                                          search_space = param_set$param_set_g,
                                          terminator = terminator))
    
    tuning_result_g = lapply(tuning_instance_g, function(x) tune_instance(tuner, x))
    
    task_m = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_m_", self$data$treat_col), x,
                                                  skip_cols = data$y_col, target = data$treat_col))
    
    ml_m <- mlr3::lrn(self$learner$ml_m)

    tuning_instance_m = lapply(task_m, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_m,
                                          resampling = CV_tune,
                                          measure = measure_m,
                                          search_space = param_set$param_set_m,
                                          terminator = terminator))
    
    tuning_result_m = lapply(tuning_instance_m, function(x) tune_instance(tuner, x))
    
    tuning_result = list("ml_g" = list(tuning_result_g, params = extract_tuned_params(tuning_result_g)),
                         "ml_m" = list(tuning_result_m, params = extract_tuned_params(tuning_result_m)))
    
    return(tuning_result)
    
  }
)
)

