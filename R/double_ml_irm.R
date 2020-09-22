#' DoubleMLIRM R6 class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export

DoubleMLIRM <- R6Class("DoubleMLIRM", inherit = DoubleML, public = list(
  ml_g = NULL, 
  ml_m = NULL, 
  g_params = NULL, 
  m_params = NULL,
  initialize = function(data, 
                        ml_g, 
                        ml_m, 
                        n_folds = 5, 
                        n_rep_cross_fit = 1,
                        score = "ATE", 
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
  }, 
  
  set__ml_nuisance_params = function(nuisance_part = NULL, treat_var = NULL, params) {
    
        # pass through internal parameter list (case: tuning with on_fold)
        if (is.null(nuisance_part) & is.null(treat_var)) {
          self$g_params = params$g_params
          self$m_params = params$m_params
        
        } else {
          
          checkmate::check_subset(treat_var, self$data$d_cols)

          if (is.null(self$g_params)){
            self$g_params = vector("list", length = length(self$data$d_cols))
            names(self$g_params) = self$data$d_cols
          }
          
          if (is.null(self$m_params)){
            self$m_params = vector("list", length = length(self$data$d_cols))
            names(self$m_params) = self$data$d_cols
          }
          
          if (nuisance_part == "ml_m"){
            self$m_params[[treat_var]] = params
           }
          
          if (nuisance_part == "ml_g"){
            self$g_params[[treat_var]] = params
          }

        }
    
  }
),
private = list(
  n_nuisance = 2,
  ml_nuisance_and_score_elements = function(data, smpls, ...) {
    
    # nuisance m
    task_m <- initiate_classif_task(paste0("nuis_m_", data$treat_col), data$data_model,
                                    skip_cols = data$y_col, target = data$treat_col)
    
    # nuisance g
    task_g <- initiate_regr_task(paste0("nuis_g_", data$y_col), data$data_model,
                                 skip_cols = data$treat_col, target = data$y_col)
    
    if (is.null(self$param_tuning)){
      
      if (is.null(self$g_params[[data$treat_col]])){
        message("Parameter of learner for nuisance part g are not tuned, results might not be valid!")
      }
      
      if (is.null(self$m_params[[data$treat_col]])){
        message("Parameter of learner for nuisance part m are not tuned, results might not be valid!")
      }
    
      # get ml learner
      ml_m <- initiate_prob_learner(self$ml_m,
                                    self$m_params[[data$treat_col]])
      
      ml_g <- initiate_learner(self$ml_g,
                               self$g_params[[data$treat_col]])

      resampling_m <- mlr3::rsmp("custom")$instantiate(task_m,
                                                       smpls$train_ids,
                                                       smpls$test_ids)
      r_m <- mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
      m_hat = extract_prob_prediction(r_m)$prob.1
      
      # get conditional samples (conditioned on D = 0 or D = 1)
      cond_smpls <- private$get_cond_smpls(smpls, data$data_model$d)
      
      resampling_g0 <- mlr3::rsmp("custom")$instantiate(task_g,
                                                        cond_smpls$train_ids_0,
                                                        smpls$test_ids)
      r_g0 <- mlr3::resample(task_g, ml_g, resampling_g0, store_models = TRUE)
      g0_hat = extract_prediction(r_g0)$response
      
      resampling_g1  <- mlr3::rsmp("custom")$instantiate(task_g,
                                                         cond_smpls$train_ids_1,
                                                         smpls$test_ids)
      r_g1 <- mlr3::resample(task_g, ml_g, resampling_g1, store_models = TRUE)
      
      g1_hat = extract_prediction(r_g1)$response
    
    }
  
    else if (!is.null(self$param_tuning)){
    
      ml_m <- lapply(params$params_m, function(x) initiate_prob_learner(self$ml_m, 
                                                                        x[[1]]))
      resampling_m = initiate_resampling(task_m, smpls$train_ids, smpls$test_ids)
      r_m = resample_dml(task_m, ml_m, resampling_m, store_models = TRUE)
      m_hat = lapply(r_m, extract_prob_prediction)
      m_hat = rearrange_prob_prediction(m_hat)  
      
      ml_g <- lapply(params$params_g, function(x) initiate_learner(self$ml_g, 
                                                                        x[[1]]))
      
      # get conditional samples (conditioned on D = 0 or D = 1)
      cond_smpls <- private$get_cond_smpls(smpls, data$data_model$d)
      
      resampling_g0 <- initiate_resampling(task_g, cond_smpls$train_ids_0, smpls$test_ids)
      r_g0 <- resample_dml(task_g, ml_g, resampling_g0, store_models = TRUE)
      g0_hat <- lapply(r_g0, extract_prediction)
      g0_hat <- rearrange_prediction(g0_hat)

      resampling_g1 <- initiate_resampling(task_g, cond_smpls$train_ids_1, smpls$test_ids)
      r_g1 <- resample_dml(task_g, ml_g, resampling_g1, store_models = TRUE)
      g1_hat <- lapply(r_g1, extract_prediction)
      g1_hat <- rearrange_prediction(g1_hat)            
    }
    
    D <- data$data_model[, data$treat_col, with = FALSE]
    Y <- data$data_model[, data$y_col, with = FALSE]
    u0_hat <- Y - g0_hat
    u1_hat <- Y - g1_hat
    
    # fraction of treated for ATTE
    p_hat <- vector('numeric', length= nrow(data$data_model))
    #if (self$dml_procedure == "dml1") {
      for (i_fold in 1:self$n_folds) {
        p_hat[smpls$test_ids[[i_fold]]] = mean(D[smpls$test_ids[[i_fold]]])
      }
    #}
    #else if (self$dml_procedure == "dml2") {
    #  p_hat = mean(D)
    #}
    
    if (self$score == 'ATE') {
      psi_b = g1_hat - g0_hat + D*(u1_hat)/m_hat - (1-D)*u0_hat/(1-m_hat)
      psi_a = rep(-1, nrow(data$data_model))
    } else if (self$score == 'ATTE') {
      psi_b = D*u0_hat/p_hat - m_hat*(1-D)*u0_hat/(p_hat*(1-m_hat))
      psi_a = -D / p_hat
    }
    
    return(list(psi_a = psi_a,
                psi_b = psi_b))
  },
 tune_params = function(data, smpls, param_set, tune_settings, ...){
   checkmate::check_class(param_set$param_set_g, "ParamSet")    
   checkmate::check_class(param_set$param_set_m, "ParamSet")
   
   data_tune_list = lapply(smpls$train_ids, function(x) extract_training_data(data$data_model, x))
   
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
    
   task_g = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_g_", data$y_col), x,
                                               skip_cols = data$treat_col, target = data$y_col))
    
   ml_g <- mlr3::lrn(self$ml_learners$mlmethod_g)
    
   tuning_instance_g = lapply(task_g, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_g,
                                          resampling = CV_tune,
                                          measure = measure_g,
                                          search_space = param_set$param_set_g,
                                          terminator = terminator))
    
   tuner = mlr3tuning::tnr(tune_settings$algorithm, resolution = tune_settings$resolution)
   tuning_result_g = lapply(tuning_instance_g, function(x) tune_instance(tuner, x))
    
   task_m = lapply(data_tune_list, function(x) initiate_classif_task(paste0("nuis_m_", data$treat_col), x,
                                                  skip_cols = data$y_col, target = data$treat_col))
    
   ml_m <- mlr3::lrn(self$ml_learners$mlmethod_m)

   tuning_instance_m = lapply(task_m, function(x) TuningInstanceSingleCrit$new(task = x,
                                         learner = ml_m,
                                         resampling = CV_tune,
                                         measure = measure_m,
                                         search_space = param_set$param_set_m,
                                         terminator = terminator))
   
   tuning_result_m = lapply(tuning_instance_m, function(x) tune_instance(tuner, x))
    
   tuning_result = list(tuning_result = list(tuning_result_g = tuning_result_g, 
                                             tuning_result_m = tuning_result_m),
                        params = list(params_g = extract_tuned_params(tuning_result_g), 
                                      params_m = extract_tuned_params(tuning_result_m)))
   
   return(tuning_result)
  },
  get_cond_smpls = function(smpls, D) {
    train_ids_0 <- lapply(1:self$n_folds, function(x) 
      smpls$train_ids[[x]][D[smpls$train_ids[[x]]] == 0])
    train_ids_1 <-  lapply(1:self$n_folds, function(x) 
      smpls$train_ids[[x]][D[smpls$train_ids[[x]]] == 1])
    return(list(train_ids_0=train_ids_0,
                train_ids_1=train_ids_1))
  }
)
)


