#' DoubleMLIRM R6 class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export

DoubleMLIRM <- R6Class("DoubleMLIRM", inherit = DoubleML, public = list(
  trimming_rule = NULL, 
  trimming_threshold = NULL,
  initialize = function(data, 
                        ml_g, 
                        ml_m, 
                        n_folds = 5, 
                        n_rep = 1,
                        score = "ATE", 
                        trimming_rule = "truncate", 
                        trimming_threshold = 1e-12,
                        dml_procedure = "dml2",
                        draw_sample_splitting = TRUE,
                        apply_cross_fitting = TRUE) {
    
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
    
    self$trimming_rule = trimming_rule
    self$trimming_threshold = trimming_threshold
  }
),
private = list(
  n_nuisance = 2,
  initialize_ml_nuisance_params = function() {
    nuisance = vector("list", self$data$n_treat())
    names(nuisance) = self$data$d_cols
    self$params = list("ml_g0" = nuisance, 
                       "ml_g1" = nuisance, 
                       "ml_m" = nuisance) 
    invisible(self)
  },
  ml_nuisance_and_score_elements = function(smpls, ...) {
    
    # nuisance m
    task_m <- initiate_classif_task(paste0("nuis_m_", self$data$treat_col), self$data$data_model,
                                    skip_cols = self$data$y_col, target = self$data$treat_col)
    # nuisance g
    task_g <- initiate_regr_task(paste0("nuis_g_", self$data$y_col), self$data$data_model,
                                 skip_cols = self$data$treat_col, target = self$data$y_col)
    
    if (!private$fold_specific_params) {
    
          for (i_nuis in self$params_names()){
            if (is.null(private$get__params(i_nuis))) {
              message(paste("Parameter of learner for nuisance part", i_nuis, "are not tuned, results might not be valid!"))
            }
          }
      ml_m = initiate_prob_learner(self$learner$ml_m,
                                    private$get__params("ml_m"))
      ml_g0 = initiate_learner(self$learner$ml_g,
                               private$get__params("ml_g1"))
      ml_g1 = initiate_learner(self$learner$ml_g,
                               private$get__params("ml_g1"))

      resampling_m <- mlr3::rsmp("custom")$instantiate(task_m,
                                                       smpls$train_ids,
                                                       smpls$test_ids)
      r_m <- mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
      m_hat = extract_prob_prediction(r_m)$prob.1
      
      # get conditional samples (conditioned on D = 0 or D = 1)
      cond_smpls <- private$get_cond_smpls(smpls, self$data$data_model[, self$data$treat_col, with = FALSE])
      
      resampling_g0 <- mlr3::rsmp("custom")$instantiate(task_g,
                                                        cond_smpls$train_ids_0,
                                                        smpls$test_ids)
      r_g0 <- mlr3::resample(task_g, ml_g0, resampling_g0, store_models = TRUE)
      g0_hat = extract_prediction(r_g0)$response
      
      resampling_g1  <- mlr3::rsmp("custom")$instantiate(task_g,
                                                         cond_smpls$train_ids_1,
                                                         smpls$test_ids)
      r_g1 <- mlr3::resample(task_g, ml_g1, resampling_g1, store_models = TRUE)
      g1_hat = extract_prediction(r_g1)$response
    } else {
      ml_m <- lapply(private$get__params("ml_m"), function(x) initiate_prob_learner(self$learner$ml_m, 
                                                                                    x))
      resampling_m = initiate_resampling(task_m, smpls$train_ids, smpls$test_ids)
      r_m = resample_dml(task_m, ml_m, resampling_m, store_models = TRUE)
      m_hat = lapply(r_m, extract_prob_prediction)
      m_hat = rearrange_prob_prediction(m_hat, smpls$test_ids) 
      
      ml_g0 <- lapply(private$get__params("ml_g0"), function(x) initiate_learner(self$learner$ml_g, 
                                                                                  x))
      ml_g1 <- lapply(private$get__params("ml_g1"), function(x) initiate_learner(self$learner$ml_g, 
                                                                                  x))
      # get conditional samples (conditioned on D = 0 or D = 1)
      cond_smpls <- private$get_cond_smpls(smpls, self$data$data_model[, self$data$treat_col, with = FALSE])
      resampling_g0 <- initiate_resampling(task_g, cond_smpls$train_ids_0, smpls$test_ids)
      r_g0 <- resample_dml(task_g, ml_g0, resampling_g0, store_models = TRUE)
      g0_hat <- lapply(r_g0, extract_prediction)
      g0_hat <- rearrange_prediction(g0_hat, smpls$test_ids)
      resampling_g1 <- initiate_resampling(task_g, cond_smpls$train_ids_1, smpls$test_ids)
      r_g1 <- resample_dml(task_g, ml_g1, resampling_g1, store_models = TRUE)
      g1_hat <- lapply(r_g1, extract_prediction)
      g1_hat <- rearrange_prediction(g1_hat, smpls$test_ids)            
    }
    
    if (self$score == "ATTE") {
      # fraction of treated for ATTE
      p_hat <- vector('numeric', length = self$data$n_obs())
      for (i_fold in 1:length(smpls$test_ids)) {
        p_hat[smpls$test_ids[[i_fold]]] = mean(self$data$data_model[smpls$test_ids[[i_fold]], self$data$treat_col, with = FALSE] )
      }
    }
    
    d = self$data$data_model[, self$data$treat_col, with = FALSE] # numeric # tbd: optimize
    y = self$data$data_model[, self$data$y_col, with=FALSE] # numeric # tbd: optimize
    u0_hat <- y - g0_hat
    u1_hat <- y - g1_hat
    
    if (self$trimming_rule == "truncate" & self$trimming_threshold > 0) {
      m_hat[m_hat < self$trimming_threshold] = self$trimming_threshold
      m_hat[m_hat > 1 - self$trimming_threshold] = 1 - self$trimming_threshold
    }
    if (self$score == 'ATE') {
      psi_b = g1_hat - g0_hat + d*(u1_hat)/m_hat - (1-d)*u0_hat/(1-m_hat)
      psi_a = rep(-1, self$data$n_obs())
    } else if (self$score == 'ATTE') {
      psi_b = d*u0_hat/p_hat - m_hat*(1-d)*u0_hat/(p_hat*(1-m_hat))
      psi_a = -d / p_hat
    }
    
    return(list(psi_a = psi_a,
                psi_b = psi_b))
  },
 ml_nuisance_tuning = function(smpls, param_set, tune_on_folds, tune_settings, ...){
   checkmate::check_class(param_set$param_set_g, "ParamSet")    
   checkmate::check_class(param_set$param_set_m, "ParamSet")
   
   if (!tune_on_folds){
    data_tune_list = list(self$data$data_model)
    } else {
    data_tune_list = lapply(smpls$train_ids, function(x) extract_training_data(self$data$data_model, x))
    }
   if (any(class(tune_settings$rsmp_tune) == "Resampling")) {
     CV_tune = tune_settings$rsmp_tune
   } else {
     CV_tune = mlr3::rsmp(tune_settings$rsmp_tune, folds = tune_settings$n_folds_tune)
   }
   if (any(class(tune_settings$measure_g) == "Measure")) {
        measure_g = tune_settings$measure_g
      } else {
          if (is.null(tune_settings$measure_g)){
            measure_g = mlr3::default_measures("regr")[[1]]
          } else {
            measure_g = mlr3::msr(tune_settings$measure_g)
        }
   }
   if (any(class(tune_settings$measure_m) == "Measure")) {
      measure_m = tune_settings$measure_m
    } else {
        if (is.null(tune_settings$measure_m)){
          measure_m = mlr3::default_measures("classif")[[1]]
        } else {
          measure_m = mlr3::msr(tune_settings$measure_m)
      }
    }
    
   terminator = tune_settings$terminator
   tuner = mlr3tuning::tnr(tune_settings$algorithm, resolution = tune_settings$resolution)
   
   indx_g0 = lapply(data_tune_list, function(x) x[self$data$treat_col] == 0)
   indx_g1 = lapply(data_tune_list, function(x) x[self$data$treat_col] == 1)
   data_tune_list_d0 = lapply(1:length(data_tune_list), function(x) data_tune_list[[x]][indx_g0[[x]], ] )
   data_tune_list_d1 = lapply(1:length(data_tune_list), function(x) data_tune_list[[x]][indx_g1[[x]], ] )

   task_g0 = lapply(data_tune_list_d0, function(x) initiate_regr_task(paste0("nuis_g_", self$data$y_col), x,
                                               skip_cols = self$data$treat_col, target = self$data$y_col))
   ml_g0 = mlr3::lrn(self$learner$ml_g)
   tuning_instance_g0 = lapply(task_g0, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_g0,
                                          resampling = CV_tune,
                                          measure = measure_g,
                                          search_space = param_set$param_set_g,
                                          terminator = terminator))
   tuning_result_g0 = lapply(tuning_instance_g0, function(x) tune_instance(tuner, x))
   
   task_g1 = lapply(data_tune_list_d1, function(x) initiate_regr_task(paste0("nuis_g_", self$data$y_col), x,
                                               skip_cols = self$data$treat_col, target = self$data$y_col))
   ml_g1 = mlr3::lrn(self$learner$ml_g)
   tuning_instance_g1 = lapply(task_g1, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_g1,
                                          resampling = CV_tune,
                                          measure = measure_g,
                                          search_space = param_set$param_set_g,
                                          terminator = terminator))  
   tuning_result_g1 = lapply(tuning_instance_g1, function(x) tune_instance(tuner, x))

   task_m = lapply(data_tune_list, function(x) initiate_classif_task(paste0("nuis_m_", self$data$treat_col), x,
                                                  skip_cols = self$data$y_col, target = self$data$treat_col))
   ml_m <- mlr3::lrn(self$learner$ml_m)
   tuning_instance_m = lapply(task_m, function(x) TuningInstanceSingleCrit$new(task = x,
                                         learner = ml_m,
                                         resampling = CV_tune,
                                         measure = measure_m,
                                         search_space = param_set$param_set_m,
                                         terminator = terminator))
   tuning_result_m = lapply(tuning_instance_m, function(x) tune_instance(tuner, x))
    
   tuning_result = list("ml_g0" = list(tuning_result_g0, params = extract_tuned_params(tuning_result_g0)),
                        "ml_g1" = list(tuning_result_g1, params = extract_tuned_params(tuning_result_g1)),
                        "ml_m"  = list(tuning_result_m, params = extract_tuned_params(tuning_result_m)))
   
   return(tuning_result)
  },
  get_cond_smpls = function(smpls, D) {
    train_ids_0 <- lapply(1:length(smpls$train_ids), function(x) 
                                                      smpls$train_ids[[x]][D[smpls$train_ids[[x]]] == 0])
    train_ids_1 <-  lapply(1:length(smpls$test_ids), function(x) 
                                                      smpls$train_ids[[x]][D[smpls$train_ids[[x]]] == 1])
    return(list(train_ids_0=train_ids_0,
                train_ids_1=train_ids_1))
  }
)
)


