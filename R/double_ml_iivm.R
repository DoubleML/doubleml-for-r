#' DoubleMLIIVM R6 class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export

DoubleMLIIVM <- R6Class("DoubleMLIIVM", inherit = DoubleML, public = list(
  ml_p = NULL, 
  ml_mu = NULL, 
  ml_m = NULL, 
  params_p = NULL, 
  params_mu = NULL, 
  params_m = NULL, 
  initialize = function(data, 
                        ml_p, 
                        ml_mu, 
                        ml_m, 
                        n_folds = 5,
                        n_rep_cross_fit = 1, 
                        score = "LATE", 
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
     self$ml_p = ml_p
     self$ml_mu = ml_mu
     self$ml_m = ml_m
  },
set__ml_nuisance_params = function(nuisance_part = NULL, treat_var = NULL, params) {
    
        # pass through internal parameter list (case: tuning with on_fold)
        if (is.null(nuisance_part) & is.null(treat_var)) {
          self$p_params = params$p_params
          self$mu_params = params$mu_params
          self$m_params = params$m_params
        
        } else {
          
          checkmate::check_subset(treat_var, self$data$d_cols)

          if (is.null(self$p_params)){
            self$p_params = vector("list", length = length(self$data$d_cols))
            names(self$p_params) = self$data$d_cols
          }
        
          if (is.null(self$mu_params)){
            self$mu_params = vector("list", length = length(self$data$d_cols))
            names(self$mu_params) = self$data$d_cols
          }
          
          if (is.null(self$m_params)){
            self$m_params = vector("list", length = length(self$data$d_cols))
            names(self$m_params) = self$data$d_cols
          }
          
          if (nuisance_part == "ml_p"){
            self$p_params[[treat_var]] = params
          }
          
          if (nuisance_part == "ml_mu"){
            self$mu_params[[treat_var]] = params
          }
          
          if (nuisance_part == "ml_m"){
            self$m_params[[treat_var]] = params
          }

        }
  }
  ),
private = list(
  n_nuisance = 3,
  ml_nuisance_and_score_elements = function(data, smpls, params) {
    
    # nuisance p
    task_p <- initiate_classif_task(paste0("nuis_p_", data$z_col), data$data_model,
                                    skip_cols = c(data$y_col, data$treat_col), target = data$z_col)
 
    # nuisance mu
    task_mu <- initiate_regr_task(paste0("nuis_mu_", data$y_col), data$data_model,
                                  skip_cols = c(data$treat_col, data$z_col), target = data$y_col)
    
    # nuisance m
    task_m <- initiate_classif_task(paste0("nuis_m_", data$treat_col), data$data_model,
                                     skip_cols = c(data$y_col, data$z_col), target = data$treat_col)
    
    if (is.null(self$param_tuning)){
    
      if (length(params$params_p)==0){
        message("Parameter of learner for nuisance part p are not tuned, results might not be valid!")
      }
      
      if (length(params$params_mu)==0){
        message("Parameter of learner for nuisance part mu are not tuned, results might not be valid!")
      }
      
      if (length(params$params_m)==0){
        message("Parameter of learner for nuisance part m are not tuned, results might not be valid!")
      }
          
      # get ml learner
      ml_p <- initiate_prob_learner(self$ml_learners$mlmethod_p,
                                    params$params_p)
      
      ml_mu <- initiate_learner(self$ml_learners$mlmethod_mu,
                                 params$params_mu)
  
      ml_m <- initiate_prob_learner(self$ml_learners$mlmethod_m,
                                     params$params_m)
  

      resampling_p  <- mlr3::rsmp("custom")$instantiate(task_p,
                                                        smpls$train_ids,
                                                        smpls$test_ids)
      
      r_p <- mlr3::resample(task_p, ml_p, resampling_p, store_models = TRUE)
      
      p_hat = extract_prob_prediction(r_p)$prob.1
      
      # get conditional samples (conditioned on z = 0 or z = 1)
      cond_smpls <- private$get_cond_smpls(smpls, data$data_model$z)
      
      resampling_mu0 <- mlr3::rsmp("custom")$instantiate(task_mu,
                                                         cond_smpls$train_ids_0,
                                                         smpls$test_ids)
      r_mu0 <- mlr3::resample(task_mu, ml_mu, resampling_mu0, store_models = TRUE)
      mu0_hat = extract_prediction(r_mu0)$response
    
      # mu1
      resampling_mu1 <- mlr3::rsmp("custom")$instantiate(task_mu,
                                                         cond_smpls$train_ids_1,
                                                         smpls$test_ids)
      r_mu1 <- mlr3::resample(task_mu, ml_mu, resampling_mu1, store_models = TRUE)
      mu1_hat = extract_prediction(r_mu1)$response
    
      if (self$subgroups$always_takers == FALSE & self$subgroups$never_takers == FALSE) {
        message("If there are no always-takers and no never-takers, ATE is estimated")
      }
      
      if (self$subgroups$always_takers == FALSE){
        m0_hat <- rep(0, nrow(data$data_model))
      }
      
      else if (self$subgroups$always_takers == TRUE){
      resampling_m0 <- mlr3::rsmp("custom")$instantiate(task_m,
                                                        cond_smpls$train_ids_0,
                                                        smpls$test_ids)
      r_m0 <- mlr3::resample(task_m, ml_m, resampling_m0, store_models = TRUE)
      m0_hat = extract_prob_prediction(r_m0)$prob.1
      }
      
      if (self$subgroups$never_takers == FALSE){
        m1_hat <- rep(1, nrow(data$data_model))
      }
      
      else if (self$subgroups$never_takers == TRUE){
      # m1
      resampling_m1 <- mlr3::rsmp("custom")$instantiate(task_m,
                                                        cond_smpls$train_ids_1,
                                                        smpls$test_ids)
      r_m1 <- mlr3::resample(task_m, ml_m, resampling_m1, store_models = TRUE)
      m1_hat = extract_prob_prediction(r_m1)$prob.1
      }
    }
    
    else if (!is.null(self$param_tuning)){
    
      ml_p <- lapply(params$params_p, function(x) initiate_prob_learner(self$ml_learners$mlmethod_p,
                                                                        x))
      resampling_p <- initiate_resampling(task_p, smpls$train_ids, smpls$test_ids)
      r_p <- resample_dml(task_p, ml_p, resampling_p, store_models = TRUE)
      p_hat <- lapply(r_p, extract_prob_prediction)
      p_hat <- rearrange_prob_prediction(p_hat)
      
      ml_mu <- lapply(params$params_mu, function(x) initiate_learner(self$ml_learners$mlmethod_mu,
                                                                        x))
      # get conditional samples (conditioned on Z = 0 or Z = 1)
      cond_smpls <- private$get_cond_smpls(smpls, data$data_model$z)
      
      resampling_mu0 <- initiate_resampling(task_mu, cond_smpls$train_ids_0, smpls$test_ids)
      r_mu0 <- resample_dml(task_mu, ml_mu, resampling_mu0, store_models = TRUE)
      mu0_hat <- lapply(r_mu0, extract_prediction)
      mu0_hat <- rearrange_prediction(mu0_hat)

      resampling_mu1 <- initiate_resampling(task_mu, cond_smpls$train_ids_1, smpls$test_ids)
      r_mu1 <- resample_dml(task_mu, ml_mu, resampling_mu1, store_models = TRUE)
      mu1_hat <- lapply(r_mu1, extract_prediction)
      mu1_hat <- rearrange_prediction(mu1_hat)             
      
      ml_m <- lapply(params$params_m, function(x) initiate_prob_learner(self$ml_learners$mlmethod_m,
                                                                        x))
      
      if (self$subgroups$always_takers == FALSE & self$subgroups$never_takers == FALSE) {
        message("If there are no always-takers and no never-takers, ATE is estimated")
      }
      
      if (self$subgroups$always_takers == FALSE){
        m0_hat <- rep(0, nrow(data$data_model))
      }
      
      else if (self$subgroups$always_takers == TRUE){
        resampling_m0 <- initiate_resampling(task_m, cond_smpls$train_ids_0, smpls$test_ids)
        r_m0 <- resample_dml(task_m, ml_m, resampling_m0, store_models = TRUE)
        m0_hat <- lapply(r_m0, extract_prob_prediction)
        m0_hat <- rearrange_prob_prediction(m0_hat)
      }
      
      if (self$subgroups$never_takers == FALSE){
        m1_hat <- rep(1, nrow(data$data_model))
      }
      
      else if (self$subgroups$never_takers == TRUE){
        resampling_m1 <- initiate_resampling(task_m, cond_smpls$train_ids_1, smpls$test_ids)
        r_m1 <- resample_dml(task_m, ml_m, resampling_m1, store_models = TRUE)
        m1_hat <- lapply(r_m1, extract_prob_prediction)
        m1_hat <- rearrange_prob_prediction(m1_hat)
      }
      
    }
    
    # compute residuals
    Z <- data$data_model[, data$z_col, with = FALSE]
    D <- data$data_model[, data$treat_col, with = FALSE]
    Y <- data$data_model[, data$y_col, with = FALSE]
    u0_hat = Y - mu0_hat
    u1_hat = Y - mu1_hat
    w0_hat = D - m0_hat
    w1_hat = D - m1_hat
    
    
    if (self$score == 'LATE') {
      psi_b = mu1_hat - mu0_hat + Z*(u1_hat)/p_hat - (1-Z)*u0_hat/(1-p_hat)
      psi_a = -1 * (m1_hat - m0_hat + Z*(w1_hat)/p_hat - (1-Z)*w0_hat/(1-p_hat))
    }
    
    return(list(psi_a = psi_a,
                psi_b = psi_b))
  },
 ml_nuisance_tuning = function(data, smpls, param_set, tune_on_folds, tune_settings, ...){
   checkmate::check_class(param_set$param_set_p, "ParamSet")    
   checkmate::check_class(param_set$param_set_mu, "ParamSet")
   checkmate::check_class(param_set$param_set_m, "ParamSet")

   data_tune_list = lapply(smpls$train_ids, function(x) extract_training_data(data$data_model, x))
   
   if (any(class(tune_settings$rsmp_tune) == "Resampling")) {
     CV_tune = tune_settings$rsmp_tune
   } else {
     CV_tune = mlr3::rsmp(tune_settings$rsmp_tune, folds = tune_settings$n_folds_tune)
   }
    
   if (any(class(tune_settings$measure_p) == "Measure")) {
        measure_p = tune_settings$measure_p
      } else {
          if (is.null(tune_settings$measure_p)){
            measure_p = mlr3::default_measures("classif")[[1]]
          } else {
            measure_p = mlr3::msr(tune_settings$measure_p)
        }
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
    
   task_p = lapply(data_tune_list, function(x) initiate_classif_task(paste0("nuis_p_", data$z_col), x,
                                               skip_cols = c(data$y_col, data$treat_col), target = data$z_col))
    
   ml_p <- mlr3::lrn(self$ml_learners$mlmethod_p)
    
   tuning_instance_p = lapply(task_p, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_p,
                                          resampling = CV_tune,
                                          measure = measure_p,
                                          search_space = param_set$param_set_p,
                                          terminator = terminator))
    
   tuner = mlr3tuning::tnr(tune_settings$algorithm, resolution = tune_settings$resolution)
   tuning_result_p = lapply(tuning_instance_p, function(x) tune_instance(tuner, x))
    
   task_mu = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_mu_", data$y_col), x,
                                                  skip_cols = c(data$treat_col, data$z_col), target = data$y_col))
    
   ml_mu <- mlr3::lrn(self$ml_learners$mlmethod_mu)

   tuning_instance_mu = lapply(task_mu, function(x) TuningInstanceSingleCrit$new(task = x,
                                         learner = ml_mu,
                                         resampling = CV_tune,
                                         measure = measure_mu,
                                         search_space = param_set$param_set_mu,
                                         terminator = terminator))
   
   tuning_result_mu = lapply(tuning_instance_mu, function(x) tune_instance(tuner, x))
 
   task_m = lapply(data_tune_list, function(x) initiate_classif_task(paste0("nuis_m_", data$treat_col), x,
                                                  skip_cols = c(data$y_col, data$z_col), target = data$treat_col))
    
   ml_m <- mlr3::lrn(self$ml_learners$mlmethod_m)

   tuning_instance_m = lapply(task_m, function(x) TuningInstanceSingleCrit$new(task = x,
                                         learner = ml_m,
                                         resampling = CV_tune,
                                         measure = measure_m,
                                         search_space = param_set$param_set_m,
                                         terminator = terminator))
   
   tuning_result_m = lapply(tuning_instance_m, function(x) tune_instance(tuner, x))
    
   tuning_result = list(tuning_result = list(tuning_result_p = tuning_result_p, 
                                             tuning_result_mu = tuning_result_mu,
                                             tuning_result_m = tuning_result_m),
                        params = list(params_p = extract_tuned_params(tuning_result_p),
                                      params_mu = extract_tuned_params(tuning_result_mu),
                                      params_m = extract_tuned_params(tuning_result_m)))
   
   return(tuning_result)
  },
  get_cond_smpls = function(smpls, Z) {
    train_ids_0 <- lapply(1:self$n_folds, function(x) 
      smpls$train_ids[[x]][Z[smpls$train_ids[[x]]] == 0])
    train_ids_1 <-  lapply(1:self$n_folds, function(x) 
      smpls$train_ids[[x]][Z[smpls$train_ids[[x]]] == 1])
    return(list(train_ids_0=train_ids_0,
                train_ids_1=train_ids_1))
  }
)
)


