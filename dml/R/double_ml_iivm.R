#' DoubleMLIIVM R6 class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export

DoubleMLIIVM <- R6Class("DoubleMLIIVM", inherit = DoubleML, public = list(
  initialize = function(n_folds,
                        ml_learners,
                        params = list(params_p = list(),
                                      params_mu = list(),
                                      params_m = list()),
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
    
    # nuisance p
    task_p <- initiate_classif_task(paste0("nuis_p_", z), data,
                                    skip_cols = c(y, d), target = z)
 
    # nuisance mu
    task_mu <- initiate_regr_task(paste0("nuis_mu_", y), data,
                                  skip_cols = c(d, z), target = y)
    
    # nuisance m
    task_m <- initiate_classif_task(paste0("nuis_m_", d), data,
                                     skip_cols = c(y, z), target = d)
    
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
      cond_smpls <- private$get_cond_smpls(smpls, data[ , z])
  
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
    
      resampling_m0 <- mlr3::rsmp("custom")$instantiate(task_m,
                                                        cond_smpls$train_ids_0,
                                                        smpls$test_ids)
      r_m0 <- mlr3::resample(task_m, ml_m, resampling_m0, store_models = TRUE)
      m0_hat = extract_prob_prediction(r_m0)$prob.1
      
      # m1
      resampling_m1 <- mlr3::rsmp("custom")$instantiate(task_m,
                                                        cond_smpls$train_ids_1,
                                                        smpls$test_ids)
      r_m1 <- mlr3::resample(task_m, ml_m, resampling_m1, store_models = TRUE)
      m1_hat = extract_prob_prediction(r_m1)$prob.1
      
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
      cond_smpls <- private$get_cond_smpls(smpls, data[ , z])
      
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
      
      resampling_m0 <- initiate_resampling(task_m, cond_smpls$train_ids_0, smpls$test_ids)
      r_m0 <- resample_dml(task_m, ml_m, resampling_m0, store_models = TRUE)
      m0_hat <- lapply(r_m0, extract_prob_prediction)
      m0_hat <- rearrange_prob_prediction(m0_hat)
      
      resampling_m1 <- initiate_resampling(task_m, cond_smpls$train_ids_1, smpls$test_ids)
      r_m1 <- resample_dml(task_m, ml_m, resampling_m1, store_models = TRUE)
      m1_hat <- lapply(r_m1, extract_prob_prediction)
      m1_hat <- rearrange_prob_prediction(m1_hat)
      
    }
    
    # compute residuals
    Z <- data[ , z]
    D <- data[ , d]
    Y <- data[ , y]
    u0_hat = Y - mu0_hat
    u1_hat = Y - mu1_hat
    w0_hat = D - m0_hat
    w1_hat = D - m1_hat
    
    
    if (self$inf_model == 'LATE') {
      score_b = mu1_hat - mu0_hat + Z*(u1_hat)/p_hat - (1-Z)*u0_hat/(1-p_hat)
      score_a = -1 * (m1_hat - m0_hat + Z*(w1_hat)/p_hat - (1-Z)*w0_hat/(1-p_hat))
    }
    
    return(list(score_a = score_a,
                score_b = score_b))
  },
 tune_params = function(data, smpls, y, d, z, param_set, tune_settings, ...){
   checkmate::check_class(param_set$param_set_p, "ParamSet")    
   checkmate::check_class(param_set$param_set_mu, "ParamSet")
   checkmate::check_class(param_set$param_set_m, "ParamSet")

   data_tune_list = lapply(smpls$train_ids, function(x) extract_training_data(data, x))
   
   if (any(class(tune_settings$rsmp_tune) == "Resampling")) {
     CV_tune = tune_settings$rsmp_tune
   } else {
     CV_tune = mlr3::rsmp(tune_settings$rsmp_tune, folds = tune_settings$n_folds_tune)
   }
    
   if (any(class(tune_settings$measure_p) == "Measure")) {
     measure_p = tune_settings$measure_p
   } else {
     measure_p = mlr3::msr(tune_settings$measure_p)
   }
    
   if (any(class(tune_settings$measure_mu) == "Measure")) {
     measure_mu = tune_settings$measure_mu
   } else {
     measure_mu = mlr3::msr(tune_settings$measure_mu)
   }
   
   if (any(class(tune_settings$measure_m) == "Measure")) {
     measure_m = tune_settings$measure_m
   } else {
     measure_m = mlr3::msr(tune_settings$measure_m)
   }
    
   terminator = tune_settings$terminator
    
   task_p = lapply(data_tune_list, function(x) initiate_classif_task(paste0("nuis_p_", z), x,
                                               skip_cols = c(y, d), target = z))
    
   ml_p <- mlr3::lrn(self$ml_learners$mlmethod_p)
    
   tuning_instance_p = lapply(task_p, function(x) TuningInstance$new(task = x,
                                          learner = ml_p,
                                          resampling = CV_tune,
                                          measures = measure_p,
                                          param_set = param_set$param_set_p,
                                          terminator = terminator))
    
   tuner = mlr3tuning::tnr(tune_settings$algorithm, resolution = tune_settings$resolution)
   tuning_result_p = lapply(tuning_instance_p, function(x) tune_instance(tuner, x))
    
   task_mu = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_mu_", y), x,
                                                  skip_cols = c(d, z), target = y))
    
   ml_mu <- mlr3::lrn(self$ml_learners$mlmethod_mu)

   tuning_instance_mu = lapply(task_mu, function(x) TuningInstance$new(task = x,
                                         learner = ml_mu,
                                         resampling = CV_tune,
                                         measures = measure_mu,
                                         param_set = param_set$param_set_mu,
                                         terminator = terminator))
   
   tuning_result_mu = lapply(tuning_instance_mu, function(x) tune_instance(tuner, x))
 
   task_m = lapply(data_tune_list, function(x) initiate_classif_task(paste0("nuis_m_", d), x,
                                                  skip_cols = c(y, z), target = d))
    
   ml_m <- mlr3::lrn(self$ml_learners$mlmethod_m)

   tuning_instance_m = lapply(task_m, function(x) TuningInstance$new(task = x,
                                         learner = ml_m,
                                         resampling = CV_tune,
                                         measures = measure_m,
                                         param_set = param_set$param_set_m,
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


#DoubleMLIIVM$debug("ml_nuisance_and_score_elements")

