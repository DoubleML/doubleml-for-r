#' DoubleMLIRM R6 class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export

DoubleMLIRM <- R6Class("DoubleMLIRM", inherit = DoubleML, public = list(
  initialize = function(n_folds,
                        ml_learners,
                        params = list(params_m = list(),
                                      params_g = list()),
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
  ml_nuisance_and_score_elements = function(data, smpls, y, d, params, ...) {
    
    # get ml learner
    ml_m <- initiate_prob_learner(self$ml_learners$mlmethod_m,
                                  params$params_m)
    
    ml_g0 <- initiate_learner(self$ml_learners$mlmethod_g,
                              params$params_g)
    ml_g1 <- initiate_learner(self$ml_learners$mlmethod_g,
                              params$params_g)
    
    
    # get conditional samples (conditioned on D = 0 or D = 1)
    cond_smpls <- private$get_cond_smpls(smpls, data[ , d])
    
    # nuisance m
    task_m <- initiate_classif_task(paste0("nuis_m_", d), data,
                                    skip_cols = y, target = d)
    
    resampling_m <- mlr3::rsmp("custom")$instantiate(task_m,
                                                     smpls$train_ids,
                                                     smpls$test_ids)
    
    r_m <- mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
    
    m_hat = extract_prob_prediction(r_m)
    
    
    # nuisance g
    task_g0 <- initiate_regr_task(paste0("nuis_g0_", y), data,
                                 skip_cols = d, target = y)
    
    resampling_g0 <- mlr3::rsmp("custom")$instantiate(task_g0,
                                                      cond_smpls$train_ids_0,
                                                      smpls$test_ids)
    
    r_g0 <- mlr3::resample(task_g0, ml_g0, resampling_g0, store_models = TRUE)
    
    g0_hat = extract_prediction(r_g0)$response
    
    # g1
    task_g1 <- initiate_regr_task(paste0("nuis_g1_", y), data,
                                  skip_cols = d, target = y)
    
    resampling_g1  <- mlr3::rsmp("custom")$instantiate(task_g1,
                                                       cond_smpls$train_ids_1,
                                                       smpls$test_ids)
    
    r_g1 <- mlr3::resample(task_g1, ml_g1, resampling_g1, store_models = TRUE)
    
    g1_hat = extract_prediction(r_g1)$response
    
    D <- data[ , d]
    Y <- data[ , y]
    u0_hat <- Y - g0_hat
    u1_hat <- Y - g1_hat
    
    # fraction of treated for ATET
    p_hat <- vector('numeric', length=nrow(data))
    #if (self$dml_procedure == "dml1") {
      for (i_fold in 1:self$n_folds) {
        p_hat[smpls$test_ids[[i_fold]]] = mean(D[smpls$test_ids[[i_fold]]])
      }
    #}
    #else if (self$dml_procedure == "dml2") {
    #  p_hat = mean(D)
    #}
    
    if (self$inf_model == 'ATE') {
      score_b = g1_hat - g0_hat + D*(u1_hat)/m_hat - (1-D)*u0_hat/(1-m_hat)
      score_a = rep(-1, nrow(data))
    } else if (self$inf_model == 'ATET') {
      score_b = D*u0_hat/p_hat - m_hat*(1-D)*u0_hat/(p_hat*(1-m_hat))
      score_a = -D / p_hat
    }
    
    return(list(score_a = score_a,
                score_b = score_b))
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


#DoubleMLIRM$debug("ml_nuisance_and_score_elements")

