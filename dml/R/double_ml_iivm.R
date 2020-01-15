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
                        se_reestimate,
                        inf_model,
                        n_rep_cross_fit=1) {
    super$initialize_double_ml(n_folds,
                               ml_learners,
                               params,
                               dml_procedure,
                               se_reestimate,
                               inf_model,
                               n_rep_cross_fit)
  }
),
private = list(
  ml_nuisance_and_score_elements = function(data, y, d, z) {
    
    # get ml learner
    ml_p <- initiate_prob_learner(self$ml_learners$mlmethod_p,
                                  self$params$params_p)
    
    ml_mu0 <- initiate_learner(self$ml_learners$mlmethod_mu,
                               self$params$params_mu)
    ml_mu1 <- initiate_learner(self$ml_learners$mlmethod_mu,
                               self$params$params_mu)
    
    ml_m0 <- initiate_prob_learner(self$ml_learners$mlmethod_m,
                                   self$params$params_m)
    ml_m1 <- initiate_prob_learner(self$ml_learners$mlmethod_m,
                                   self$params$params_m)
    
    
    # get conditional samples (conditioned on z = 0 or z = 1)
    private$get_cond_smpls(data[ , z])
    
    # nuisance p
    task_p <- initiate_classif_task(paste0("nuis_p_", z), data,
                                    skip_cols = c(y, d), target = z)
    
    resampling_p <- private$instantiate_resampling(task_p)
    
    r_p <- mlr3::resample(task_p, ml_p, resampling_p, store_models = TRUE)
    
    p_hat = extract_prob_prediction(r_p)
    
    
    # nuisance mu
    task_mu0 <- initiate_regr_task(paste0("nuis_mu0_", y), data,
                                  skip_cols = c(d, z), target = y)
    
    resampling_mu0 <- private$instantiate_resampling(task_mu0, private$smpls$train_ids_0)
    
    r_mu0 <- mlr3::resample(task_mu0, ml_mu0, resampling_mu0, store_models = TRUE)
    
    mu0_hat = extract_prediction(r_mu0)
    
    # mu1
    task_mu1 <- initiate_regr_task(paste0("nuis_mu1_", y), data,
                                   skip_cols = c(d, z), target = y)
    
    resampling_mu1 <- private$instantiate_resampling(task_mu1, private$smpls$train_ids_1)
    
    r_mu1 <- mlr3::resample(task_mu1, ml_mu1, resampling_mu1, store_models = TRUE)
    
    mu1_hat = extract_prediction(r_mu1)
    
    # nuisance m
    task_m0 <- initiate_classif_task(paste0("nuis_m0_", d), data,
                                     skip_cols = c(y, z), target = d)
    
    resampling_m0 <- private$instantiate_resampling(task_m0, private$smpls$train_ids_0)
    
    r_m0 <- mlr3::resample(task_m0, ml_m0, resampling_m0, store_models = TRUE)
    
    m0_hat = extract_prob_prediction(r_m0)
    
    # m1
    task_m1 <- initiate_classif_task(paste0("nuis_m1_", d), data,
                                     skip_cols = c(y, z), target = d)
    
    resampling_m1 <- private$instantiate_resampling(task_m0, private$smpls$train_ids_1)
    
    r_m1 <- mlr3::resample(task_m1, ml_m1, resampling_m1, store_models = TRUE)
    
    m1_hat = extract_prob_prediction(r_m1)
    
    
    # compute residuals
    Z <- data[ , z]
    D <- data[ , d]
    Y <- data[ , y]
    u0_hat = Y - mu0_hat
    u1_hat = Y - mu1_hat
    w0_hat = D - m0_hat
    w1_hat = D - m1_hat
    
    
    if (self$inf_model == 'LATE') {
      private$score_b = mu1_hat - mu0_hat + Z*(u1_hat)/p_hat - (1-Z)*u0_hat/(1-p_hat)
      private$score_a = -1 * (m1_hat - m0_hat + Z*(w1_hat)/p_hat - (1-Z)*w0_hat/(1-p_hat))
    }
    
    invisible(self)
  },
  get_cond_smpls = function(Z) {
    private$smpls$train_ids_0 <- lapply(1:self$n_folds, function(x) 
      private$smpls$train_ids[[x]][Z[private$smpls$train_ids[[x]]] == 0])
    private$smpls$train_ids_1 <-  lapply(1:self$n_folds, function(x) 
      private$smpls$train_ids[[x]][Z[private$smpls$train_ids[[x]]] == 1])
  }
)
)


#DoubleMLIIVM$debug("ml_nuisance_and_score_elements")

