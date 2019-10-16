#' DoubleMLIIVM R6 class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export

DoubleMLIIVM <- R6Class("DoubleMLIIVM", inherit = DoubleML, public = list(
  initialize = function(n_folds,
                        ml_learners,
                        params = list(params_m = list(),
                                      params_g0 = list(),
                                      params_g1 = list(),
                                      params_r0 = list(),
                                      params_r1 = list()),
                        dml_procedure,
                        inf_model,
                        n_rep_cross_fit=1) {
    super$initialize_double_ml(n_folds,
                               ml_learners,
                               params,
                               dml_procedure,
                               inf_model,
                               n_rep_cross_fit)
  }
),
private = list(
  ml_nuisance_and_score_elements = function(data, y, d, z) {
    
    # get conditional samples (conditioned on z = 0 or z = 1)
    private$get_cond_smpls(data[ , z])
    
    # nuisance p
    p_indx <- names(data) != y & names(data) != d
    data_p <- data[, p_indx, drop = FALSE]
    data_p[, z] <- factor(data_p[, z])
    
    task_p <- mlr3::TaskClassif$new(id = paste0("nuis_p_", z), backend = data_p,
                                    target = z, positive = "1")
    
    ml_p <- initiate_prob_learner(self$ml_learners$mlmethod_p,
                                  self$params$params_p)
    
    # instantiate resampling
    resampling_p <- private$instantiate_resampling(task_p)
    
    r_p <- mlr3::resample(task_p, ml_p, resampling_p, store_models = TRUE)
    
    p_hat = extract_prob_prediction(r_p)
    
    
    # nuisance mu
    mu_indx <- names(data) != d & names(data) != z
    data_mu <- data[ , mu_indx, drop = FALSE]
    
    # mu0
    task_mu0 <- mlr3::TaskRegr$new(id = paste0("nuis_mu0_", y), backend = data_mu, target = y)
    
    # instantiate resampling
    resampling_mu0 <- private$instantiate_resampling(task_mu0, private$smpls$train_ids_0)
    
    ml_mu0 <- initiate_learner(self$ml_learners$mlmethod_mu0,
                               self$params$params_mu0)
    
    r_mu0 <- mlr3::resample(task_mu0, ml_mu0, resampling_mu0, store_models = TRUE)
    
    mu0_hat = extract_prediction(r_mu0)
    
    # mu1
    task_mu1 <- mlr3::TaskRegr$new(id = paste0("nuis_mu1_", y), backend = data_mu, target = y)
    
    # instantiate resampling
    resampling_mu1 <- private$instantiate_resampling(task_mu1, private$smpls$train_ids_1)
    
    ml_mu1 <- initiate_learner(self$ml_learners$mlmethod_mu1,
                               self$params$params_mu1)
    
    r_mu1 <- mlr3::resample(task_mu1, ml_mu1, resampling_mu1, store_models = TRUE)
    
    mu1_hat = extract_prediction(r_mu1)
    
    # nuisance m
    m_indx <- names(data) != y & names(data) != z
    data_m <- data[ , m_indx, drop = FALSE]
    data_m[, d] <- factor(data_m[, d])
    
    # m0
    task_m0 <- mlr3::TaskClassif$new(id = paste0("nuis_m0_", d), backend = data_m,
                                     target = d, positive = "1")
    
    # instantiate resampling
    resampling_m0 <- private$instantiate_resampling(task_m0, private$smpls$train_ids_0)
    
    ml_m0 <- initiate_prob_learner(self$ml_learners$mlmethod_m0,
                                   self$params$params_m0)
    
    r_m0 <- mlr3::resample(task_m0, ml_m0, resampling_m0, store_models = TRUE)
    
    m0_hat = extract_prob_prediction(r_m0)
    
    # m1
    task_m1 <- mlr3::TaskClassif$new(id = paste0("nuis_m1_", d), backend = data_m,
                                     target = d, positive = "1")
    
    # instantiate resampling
    resampling_m1 <- private$instantiate_resampling(task_m0, private$smpls$train_ids_1)
    
    ml_m1 <- initiate_prob_learner(self$ml_learners$mlmethod_m1,
                                   self$params$params_m1)
    
    r_m1 <- mlr3::resample(task_m1, ml_m1, resampling_m1, store_models = TRUE)
    
    m1_hat = extract_prob_prediction(r_m1)
    
    
    # compute residuals
    Z <- data[ , z]
    D <- data[ , d]
    Y <- data[ , y]
    u1_hat = Y - mu1_hat
    w0_hat = D - m0_hat
    w1_hat = D - m1_hat
    
    
    if (self$inf_model == 'LATE') {
      private$score_b = mu1_hat - mu0_hat + Z*(u1_hat)/p_hat - (1-Z)*u1_hat/(1-p_hat)
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

