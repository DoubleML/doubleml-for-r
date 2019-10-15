#' DoubleMLIRM R6 class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export

DoubleMLIRM <- R6Class("DoubleMLIRM", inherit = DoubleML, public = list(
  initialize = function(n_folds,
                        ml_learners,
                        params = list(params_m = list(),
                                      params_g0 = list(),
                                      params_g1 = list()),
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
  ml_nuisance_and_score_elements = function(data, y, d, ...) {
    
    # get conditional samples (conditioned on D = 0 or D = 1)
    private$get_cond_smpls(data[ , d])
    
    # nuisance m
    m_indx <- names(data) != y
    data_m <- data[, m_indx, drop = FALSE]
    data_m[, d] <- factor(data_m[, d])
    
    task_m <- mlr3::TaskClassif$new(id = paste0("nuis_p_", d), backend = data_m,
                                    target = d, positive = "1")
    ml_m <- mlr3::lrn(self$ml_learners$mlmethod_m, predict_type = "prob")
    ml_m$param_set$values <- self$params$params_m # tbd: check if parameter passing really works
    
    # instantiate resampling
    resampling_m <- private$instantiate_resampling(task_m)
    
    r_m <- mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
    
    m_hat = extract_prob_prediction(r_m)
    
    
    # nuisance g
    g_indx <- names(data) != d
    data_g <- data[ , g_indx, drop = FALSE]
    
    # g0
    task_g0 <- mlr3::TaskRegr$new(id = paste0("nuis_g0_", d), backend = data_g, target = y)
    
    # instantiate resampling
    resampling_g0 <- private$instantiate_resampling(task_g0, private$smpls$train_ids_0)
    
    ml_g0 <- mlr3::lrn(self$ml_learners$mlmethod_g0)
    ml_g0$param_set$values <- self$params$params_g0
    
    r_g0 <- mlr3::resample(task_g0, ml_g0, resampling_g0, store_models = TRUE)
    
    g0_hat = extract_prediction(r_g0)
    
    # g1
    task_g1 <- mlr3::TaskRegr$new(id = paste0("nuis_g1_", d), backend = data_g, target = y)
    
    # instantiate resampling
    resampling_g1 <- private$instantiate_resampling(task_g1, private$smpls$train_ids_1)
    
    ml_g1 <- mlr3::lrn(self$ml_learners$mlmethod_g1)
    ml_g1$param_set$values <- self$params$params_g1
    
    r_g1 <- mlr3::resample(task_g1, ml_g1, resampling_g1, store_models = TRUE)
    
    g1_hat = extract_prediction(r_g1)
    
    D <- data[ , d]
    Y <- data[ , y]
    u0_hat <- Y - g0_hat
    u1_hat <- Y - g1_hat
    
    # fraction of treated for ATET
    p_hat <- vector('numeric', length=nrow(data))
    #if (self$dml_procedure == "dml1") {
      for (i_fold in 1:self$n_folds) {
        p_hat[private$smpls$test_ids[[i_fold]]] = mean(D[private$smpls$test_ids[[i_fold]]])
      }
    #}
    #else if (self$dml_procedure == "dml2") {
    #  p_hat = mean(D)
    #}
    
    if (self$inf_model == 'ATE') {
      private$score_b = g1_hat - g0_hat + D*(u1_hat)/m_hat - (1-D)*u0_hat/(1-m_hat)
      private$score_a = rep(-1, nrow(data))
    } else if (self$inf_model == 'ATET') {
      private$score_b = D*u0_hat/p_hat - m_hat*(1-D)*u0_hat/(p_hat*(1-m_hat))
      private$score_a = -D / p_hat
    }
    
    invisible(self)
  },
  get_cond_smpls = function(D) {
    private$smpls$train_ids_0 <- lapply(1:self$n_folds, function(x) 
      private$smpls$train_ids[[x]][D[private$smpls$train_ids[[x]]] == 0])
    private$smpls$train_ids_1 <-  lapply(1:self$n_folds, function(x) 
      private$smpls$train_ids[[x]][D[private$smpls$train_ids[[x]]] == 1])
  }
)
)


#DoubleMLIRM$debug("ml_nuisance_and_score_elements")

