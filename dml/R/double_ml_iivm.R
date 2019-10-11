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
                                      params_g2 = list(),
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
    
    # nuisance m
    m_indx <- names(data) != d & names(data) != z
    data_m <- data[, m_indx, drop = FALSE]
    data_m[, z] <- factor(data_m[, z])
    
    task_m <- mlr3::TaskClassif$new(id = paste0("nuis_p_", z), backend = data_m,
                                    target = z, positive = "1")
    ml_m <- mlr3::lrn(self$ml_learners$mlmethod_m, predict_type = "prob")
    ml_m$param_set$values <- self$params$params_m # tbd: check if parameter passing really works
    
    # instantiate custom resampling with already sampled train and test ids
    resampling_m <- mlr3::rsmp("custom")
    resampling_m$instantiate(task_m,
                             private$smpls$train_ids,
                             private$smpls$test_ids)
    
    r_m <- mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
    
    m_hat <- as.data.table(r_m$prediction())
    setorder(m_hat, 'row_id')
    m_hat <- m_hat$prob.1
    
    
    # nuisance g
    g_indx <- names(data) != d & names(data) != z
    data_g <- data[ , g_indx, drop = FALSE]
    
    # g0
    task_g0 <- mlr3::TaskRegr$new(id = paste0("nuis_g0_", y), backend = data_g, target = y)
    
    # instantiate custom resampling with already sampled train and test ids
    resampling_g0 <- mlr3::rsmp("custom")
    resampling_g0$instantiate(task_g0,
                              private$smpls$train_ids_0,
                              private$smpls$test_ids)
    
    ml_g0 <- mlr3::lrn(self$ml_learners$mlmethod_g0)
    ml_g0$param_set$values <- self$params$params_g0
    
    r_g0 <- mlr3::resample(task_g0, ml_g0, resampling_g0, store_models = TRUE)
    
    g0_hat = as.data.table(r_g0$prediction())
    setorder(g0_hat, 'row_id')
    g0_hat = g0_hat$response
    
    # g1
    task_g1 <- mlr3::TaskRegr$new(id = paste0("nuis_g1_", y), backend = data_g, target = y)
    
    # instantiate custom resampling with already sampled train and test ids
    resampling_g1 <- mlr3::rsmp("custom")
    resampling_g1$instantiate(task_g1,
                              private$smpls$train_ids_1,
                              private$smpls$test_ids)
    
    ml_g1 <- mlr3::lrn(self$ml_learners$mlmethod_g1)
    ml_g1$param_set$values <- self$params$params_g1
    
    r_g1 <- mlr3::resample(task_g1, ml_g1, resampling_g1, store_models = TRUE)
    
    g1_hat = as.data.table(r_g1$prediction())
    setorder(g1_hat, 'row_id')
    g1_hat = g1_hat$response
    
    # nuisance r
    r_indx <- names(data) != d & names(data) != z
    data_r <- data[ , r_indx, drop = FALSE]
    data_r[, d] <- factor(data_r[, d])
    
    # r0
    task_r0 <- mlr3::TaskClassif$new(id = paste0("nuis_r0_", d), backend = data_r,
                                     target = d, positive = "1")
    
    # instantiate custom resampling with already sampled train and test ids
    resampling_r0 <- mlr3::rsmp("custom")
    resampling_r0$instantiate(task_r0,
                              private$smpls$train_ids_0,
                              private$smpls$test_ids)
    
    ml_r0 <- mlr3::lrn(self$ml_learners$mlmethod_r0, predict_type = "prob")
    ml_r0$param_set$values <- self$params$params_r0
    
    r_r0 <- mlr3::resample(task_r0, ml_r0, resampling_r0, store_models = TRUE)
    
    r0_hat = as.data.table(r_r0$prediction())
    setorder(r0_hat, 'row_id')
    r0_hat <- r0_hat$prob.1
    
    # r1
    task_r1 <- mlr3::TaskClassif$new(id = paste0("nuis_r1_", d), backend = data_r,
                                     target = d, positive = "1")
    
    # instantiate custom resampling with already sampled train and test ids
    resampling_r1 <- mlr3::rsmp("custom")
    resampling_r1$instantiate(task_r1,
                              private$smpls$train_ids_1,
                              private$smpls$test_ids)
    
    ml_r1 <- mlr3::lrn(self$ml_learners$mlmethod_r1, predict_type = "prob")
    ml_r1$param_set$values <- self$params$params_r1
    
    r_r1 <- mlr3::resample(task_r1, ml_r1, resampling_r1, store_models = TRUE)
    
    r1_hat = as.data.table(r_r1$prediction())
    setorder(r1_hat, 'row_id')
    r1_hat <- r1_hat$prob.1
    
    
    # compute residuals
    Z <- data[ , z]
    D <- data[ , d]
    Y <- data[ , y]
    u_hat1 = Y - g_hat1
    w_hat0 = D - r_hat0
    w_hat1 = D - r_hat1
    
    
    if (self$inf_model == 'LATE') {
      private$score_b = g1_hat - g0_hat + Z*(u1_hat)/m_hat - (1-Z)*u1_hat/(1-m_hat)
      private$score_a = -1 * (r1_hat - r0_hat) + Z*(w1_hat)/m_hat - (1-Z)*w0_hat/(1-m_hat)
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

