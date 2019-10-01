

DoubleMLPLR <- R6Class("DoubleMLPLR", inherit = DoubleML, public = list(
  initialize = function(n_folds,
                        ml_learners,
                        params = list(params_m = list(),
                                      params_g = list()),
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
  ml_nuisance_and_score_elements = function(data, y, d) {
    # nuisance g
    g_indx <- names(data) != d
    data_g <- data[ , g_indx, drop = FALSE]
    task_g <- mlr3::TaskRegr$new(id = paste0("nuis_g_", d), backend = data_g, target = y)
    
    # instantiate custom resampling with already sampled train and test ids
    resampling_g <- mlr3::rsmp("custom")
    resampling_g$instantiate(task_g,
                             private$smpls$train_ids,
                             private$smpls$test_ids)
    
    ml_g <- mlr3::lrn(self$ml_learners$mlmethod_g)
    ml_g$param_set$values <- self$params$params_g
    
    r_g <- mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
    
    g_hat = as.data.table(r_g$prediction())
    setorder(g_hat, 'row_id')
    g_hat = g_hat$response
    
    # nuisance m
    m_indx <- names(data) != y
    data_m <- data[, m_indx, drop = FALSE]
    task_m <- mlr3::TaskRegr$new(id = paste0("nuis_m_", d), backend = data_m, target = d)
    ml_m <- mlr3::lrn(self$ml_learners$mlmethod_m)
    ml_m$param_set$values <- self$params$params_m # tbd: check if parameter passing really works
    
    # instantiate custom resampling with already sampled train and test ids
    resampling_m <- mlr3::rsmp("custom")
    resampling_m$instantiate(task_m,
                             private$smpls$train_ids,
                             private$smpls$test_ids)
    
    r_m <- mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
    
    m_hat <- as.data.table(r_m$prediction())
    setorder(m_hat, 'row_id')
    m_hat <- m_hat$response
    
    D <- data[ , d]
    Y <- data[ , y]
    v_hat <- D - m_hat
    u_hat <- Y - g_hat
    v_hatd <- v_hat * D
    
    if (self$inf_model == 'IV-type') {
      private$score_a = -v_hatd
    } else if (self$inf_model == 'DML2018') {
      private$score_a = -v_hat * v_hat
    }
    private$score_b = v_hat * u_hat
    
    invisible(self)
  }
)
)

