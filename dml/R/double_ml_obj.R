library('R6')
library('data.table')

DoubleML <- R6Class("DoubleML", public = list(
  n_folds = NULL,
  ml_learners = NULL,
  params = NULL,
  dml_procedure = NULL,
  inf_model = NULL,
  n_rep_cross_fit = NULL,
  initialize = function(...) {
    stop("DoubleML is an abstract class that can't be initialized.")
  },
  fit = function(data, y, d) {
    # perform sample splitting based on a dummy task with the whole data set
    private$split_samples(data)
    
    # ml estimation of nuisance models and computation of score elements
    private$ml_nuisance_and_score_elements(data, y, d)
    
    invisible(self)
  }
),
private = list(
  smpls = NULL,
  score_a = NULL,
  score_b = NULL,
  initialize_double_ml = function(n_folds,
                        ml_learners,
                        params,
                        dml_procedure,
                        inf_model,
                        n_rep_cross_fit) {
    stopifnot(is.numeric(n_folds), length(n_folds) == 1)
    # TODO add input checks for ml_learners
    stopifnot(is.character(dml_procedure), length(dml_procedure) == 1)
    stopifnot(is.character(inf_model), length(inf_model) == 1)
    stopifnot(is.numeric(n_rep_cross_fit), length(n_rep_cross_fit) == 1)
    
    self$n_folds <- n_folds
    self$ml_learners <- ml_learners
    self$params <- params
    self$dml_procedure <- dml_procedure
    self$inf_model <- inf_model
    self$n_rep_cross_fit <- n_rep_cross_fit
  },
  split_samples = function(data) {
    dummy_task = Task$new('dummy_resampling', 'regr', data)
    dummy_resampling_scheme <- rsmp("cv", folds = self$n_folds)$instantiate(dummy_task)
    train_ids <- lapply(1:self$n_folds, function(x) dummy_resampling_scheme$train_set(x))
    test_ids <- lapply(1:self$n_folds, function(x) dummy_resampling_scheme$test_set(x))
    private$smpls <- list(train_ids = train_ids,
                          test_ids = test_ids)
    
    invisible(self)
  }
)
)

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


    if (self$inf_model == "DML2018") {
      private$score_a = -v_hatd
    }

    else if (self$inf_model == 'IV-type') {
      private$score_a = -v_hat * v_hat
    }
    private$score_b = v_hat * u_hat

    invisible(self)
  }
)
)

#DoubleML$debug("fit")
#DoubleMLPLR$debug("ml_nuisance_and_score_elements")
