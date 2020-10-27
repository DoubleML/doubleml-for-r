#' DoubleMLPLIV R6 class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export

DoubleMLPLIV <- R6Class("DoubleMLPLIV", inherit = DoubleML, public = list(
  partialX = NULL, 
  partialZ = NULL, 
  initialize = function(data, 
                        ml_g,
                        ml_m, 
                        ml_r, 
                        partialX = TRUE,
                        partialZ = FALSE,
                        n_folds = 5,
                        n_rep = 1, 
                        score = "partialling out", 
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
    
    self$partialX = partialX
    self$partialZ = partialZ
    
    if (!self$partialX & self$partialZ) {
      self$learner = list("ml_r" = ml_r)
    } else {
      self$learner = list("ml_g" = ml_g, 
                     "ml_m" = ml_m, 
                     "ml_r" = ml_r)
    } 
    private$initialize_ml_nuisance_params()
  }
  ),
private = list(
  n_nuisance = 3,
  i_instr = NULL,
  initialize_ml_nuisance_params = function() {
    if (self$partialX & !self$partialZ) {
      if (self$data$n_instr() == 1) {
        valid_learner = c("ml_g", "ml_m", "ml_r")
      } else {
        valid_learner = c("ml_g", "ml_r", paste0("ml_m_", self$data$z_cols))
      } 
    } else if (self$partialX & self$partialZ) {
      valid_learner = c("ml_g", "ml_m", "ml_r") 
    } else if (!self$partialX & self$partialZ) {
      valid_learner = c("ml_r")
    }
    nuisance = vector("list", self$data$n_treat())
    names(nuisance) = self$data$d_cols
    
    self$params = rep(list(nuisance), length(valid_learner))
    names(self$params) = valid_learner
    invisible(self)
  },
  ml_nuisance_and_score_elements = function(smpls, ...) {
    if (self$partialX & !self$partialZ) {
      res = private$ml_nuisance_and_score_elements_partialX(smpls, ...)
      
    } else if (!self$partialX & self$partialZ) {
      res = private$ml_nuisance_and_score_elements_partialZ(smpls, ...)
      
    } else if (self$partialX & self$partialZ) {      
      res = private$ml_nuisance_and_score_elements_partialXZ(smpls, ...)

    }
    
    return(res)
  },
  
  ml_nuisance_and_score_elements_partialX = function(smpls, ...) {
    
    # nuisance g
    task_g <- initiate_regr_task(paste0("nuis_g_", self$data$y_col), self$data$data_model,
                                 skip_cols = c(self$data$treat_col, self$data$z_cols), 
                                 target = self$data$y_col)
    
    # nuisance r
    task_r <- initiate_regr_task(paste0("nuis_r_", self$data$treat_col), self$data$data_model,
                                 skip_cols = c(self$data$y_col, self$data$z_cols), 
                                 target = self$data$treat_col)
    
    # nuisance m
    if (self$data$n_instr() == 1) {
      # one instrument: just identified case
      task_m <- initiate_regr_task(paste0("nuis_m_", self$data$z_cols), self$data$data_model,
                                   skip_cols = c(self$data$y_col, self$data$treat_col), 
                                   target = self$data$z_cols)
    } else {
      # multiple instruments: 2sls
      task_m = lapply(self$data$z_cols, function(x) initiate_regr_task(paste0("nuis_m_", x),
                                                      self$data$data_model,
                                                      skip_cols = c(self$data$y_col, self$data$treat_col,
                                                                    self$data$z_cols[self$data$z_cols != x]),
                                                      target = x))
    }
    
    if (!private$fold_specific_params) {
      
      for (i_nuis in self$params_names()){
        if (is.null(private$get__params(i_nuis))) {
          message(paste("Parameter of learner for nuisance part", i_nuis, "are not tuned, results might not be valid!"))
        }
      }
      ml_g <- initiate_learner(self$learner$ml_g,
                               private$get__params("ml_g"))
      resampling_g <- mlr3::rsmp("custom")$instantiate(task_g,
                                                       smpls$train_ids,
                                                       smpls$test_ids)
      r_g <- mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
      g_hat <- extract_prediction(r_g)$response
      
      ml_r <- initiate_learner(self$learner$ml_r,
                               private$get__params("ml_r"))
      resampling_r <- mlr3::rsmp("custom")$instantiate(task_r,
                                                     smpls$train_ids,
                                                     smpls$test_ids)
      r_r <- mlr3::resample(task_r, ml_r, resampling_r, store_models = TRUE)
      r_hat <- extract_prediction(r_r)$response
      
      if (self$data$n_instr() == 1) {
        ml_m <- initiate_learner(self$learner$ml_m,
                               private$get__params("ml_m"))
        resampling_m <- mlr3::rsmp("custom")$instantiate(task_m,
                                                         smpls$train_ids,
                                                         smpls$test_ids)
        r_m <- mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
        m_hat <- extract_prediction(r_m)$response
        
      } else {
        ml_m = lapply(self$data$z_cols, function (i_instr) 
                                        initiate_learner(self$learner$ml_m,
                                                         private$get__params(paste0("ml_m_", i_instr))))
        resampling_m = lapply(task_m, function(x) mlr3::rsmp("custom")$instantiate(x,
                                                  smpls$train_ids, smpls$test_ids))
        
        r_m = lapply(1:self$data$n_instr(), function(x) mlr3::resample(task_m[[x]], ml_m[[x]], 
                                                    resampling_m[[x]], store_models = TRUE))
        m_hat = lapply(r_m, extract_prediction)
        #m_hat = rearrange_prediction(m_hat, smpls$test_ids)
        m_hat = lapply(1:self$data$n_instr(), function(x) 
                                            setnames(m_hat[[x]], "response", self$data$z_cols[x]))
        m_hat = Reduce(function(x,y) data.table::merge.data.table(x,y, by = "row_id"), m_hat)
        row_id_indx = names(m_hat)!="row_id"
        m_hat = m_hat[, row_id_indx, with = FALSE]
      }
    } else {
      ml_g <- lapply(private$get__params("ml_g"), function(x) initiate_learner(self$learner$ml_g, 
                                                                                x))
      resampling_g <- initiate_resampling(task_g, smpls$train_ids, smpls$test_ids)
      r_g <- resample_dml(task_g, ml_g, resampling_g, store_models = TRUE)
      g_hat <- lapply(r_g, extract_prediction)
      g_hat <- rearrange_prediction(g_hat, smpls$test_ids)
      
      ml_r <- lapply(private$get__params("ml_r"), function(x) initiate_learner(self$learner$ml_r, 
                                                                                x))
      resampling_r = initiate_resampling(task_r, smpls$train_ids, smpls$test_ids)
      r_r = resample_dml(task_r, ml_r, resampling_r, store_models = TRUE)
      r_hat = lapply(r_r, extract_prediction)
      r_hat = rearrange_prediction(r_hat, smpls$test_ids)
      
      # TBD: 1-iv vs. multi-iv case
      if (self$data$n_instr() == 1) {
        ml_m <- lapply(private$get__params("ml_m"), function(x) initiate_learner(self$learner$ml_m, 
                                                                                  x))
        resampling_m = initiate_resampling(task_m, smpls$train_ids, smpls$test_ids)
        r_m = resample_dml(task_m, ml_m, resampling_m, store_models = TRUE)
        m_hat = lapply(r_m, extract_prediction)
        m_hat = rearrange_prediction(m_hat, smpls$test_ids)
        
      } else {
        m_hat = vector("list", length = self$data$n_instr())
        names(m_hat) = self$data$z_cols
        
        for (i_instr in 1:self$data$n_instr()) {
          this_z = self$data$z_cols[i_instr]
          ml_m = lapply(private$get__params(paste0("ml_m_", this_z)), function(x) initiate_learner(self$learner$ml_m, 
                                                                                    x))
         
          resampling_m = initiate_resampling(task_m[[i_instr]], smpls$train_ids, smpls$test_ids)
          r_m = resample_dml(task_m[[i_instr]], ml_m, resampling_m, store_models = TRUE)
          m_hat_prelim = lapply(r_m, extract_prediction)
          m_hat[[i_instr]] = rearrange_prediction(m_hat_prelim, smpls$test_ids, keep_rowids = TRUE)
        }
        m_hat = lapply(1:self$data$n_instr(), function(x) 
                                            setnames(m_hat[[x]], "response", self$data$z_cols[x]))
        m_hat = Reduce(function(x,y) data.table::merge.data.table(x,y, by = "row_id"), m_hat)
        row_id_indx = names(m_hat)!="row_id"
        m_hat = m_hat[, row_id_indx, with = FALSE]
      }
    }
  
    d = self$data$data_model[, self$data$treat_col, with = FALSE]
    y = self$data$data_model[, self$data$y_col, with = FALSE]
    z = self$data$data_model[, self$data$z_cols, with = FALSE]
    
    u_hat = y - g_hat
    w_hat = d - r_hat
    v_hat = z - m_hat
    
    if (self$data$n_instr() > 1) {
      
      stopifnot(self$apply_cross_fitting) 
      
      # Projection: r_hat from projection on m_hat
      data_aux = data.table::data.table(w_hat, v_hat)
      task_r_tilde = initiate_regr_task("nuis_r_tilde", data_aux, skip_cols = NULL, 
                                        target = "w_hat")
      ml_r_tilde = mlr3::lrn("regr.lm")
      resampling_r_tilde = rsmp("insample")$instantiate(task_r_tilde)
      r_r_tilde <- mlr3::resample(task_r_tilde, ml_r_tilde, resampling_r_tilde,
                                  store_models = TRUE)
      r_hat_tilde <- extract_prediction(r_r_tilde)$response
    }
    
    
    if (self$data$n_instr() == 1) {
      if (self$score == 'partialling out') {
        psi_a = -w_hat * v_hat
        psi_b = v_hat * u_hat
      }
      
    } else {
      if (self$score == 'partialling out') {
        psi_a = -w_hat * r_hat_tilde
        psi_b = r_hat_tilde * u_hat
      }
    }
      
    return(list(psi_a = psi_a,
                psi_b = psi_b))
  },
  
  ml_nuisance_and_score_elements_partialXZ = function(smpls, ...) {
    
    # nuisance g
    task_g <- initiate_regr_task(paste0("nuis_g_", self$data$y_col), self$data$data_model,
                                 skip_cols = c(self$data$treat_col, self$data$z_cols), 
                                 target = self$data$y_col)
    
    # nuisance m: Predict d with X and Z
    task_m <- initiate_regr_task(paste0("nuis_m_", self$data$treat_col), self$data$data_model,
                                 skip_cols = c(self$data$y_col), target = self$data$treat_col)
    
     if (!private$fold_specific_params){
       for (i_nuis in self$params_names()){
        if (is.null(private$get__params(i_nuis))) {
          message(paste("Parameter of learner for nuisance part", i_nuis, "are not tuned, results might not be valid!"))
        }
      }
      ml_g <- initiate_learner(self$learner$ml_g,
                               private$get__params("ml_g"))
      resampling_g <- mlr3::rsmp("custom")$instantiate(task_g,
                                                       smpls$train_ids,
                                                       smpls$test_ids)
      r_g <- mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
      g_hat <- extract_prediction(r_g)$response
      
      ml_m <- initiate_learner(self$learner$ml_m,
                               private$get__params("ml_m"))
      resampling_m = mlr3::rsmp("custom")$instantiate(task_m, 
                                                      smpls$train_ids, 
                                                      smpls$test_ids)
      r_m = mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
      m_hat = extract_prediction(r_m)$response  
      
      resampling_m_on_train = mlr3::rsmp("custom")$instantiate(task_m,
                                                      smpls$train_ids,
                                                      smpls$train_ids)
      r_m_on_train = mlr3::resample(task_m, ml_m, resampling_m_on_train, store_models = TRUE)
      m_hat_on_train = extract_prediction(r_m_on_train, return_train_preds = TRUE)

      # nuisance r_tilde: Predict predicted values from nuisance m, only with X
      data_aux_list = lapply(m_hat_on_train, function(x) 
                                              data.table(self$data$data_model, "m_hat_on_train" = x$response))
      
      task_r = lapply(1:self$n_folds, function(x) initiate_regr_task("nuis_r_m_hat_on_train", data_aux_list[[x]],
                                                   skip_cols = c(self$data$y_col, self$data$z_cols, 
                                                                 self$data$treat_col), 
                                                   target = "m_hat_on_train"))
      ml_r = initiate_learner(self$learner$ml_r, private$get__params("ml_r"))
      resampling_r = lapply(1:self$n_folds, function(x) 
                                                  mlr3::rsmp("custom")$instantiate(task_r[[x]], 
                                                                                    list(smpls$train_ids[[x]]), 
                                                                                    list(smpls$test_ids[[x]])))
      r_r = lapply(1:self$n_folds, function(x) mlr3::resample(task_r[[x]], ml_r, 
                                                resampling_r[[x]], store_models = TRUE))
      m_hat_tilde = lapply(r_r, extract_prediction)
      m_hat_tilde = rearrange_prediction(m_hat_tilde, smpls$test_ids)
    } else {
      ml_g <- lapply(private$get__params("ml_g"), function(x) initiate_learner(self$learner$ml_g, 
                                                                                x))
      resampling_g <- initiate_resampling(task_g, smpls$train_ids, smpls$test_ids)
      r_g <- resample_dml(task_g, ml_g, resampling_g, store_models = TRUE)
      g_hat <- lapply(r_g, extract_prediction)
      g_hat <- rearrange_prediction(g_hat, smpls$test_ids)
      
      ml_m <- lapply(private$get__params("ml_m"), function(x) initiate_learner(self$learner$ml_m, 
                                                                                x))
      resampling_m = initiate_resampling(task_m, smpls$train_ids, smpls$test_ids)
      r_m  = resample_dml(task_m, ml_m, resampling_m, store_models = TRUE)
      m_hat = lapply(r_m, extract_prediction)
      m_hat = rearrange_prediction(m_hat, smpls$test_ids)
      
      resampling_m_on_train = initiate_resampling(task_m, smpls$train_ids, smpls$train_ids)
      r_m_on_train = resample_dml(task_m, ml_m, resampling_m_on_train, store_models = TRUE)
      m_hat_on_train = vapply(r_m_on_train, function(x) 
                                              extract_prediction(x, return_train_preds = TRUE), list(1L))
      
      # nuisance r_tilde: Predict predicted values from nuisance m, only with X
      data_aux_list = lapply(m_hat_on_train, function(x) 
                                              data.table(self$data$data_model, "m_hat_on_train" = x$response))
      
      task_r = lapply(1:self$n_folds, function(x) initiate_regr_task("nuis_r_m_hat_on_train", data_aux_list[[x]],
                                                    skip_cols = c(self$data$y_col, self$data$z_cols,
                                                                  self$data$treat_col),
                                                    target = "m_hat_on_train"))
      ml_r = lapply(private$get__params("ml_r"), function(x) initiate_learner(self$learner$ml_r, x))
      resampling_r = lapply(1:self$n_folds, function(x) 
                                                  mlr3::rsmp("custom")$instantiate(task_r[[x]], 
                                                                                    list(smpls$train_ids[[x]]), 
                                                                                    list(smpls$test_ids[[x]])))
      r_r = lapply(1:self$n_folds, function(x) mlr3::resample(task_r[[x]], ml_r[[x]], 
                                                resampling_r[[x]], store_models = TRUE))
      
      m_hat_tilde = lapply(r_r, extract_prediction)
      m_hat_tilde = rearrange_prediction(m_hat_tilde, smpls$test_ids)
    }
    
    d <- self$data$data_model[, self$data$treat_col, with = FALSE]
    y <- self$data$data_model[, self$data$y_col, with = FALSE]

    u_hat <- y - g_hat
    w_hat <- d - m_hat_tilde
    
    if (self$score == 'partialling out') {
        psi_a = -w_hat * (m_hat - m_hat_tilde)
        psi_b = (m_hat - m_hat_tilde) * u_hat
    }
      
    return(list(psi_a = psi_a,
                psi_b = psi_b))
  },
  
  ml_nuisance_and_score_elements_partialZ = function(smpls, ...) {
    
    # nuisance r
    task_r <- initiate_regr_task(paste0("nuis_r_", self$data$treat_col), self$data$data_model,
                                 skip_cols = c(self$data$y_col), target = self$data$treat_col)
    
    if (!private$fold_specific_params){
       for (i_nuis in self$params_names()){
        if (is.null(private$get__params(i_nuis))) {
          message(paste("Parameter of learner for nuisance part", i_nuis, "are not tuned, results might not be valid!"))
        }
      }
      ml_r <- initiate_learner(self$learner$ml_r,
                               private$get__params("ml_r"))
      resampling_r <- mlr3::rsmp("custom")$instantiate(task_r,
                                                     smpls$train_ids,
                                                     smpls$test_ids)
      r_r <- mlr3::resample(task_r, ml_r, resampling_r, store_models = TRUE)
      r_hat <- extract_prediction(r_r)$response
    }
    
    else if (!is.null(self$param_tuning)){
      ml_r <- lapply(self$r_params, function(x) initiate_learner(self$ml_r, 
                                                                        x[[1]]))
      resampling_r = initiate_resampling(task_r, smpls$train_ids, smpls$test_ids)
      r_r = resample_dml(task_r, ml_r, resampling_r, store_models = TRUE)
      r_hat = lapply(r_r, extract_prediction)
      r_hat = rearrange_prediction(r_hat, smpls$test_ids)
    }
    
    D <- self$data$data_model[, self$data$treat_col, with = FALSE]
    Y <- self$data$data_model[, self$data$y_col, with = FALSE]
    Z <- self$data$data_model[, self$data$z_cols, with = FALSE]
    
    if (self$score == 'partialling out') {
        psi_a = -r_hat* D
        psi_b =  r_hat* Y
     }
      
    return(list(psi_a = psi_a,
                psi_b = psi_b))
  },
  
  
  ml_nuisance_tuning  = function(smpls, param_set, tune_on_folds, tune_settings, ...){
    if (self$partialX & !self$partialZ) {
      res = private$ml_nuisance_tuning_partialX(smpls, param_set, tune_on_folds, tune_settings, ...)
      
    } else if (!self$partialX & self$partialZ) {
      res = private$ml_nuisance_tuning_partialZ(smpls, param_set, tune_on_folds, tune_settings, ...)
      
    } else if (self$partialX & self$partialZ) {
      res =  private$ml_nuisance_tuning_partialXZ(smpls, param_set, tune_on_folds, tune_settings, ...)
    }
    
    return(res)
  },
  
  ml_nuisance_tuning_partialX = function(smpls, param_set, tune_on_folds, tune_settings, ...){
    checkmate::check_class(param_set$param_set_g, "ParamSet")    
    checkmate::check_class(param_set$param_set_m, "ParamSet")
    checkmate::check_class(param_set$param_set_r, "ParamSet")

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
    if (any(class(tune_settings$measure$measure_g) == "Measure")) {
      measure_g = tune_settings$measure$measure_g
    } else {
        if (is.null(tune_settings$measure$measure_g)){
          measure_g = mlr3::default_measures("regr")[[1]]
        } else {
          measure_g = mlr3::msr(tune_settings$measure$measure_g)
      }
    }
    if (any(class(tune_settings$measure$measure_m) == "Measure")) {
      measure_m = tune_settings$measure$measure_m
    } else {
        if (is.null(tune_settings$measure$measure_m)){
          measure_m = mlr3::default_measures("regr")[[1]]
        } else {
          measure_m = mlr3::msr(tune_settings$measure$measure_m)
      }
    }
    if (any(class(tune_settings$measure$measure_r) == "Measure")) {
      measure_r = tune_settings$measure$measure_r
    } else {
        if (is.null(tune_settings$measure$measure_r)){
          measure_r = mlr3::default_measures("regr")[[1]]
        } else {
          measure_r = mlr3::msr(tune_settings$measure$measure_r)
      }
    }
    
    terminator = tune_settings$terminator
    tuner = mlr3tuning::tnr(tune_settings$algorithm, resolution = tune_settings$resolution)
    
    task_g = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_g_", self$data$y_col), x,
                                                skip_cols = c(self$data$treat_col, self$data$z_cols), 
                                                target = self$data$y_col))
    ml_g = mlr3::lrn(self$learner$ml_g)
    tuning_instance_g = lapply(task_g, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_g,
                                          resampling = CV_tune,
                                          measure = measure_g,
                                          search_space = param_set$param_set_g,
                                          terminator = terminator))
    
    tuning_result_g = lapply(tuning_instance_g, function(x) tune_instance(tuner, x))
    
    task_r = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_r_", self$data$treat_col), x,
                                                skip_cols = c(self$data$y_col, self$data$z_cols),
                                                target = self$data$treat_col))
    ml_r = mlr3::lrn(self$learner$ml_r)
    tuning_instance_r = lapply(task_r, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_r,
                                          resampling = CV_tune,
                                          measure = measure_r,
                                          search_space = param_set$param_set_r,
                                          terminator = terminator))
    tuning_result_r = lapply(tuning_instance_r, function(x) tune_instance(tuner, x))

    ml_m = mlr3::lrn(self$learner$ml_m) 
    
    if (self$data$n_instr() == 1) {
      task_m = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_r_", self$data$z_cols), x,
                                                  skip_cols = c(self$data$y_col, self$data$treat_col),
                                                  target = self$data$z_cols))
      tuning_instance_m = lapply(task_m, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_m,
                                          resampling = CV_tune,
                                          measure = measure_m,
                                          search_space = param_set$param_set_m,
                                          terminator = terminator))
      tuning_result_m = lapply(tuning_instance_m, function(x) tune_instance(tuner, x))
    
      tuning_result = list("ml_g" = list(tuning_result_g,
                                         params = extract_tuned_params(tuning_result_g)),
                           "ml_m" = list(tuning_result_m, 
                                         params = extract_tuned_params(tuning_result_m)), 
                           "ml_r" = list(tuning_result_r, 
                                         params = extract_tuned_params(tuning_result_r)))
    } else {
      tuning_result_m = vector("list", length = self$data$n_instr())
      names(tuning_result_m) = self$data$z_cols
      
      for (i_instr in 1:self$data$n_instr()) {
        this_z = self$data$z_cols[i_instr] 
        task_m = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_r_", self$data$z_cols[i_instr]), x,
                                                     skip_cols = c(self$data$y_col, self$data$treat_col, 
                                                                   self$data$z_cols[self$data$z_cols != this_z]),
                                                     target = this_z))
        tuning_instance_m = lapply(task_m, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_m,
                                          resampling = CV_tune,
                                          measure = measure_m,
                                          search_space = param_set$param_set_m,
                                          terminator = terminator))
        tuning_result_m[[this_z]] = lapply(tuning_instance_m, function(x) tune_instance(tuner, x))
     }
      tuning_result = list("ml_g" = list(tuning_result_g,
                                         params = extract_tuned_params(tuning_result_g)),
                           "ml_r" = list(tuning_result_r, 
                                         params = extract_tuned_params(tuning_result_r)))
        for (this_z in self$data$z_cols) {
          tuning_result[[paste0("ml_m_", this_z)]] = list(tuning_result_m[[this_z]], 
                                                          params = extract_tuned_params(tuning_result_m[[this_z]]))
        }
    }
    return(tuning_result)
    
  },
  
   ml_nuisance_tuning_partialXZ = function(smpls, param_set, tune_on_folds, tune_settings, ...){
    
    checkmate::check_class(param_set$param_set_g, "ParamSet")    
    checkmate::check_class(param_set$param_set_m, "ParamSet")
    checkmate::check_class(param_set$param_set_r, "ParamSet")

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
    if (any(class(tune_settings$measure$measure_g) == "Measure")) {
      measure_g = tune_settings$measure$measure_g
    } else {
        if (is.null(tune_settings$measure$measure_g)){
          measure_g = mlr3::default_measures("regr")[[1]]
        } else {
          measure_g = mlr3::msr(tune_settings$measure$measure_g)
      }
    }
    if (any(class(tune_settings$measure$measure_m) == "Measure")) {
      measure_m = tune_settings$measure$measure_m
    } else {
        if (is.null(tune_settings$measure$measure_m)){
          measure_m = mlr3::default_measures("regr")[[1]]
        } else {
          measure_m = mlr3::msr(tune_settings$measure$measure_m)
      }
    }
    if (any(class(tune_settings$measure$measure_r) == "Measure")) {
      measure_r = tune_settings$measure$measure_r
    } else {
        if (is.null(tune_settings$measure$measure_r)){
          measure_r = mlr3::default_measures("regr")[[1]]
        } else {
          measure_r = mlr3::msr(tune_settings$measure$measure_r)
      }
    }
    terminator = tune_settings$terminator
    tuner = mlr3tuning::tnr(tune_settings$algorithm, resolution = tune_settings$resolution)
    
    task_g = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_g_", self$data$y_col), x,
                                                skip_cols = c(self$data$treat_col, self$data$z_cols), 
                                                target = self$data$y_col))
    ml_g = mlr3::lrn(self$learner$ml_g)
    tuning_instance_g = lapply(task_g, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_g,
                                          resampling = CV_tune,
                                          measure = measure_g,
                                          search_space = param_set$param_set_g,
                                          terminator = terminator))
    tuning_result_g = lapply(tuning_instance_g, function(x) tune_instance(tuner, x))
    
    ml_m = mlr3::lrn(self$learner$ml_m) 
    task_m = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_m_", self$data$treat_col), x,
                                 skip_cols = c(self$data$y_col), target = self$data$treat_col))
    tuning_instance_m = lapply(task_m, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_m,
                                          resampling = CV_tune,
                                          measure = measure_m,
                                          search_space = param_set$param_set_m,
                                          terminator = terminator))
    tuning_result_m = lapply(tuning_instance_m, function(x) tune_instance(tuner, x))
    
    m_params = extract_tuned_params(tuning_result_m)
    ml_m = lapply(m_params, function(x) initiate_learner(self$learner$ml_m, params = x))
    
    resampling_m_on_train = lapply(task_m, function(x) mlr3::rsmp("insample")$instantiate(x))
    r_m_on_train = lapply(1:length(data_tune_list), function(x) 
                                        mlr3::resample(task_m[[x]], ml_m[[x]], resampling_m_on_train[[x]], store_models = TRUE))
    m_hat_on_train = lapply(r_m_on_train, function(x) extract_prediction(x, return_train_preds = TRUE))
    m_hat_on_train = lapply(m_hat_on_train, function(x) x[[1]]$response)
      
    data_aux_list = lapply(1:length(data_tune_list), function(x) 
                                              data.table(data_tune_list[[x]], "m_hat_on_train" = m_hat_on_train[[x]]))
    task_r = lapply(data_aux_list, function(x) initiate_regr_task("nuis_r_m_hat_on_train", x,
                                                    skip_cols = c(self$data$y_col, self$data$z_cols, 
                                                                  self$data$treat_col), 
                                                    target = "m_hat_on_train"))
    ml_r = mlr3::lrn(self$learner$ml_r)
    tuning_instance_r = lapply(task_r, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_r,
                                          resampling = CV_tune,
                                          measure = measure_r,
                                          search_space = param_set$param_set_r,
                                          terminator = terminator))
    tuning_result_r = lapply(tuning_instance_r, function(x) tune_instance(tuner, x))

    tuning_result = list("ml_g" = list(tuning_result_g,
                                         params = extract_tuned_params(tuning_result_g)),
                           "ml_m" = list(tuning_result_m, 
                                         params = extract_tuned_params(tuning_result_m)), 
                           "ml_r" = list(tuning_result_r, 
                                         params = extract_tuned_params(tuning_result_r)))
    return(tuning_result)
  },
  
  ml_nuisance_tuning_partialZ = function(smpls, param_set, tune_on_folds, tune_settings, ...){
    
    checkmate::check_class(param_set$param_set_r, "ParamSet")

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
  
    if (any(class(tune_settings$measure_r) == "Measure")) {
      measure_r = tune_settings$measure_r
    } else {
        if (is.null(tune_settings$measure_r)){
          measure_r = mlr3::default_measures("regr")[[1]]
        } else {
          measure_r = mlr3::msr(tune_settings$measure_r)
      }
    }
    
    terminator = tune_settings$terminator
    tuner = mlr3tuning::tnr(tune_settings$algorithm, resolution = tune_settings$resolution)
    
    task_r = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_r_", self$data$z_cols), x,
                                                   skip_cols = c(self$data$y_col, self$data$z_cols), 
                                                   target = self$data$treat_col))
    ml_r <- mlr3::lrn(self$ml_r)

    tuning_instance_r = lapply(task_r, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_r,
                                          resampling = CV_tune,
                                          measure = measure_r,
                                          search_space = param_set$param_set_r,
                                          terminator = terminator))
    
    tuning_result_r = lapply(tuning_instance_r, function(x) tune_instance(tuner, x))
    
    tuning_result = list(tuning_result = list(tuning_result_r = tuning_result_r),
                         params = list(r_params = extract_tuned_params(tuning_result_r)))
    
    return(tuning_result)
    
  }
)
)



# Initializer for partialX
#' @export
DoubleMLPLIV.partialX = function(data, 
                      ml_g,
                      ml_m, 
                      ml_r, 
                      n_folds = 5,
                      n_rep = 1, 
                      score = "partialling out", 
                      dml_procedure = "dml2",
                      draw_sample_splitting = TRUE, 
                      apply_cross_fitting = TRUE) {
    
    obj = DoubleMLPLIV$new(data, 
                           ml_g,
                           ml_m, 
                           ml_r,
                           partialX = TRUE, 
                           partialZ = FALSE,
                           n_folds,
                           n_rep, 
                           score, 
                           dml_procedure,
                           draw_sample_splitting, 
                           apply_cross_fitting)

    return(obj)
}


# Initializer for partialZ
#' @export
DoubleMLPLIV.partialZ = function(data, 
                      ml_r, 
                      n_folds = 5,
                      n_rep = 1, 
                      score = "partialling out", 
                      dml_procedure = "dml2",
                      draw_sample_splitting = TRUE, 
                      apply_cross_fitting = TRUE) {
    
    obj = DoubleMLPLIV$new(data, 
                           ml_g = NULL,
                           ml_m = NULL, 
                           ml_r, 
                           partialX = FALSE, 
                           partialZ = TRUE,
                           n_folds,
                           n_rep, 
                           score, 
                           dml_procedure,
                           draw_sample_splitting, 
                           apply_cross_fitting)
    
    return(obj)
}



# Initializer for partialXZ
#' @export
DoubleMLPLIV.partialXZ = function(data, 
                      ml_g,
                      ml_m, 
                      ml_r, 
                      n_folds = 5,
                      n_rep = 1, 
                      score = "partialling out", 
                      dml_procedure = "dml2",
                      draw_sample_splitting = TRUE, 
                      apply_cross_fitting = TRUE) {
    
    obj = DoubleMLPLIV$new(data, 
                           ml_g,
                           ml_m, 
                           ml_r, 
                           partialX = TRUE, 
                           partialZ = TRUE,
                           n_folds,
                           n_rep, 
                           score, 
                           dml_procedure,
                           draw_sample_splitting, 
                           apply_cross_fitting)

    return(obj)
}
