#' DoubleMLPLIV R6 class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export

DoubleMLPLIV <- R6Class("DoubleMLPLIV", inherit = DoubleML, public = list(
  ml_g = NULL, 
  ml_m = NULL, 
  ml_r = NULL, 
  partialX = NULL, 
  partialZ = NULL, 
  g_params = NULL, 
  m_params = NULL,
  m_params_mult_instr = NULL,
  r_params = NULL,
  
  initialize = function(data, 
                        ml_g,
                        ml_m, 
                        ml_r, 
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
    
    self$ml_g = ml_g
    self$ml_m = ml_m
    self$ml_r = ml_r
    self$partialX = TRUE
    self$partialZ = FALSE
  }, 
  
 set__ml_nuisance_params = function(nuisance_part = NULL, treat_var = NULL, instr_var = NULL, 
                                    params) {
    
      # pass through internal parameter list (case: tuning with on_fold)
      if (is.null(nuisance_part) & is.null(treat_var)) {
        self$g_params = params$g_params
        self$r_params = params$r_params
        self$m_params = params$m_params
        
        if (self$partialX & self$data$n_instr()>1) {
          self$m_params_mult_instr = params$m_params_mult_instr
        }
      } else {
        checkmate::check_subset(treat_var, self$data$d_cols)
        
        if (self$partialX & !self$partialZ) {
          if (is.null(self$g_params)){
            self$g_params = vector("list", length = self$data$n_treat())
            names(self$g_params) = self$data$d_cols
          }
          if (is.null(self$r_params)){
            self$r_params = vector("list", length = self$data$n_treat())
            names(self$r_params) = self$data$d_cols
          }
          if (self$data$n_instr() == 1) {
            if (is.null(self$m_params)){
              self$m_params = vector("list", length = self$data$n_treat())
              names(self$m_params) = self$data$d_cols
            }
          } else {
              if (is.null(self$m_params_mult_instr)){
                params_NULL = vector("list", length = self$data$n_treat())
                names(params_NULL) = self$data$d_cols
                self$m_params_mult_instr = rep(list(params_NULL), self$data$n_instr())
                names(self$m_params_mult_instr) = self$data$z_cols
              }
          }
            if (nuisance_part == "ml_g"){
            self$g_params[[treat_var]] = params
            }
          
            if (nuisance_part == "ml_r"){
              self$r_params[[treat_var]] = params
            }
            
            if (self$data$n_instr() == 1) {
              if (nuisance_part == "ml_m"){
                self$m_params[[treat_var]] = params
              }
            } else {
              if (nuisance_part == "ml_m_mult_instr") {
                self$m_params_mult_instr[[instr_var]][[treat_var]] = params
              }
            }
        } else if (!self$partialX & self$partialZ) {
            if (is.null(self$r_params)){
              self$r_params = vector("list", length = self$data$n_treat())
              names(self$r_params) = self$data$d_cols
            }
            if (nuisance_part == "ml_r"){
              self$r_params[[treat_var]] = params
            }
        } else if (self$partialX & self$partialZ) {
            if (is.null(self$g_params)){
              self$g_params = vector("list", length = self$data$n_treat())
              names(self$g_params) = self$data$d_cols
            }
            if (is.null(self$r_params)){
              self$r_params = vector("list", length = self$data$n_treat())
              names(self$r_params) = self$data$d_cols
            }
            if (is.null(self$m_params)){
              self$m_params = vector("list", length = self$data$n_treat())
              names(self$m_params) = self$data$d_cols
            }
            if (nuisance_part == "ml_g"){
              self$g_params[[treat_var]] = params
            }
            if (nuisance_part == "ml_r"){
                self$r_params[[treat_var]] = params
            }
            if (nuisance_part == "ml_m"){
                self$m_params[[treat_var]] = params
            }
        }
     }
  }
  ),
private = list(
  n_nuisance = 3,
  i_instr = NULL,
  
  ml_nuisance_and_score_elements = function(data, smpls, ...) {
    if (self$partialX & !self$partialZ) {
      res = private$ml_nuisance_and_score_elements_partialX(data, smpls, ...)
      
    } else if (!self$partialX & self$partialZ) {
      res = private$ml_nuisance_and_score_elements_partialZ(data, smpls, ...)
      
    } else if (self$partialX & self$partialZ) {      
      res = private$ml_nuisance_and_score_elements_partialXZ(data, smpls, ...)

    }
    
    return(res)
  },
  
  ml_nuisance_and_score_elements_partialX = function(data, smpls, ...) {
    
    # nuisance g
    task_g <- initiate_regr_task(paste0("nuis_g_", data$y_col), data$data_model,
                                 skip_cols = c(data$treat_col, data$z_cols), target = data$y_col)
    
    # nuisance r
    task_r <- initiate_regr_task(paste0("nuis_r_", data$treat_col), data$data_model,
                                 skip_cols = c(data$y_col, data$z_cols), target = data$treat_col)
    
    # nuisance m
    if (data$n_instr() == 1) {
      # one instrument: just identified case
      task_m <- initiate_regr_task(paste0("nuis_m_", data$z_cols), data$data_model,
                                   skip_cols = c(data$y_col, data$treat_col), target = data$z_cols)
    } else {
      # multiple instruments: 2sls
      task_m = lapply(data$z_cols, function(x) initiate_regr_task(paste0("nuis_m_", x), data$data_model,
                                    skip_cols = c(data$y_col, data$treat_col, data$z_cols[data$z_cols != x]),
                                    target = x))
    }
    
    if (is.null(self$param_tuning)){
      
       if (is.null(self$g_params[[data$treat_col]])){
          message("Parameter of learner for nuisance part g are not tuned, results might not be valid!")
       }
       
       if (data$n_instr() == 1) {
         # TODO: assert for multi-IV case
         if (is.null(self$m_params[[data$treat_col]])){
            message("Parameter of learner for nuisance part ml_m_mult_instr are not tuned, results might not be valid!")
         }
       } else {
          if (is.null(self$m_params_mult_instr)){
            message("Parameter of learner for nuisance part ml_m_mult_instr are not tuned, results might not be valid!")
         }
       }
       
       if (is.null(self$r_params[[data$treat_col]])){
          message("Parameter of learner for nuisance part r are not tuned, results might not be valid!")
       }
    
      ml_g <- initiate_learner(self$ml_g,
                               self$g_params[[data$treat_col]])
      resampling_g <- mlr3::rsmp("custom")$instantiate(task_g,
                                                       smpls$train_ids,
                                                       smpls$test_ids)
      r_g <- mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
      g_hat <- extract_prediction(r_g)$response
      
      ml_r <- initiate_learner(self$ml_r,
                               self$r_params[[data$treat_col]])
      resampling_r <- mlr3::rsmp("custom")$instantiate(task_r,
                                                     smpls$train_ids,
                                                     smpls$test_ids)
      r_r <- mlr3::resample(task_r, ml_r, resampling_r, store_models = TRUE)
      r_hat <- extract_prediction(r_r)$response
      
      if (data$n_instr() == 1) {
        ml_m <- initiate_learner(self$ml_m,
                                 self$m_params[[data$treat_col]])
        resampling_m <- mlr3::rsmp("custom")$instantiate(task_m,
                                                         smpls$train_ids,
                                                         smpls$test_ids)
        r_m <- mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
        m_hat <- extract_prediction(r_m)$response
        
      } else {
        ml_m = lapply(1:data$n_instr(), function (i_instr) initiate_learner(self$ml_m,
                                        self$m_params_mult_instr[[data$z_cols[i_instr]]][[data$treat_col]]))
        resampling_m = lapply(task_m, function(x) mlr3::rsmp("custom")$instantiate(x,
                                                  smpls$train_ids, smpls$test_ids))
        
        r_m = lapply(1:data$n_instr(), function(x) mlr3::resample(task_m[[x]], ml_m[[x]], 
                                              resampling_m[[x]], store_models = TRUE))
        m_hat = lapply(r_m, extract_prediction)
        #m_hat = rearrange_prediction(m_hat, smpls$test_ids)
        m_hat = lapply(1:data$n_instr(), function(x) 
                                            setnames(m_hat[[x]], "response", data$z_cols[x]))
        m_hat = Reduce(function(x,y) data.table::merge.data.table(x,y, by = "row_id"), m_hat)
        row_id_indx = names(m_hat)!="row_id"
        m_hat = m_hat[, row_id_indx, with = FALSE]
      }
    }
    
    else if (!is.null(self$param_tuning)){
      ml_g <- lapply(self$g_params, function(x) initiate_learner(self$ml_g, 
                                                                        x[[1]]))
      resampling_g <- initiate_resampling(task_g, smpls$train_ids, smpls$test_ids)
      r_g <- resample_dml(task_g, ml_g, resampling_g, store_models = TRUE)
      g_hat <- lapply(r_g, extract_prediction)
      g_hat <- rearrange_prediction(g_hat, smpls$test_ids)
      
      ml_r <- lapply(self$r_params, function(x) initiate_learner(self$ml_r, 
                                                                        x[[1]]))
      resampling_r = initiate_resampling(task_r, smpls$train_ids, smpls$test_ids)
      r_r = resample_dml(task_r, ml_r, resampling_r, store_models = TRUE)
      r_hat = lapply(r_r, extract_prediction)
      r_hat = rearrange_prediction(r_hat, smpls$test_ids)
      
      # TBD: 1-iv vs. multi-iv case
      if (data$n_instr() == 1) {
        ml_m <- lapply(self$m_params, function(x) initiate_learner(self$ml_m, 
                                                                        x[[1]]))
        resampling_m = initiate_resampling(task_m, smpls$train_ids, smpls$test_ids)
        r_m = resample_dml(task_m, ml_m, resampling_m, store_models = TRUE)
        m_hat = lapply(r_m, extract_prediction)
        m_hat = rearrange_prediction(m_hat, smpls$test_ids)
        
      } else {
        
        m_hat = vector("list", length = data$n_instr())
        names(m_hat) = data$z_cols
        
        for (i_instr in 1:data$n_instr()) {
          this_z = data$z_cols[i_instr]
          # if (!tune_on_folds) {
          param_indx = which(names(self$m_params_mult_instr) == this_z)
          this_params = self$m_params_mult_instr[param_indx]
          this_params = lapply(1:length(this_params), function(x) this_params[[x]][[1]])
          ml_m = lapply(this_params, function(x) initiate_learner(self$ml_m, 
                                                                        x[[1]]))
          # } else {
          #   ml_m = lapply(self$m_params_mult_instr[[i_instr]], function(x) initiate_learner(self$ml_m, 
          #                                                               x[[1]]))
          # }
          resampling_m = initiate_resampling(task_m[[i_instr]], smpls$train_ids, smpls$test_ids)
          r_m = resample_dml(task_m[[i_instr]], ml_m, resampling_m, store_models = TRUE)
          m_hat_prelim = lapply(r_m, extract_prediction)
          m_hat[[i_instr]] = rearrange_prediction(m_hat_prelim, smpls$test_ids, keep_rowids = TRUE)
        }
        m_hat = lapply(1:data$n_instr(), function(x) 
                                            setnames(m_hat[[x]], "response", data$z_cols[x]))
        m_hat = Reduce(function(x,y) data.table::merge.data.table(x,y, by = "row_id"), m_hat)
        row_id_indx = names(m_hat)!="row_id"
        m_hat = m_hat[, row_id_indx, with = FALSE]
      }
    }
  
    d = data$data_model[, data$treat_col, with = FALSE]
    y = data$data_model[, data$y_col, with = FALSE]
    z = data$data_model[, data$z_cols, with = FALSE]
    
    u_hat = y - g_hat
    w_hat = d - r_hat
    v_hat = z - m_hat
    
    if (data$n_instr() > 1) {
      
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
    
    
    if (data$n_instr() == 1) {
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
  
  ml_nuisance_and_score_elements_partialXZ = function(data, smpls, ...) {
    
    # nuisance g
    task_g <- initiate_regr_task(paste0("nuis_g_", data$y_col), data$data_model,
                                 skip_cols = c(data$treat_col, data$z_cols), target = data$y_col)
    
    # nuisance r: Predict d with X and Z
    task_r <- initiate_regr_task(paste0("nuis_r_", data$treat_col), data$data_model,
                                 skip_cols = c(data$y_col), target = data$treat_col)
    
    if (is.null(self$param_tuning)){
      
       if (is.null(self$g_params[[data$treat_col]])){
          message("Parameter of learner for nuisance part g are not tuned, results might not be valid!")
       }
  
       if (is.null(self$r_params[[data$treat_col]])){
          message("Parameter of learner for nuisance part r are not tuned, results might not be valid!")
       }
    
      ml_g <- initiate_learner(self$ml_g,
                               self$g_params[[data$treat_col]])
      resampling_g <- mlr3::rsmp("custom")$instantiate(task_g,
                                                       smpls$train_ids,
                                                       smpls$test_ids)
      r_g <- mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
      g_hat <- extract_prediction(r_g)$response
      
      ml_r <- initiate_learner(self$ml_r,
                               self$r_params[[data$treat_col]])
      resampling_r <- mlr3::rsmp("custom")$instantiate(task_r,
                                                     smpls$train_ids,
                                                     smpls$test_ids)
      r_r <- mlr3::resample(task_r, ml_r, resampling_r, store_models = TRUE)
      r_hat <- extract_prediction(r_r)$response
      
      # nuisance r_tilde: Predict residuals from nuisance r, only with X
      data_aux = data.table(data$data_model, r_hat)
      task_r_tilde <- initiate_regr_task("nuis_r_hat", data_aux,
                                   skip_cols = c(data$y_col, data$treat_col, data$z_cols), target = "r_hat")
      
      ml_r_tilde <- initiate_learner(self$ml_m,
                               self$m_params[[data$treat_col]])
      
      resampling_r_tilde <- mlr3::rsmp("custom")$instantiate(task_r_tilde,
                                                     smpls$train_ids,
                                                     smpls$test_ids)
      r_r_tilde <- mlr3::resample(task_r_tilde, ml_r_tilde, resampling_r_tilde, store_models = TRUE)
      r_hat_tilde <- extract_prediction(r_r_tilde)$response
    }
    
    else if (!is.null(self$param_tuning)){
      ml_g <- lapply(self$g_params, function(x) initiate_learner(self$ml_g, 
                                                                        x[[1]]))
      resampling_g <- initiate_resampling(task_g, smpls$train_ids, smpls$test_ids)
      r_g <- resample_dml(task_g, ml_g, resampling_g, store_models = TRUE)
      g_hat <- lapply(r_g, extract_prediction)
      g_hat <- rearrange_prediction(g_hat, smpls$test_ids)
      
      ml_r <- lapply(self$r_params, function(x) initiate_learner(self$ml_r, 
                                                                        x[[1]]))
      resampling_r = initiate_resampling(task_r, smpls$train_ids, smpls$test_ids)
      r_r = resample_dml(task_r, ml_r, resampling_r, store_models = TRUE)
      r_hat = lapply(r_r, extract_prediction)
      r_hat = rearrange_prediction(r_hat, smpls$test_ids)
      
      # nuisance r_tilde: Predict residuals from nuisance r, only with X
      data_aux = data.table(data$data_model, r_hat)
      task_r_tilde <- initiate_regr_task("nuis_r_hat", data_aux,
                                   skip_cols = c(data$y_col, data$treat_col, data$z_cols), target = "r_hat")
      
      ml_r_tilde <- lapply(self$m_params, function(x) initiate_learner(self$ml_m, 
                                                                        x[[1]]))
    
      resampling_r_tilde = initiate_resampling(task_r_tilde, smpls$train_ids, smpls$test_ids)
      r_r_tilde = resample_dml(task_r_tilde, ml_r_tilde, resampling_r_tilde, store_models = TRUE)
      r_hat_tilde = lapply(r_r_tilde, extract_prediction)
      r_hat_tilde = rearrange_prediction(r_hat_tilde, smpls$test_ids)
  
    }
    
    D <- data$data_model[, data$treat_col, with = FALSE]
    Y <- data$data_model[, data$y_col, with = FALSE]

    u_hat <- Y - g_hat
    w_hat <- D - r_hat_tilde
    
    if (self$score == 'partialling out') {
        psi_a = -w_hat * (r_hat - r_hat_tilde)
        psi_b = (r_hat - r_hat_tilde) * u_hat
    }
      
    return(list(psi_a = psi_a,
                psi_b = psi_b))
  },
  
  ml_nuisance_and_score_elements_partialZ = function(data, smpls, ...) {
    
    # nuisance r
    task_r <- initiate_regr_task(paste0("nuis_r_", data$treat_col), data$data_model,
                                 skip_cols = c(data$y_col), target = data$treat_col)
    
    if (is.null(self$param_tuning)){
      
       if (is.null(self$r_params[[data$treat_col]])){
          message("Parameter of learner for nuisance part r are not tuned, results might not be valid!")
       }
    
      ml_r <- initiate_learner(self$ml_r,
                               self$r_params[[data$treat_col]])
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
    
    D <- data$data_model[, data$treat_col, with = FALSE]
    Y <- data$data_model[, data$y_col, with = FALSE]
    Z <- data$data_model[, data$z_cols, with = FALSE]
    
    if (self$score == 'partialling out') {
        psi_a = -r_hat* D
        psi_b =  r_hat* Y
     }
      
    return(list(psi_a = psi_a,
                psi_b = psi_b))
  },
  
  
  ml_nuisance_tuning  = function(data, smpls, param_set, tune_on_folds, tune_settings, ...){
    if (self$partialX & !self$partialZ) {
      res = private$ml_nuisance_tuning_partialX(data, smpls, param_set, tune_on_folds, tune_settings, ...)
      
    } else if (!self$partialX & self$partialZ) {
      res = private$ml_nuisance_tuning_partialZ(data, smpls, param_set, tune_on_folds, tune_settings, ...)
      
    } else if (self$partialX & self$partialZ) {
      res =  private$ml_nuisance_tuning_partialXZ(data, smpls, param_set, tune_on_folds, tune_settings, ...)
    }
    
    return(res)
  },
  
  ml_nuisance_tuning_partialX = function(data, smpls, param_set, tune_on_folds, tune_settings, ...){
    checkmate::check_class(param_set$param_set_g, "ParamSet")    
    checkmate::check_class(param_set$param_set_m, "ParamSet")
    checkmate::check_class(param_set$param_set_r, "ParamSet")

    if (!tune_on_folds){
      data_tune_list = list(data$data_model)
    } else {
      data_tune_list = lapply(smpls$train_ids, function(x) extract_training_data(data$data_model, x))
    }

    if (any(class(tune_settings$rsmp_tune) == "Resampling")) {
      CV_tune = tune_settings$rsmp_tune
    } else {
      CV_tune = mlr3::rsmp(tune_settings$rsmp_tune, folds = tune_settings$n_folds_tune)
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
          measure_m = mlr3::default_measures("regr")[[1]]
        } else {
          measure_m = mlr3::msr(tune_settings$measure_m)
      }
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
    
    task_g = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_g_", data$y_col), x,
                                                skip_cols = c(data$treat_col, data$z_cols), target = data$y_col))
    ml_g = mlr3::lrn(self$ml_g)
    tuning_instance_g = lapply(task_g, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_g,
                                          resampling = CV_tune,
                                          measure = measure_g,
                                          search_space = param_set$param_set_g,
                                          terminator = terminator))
    
    tuning_result_g = lapply(tuning_instance_g, function(x) tune_instance(tuner, x))
    
    task_r = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_r_", data$treat_col), x,
                                                skip_cols = c(data$y_col, data$z_cols), target = data$treat_col))
    ml_r = mlr3::lrn(self$ml_r)
    tuning_instance_r = lapply(task_r, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_r,
                                          resampling = CV_tune,
                                          measure = measure_r,
                                          search_space = param_set$param_set_r,
                                          terminator = terminator))
    tuning_result_r = lapply(tuning_instance_r, function(x) tune_instance(tuner, x))

    ml_m = mlr3::lrn(self$ml_m) 
    
    if (data$n_instr() == 1) {
      task_m = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_r_", data$z_cols), x,
                                                  skip_cols = c(data$y_col, data$treat_col),
                                                  target = data$z_cols))
      tuning_instance_m = lapply(task_m, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_m,
                                          resampling = CV_tune,
                                          measure = measure_m,
                                          search_space = param_set$param_set_m,
                                          terminator = terminator))
      tuning_result_m = lapply(tuning_instance_m, function(x) tune_instance(tuner, x))
    
      tuning_result = list(tuning_result = list(tuning_result_g = tuning_result_g, 
                                              tuning_result_m = tuning_result_m, 
                                              tuning_result_r = tuning_result_r),
                         params = list(g_params = extract_tuned_params(tuning_result_g), 
                                       m_params = extract_tuned_params(tuning_result_m),
                                       r_params = extract_tuned_params(tuning_result_r)))
        
    } else {
      
      tuning_result_m_params_mult_instr = vector("list", length = data$n_instr())
      names(tuning_result_m_params_mult_instr) = data$z_cols
      
      for (i_instr in 1:data$n_instr()){
        this_z = data$z_cols[i_instr]
        task_m = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_r_", data$z_cols[i_instr]), x,
                                                     skip_cols = c(data$y_col, data$treat_col, 
                                                                   data$z_cols[data$z_cols != this_z]),
                                                     target = this_z))
        tuning_instance_m = lapply(task_m, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_m,
                                          resampling = CV_tune,
                                          measure = measure_m,
                                          search_space = param_set$param_set_m,
                                          terminator = terminator))
        tuning_result_m_params_mult_instr[[i_instr]] = lapply(tuning_instance_m, function(x) tune_instance(tuner, x))
        
      }
      
      m_params_mult_instr = lapply(tuning_result_m_params_mult_instr, extract_tuned_params)
      
      tuning_result = list(tuning_result = list(tuning_result_g = tuning_result_g, 
                                              tuning_result_m_params_mult_instr = tuning_result_m_params_mult_instr, 
                                              tuning_result_r = tuning_result_r),
                           params = list(g_params = extract_tuned_params(tuning_result_g), 
                                          m_params_mult_instr = m_params_mult_instr,
                                          r_params = extract_tuned_params(tuning_result_r)))
    }

    return(tuning_result)
    
  },
  
   ml_nuisance_tuning_partialXZ = function(data, smpls, param_set, tune_on_folds, tune_settings, ...){
    
    stop("Tuning not implemented for partialXZ case.")
    
  },
  
  ml_nuisance_tuning_partialZ = function(data, smpls, param_set, tune_on_folds, tune_settings, ...){
    
    checkmate::check_class(param_set$param_set_r, "ParamSet")

    if (!tune_on_folds){
      data_tune_list = list(data$data_model)
    } else {
      data_tune_list = lapply(smpls$train_ids, function(x) extract_training_data(data$data_model, x))
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
    
    task_r = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_r_", data$z_cols), x,
                                                   skip_cols = c(data$y_col, data$z_cols), target = data$treat_col))
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
                           n_folds,
                           n_rep, 
                           score, 
                           dml_procedure,
                           draw_sample_splitting, 
                           apply_cross_fitting)
    obj$partialX = FALSE
    obj$partialZ = TRUE
    
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
                           n_folds,
                           n_rep, 
                           score, 
                           dml_procedure,
                           draw_sample_splitting, 
                           apply_cross_fitting)
    obj$partialX = TRUE
    obj$partialZ = TRUE
    
    return(obj)
}
