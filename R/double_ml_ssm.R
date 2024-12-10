
DoubleMLSSM = R6Class("DoubleMLSSM",
  inherit = DoubleML, 
  
  active = list(
    trimming_rule = function(value) {
      if (missing(value)) {
        return(private$trimming_rule_)
      } else {
        stop("can't set field trimming_rule")
      }
    },
    
    trimming_threshold = function(value) {
      if (missing(value)) {
        return(private$trimming_threshold_)
      } else {
        stop("can't set field trimming_threshold")
      }
    }),
  
  
  public = list(
    
    initialize = function(data,
      ml_g,
      ml_pi,
      ml_m,
      n_folds = 5,
      n_rep = 1,
      score = "missing-at-random",
      normalize_ipw = FALSE,
      trimming_rule = "truncate",
      trimming_threshold = 1e-12,
      dml_procedure = "dml2",
      draw_sample_splitting = TRUE,
      apply_cross_fitting = TRUE) {

      super$initialize_double_ml(
        data,
        n_folds,
        n_rep,
        score,
        dml_procedure,
        draw_sample_splitting,
        apply_cross_fitting)
      
      private$normalize_ipw = normalize_ipw

      private$check_data(self$data)
      private$check_score(self$score)
      ml_g = private$assert_learner(ml_g, "ml_g", Regr = TRUE, Classif = TRUE)
      ml_pi = private$assert_learner(ml_pi, "ml_pi", Regr = FALSE, Classif = TRUE)
      ml_m = private$assert_learner(ml_m, "ml_m", Regr = FALSE, Classif = TRUE)

      private$learner_ = list(
        "ml_g" = ml_g,
        "ml_pi" = ml_pi,
        "ml_m" = ml_m)

      private$initialize_ml_nuisance_params()
      
      private$trimming_rule_ = trimming_rule
      private$trimming_threshold_ = trimming_threshold
    },
    
    set_ml_nuisance_params = function(learner = NULL, treat_var = NULL, params,
      set_fold_specific = FALSE) {
      assert_character(learner, len = 1)

      super$set_ml_nuisance_params(
        learner, treat_var, params,
        set_fold_specific)
    },

    
    tune = function(param_set, tune_settings = list(
      n_folds_tune = 5,
      rsmp_tune = mlr3::rsmp("cv", folds = 5),
      measure = NULL,
      terminator = mlr3tuning::trm("evals", n_evals = 20),
      algorithm = mlr3tuning::tnr("grid_search"),
      resolution = 5),
    tune_on_folds = FALSE) {

      assert_list(param_set)
      assert_list(tune_settings)
      if (test_names(names(tune_settings), must.include = "measure") && !is.null(tune_settings$measure)) {
        assert_list(tune_settings$measure)
      }

      super$tune(param_set, tune_settings, tune_on_folds)
    }
  ),
  
  
  private = list(
    n_nuisance = 4,
    normalize_ipw = FALSE,
    trimming_rule_ = NULL,
    trimming_threshold_ = NULL,
    initialize_ml_nuisance_params = function() {
      nuisance = vector("list", self$data$n_treat)
      names(nuisance) = self$data$d_cols
      private$params_ = list(
        "ml_g_d0" = nuisance,
        "ml_g_d1" = nuisance,
        "ml_pi" = nuisance,
        "ml_m" = nuisance)
      invisible(self)
    },

    nuisance_est = function(smpls, ...) {
      
      if(self$score == "missing-at-random") {
        
        smpls_d_s = get_cond_samples_2d(smpls, self$data$data_model[[self$data$treat_col]], 
                                        self$data$data_model[[self$data$s_col]])
        smpls_d0_s1 = smpls_d_s$smpls_01
        smpls_d1_s1 = smpls_d_s$smpls_11
        
        pi_hat = dml_cv_predict(self$learner$ml_pi,
          c(self$data$x_cols, self$data$other_treat_cols, self$data$d_cols),
          self$data$s_col,
          self$data$data_model,
          nuisance_id = "nuis_pi",
          smpls = smpls,
          est_params = self$get_params("ml_pi"),
          return_train_preds = FALSE,
          task_type = private$task_type$ml_pi,
          fold_specific_params = private$fold_specific_params)
      
        m_hat = dml_cv_predict(self$learner$ml_m,
          c(self$data$x_cols, self$data$other_treat_cols),
          self$data$d_cols,
          self$data$data_model,
          nuisance_id = "nuis_m",
          smpls = smpls,
          est_params = self$get_params("ml_m"),
          return_train_preds = FALSE,
          task_type = private$task_type$ml_m,
          fold_specific_params = private$fold_specific_params)
  
        g_hat_d0 = dml_cv_predict(self$learner$ml_g,
          c(self$data$x_cols, self$data$other_treat_cols),
          self$data$y_col,
          self$data$data_model,
          nuisance_id = "nuis_g_d0",
          smpls = smpls_d0_s1,
          est_params = self$get_params("ml_g_d0"),
          return_train_preds = FALSE,
          task_type = private$task_type$ml_g,
          fold_specific_params = private$fold_specific_params)
              
        g_hat_d1 = dml_cv_predict(self$learner$ml_g,
          c(self$data$x_cols, self$data$other_treat_cols),
          self$data$y_col,
          self$data$data_model,
          nuisance_id = "nuis_g_d1",
          smpls = smpls_d1_s1,
          est_params = self$get_params("ml_g_d1"),
          return_train_preds = FALSE,
          task_type = private$task_type$ml_g,
          fold_specific_params = private$fold_specific_params)
        
      } else { #nonignorable
        
        pi_hat = list(preds = NULL, models = NULL)
        m_hat = list(preds = NULL, models = NULL)
        g_hat_d0 = list(preds = NULL, models = NULL)
        g_hat_d1 = list(preds = NULL, models = NULL)
        
        preds_pi_hat = numeric(nrow(self$data$data))
        preds_m_hat = numeric(nrow(self$data$data))
        preds_g_hat_d0 = numeric(nrow(self$data$data))
        preds_g_hat_d1 = numeric(nrow(self$data$data))
        
        strata = self$data$data$d + 2 * self$data$data$s
        self$data$data[,strata := strata]
        
        
        for(i_fold in 1:(self$n_folds)){
          
          train_inds = smpls$train_ids[[i_fold]]
          test_inds =  smpls$test_ids[[i_fold]]
          
          
          # split train_inds into 2 sets
          dummy_train_task = Task$new("dummy", "regr", self$data$data)
          dummy_train_task$set_col_roles("strata", c("target", "stratum"))
          dummy_train_resampling = rsmp("holdout", ratio = 0.5)$instantiate(dummy_train_task$filter(train_inds))
          train1 = dummy_train_resampling$train_set(1)
          train2 = dummy_train_resampling$test_set(1)
          
          # pi_hat_prelim and pi_hat
          task_pred_pi_hat = initiate_task(
            id = "nuis_pi", 
            data = self$data$data_model,
            target = self$data$s_col,
            select_cols = c(self$data$x_cols, self$data$other_treat_cols, self$data$d_cols, self$data$z_cols),
            task_type = private$task_type$ml_pi)
          
          ml_learner_pi_hat = initiate_learner(
            learner = self$learner$ml_pi, 
            task_type = private$task_type$ml_pi,
            params = self$get_params("ml_pi"), 
            return_train_preds = FALSE)
          
          resampling_smpls_pi_hat = rsmp("custom")$instantiate(
            task_pred_pi_hat, list(train1), list(1:nrow(self$data$data)))
          
          resampling_pred_pi_hat = resample(task_pred_pi_hat, ml_learner_pi_hat, resampling_smpls_pi_hat, store_models = TRUE)
          
          pi_hat$models[[i_fold]] = resampling_pred_pi_hat$score()$learner
          
          preds_pi_hat_prelim = extract_prediction(resampling_pred_pi_hat, private$task_type$ml_pi, n_obs=nrow(self$data$data))
          
          preds_pi_hat[test_inds] = preds_pi_hat_prelim[test_inds]
          
          # add pi_hat_prelim
          self$data$data_model[,pi_hat_prelim := preds_pi_hat_prelim]
          
          # m_hat
          task_pred_m_hat = initiate_task(
            id = "nuis_m", 
            data = self$data$data_model,
            target = self$data$d_cols,
            select_cols = c(self$data$x_cols, self$data$other_treat_cols, "pi_hat_prelim"),
            task_type = private$task_type$ml_m)
          
          ml_learner_m_hat = initiate_learner(
            learner = self$learner$ml_m, 
            task_type = private$task_type$ml_pi,
            params = self$get_params("ml_m"), 
            return_train_preds = FALSE)
          
          resampling_smpls_m_hat = rsmp("custom")$instantiate(
            task_pred_m_hat, list(train2), list(test_inds))
          
          resampling_pred_m_hat = resample(task_pred_m_hat, ml_learner_m_hat, resampling_smpls_m_hat, store_models = TRUE)
          
          m_hat$models[[i_fold]] = resampling_pred_m_hat$score()$learner
          
          preds_m_hat[test_inds] = extract_prediction(resampling_pred_m_hat, private$task_type$ml_m, n_obs=nrow(self$data$data))[test_inds]
          
          
          # g_hat_d0
          d = self$data$data_model[[self$data$treat_col]]
          s = self$data$data_model[[self$data$s_col]]
          train2_d0_s1 = train2[d[train2] == 0 & s[train2] == 1]
          
          task_pred_g_hat_d0 = initiate_task(
            id = "nuis_g_d0", 
            data = self$data$data_model,
            target = self$data$y_col,
            select_cols = c(self$data$x_cols, self$data$other_treat_cols, "pi_hat_prelim"),
            task_type = private$task_type$ml_g)
          
          ml_learner_g_hat_d0 = initiate_learner(
            learner = self$learner$ml_g, 
            task_type = private$task_type$ml_g,
            params = self$get_params("ml_g_d0"), 
            return_train_preds = FALSE)
          
          resampling_smpls_g_hat_d0 = rsmp("custom")$instantiate(
            task_pred_g_hat_d0, list(train2_d0_s1), list(test_inds))
          
          resampling_pred_g_hat_d0 = resample(task_pred_g_hat_d0, ml_learner_g_hat_d0, resampling_smpls_g_hat_d0, store_models = TRUE)
          
          g_hat_d0$models[[i_fold]] = resampling_pred_g_hat_d0$score()$learner
          
          preds_g_hat_d0[test_inds] = extract_prediction(resampling_pred_g_hat_d0, private$task_type$ml_g, n_obs=nrow(self$data$data))[test_inds]
          
          # g_hat_d1
          train2_d1_s1 = train2[d[train2] == 1 & s[train2] == 1]
          
          task_pred_g_hat_d1 = initiate_task(
            id = "nuis_g_d1", 
            data = self$data$data_model,
            target = self$data$y_col,
            select_cols = c(self$data$x_cols, self$data$other_treat_cols, "pi_hat_prelim"),
            task_type = private$task_type$ml_g)
          
          ml_learner_g_hat_d1 = initiate_learner(
            learner = self$learner$ml_g, 
            task_type = private$task_type$ml_g,
            params = self$get_params("ml_g_d1"), 
            return_train_preds = FALSE)
          
          resampling_smpls_g_hat_d1 = rsmp("custom")$instantiate(
            task_pred_g_hat_d1, list(train2_d1_s1), list(test_inds))
          
          resampling_pred_g_hat_d1 = resample(task_pred_g_hat_d1, ml_learner_g_hat_d1, resampling_smpls_g_hat_d1, store_models = TRUE)
          
          g_hat_d1$models[[i_fold]] = resampling_pred_g_hat_d1$score()$learner
          
          preds_g_hat_d1[test_inds] = extract_prediction(resampling_pred_g_hat_d1, private$task_type$ml_g, n_obs=nrow(self$data$data))[test_inds]
          
        }
        
        pi_hat$preds = preds_pi_hat
        m_hat$preds = preds_m_hat
        g_hat_d0$preds = preds_g_hat_d0
        g_hat_d1$preds = preds_g_hat_d1
        
        self$data$data[,strata := NULL]
        self$data$data_model[,pi_hat_prelim := NULL]
        
      }
      
      
      d = self$data$data_model[[self$data$treat_col]]
      y = self$data$data_model[[self$data$y_col]]
      s = self$data$data_model[[self$data$s_col]]
      
      res = private$score_elements(
        y, d, s, pi_hat$preds, m_hat$preds, g_hat_d0$preds, g_hat_d1$preds,
        smpls)
      res$preds = list(
        "ml_pi" = pi_hat$preds,
        "ml_m" = m_hat$preds,
        "ml_g_d0" = g_hat_d0$preds,
        "ml_g_d1" = g_hat_d1$preds)
      res$models = list(
        "ml_pi" = pi_hat$models,
        "ml_m" = m_hat$models,
        "ml_g_d0" = g_hat_d0$models,
        "ml_g_d1" = g_hat_d1$models)
      return(res)
    },

    
    score_elements = function(y, d, s, pi_hat, m_hat, g_hat_d0, g_hat_d1, smpls) {
      
      dtreat = (d == 1)
      dcontrol = (d == 0)
      
      if (self$trimming_rule == "truncate" & self$trimming_threshold > 0) {
        m_hat[m_hat < self$trimming_threshold] = self$trimming_threshold
        m_hat[m_hat > 1 - self$trimming_threshold] = 1 - self$trimming_threshold
      }
      
      psi_a = -1
      
      if (private$normalize_ipw == TRUE) {
        weight_treat = sum(dtreat) / sum((dtreat * s) / (pi_hat * m_hat))
        weight_control = sum(dcontrol) / sum((dcontrol * s) / (pi_hat * (1 - m_hat)))
        
        psi_b1 = weight_treat * ((dtreat * s * (y - g_hat_d1)) / (m_hat * pi_hat)) + g_hat_d1
        psi_b0 = weight_control * ((dcontrol * s * (y - g_hat_d0)) / ((1 - m_hat) * pi_hat)) + g_hat_d0
        
      } else {
        psi_b1 = (dtreat * s * (y - g_hat_d1)) / (m_hat * pi_hat) + g_hat_d1
        psi_b0 = (dcontrol * s * (y - g_hat_d0)) / ((1 - m_hat) * pi_hat) + g_hat_d0
        

      }
      
      psi_b = psi_b1 - psi_b0
      
      psis = list(
        psi_a = psi_a,
        psi_b = psi_b)
      
      return(psis)
    },

    nuisance_tuning = function(smpls, param_set, tune_settings,
      tune_on_folds, ...) {

      if (!tune_on_folds) {
        data_tune_list = list(self$data$data_model)
      } else {
        data_tune_list = lapply(smpls$train_ids, function(x) {
          extract_training_data(self$data$data_model, x)
        })
      }
      
      indx_d0_s1 = lapply(data_tune_list, function(x) x[[self$data$d_cols]] == 0 & x[[self$data$s_col]] == 1)
      indx_d1_s1 = lapply(data_tune_list, function(x) x[[self$data$d_cols]] == 1 & x[[self$data$s_col]] == 1)
      data_tune_list_d0_s1 = lapply(
        seq_len(length(data_tune_list)),
        function(x) data_tune_list[[x]][indx_d0_s1[[x]], ])
      data_tune_list_d1_s1 = lapply(
        seq_len(length(data_tune_list)),
        function(x) data_tune_list[[x]][indx_d1_s1[[x]], ])
      
      tuning_result_pi = dml_tune(self$learner$ml_pi,
       c(self$data$x_cols, self$data$other_treat_cols, self$data$d_cols, self$data$z_cols),
        self$data$s_col, data_tune_list,
        nuisance_id = "nuis_pi",
        param_set$ml_pi, tune_settings,
        tune_settings$measure$ml_pi,
        private$task_type$ml_pi)

      tuning_result_m = dml_tune(self$learner$ml_m,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$d_cols, data_tune_list,
        nuisance_id = "nuis_m",
        param_set$ml_m, tune_settings,
        tune_settings$measure$ml_m,
        private$task_type$ml_m)
      
      tuning_result_g_d0 = dml_tune(self$learner$ml_g,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$y_col, data_tune_list_d0_s1,
        nuisance_id = "nuis_g_d0",
        param_set$ml_g, tune_settings,
        tune_settings$measure$ml_g,
        private$task_type$ml_g)
      
      tuning_result_g_d1 = dml_tune(self$learner$ml_g,
        c(self$data$x_cols, self$data$other_treat_cols),
        self$data$y_col, data_tune_list_d1_s1,
        nuisance_id = "nuis_g_d1",
        param_set$ml_g, tune_settings,
        tune_settings$measure$ml_g,
        private$task_type$ml_g)

      
      tuning_result = list(
          "ml_pi" = list(tuning_result_pi, params = tuning_result_pi$params),
          "ml_m" = list(tuning_result_m, params = tuning_result_m$params),
          "ml_g_d0" = list(tuning_result_g_d0, params = tuning_result_g_d0$params),
          "ml_g_d1" = list(tuning_result_g_d1, params = tuning_result_g_d1$params))

      return(tuning_result)
    },

    check_score = function(score) {
      assert(
        check_character(score),
        check_class(score, "function"))
      if (is.character(score)) {
        valid_score = c("missing-at-random", "nonignorable")
        assertChoice(score, valid_score)
      }
      return()
    },

    check_data = function(obj_dml_data) {
      if (!is.null(obj_dml_data$z_cols) && self$score == "missing-at-random") {
        warning(paste(
          paste(obj_dml_data$z_cols, collapse = ", "), "has been set as instrumental variable(s).\n",
          "You are estimating the effect under the assumption of data missing at random.\n",
          "Instrumental variables will not be used in estimation."
        ))
      }
      if (is.null(obj_dml_data$z_cols) && self$score == "nonignorable") {
        stop(paste(
          "Sample selection by nonignorable nonresponse was set but instrumental variable \n",
          "is NULL. To estimate treatment effect under nonignorable nonresponse, \n",
          "specify an instrument for the selection variable."
        ))
      }
      return()
    }

  )
)

