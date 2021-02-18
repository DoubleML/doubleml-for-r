#' @title Double machine learning for partially linear IV regression models
#' 
#' @description
#' Double machine learning for partially linear IV regression models. 
#' 
#' @format [R6::R6Class] object inheriting from [DoubleML].
#' 
#' @family DoubleML
#' 
#' @details 
#' Partially linear IV regression (PLIV) models take the form 
#' 
#' \eqn{Y - D\theta_0 = g_0(X) + \zeta},  
#' 
#' \eqn{Z = m_0(X) + V}, 
#' 
#' with \eqn{E[\zeta|Z,X]=0} and \eqn{E[V|X] = 0}. \eqn{Y} is the outcome variable variable, \eqn{D} is the policy variable of interest and \eqn{Z} denotes one or multiple instrumental variables. The high-dimensional vector \eqn{X = (X_1, \ldots, X_p)} consists of other confounding covariates, and \eqn{\zeta} and \eqn{V} are stochastic errors.  
#' 
#' @usage NULL
#' 
#' @examples
#' \donttest{
#' library(DoubleML)
#' library(mlr3)
#' library(mlr3learners)
#' library(data.table)
#' set.seed(2)
#' ml_g = lrn("regr.ranger", num.trees = 100, mtry = 20, min.node.size = 2, max.depth = 5)
#' ml_m = ml_g$clone()
#' ml_r = ml_g$clone()
#' obj_dml_data = make_pliv_CHS2015(alpha = 1, n_obs = 500, dim_x = 20, dim_z = 1)
#' dml_pliv_obj = DoubleMLPLIV$new(obj_dml_data, ml_g, ml_m, ml_r)
#' dml_pliv_obj$fit()
#' dml_pliv_obj$summary()
#' }
#' @export
DoubleMLPLIV = R6Class("DoubleMLPLIV", inherit = DoubleML, public = list(
  #' @field partialX (`logical(1)`)  \cr
  #' Indicates whether covariates \eqn{X} should be partialled out. 
  partialX = NULL,
  
  #' @field partialZ (`logical(1)`) \cr
  #' Indicates whether instruments \eqn{Z} should be partialled out. 
  partialZ = NULL,
  
  #' @description 
  #' Creates a new instance of this R6 class. 
  #' 
  #' @param data (`DoubleMLData`) \cr
  #' The `DoubleMLData` object providing the data and specifying the variables of the causal model.
  #' 
  #' @param ml_g ([`LearnerRegr`][mlr3::LearnerRegr], `character(1)`) \cr
  #' An object of the class [mlr3 regression learner][mlr3::LearnerRegr] to pass a learner, possibly with specified parameters, for example `lrn(regr.cv_glmnet, s = "lambda.min")`. 
  #' Alternatively, a `character(1)` specifying the name of a [mlr3 regression learner][mlr3::LearnerRegr] that is available in [mlr3](https://mlr3.mlr-org.com/index.html) or its extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or [mlr3extralearners](https://mlr3extralearners.mlr-org.com/), for example `"regr.cv_glmnet"`.  \cr
  #' `ml_g` refers to the nuisance function \eqn{g_0(X) = E[Y|X]}.
  #' 
  #' @param ml_m ([`LearnerRegr`][mlr3::LearnerRegr], `character(1)`) \cr
  #' An object of the class [mlr3 regression learner][mlr3::LearnerRegr] to pass a learner, possibly with specified parameters, for example `lrn(regr.cv_glmnet, s = "lambda.min")`. 
  #' Alternatively, a `character(1)` specifying the name of a [mlr3 regression learner][mlr3::LearnerRegr] that is available in [mlr3](https://mlr3.mlr-org.com/index.html) or its extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or [mlr3extralearners](https://mlr3extralearners.mlr-org.com/), for example `"regr.cv_glmnet"`.  \cr
  #' `ml_m` refers to the nuisance function \eqn{m_0(X) = E[Z|X]}.
  #' 
  #' @param ml_r ([`LearnerRegr`][mlr3::LearnerRegr], `character(1)`) \cr
  #' An object of the class [mlr3 regression learner][mlr3::LearnerRegr] to pass a learner, possibly with specified parameters, for example `lrn(regr.cv_glmnet, s = "lambda.min")`.
  #' Alternatively, a `character(1)` specifying the name of a [mlr3 regression learner][mlr3::LearnerRegr] that is available in [mlr3](https://mlr3.mlr-org.com/index.html) or its extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or [mlr3extralearners](https://mlr3extralearners.mlr-org.com/), for example `"regr.cv_glmnet"`.   \cr
  #' `ml_r` refers to the nuisance function \eqn{r_0(X) = E[D|X]}.
  #' 
  #' @param partialX (`logical(1)`)  \cr
  #' Indicates whether covariates \eqn{X} should be partialled out. Default is `TRUE`. 
  #' 
  #' @param partialZ (`logical(1)`) \cr
  #' Indicates whether instruments \eqn{Z} should be partialled out. Default is `FALSE`.
  #' 
  #' @param n_folds (`integer(1)`)\cr
  #' Number of folds. Default is `5`. 
  #' 
  #' @param n_rep (`integer(1)`) \cr
  #' Number of repetitions for the sample splitting. Default is `1`. 
  #' 
  #' @param score (`character(1)`, `function()`) \cr
  #' A `character(1)` (`"partialling out"` is the only choice) or a `function()` specifying the score function.
  #' If a `function()` is provided, it must be of the form `function(y, z, d, g_hat, m_hat, r_hat, smpls)` and
  #' the returned output must be a named `list()` with elements `psi_a` and `psi_b`. Default is `"partialling out"`. 
  #' 
  #' @param dml_procedure (`character(1)`) \cr
  #' A `character(1)` (`"dml1"` or `"dml2"`) specifying the double machine learning algorithm. Default is `"dml2"`. 
  #' 
  #' @param draw_sample_splitting (`logical(1)`) \cr
  #' Indicates whether the sample splitting should be drawn during initialization of the object. Default is `TRUE`. 
  #' 
  #' @param apply_cross_fitting (`logical(1)`) \cr
  #' Indicates whether cross-fitting should be applied. Default is `TRUE`.  
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
    checkmate::check_logical(partialX, len = 1)
    checkmate::check_logical(partialZ, len = 1)
    self$partialX = partialX
    self$partialZ = partialZ
    
    if (!self$partialX & self$partialZ) {
      ml_r = private$assert_learner(ml_r, "ml_r", Regr = TRUE, Classif = FALSE)
      self$learner = list("ml_r" = ml_r)
    } else {
      ml_g = private$assert_learner(ml_g, "ml_g", Regr = TRUE, Classif = FALSE)
      ml_m = private$assert_learner(ml_m, "ml_m", Regr = TRUE, Classif = FALSE)
      ml_r = private$assert_learner(ml_g, "ml_r", Regr = TRUE, Classif = FALSE)
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
      if (self$data$n_instr == 1) {
        valid_learner = c("ml_g", "ml_m", "ml_r")
      } else {
        valid_learner = c("ml_g", "ml_r", paste0("ml_m_", self$data$z_cols))
      } 
    } else if (self$partialX & self$partialZ) {
      valid_learner = c("ml_g", "ml_m", "ml_r") 
    } else if (!self$partialX & self$partialZ) {
      valid_learner = c("ml_r")
    }
    nuisance = vector("list", self$data$n_treat)
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
    
    g_hat = dml_cv_predict(self$learner$ml_g, c(self$data$x_cols, self$data$other_treat_cols), self$data$y_col, 
                           self$data$data_model, nuisance_id = "nuis_g",  
                           smpls, self$get_params("ml_g"), return_train_preds = FALSE, 
                           learner_class = private$learner_class["ml_g"], private$fold_specific_params)
      
    r_hat = dml_cv_predict(self$learner$ml_r, c(self$data$x_cols, self$data$other_treat_cols), self$data$treat_col, 
                           self$data$data_model, nuisance_id = "nuis_r",  
                           smpls, self$get_params("ml_r"), return_train_preds = FALSE, 
                           learner_class = private$learner_class["ml_r"], private$fold_specific_params)
    
    if (self$data$n_instr == 1) {
      m_hat = dml_cv_predict(self$learner$ml_m, c(self$data$x_cols, self$data$other_treat_cols), self$data$z_cols, 
                             self$data$data_model, nuisance_id = "nuis_m",  
                             smpls, self$get_params("ml_m"), return_train_preds = FALSE, 
                             learner_class = private$learner_class["ml_m"], private$fold_specific_params)
    } else {
      m_hat = do.call(cbind, lapply(self$data$z_cols, function(x) 
                                        dml_cv_predict(self$learner$ml_m, c(self$data$x_cols, self$data$other_treat_cols),  x, 
                                                       self$data$data_model, nuisance_id = "nuis_m",  
                                                       smpls, self$get_params(paste0("ml_m_", x)), return_train_preds = FALSE, 
                                                       learner_class = private$learner_class["ml_m"], private$fold_specific_params)))
    }
    
    d = self$data$data_model[[self$data$treat_col]]
    y = self$data$data_model[[self$data$y_col]]
    
    u_hat = y - g_hat
    w_hat = d - r_hat
    
    if (self$data$n_instr == 1) {
      z = self$data$data_model[[self$data$z_cols]]
      v_hat = z - m_hat
    } else {
      z = self$data$data_model[, self$data$z_cols, with = FALSE]
      v_hat = z - m_hat
      
      stopifnot(self$apply_cross_fitting) 
      
      # Projection: r_hat from projection on m_hat
      data_aux = data.table(w_hat, v_hat)
      task_r_tilde = initiate_regr_task("nuis_r_tilde", data_aux, select_cols = NULL, 
                                        target = "w_hat")
      ml_r_tilde = lrn("regr.lm")
      resampling_r_tilde = rsmp("insample")$instantiate(task_r_tilde)
      r_r_tilde = resample(task_r_tilde, ml_r_tilde, resampling_r_tilde,
                           store_models = TRUE)
      r_hat_tilde = extract_response_prediction(r_r_tilde)$response
    }
    
    score = self$score
    private$check_score(score)
    
    if (is.character(self$score)) {
      if (self$data$n_instr == 1) {
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
    psis = list(psi_a = psi_a, 
                psi_b = psi_b)
    } else if (is.function(self$score)) {
      if (self$data$n_instr > 1) {
        stop("Callable score not implemented for DoubleMLPLIV with partialX=TRUE and partialZ=FALSE 
              with several instruments")
      }
      psis = self$score(y, z, d, g_hat, m_hat, r_hat, smpls)
    }
    return(psis)
  },
  
  ml_nuisance_and_score_elements_partialXZ = function(smpls, ...) {
    
    g_hat = dml_cv_predict(self$learner$ml_g, c(self$data$x_cols, self$data$other_treat_cols), self$data$y_col, 
                           self$data$data_model, nuisance_id = "nuis_g",  
                           smpls, self$get_params("ml_g"), return_train_preds = FALSE, 
                           learner_class = private$learner_class["ml_g"], private$fold_specific_params)
      
    m_hat_list = dml_cv_predict(self$learner$ml_m, c(self$data$x_cols, self$data$other_treat_cols, self$data$z_cols), 
                           self$data$treat_col, 
                           self$data$data_model, nuisance_id = "nuis_m",  
                           smpls, self$get_params("ml_m"), return_train_preds = TRUE, 
                           learner_class = private$learner_class["ml_m"], private$fold_specific_params)
    m_hat = m_hat_list$preds
    data_aux_list = lapply(m_hat_list$train_preds, function(x) 
                                              data.table(self$data$data_model, "m_hat_on_train" = x))
    
    m_hat_tilde = dml_cv_predict(self$learner$ml_r, c(self$data$x_cols, self$data$other_treat_cols), 
                                                         "m_hat_on_train", 
                                                         data_aux_list, nuisance_id = "nuis_r",  
                                                         smpls, self$get_params("ml_r"), return_train_preds = FALSE, 
                                                         learner_class = private$learner_class["ml_r"], private$fold_specific_params)
    
    d = self$data$data_model[[self$data$treat_col]]
    y = self$data$data_model[[self$data$y_col]]

    u_hat = y - g_hat
    w_hat = d - m_hat_tilde

    score = self$score
    private$check_score(score)
    
    if (is.character(self$score)) {
      if (self$score == 'partialling out') {
          psi_a = -w_hat * (m_hat - m_hat_tilde)
          psi_b = (m_hat - m_hat_tilde) * u_hat
      }
      psis = list(psi_a = psi_a,
                  psi_b = psi_b)
    } else if (is.function(self$score)) {
      stop("Callable score not implemented for DoubleMLPLIV with partialX=TRUE and partialZ=TRUE.")
      # psis = self$score(y, d, g_hat, m_hat, m_hat_tilde) 
    }
    return(psis)
  },
  
  ml_nuisance_and_score_elements_partialZ = function(smpls, ...) {
    
    # nuisance r
    r_hat = dml_cv_predict(self$learner$ml_r, 
                           c(self$data$x_cols, self$data$other_treat_cols, self$data$z_cols), self$data$treat_col, 
                           self$data$data_model, nuisance_id = "nuis_r",  
                           smpls, self$get_params("ml_r"), return_train_preds = FALSE, 
                           learner_class = private$learner_class["ml_r"], private$fold_specific_params)
    
    d = self$data$data_model[[self$data$treat_col]]
    y = self$data$data_model[[self$data$y_col]]
    
    score = self$score
    private$check_score(score)
    
    if (is.character(self$score)) {
      if (self$score == 'partialling out') {
          psi_a = -r_hat* d
          psi_b =  r_hat* y
       }
      psis = list(psi_a = psi_a, psi_b = psi_b)
    } else if (is.function(self$score)) {
      stop("Callable score not implemented for DoubleMLPLIV with partialX=FALSE and partialZ=TRUE.")
      # psis = self$score(y, z, d, r_hat)
    }
    return(psis)
  },
  
  
  ml_nuisance_tuning  = function(smpls, param_set, tune_settings, tune_on_folds, ...){
    if (self$partialX & !self$partialZ) {
      res = private$ml_nuisance_tuning_partialX(smpls, param_set, tune_settings, tune_on_folds, ...)
      
    } else if (!self$partialX & self$partialZ) {
      res = private$ml_nuisance_tuning_partialZ(smpls, param_set, tune_settings, tune_on_folds, ...)
      
    } else if (self$partialX & self$partialZ) {
      res =  private$ml_nuisance_tuning_partialXZ(smpls, param_set, tune_settings, tune_on_folds, ...)
    }
    
    return(res)
  },
  
  ml_nuisance_tuning_partialX = function(smpls, param_set, tune_settings, tune_on_folds, ...){
    if (!tune_on_folds){
      data_tune_list = list(self$data$data_model)
    } else {
      data_tune_list = lapply(smpls$train_ids, function(x) extract_training_data(self$data$data_model, x))
    }
    if (any(class(tune_settings$rsmp_tune) == "Resampling")) {
      CV_tune = tune_settings$rsmp_tune
    } else {
      CV_tune = rsmp(tune_settings$rsmp_tune, folds = tune_settings$n_folds_tune)
    }
    if (any(class(tune_settings$measure$ml_g) == "Measure")) {
      measure_g = tune_settings$measure$ml_g
    } else {
        if (is.null(tune_settings$measure$ml_g)){
          measure_g = default_measures("regr")[[1]]
        } else {
          measure_g = msr(tune_settings$measure$ml_g)
      }
    }
    if (any(class(tune_settings$measure$ml_m) == "Measure")) {
      measure_m = tune_settings$measure$ml_m
    } else {
        if (is.null(tune_settings$measure$ml_m)){
          measure_m = default_measures("regr")[[1]]
        } else {
          measure_m = msr(tune_settings$measure$ml_m)
      }
    }
    if (any(class(tune_settings$measure$ml_r) == "Measure")) {
      measure_r = tune_settings$measure$ml_r
    } else {
        if (is.null(tune_settings$measure$ml_r)){
          measure_r = default_measures("regr")[[1]]
        } else {
          measure_r = msr(tune_settings$measure$ml_r)
      }
    }
    
    terminator = tune_settings$terminator
    tuner = tnr(tune_settings$algorithm, resolution = tune_settings$resolution)
    
    task_g = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_g_", self$data$y_col), x,
                                                  select_cols = c(self$data$x_cols, self$data$other_treat_cols),
                                                  target = self$data$y_col))
    ml_g = initiate_regr_learner(self$learner$ml_g, params = list())
    tuning_instance_g = lapply(task_g, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_g,
                                          resampling = CV_tune,
                                          measure = measure_g,
                                          search_space = param_set$ml_g,
                                          terminator = terminator))
    
    tuning_result_g = lapply(tuning_instance_g, function(x) tune_instance(tuner, x))
    
    task_r = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_r_", self$data$treat_col), x,
                                                  select_cols = c(self$data$x_cols, self$data$other_treat_cols),
                                                  target = self$data$treat_col))
    ml_r = initiate_regr_learner(self$learner$ml_r, params = list())
    tuning_instance_r = lapply(task_r, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_r,
                                          resampling = CV_tune,
                                          measure = measure_r,
                                          search_space = param_set$ml_r,
                                          terminator = terminator))
    tuning_result_r = lapply(tuning_instance_r, function(x) tune_instance(tuner, x))
    ml_m = initiate_regr_learner(self$learner$ml_m, params = list())
    
    if (self$data$n_instr == 1) {
      task_m = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_r_", self$data$z_cols), x,
                                                    select_cols = c(self$data$x_cols, self$data$other_treat_cols),
                                                    target = self$data$z_cols))
      tuning_instance_m = lapply(task_m, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_m,
                                          resampling = CV_tune,
                                          measure = measure_m,
                                          search_space = param_set$ml_m,
                                          terminator = terminator))
      tuning_result_m = lapply(tuning_instance_m, function(x) tune_instance(tuner, x))
    
      tuning_result = list("ml_g" = list(tuning_result_g,
                                         params = extract_tuned_params(tuning_result_g)),
                           "ml_m" = list(tuning_result_m, 
                                         params = extract_tuned_params(tuning_result_m)), 
                           "ml_r" = list(tuning_result_r, 
                                         params = extract_tuned_params(tuning_result_r)))
    } else {
      tuning_result_m = vector("list", length = self$data$n_instr)
      names(tuning_result_m) = self$data$z_cols
      
      for (i_instr in 1:self$data$n_instr) {
        this_z = self$data$z_cols[i_instr] 
        task_m = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_r_", self$data$z_cols[i_instr]), x,
                                                      select_cols = c(self$data$x_cols, self$data$other_treat_cols),
                                                      target = this_z))
        tuning_instance_m = lapply(task_m, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_m,
                                          resampling = CV_tune,
                                          measure = measure_m,
                                          search_space = param_set$ml_m,
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
  
   ml_nuisance_tuning_partialXZ = function(smpls, param_set, tune_settings, tune_on_folds, ...){
    if (!tune_on_folds){
      data_tune_list = list(self$data$data_model)
    } else {
      data_tune_list = lapply(smpls$train_ids, function(x) extract_training_data(self$data$data_model, x))
    }
    if (any(class(tune_settings$rsmp_tune) == "Resampling")) {
      CV_tune = tune_settings$rsmp_tune
    } else {
      CV_tune = rsmp(tune_settings$rsmp_tune, folds = tune_settings$n_folds_tune)
    }
    if (any(class(tune_settings$measure$ml_g) == "Measure")) {
      measure_g = tune_settings$measure$ml_g
    } else {
        if (is.null(tune_settings$measure$ml_g)){
          measure_g = default_measures("regr")[[1]]
        } else {
          measure_g = msr(tune_settings$measure$ml_g)
      }
    }
    if (any(class(tune_settings$measure$ml_m) == "Measure")) {
      measure_m = tune_settings$measure$ml_m
    } else {
        if (is.null(tune_settings$measure$ml_m)){
          measure_m = default_measures("regr")[[1]]
        } else {
          measure_m = msr(tune_settings$measure$ml_m)
      }
    }
    if (any(class(tune_settings$measure$ml_r) == "Measure")) {
      measure_r = tune_settings$measure$ml_r
    } else {
        if (is.null(tune_settings$measure$ml_r)){
          measure_r = default_measures("regr")[[1]]
        } else {
          measure_r = msr(tune_settings$measure$ml_r)
      }
    }
    terminator = tune_settings$terminator
    tuner = tnr(tune_settings$algorithm, resolution = tune_settings$resolution)
    
    task_g = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_g_", self$data$y_col), x,
                                                  select_cols = c(self$data$x_cols), 
                                                  target = self$data$y_col))
    ml_g = initiate_regr_learner(self$learner$ml_g, params = list())
    tuning_instance_g = lapply(task_g, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_g,
                                          resampling = CV_tune,
                                          measure = measure_g,
                                          search_space = param_set$ml_g,
                                          terminator = terminator))
    tuning_result_g = lapply(tuning_instance_g, function(x) tune_instance(tuner, x))
    
    ml_m = initiate_regr_learner(self$learner$ml_m, params = list())
    task_m = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_m_", self$data$treat_col), x,
                                                  select_cols = c(self$data$x_cols, self$data$z_cols), 
                                                  target = self$data$treat_col))
    tuning_instance_m = lapply(task_m, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_m,
                                          resampling = CV_tune,
                                          measure = measure_m,
                                          search_space = param_set$ml_m,
                                          terminator = terminator))
    tuning_result_m = lapply(tuning_instance_m, function(x) tune_instance(tuner, x))
    
    m_params = extract_tuned_params(tuning_result_m)
    ml_m = lapply(m_params, function(x) initiate_regr_learner(self$learner$ml_m, params = x))
    
    resampling_m_on_train = lapply(task_m, function(x) rsmp("insample")$instantiate(x))
    r_m_on_train = lapply(1:length(data_tune_list), function(x) 
                                        resample(task_m[[x]], ml_m[[x]], resampling_m_on_train[[x]], store_models = TRUE))
    m_hat_on_train = lapply(r_m_on_train, function(x) extract_response_prediction(x, return_train_preds = TRUE))
    m_hat_on_train = lapply(m_hat_on_train, function(x) x[[1]]$response)
      
    data_aux_list = lapply(1:length(data_tune_list), function(x) 
                                              data.table(data_tune_list[[x]], "m_hat_on_train" = m_hat_on_train[[x]]))
    task_r = lapply(data_aux_list, function(x) initiate_regr_task("nuis_r_m_hat_on_train", x,
                                                    select_cols = c(self$data$x_cols, self$data$other_treat_cols),
                                                    target = "m_hat_on_train"))
    ml_r = initiate_regr_learner(self$learner$ml_r, params = list())
    tuning_instance_r = lapply(task_r, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_r,
                                          resampling = CV_tune,
                                          measure = measure_r,
                                          search_space = param_set$ml_r,
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
  
  ml_nuisance_tuning_partialZ = function(smpls, param_set, tune_settings, tune_on_folds, ...){
    if (!tune_on_folds){
      data_tune_list = list(self$data$data_model)
    } else {
      data_tune_list = lapply(smpls$train_ids, function(x) extract_training_data(self$data$data_model, x))
    }

    if (any(class(tune_settings$rsmp_tune) == "Resampling")) {
      CV_tune = tune_settings$rsmp_tune
    } else {
      CV_tune = rsmp(tune_settings$rsmp_tune, folds = tune_settings$n_folds_tune)
    }
  
    if (any(class(tune_settings$measure_r) == "Measure")) {
      measure_r = tune_settings$measure_r
    } else {
        if (is.null(tune_settings$measure_r)){
          measure_r = default_measures("regr")[[1]]
        } else {
          measure_r = msr(tune_settings$measure_r)
      }
    }
    
    terminator = tune_settings$terminator
    tuner = tnr(tune_settings$algorithm, resolution = tune_settings$resolution)
    
    task_r = lapply(data_tune_list, function(x) initiate_regr_task(paste0("nuis_r_", self$data$treat_col), x,
                                                   select_cols = c(self$data$x_cols, self$data$other_treat_cols, 
                                                                   self$data$z_cols), 
                                                   target = self$data$treat_col))
    ml_r = initiate_regr_learner(self$learner$ml_r, params = list())
    tuning_instance_r = lapply(task_r, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_r,
                                          resampling = CV_tune,
                                          measure = measure_r,
                                          search_space = param_set$ml_r,
                                          terminator = terminator))
    
    tuning_result_r = lapply(tuning_instance_r, function(x) tune_instance(tuner, x))
    
    tuning_result = list(tuning_result = list(tuning_result_r = tuning_result_r),
                         params = list(r_params = extract_tuned_params(tuning_result_r)))
    
    return(tuning_result)
    
  },
  check_score = function(score){
    checkmate::assert(checkmate::check_character(score),
                      checkmate::check_class(score, "function"))
    if (is.character(score)) {
      valid_score = c("partialling out")
      if (! (score %in% valid_score)) {
        checkmate::assertChoice(score, valid_score)
      }
    }
    return(score)
  }, 
  check_data = function(obj_dml_data) {
    return()
  }
)
)

# Initializer for partialX
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
