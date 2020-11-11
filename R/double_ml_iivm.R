#' @title Double machine learning for interactive IV regression models
#' 
#' @description
#' Double machine learning for interactive IV regression models. 
#' 
#' @format [R6::R6Class] object inheriting from [DoubleML].
#' 
#' @details 
#' Interactive IV regression (IIVM) models take the form 
#' 
#' \eqn{Y = g_0(D,X) + \zeta},
#' 
#' \eqn{Z = m_0(X) + V}, 
#' 
#' with \eqn{\mathbb{E}[\zeta|X,Z]=0} and \eqn{\mathbb{E}[V|X] = 0}. \eqn{Y} is the outcome variable, \eqn{D \in \{0,1\}} is the binary treatment variable and \eqn{Z \in \{0,1\}} is a binary instrumental variable. Consider the functions \eqn{g_0}, \eqn{r_0} and \eqn{m_0}, where \eqn{g_0} maps the support of \eqn{(Z,X)} to \eqn{\mathbb{R}} and \eqn{r_0} and \eqn{m_0}, respectively, map the support of \eqn{(Z,X)} and \eqn{X} to \eqn{(\epsilon, 1-\epsilon)} for some \eqn{\epsilon \in (1, 1/2)}, such that 
#' 
#' \eqn{Y = g_0(D,X) + \zeta,}
#'  
#' \eqn{D = r_0(D,X) + U,} 
#' 
#' \eqn{Z = m_0(X) + V,} 
#' 
#' with \eqn{\mathbb{E}[\zeta|Z,X]=0}, \eqn{\mathbb{E}[U|Z,X]=0} and \eqn{\mathbb{E}[V|X]=0}. The target parameter of interest in this model is the local average treatment effect (LATE), 
#' 
#' \eqn{\theta_0 = \frac{\mathbb{E}[g_0(1,X)] - \mathbb{E}[g_0(0,X)]}{\mathbb{E}[r(1,X)] - \mathbb{E}[r(0,X)]}.}
#' 
#' 
#' @usage NULL
#' 
#' @examples
#' library(DoubleML)
#' library(mlr3)
#' library(mlr3learners)
#' library(data.table)
#' set.seed(2)
#' ml_g = lrn("regr.ranger", num.trees = 10, max.depth = 2)
#' ml_m = lrn("classif.ranger", num.trees = 10, max.depth = 2)
#' ml_r = ml_m$clone() 
#' obj_dml_data = make_iivm_data(theta = 1)
#' dml_iivm_obj = DoubleMLIIVM$new(obj_dml_data, ml_g, ml_m, ml_r)
#' dml_iivm_obj$fit()
#' dml_iivm_obj$summary()
#' @export
DoubleMLIIVM <-R6:: R6Class("DoubleMLIIVM", inherit = DoubleML, public = list(
  #' @field subgroups (named `list(2)`) \cr
  #' Named `list(2)` with options to adapt to cases with and without the subgroups of always-takers and never-takes. The entry `always_takers`(`logical(1)`) speficies whether there are always takers in the sample. The entry `never_takers` (`logical(1)`) speficies whether there are never takers in the sample.
  subgroups = NULL, 
  
  #' @field trimming_rule (`character(1)`) \cr
  #' A `character(1)` specifying the trimming approach. 
  trimming_rule = NULL, 
  
  #' @field trimming_threshold (`numeric(1)`) \cr
  #' The threshold used for timming.
  trimming_threshold = NULL,
  
   #' @description 
  #' Creates a new instance of this R6 class. 
  #' 
  #' @param data (`DoubleMLData`) \cr
  #' The `DoubleMLData` object providing the data and specifying the variables of the causal model.
  #' 
  #' @param ml_g (`character(1)`) \cr
  #' A `character(1)` specifying the name of a [mlr3 regression learner][mlr3::LearnerRegr] that is available in [mlr3](https://mlr3.mlr-org.com/index.html) or its extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or [mlr3extralearners](https://mlr3extralearners.mlr-org.com/), for example `"regr.cv_glmnet"`. \cr
  #' `ml_g` refers to the nuisance function \eqn{g_0(Z,X) = \mathbb{E}[Y|X,Z]}.
  #' 
  #' @param ml_m (`character(1)`) \cr
  #' A `character(1)` specifying the name of a [mlr3 classification learner][mlr3::LearnerClassif] that is available in [mlr3](https://mlr3.mlr-org.com/index.html) or its extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or [mlr3extralearners](https://mlr3extralearners.mlr-org.com/), for example `"classif.cv_glmnet"`. \cr
  #' `ml_m` refers to the nuisance function \eqn{m_0(X) = \mathbb{E}[Z|X]}.
  #' 
  #' @param ml_r (`character(1)`) \cr
  #' A `character(1)` specifying the name of a [mlr3 classification learner][mlr3::LearnerClassif] that is available in [mlr3](https://mlr3.mlr-org.com/index.html) or its extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or [mlr3extralearners](https://mlr3extralearners.mlr-org.com/), for example `"classif.cv_glmnet"`. \cr
  #' `ml_r` refers to the nuisance function \eqn{r_0(Z,X) = \mathbb{E}[D|X,Z]}.
  #' 
  #' @param n_folds (`integer(1)`)\cr
  #' Number of folds. Default is `5`. 
  #' 
  #' @param n_rep (`integer(1)`) \cr
  #' Number of repetitions for the sample splitting. Default is `1`. 
  #' 
  #' @param score (`character(1)`, `function()`) \cr
  #' A `character(1)` (`"LATE"` is the only choice) specifying the score function. 
  #' If a `function()` is provided, it must be of the form 
  #' `function(y, z, d, g0_hat, g1_hat, m_hat, r0_hat, r1_hat, smpls)` and the returned output 
  #' must be a named `list()` with elements `psi_a` and `psi_b`. Default is `"LATE"`. 
  #' 
  #' @param subgroups (named `list(2)`) \cr
  #' Named `list(2)` with options to adapt to cases with and without the subgroups of always-takers and never-takes. The entry `always_takers`(`logical(1)`) speficies whether there are always takers in the sample. The entry `never_takers` (`logical(1)`) speficies whether there are never takers in the sample. Default is `list(always_takers = TRUE, never_takers = TRUE)`.
  #' 
  #' @param trimming_rule (`character(1)`) \cr
  #' A `character(1)` (`"truncate"` is the only choice) specifying the trimming approach. Default is `"truncate"`. 
  #' @param trimming_threshold (`numeric(1)`) \cr
  #' The threshold used for timming. Default is `1e-12`. 
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
                        n_folds = 5,
                        n_rep = 1, 
                        score = "LATE", 
                        subgroups = list(always_takers = TRUE, 
                                         never_takers = TRUE),
                        dml_procedure = "dml2", 
                        trimming_rule = "truncate", 
                        trimming_threshold = 1e-12,
                        draw_sample_splitting = TRUE,
                        apply_cross_fitting = TRUE) {
    
    super$initialize_double_ml(data, 
                               n_folds,
                               n_rep,
                               score, 
                               dml_procedure, 
                               draw_sample_splitting, 
                               apply_cross_fitting)
     self$learner = list("ml_g" = ml_g,
                         "ml_m" = ml_m, 
                         "ml_r" = ml_r) 
     private$initialize_ml_nuisance_params()
     
     self$subgroups = subgroups
     self$trimming_rule = trimming_rule
     self$trimming_threshold = trimming_threshold
  }
  ),
private = list(
  n_nuisance = 3,
  initialize_ml_nuisance_params = function() {
    nuisance = vector("list", self$data$n_treat)
    names(nuisance) = self$data$d_cols
    self$params = list("ml_g0" = nuisance, 
                       "ml_g1" = nuisance, 
                       "ml_m" = nuisance, 
                       "ml_r0" = nuisance, 
                       "ml_r1" = nuisance)
    invisible(self)
  },
  ml_nuisance_and_score_elements = function(smpls, ...) {
    # nuisance m
    task_m <- initiate_classif_task(paste0("nuis_p_", self$data$z_cols), self$data$data_model,
                                    select_cols = c(self$data$x_cols, self$data$other_treat_cols),
                                    target = self$data$z_cols)
    # nuisance m
    task_g = initiate_regr_task(paste0("nuis_mu_", self$data$y_col), self$data$data_model,
                                select_cols = c(self$data$x_cols, self$data$other_treat_cols),
                                target = self$data$y_col)
    # task_r
    task_r = initiate_classif_task(paste0("nuis_m_", self$data$treat_col), self$data$data_model,
                                   select_cols = c(self$data$x_cols, self$data$other_treat_cols),
                                   target = self$data$treat_col)
    if (!private$fold_specific_params) {
        for (i_nuis in self$params_names()){
          if (is.null(self$get_params(i_nuis))) {
            message(paste("Parameter of learner for nuisance part", i_nuis, "are not tuned, results might not be valid!"))
          }
        }
      # get conditional samples (conditioned on z = 0 or z = 1)
      cond_smpls <- private$get_cond_smpls(smpls, self$data$data_model$z)
      
      # nuisance m
      ml_m = initiate_prob_learner(self$learner$ml_m, self$get_params("ml_m"))                  
      resampling_m = mlr3::rsmp("custom")$instantiate(task_m,
                                                        smpls$train_ids,
                                                        smpls$test_ids)
      r_m = mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
      m_hat = extract_prob_prediction(r_m)$prob.1
      
      # nuisance g
      ml_g0 = initiate_learner(self$learner$ml_g, self$get_params("ml_g0"))
      resampling_g0 <- mlr3::rsmp("custom")$instantiate(task_g,
                                                         cond_smpls$train_ids_0,
                                                         smpls$test_ids)
      r_g0 <- mlr3::resample(task_g, ml_g0, resampling_g0, store_models = TRUE)
      g0_hat = extract_prediction(r_g0)$response
      
      ml_g1 = initiate_learner(self$learner$ml_g, self$get_params("ml_g1"))
      resampling_g1 <- mlr3::rsmp("custom")$instantiate(task_g,
                                                         cond_smpls$train_ids_1,
                                                         smpls$test_ids)
      r_g1 <- mlr3::resample(task_g, ml_g1, resampling_g1, store_models = TRUE)
      g1_hat = extract_prediction(r_g1)$response
    
      # nuisance r
      if (self$subgroups$always_takers == FALSE & self$subgroups$never_takers == FALSE) {
        message("If there are no always-takers and no never-takers, ATE is estimated")
      }
      
      if (self$subgroups$always_takers == FALSE){
        r0_hat <- rep(0, nrow(self$data$data_model))
      } else if (self$subgroups$always_takers == TRUE){
        ml_r0 = initiate_prob_learner(self$learner$ml_r, self$get_params("ml_r0"))
        resampling_r0 <- mlr3::rsmp("custom")$instantiate(task_r,
                                                          cond_smpls$train_ids_0,
                                                          smpls$test_ids)
        r_r0 <- mlr3::resample(task_r, ml_r0, resampling_r0, store_models = TRUE)
        r0_hat = extract_prob_prediction(r_r0)$prob.1
      }
      
      if (self$subgroups$never_takers == FALSE){
        r1_hat <- rep(1, nrow(self$data$data_model))
      } else if (self$subgroups$never_takers == TRUE){
        ml_r1 = initiate_prob_learner(self$learner$ml_r, self$get_params("ml_r1"))
        resampling_r1 <- mlr3::rsmp("custom")$instantiate(task_r,
                                                           cond_smpls$train_ids_1,
                                                           smpls$test_ids)
        r_r1 <- mlr3::resample(task_r, ml_r1, resampling_r1, store_models = TRUE)
        r1_hat = extract_prob_prediction(r_r1)$prob.1
      }
    } else {
      # get conditional samples (conditioned on Z = 0 or Z = 1)
      cond_smpls <- private$get_cond_smpls(smpls, self$data$data_model$z)
      
      # nuisance m
      ml_m = lapply(self$get_params("ml_m"), function(x) 
                                                  initiate_prob_learner(self$learner$ml_m, x))
      resampling_m = initiate_resampling(task_m, smpls$train_ids, smpls$test_ids)
      r_m = resample_dml(task_m, ml_m, resampling_m, store_models = TRUE)
      m_hat = lapply(r_m, extract_prob_prediction)
      m_hat = rearrange_prob_prediction(m_hat, smpls$test_ids)
      
      # nuisance g
      ml_g0 = lapply(self$get_params("ml_g0"), function(x) 
                                                    initiate_learner(self$learner$ml_g, x))
      resampling_g0 <- initiate_resampling(task_g, cond_smpls$train_ids_0, smpls$test_ids)
      r_g0 <- resample_dml(task_g, ml_g0, resampling_g0, store_models = TRUE)
      g0_hat <- lapply(r_g0, extract_prediction)
      g0_hat <- rearrange_prediction(g0_hat, smpls$test_ids)

      ml_g1 = lapply(self$get_params("ml_g1"), function(x) 
                                                  initiate_learner(self$learner$ml_g, x))
      resampling_g1 <- initiate_resampling(task_g, cond_smpls$train_ids_1, smpls$test_ids)
      r_g1 <- resample_dml(task_g, ml_g1, resampling_g1, store_models = TRUE)
      g1_hat <- lapply(r_g1, extract_prediction)
      g1_hat <- rearrange_prediction(g1_hat, smpls$test_ids)             
      
      if (self$subgroups$always_takers == FALSE & self$subgroups$never_takers == FALSE) {
        message("If there are no always-takers and no never-takers, ATE is estimated")
      }
      if (self$subgroups$always_takers == FALSE){
        r0_hat <- rep(0, nrow(self$data$data_model))
      } else if (self$subgroups$always_takers == TRUE){
        ml_r0 = lapply(self$get_params("ml_r0"), function(x) 
                                                  initiate_prob_learner(self$learner$ml_r, x))
        resampling_r0 <- initiate_resampling(task_r, cond_smpls$train_ids_0, smpls$test_ids)
        r_r0 <- resample_dml(task_r, ml_r0, resampling_r0, store_models = TRUE)
        r0_hat <- lapply(r_r0, extract_prob_prediction)
        r0_hat <- rearrange_prob_prediction(r0_hat, smpls$test_ids)
      }
      if (self$subgroups$never_takers == FALSE){
        r1_hat <- rep(1, nrow(self$data$data_model))
      } else if (self$subgroups$never_takers == TRUE){
        ml_r1 = lapply(self$get_params("ml_r1"), function(x) 
                                                  initiate_prob_learner(self$learner$ml_r, x))
        resampling_r1 <- initiate_resampling(task_r, cond_smpls$train_ids_1, smpls$test_ids)
        r_r1 <- resample_dml(task_r, ml_r1, resampling_r1, store_models = TRUE)
        r1_hat <- lapply(r_r1, extract_prob_prediction)
        r1_hat <- rearrange_prob_prediction(r1_hat, smpls$test_ids)
      }
    }
    
    # compute residuals
    z <- self$data$data_model[, self$data$z_cols, with = FALSE]
    d <- self$data$data_model[, self$data$treat_col, with = FALSE]
    y <- self$data$data_model[, self$data$y_col, with = FALSE]
    u0_hat = y - g0_hat
    u1_hat = y - g1_hat
    w0_hat = d - r0_hat
    w1_hat = d - r1_hat
    
    if (self$trimming_rule == "truncate" & self$trimming_threshold > 0) {
      m_hat[m_hat < self$trimming_threshold] = self$trimming_threshold
      m_hat[m_hat > 1 - self$trimming_threshold] = 1 - self$trimming_threshold
    }
    score = self$score
    private$check_score(score)
    
    if (is.character(self$score)) {
      if (self$score == 'LATE') {
        psi_b = g1_hat - g0_hat + z*(u1_hat)/m_hat - (1-z)*u0_hat/(1-m_hat)
        psi_a = -1 * (r1_hat - r0_hat + z*(w1_hat)/m_hat - (1-z)*w0_hat/(1-m_hat))
      }
      psis = list(psi_a = psi_a, psi_b = psi_b)
    } else if (is.function(self$score)) {
      psis = self$score(y, z, d, g0_hat, g1_hat1, m_hat, r0_hat, r1_hat, smpls)
    }
    return(psis)
  },
 ml_nuisance_tuning = function(smpls, param_set, tune_settings, tune_on_folds, ...){
   checkmate::check_class(param_set$ml_g, "ParamSet")    
   checkmate::check_class(param_set$ml_m, "ParamSet")
   checkmate::check_class(param_set$ml_r, "ParamSet")

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
   if (any(class(tune_settings$measure$ml_m) == "Measure")) {
        measure_m = tune_settings$measure$ml_m
      } else {
          if (is.null(tune_settings$measure$ml_m)){
            measure_m = mlr3::default_measures("classif")[[1]]
          } else {
            measure_m = mlr3::msr(tune_settings$measure$ml_m)
        }
   }
   if (any(class(tune_settings$measure$ml_g) == "Measure")) {
        measure_g = tune_settings$measure$ml_g
      } else {
          if (is.null(tune_settings$measure$ml_g)){
            measure_g = mlr3::default_measures("regr")[[1]]
          } else {
            measure_g = mlr3::msr(tune_settings$measure$ml_g)
        }
   }
   if (any(class(tune_settings$measure$ml_r) == "Measure")) {
        measure_r = tune_settings$measure$ml_r
      } else {
          if (is.null(tune_settings$measure$ml_r)){
            measure_r= mlr3::default_measures("classif")[[1]]
          } else {
            measure_r = mlr3::msr(tune_settings$measure$ml_r)
        }
      }
   
   terminator = tune_settings$terminator
   tuner = mlr3tuning::tnr(tune_settings$algorithm, resolution = tune_settings$resolution)
    
   indx_g0 = lapply(data_tune_list, function(x) x[self$data$z_cols] == 0)
   indx_g1 = lapply(data_tune_list, function(x) x[self$data$z_cols] == 1)
   data_tune_list_z0 = lapply(1:length(data_tune_list), function(x) data_tune_list[[x]][indx_g0[[x]], ] )
   data_tune_list_z1 = lapply(1:length(data_tune_list), function(x) data_tune_list[[x]][indx_g1[[x]], ] )

   task_m = lapply(data_tune_list, function(x) initiate_classif_task(paste0("nuis_p_", self$data$z_cols), x,
                                                select_cols = c(self$data$x_cols, self$data$other_treat_cols),
                                                target = self$data$z_cols))
   ml_m = initiate_prob_learner(self$learner$ml_m, params = list())
   tuning_instance_m = lapply(task_m, function(x) TuningInstanceSingleCrit$new(task = x,
                                          learner = ml_m,
                                          resampling = CV_tune,
                                          measure = measure_m,
                                          search_space = param_set$ml_m,
                                          terminator = terminator))
   tuning_result_m = lapply(tuning_instance_m, function(x) tune_instance(tuner, x))
   
   task_g0 = lapply(data_tune_list_z0, function(x) initiate_regr_task(paste0("nuis_mu_", self$data$y_col), x,
                                                     select_cols = c(self$data$x_cols, self$data$other_treat_cols),
                                                     target = self$data$y_col))
   ml_g0 = initiate_learner(self$learner$ml_g, params = list())
   tuning_instance_g0 = lapply(task_g0, function(x) TuningInstanceSingleCrit$new(task = x,
                                         learner = ml_g0,
                                         resampling = CV_tune,
                                         measure = measure_g,
                                         search_space = param_set$ml_g,
                                         terminator = terminator))
   tuning_result_g0 = lapply(tuning_instance_g0, function(x) tune_instance(tuner, x))
   
   task_g1 = lapply(data_tune_list_z0, function(x) initiate_regr_task(paste0("nuis_mu_", self$data$y_col), x,
                                                     select_cols = c(self$data$x_cols, self$data$other_treat_cols),
                                                     target = self$data$y_col))
   ml_g1 = initiate_learner(self$learner$ml_g, params = list())
   tuning_instance_g1 = lapply(task_g1, function(x) TuningInstanceSingleCrit$new(task = x,
                                         learner = ml_g1,
                                         resampling = CV_tune,
                                         measure = measure_g,
                                         search_space = param_set$ml_g,
                                         terminator = terminator))
   tuning_result_g1 = lapply(tuning_instance_g1, function(x) tune_instance(tuner, x))
   
   if (self$subgroups$always_takers == TRUE){
     task_r0 = lapply(data_tune_list_z0, function(x) 
                                            initiate_classif_task(paste0("nuis_m_", self$data$treat_col), x,
                                              select_cols = c(self$data$x_cols, self$data$other_treat_cols),
                                              target = self$data$treat_col))
     ml_r0 = initiate_prob_learner(self$learner$ml_r, params = list())
     tuning_instance_r0 = lapply(task_r0, function(x) TuningInstanceSingleCrit$new(task = x,
                                         learner = ml_r0,
                                         resampling = CV_tune,
                                         measure = measure_r,
                                         search_space = param_set$ml_r,
                                         terminator = terminator))
     tuning_result_r0 = lapply(tuning_instance_r0, function(x) tune_instance(tuner, x))
    } else {
     tuning_result_r0 = NULL
    }
   
   if (self$subgroups$never_takers == TRUE){
     task_r1 = lapply(data_tune_list_z1, function(x) 
                                            initiate_classif_task(paste0("nuis_m_", self$data$treat_col), x,
                                              select_cols = c(self$data$x_cols, self$data$other_treat_cols),
                                              target = self$data$treat_col))
     ml_r1 = initiate_prob_learner(self$learner$ml_r, params = list())
     tuning_instance_r1 = lapply(task_r1, function(x) TuningInstanceSingleCrit$new(task = x,
                                         learner = ml_r1,
                                         resampling = CV_tune,
                                         measure = measure_r,
                                         search_space = param_set$ml_r,
                                         terminator = terminator))
     tuning_result_r1 = lapply(tuning_instance_r1, function(x) tune_instance(tuner, x))
   } else {
     tuning_result_r1 = NULL
   }
   tuning_result = list("ml_m" = list(tuning_result_m, params = extract_tuned_params(tuning_result_m)),
                        "ml_g0" = list(tuning_result_g0, params = extract_tuned_params(tuning_result_g0)),
                        "ml_g1" = list(tuning_result_g1, params = extract_tuned_params(tuning_result_g1)),
                        "ml_r0" = list(tuning_result_r0, params = extract_tuned_params(tuning_result_r0)), 
                        "ml_r1" = list(tuning_result_r1, params = extract_tuned_params(tuning_result_r1)))
   return(tuning_result)
  },
  get_cond_smpls = function(smpls, Z) {
    train_ids_0 <- lapply(1:length(smpls$train_ids), function(x) 
      smpls$train_ids[[x]][Z[smpls$train_ids[[x]]] == 0])
    train_ids_1 <-  lapply(1:length(smpls$test_ids), function(x) 
      smpls$train_ids[[x]][Z[smpls$train_ids[[x]]] == 1])
    return(list(train_ids_0=train_ids_0,
                train_ids_1=train_ids_1))
  },
  check_score = function(score){
    if (is.character(score)) {
      valid_score = c("LATE")
      if (! (score %in% valid_score)) {
        stop(paste("Invalid score", score, "\n valid score", list(valid_score)))
      }
    } else if (!is.function(score)) {
      stop("Score should be either a character or a function.")
    }
    return(score)
  }
)
)



