#' @title Double machine learning for partially linear regression models
#' 
#' @description
#' Double machine learning for partially linear regression models. 
#' 
#' @format [R6::R6Class] object inheriting from [DoubleML].
#' 
#' @family DoubleML
#' @details 
#' Partially linear regression (PLR) models take the form 
#' 
#' \eqn{Y = D\theta_0 + g_0(X) + \zeta,}
#' 
#' \eqn{D = m_0(X) + V,}
#' 
#' with \eqn{E[\zeta|D,X]=0} and \eqn{E[V|X] = 0}. \eqn{Y} is the outcome variable variable and \eqn{D} is the policy variable of interest. The high-dimensional vector \eqn{X = (X_1, \ldots, X_p)} consists of other confounding covariates, and \eqn{\zeta} and \eqn{V} are stochastic errors.  
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
#' ml_g = lrn("regr.ranger", num.trees = 10, max.depth = 2)
#' ml_m = ml_g$clone()
#' obj_dml_data = make_plr_CCDDHNR2018(alpha = 0.5)
#' dml_plr_obj = DoubleMLPLR$new(obj_dml_data, ml_g, ml_m)
#' dml_plr_obj$fit()
#' dml_plr_obj$summary()
#' }
#' 
#' \dontrun{
#' library(DoubleML)
#' library(mlr3)
#' library(mlr3learners)
#' library(mlr3tuning)
#' library(data.table)
#' set.seed(2)
#' ml_g = lrn("regr.rpart")
#' ml_m = ml_g$clone()
#' obj_dml_data = make_plr_CCDDHNR2018(alpha = 0.5)
#' dml_plr_obj = DoubleMLPLR$new(obj_dml_data, ml_g, ml_m)
#' 
#' param_grid = list("ml_g" = paradox::ParamSet$new(
#'                      list(paradox::ParamDbl$new("cp", lower = 0.01, upper = 0.02),
#'                           paradox::ParamInt$new("minsplit", lower = 1, upper = 2))),
#'                  "ml_m" = paradox::ParamSet$new(
#'                      list(paradox::ParamDbl$new("cp", lower = 0.01, upper = 0.02),
#'                           paradox::ParamInt$new("minsplit", lower = 1, upper = 2))))
#' 
#' # minimum requirements for tune_settings
#' tune_settings = list(terminator = mlr3tuning::trm("evals", n_evals = 5), 
#'                       algorithm = mlr3tuning::tnr("grid_search", resolution = 5))
#' dml_plr_obj$tune(param_set = param_grid, tune_settings = tune_settings)
#' dml_plr_obj$fit()
#' dml_plr_obj$summary()
#' }
#' @export
DoubleMLPLR = R6Class("DoubleMLPLR", inherit = DoubleML, public = list(
  #' @description 
  #' Creates a new instance of this R6 class. 
  #' 
  #' @param data (`DoubleMLData`) \cr
  #' The `DoubleMLData` object providing the data and specifying the variables of the causal model. 
  #' 
  #' @param ml_g ([`LearnerRegr`][mlr3::LearnerRegr], `character(1)`,) \cr
  #' An object of the class [mlr3 regression learner][mlr3::LearnerRegr] to pass a learner, possibly with specified parameters, for example `lrn("regr.cv_glmnet", s = "lambda.min")`. 
  #' Alternatively, a `character(1)` specifying the name of a [mlr3 regression learner][mlr3::LearnerRegr] that is available in [mlr3](https://mlr3.mlr-org.com/index.html) or its extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or [mlr3extralearners](https://mlr3extralearners.mlr-org.com/), for example `"regr.cv_glmnet"`. \cr
  #' `ml_g` refers to the nuisance function \eqn{g_0(X) = E[Y|X]}.
  #' 
  #' @param ml_m ([`LearnerRegr`][mlr3::LearnerRegr], [`LearnerClassif`][mlr3::LearnerClassif], `character(1)`,) \cr
  #' An object of the class [mlr3 regression learner][mlr3::LearnerRegr] to pass a learner, possibly with specified parameters, for example `lrn("regr.cv_glmnet", s = "lambda.min")`. For binary treatment variables, an object of the class [`LearnerClassif`][mlr3::LearnerClassif] can be passed, for example `lrn("classif.cv_glmnet", s = "lambda.min")`. Alternatively, a `character(1)` specifying the name of a [mlr3 regression learner][mlr3::LearnerRegr] that is available in [mlr3](https://mlr3.mlr-org.com/index.html) or its extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or [mlr3extralearners](https://mlr3extralearners.mlr-org.com/), for example `"regr.cv_glmnet"`. \cr
  #' `ml_m` refers to the nuisance function \eqn{m_0(X) = E[D|X]}.
 
  #' 
  #' @param n_folds (`integer(1)`)\cr
  #' Number of folds. Default is `5`. 
  #' 
  #' @param n_rep (`integer(1)`) \cr
  #' Number of repetitions for the sample splitting. Default is `1`. 
  #' 
  #' @param score (`character(1)`, `function()`) \cr
  #' A `character(1)` (`"partialling out"` or `IV-type`) or a `function()` specifying the score function. 
  #' If a `function()` is provided, it must be of the form `function(y, d, g_hat, m_hat, smpls)` and 
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
                        n_folds = 5,
                        n_rep = 1,
                        score = "partialling out", 
                        dml_procedure = "dml2",
                        draw_sample_splitting = TRUE,
                        apply_cross_fitting = TRUE)  {
    
    super$initialize_double_ml(data, 
                        n_folds,
                        n_rep,
                        score,
                        dml_procedure,
                        draw_sample_splitting,
                        apply_cross_fitting)
    
    private$learner_class = list("ml_g" = NULL,
                                 "ml_m" = NULL)
    ml_g = private$assert_learner(ml_g, "ml_g", Regr = TRUE, Classif = FALSE)
    ml_m = private$assert_learner(ml_m, "ml_m", Regr = TRUE, Classif = TRUE)

    self$learner = list("ml_g" = ml_g, 
                        "ml_m" = ml_m)
    private$initialize_ml_nuisance_params()
    
  } 
),
private = list(
  n_nuisance = 2,
  initialize_ml_nuisance_params = function() {
    
   nuisance = vector("list", self$data$n_treat) 
   names(nuisance) = self$data$d_cols
   self$params = list("ml_g" = nuisance, 
                      "ml_m" = nuisance)
   invisible(self)
  },
  
  ml_nuisance_and_score_elements = function(smpls, ...) {
    g_hat = dml_cv_predict(self$learner$ml_g, c(self$data$x_cols, self$data$other_treat_cols), self$data$y_col, 
                           self$data$data_model, nuisance_id = "nuis_g",  
                           smpls = smpls, est_params = self$get_params("ml_g"),
                           return_train_preds = FALSE, 
                           learner_class = private$learner_class$ml_g,
                           fold_specific_params = private$fold_specific_params)
    
    m_hat = dml_cv_predict(self$learner$ml_m, c(self$data$x_cols, self$data$other_treat_cols), self$data$treat_col,
                           self$data$data_model, nuisance_id = "nuis_m", 
                           smpls = smpls, est_params = self$get_params("ml_m"),
                           return_train_preds = FALSE,
                           learner_class = private$learner_class$ml_m,
                           fold_specific_params = private$fold_specific_params)
    
    d = self$data$data_model[[self$data$treat_col]]
    y = self$data$data_model[[self$data$y_col]]
    
    res = private$score_elements(y, d, g_hat, m_hat, smpls)
    res$preds = list("ml_g" = g_hat,
                     "ml_m" = m_hat)
    return(res)
  },
   score_elements = function(y, d, g_hat, m_hat, smpls) {
    v_hat = d - m_hat
    u_hat = y - g_hat
    v_hatd = v_hat * d # tbd: handle product of numeric and binary in data.table
    
    if (is.character(self$score)) {
      if (self$score == 'IV-type') {
        psi_a = -v_hatd
      } else if (self$score == 'partialling out') {
        psi_a = -v_hat * v_hat
      }
      psi_b = v_hat * u_hat
      psis = list(psi_a = psi_a, 
                  psi_b = psi_b)
    } else if (is.function(self$score)) {
      psis = self$score(y, d, g_hat, m_hat, smpls)
    }
    return(psis)
  },
  ml_nuisance_tuning = function(smpls, param_set, tune_settings, tune_on_folds, ...){
    
    if (!tune_on_folds){
      data_tune_list = list(self$data$data_model)
    } else {
      data_tune_list = lapply(smpls$train_ids, function(x) 
                                                extract_training_data(self$data$data_model, x))
    }
    
    tuning_result_g = dml_tune(self$learner$ml_g, c(self$data$x_cols, self$data$other_treat_cols),
                               self$data$y_col, data_tune_list, nuisance_id = "nuis_g", 
                               param_set$ml_g, tune_settings, tune_settings$measure$ml_g, 
                               private$learner_class$ml_g)
    
    tuning_result_m = dml_tune(self$learner$ml_m, c(self$data$x_cols, self$data$other_treat_cols),
                               self$data$treat_col, data_tune_list, nuisance_id = "nuis_m", 
                               param_set$ml_m, tune_settings, tune_settings$measure$ml_m, 
                               private$learner_class$ml_m)
    
    tuning_result = list("ml_g" = list(tuning_result_g, params = tuning_result_g$params),
                         "ml_m" = list(tuning_result_m, params = tuning_result_m$params))
    return(tuning_result)
  },
  check_score = function(score){
    checkmate::assert(checkmate::check_character(score),
                      checkmate::check_class(score, "function"))
    if (is.character(score)) {
      valid_score = c("IV-type", "partialling out")
      if (! (score %in% valid_score)) {
        checkmate::assertChoice(score, valid_score)
      }
    }
    return(score)
  },
  check_data = function(obj_dml_data) {
    if (!is.null(obj_dml_data$z_cols)) {
      stop(paste("Incompatible data.\n", paste(obj_dml_data$z_cols, collapse = ", "), 
                 "has been set as instrumental variable(s).\n", 
                  "To fit a partially linear IV regression model use DoubleMLPLIV instead of DoubleMLPLR."))
    }
    return()
  }
)
)

