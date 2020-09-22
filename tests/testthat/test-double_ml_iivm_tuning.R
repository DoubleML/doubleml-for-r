context("Unit tests for tuning of IIVM")

library("mlr3learners")
library("mlr3tuning")
library("paradox")
library('data.table')
library('mlr3')
lgr::get_logger("mlr3")$set_threshold("warn")

# settings for parameter provision
learner = c('rpart')

learner_list = list("mlmethod_p" = learner, "mlmethod_mu" = learner, "mlmethod_m" = learner)

tune_settings = list(n_folds_tune = 3,
                      n_rep_tune = 1, 
                      rsmp_tune = "cv", 
                      measure_p = "classif.ce",
                      measure_mu = "regr.mse", 
                      measure_m = "classif.ce",
                      terminator = mlr3tuning::trm("evals", n_evals = 5), 
                      algorithm = "grid_search",
                      tuning_instance_p = NULL, 
                      tuning_instance_mu = NULL,
                      tuning_instance_m = NULL,
                      tuner = "grid_search",
                      resolution = 5)

test_cases = expand.grid(learner = learner,
                         dml_procedure = c('dml1', 'dml2'),
                         se_reestimate = c(FALSE),
                         score = c('LATE'),
                         i_setting = 1:(length(data_iivm)),
                         n_rep_cross_fit = c(1, 3),
                         stringsAsFactors = FALSE)
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for tuning of IIVM:",
                                   .cases = test_cases, {
  
  learner_pars <- get_default_mlmethod_iivm(learner)
  n_rep_boot = 498
  n_folds = 2
  
  set.seed(i_setting)
  Xnames = names(data_iivm[[i_setting]])[names(data_iivm[[i_setting]]) %in% c("y", "d", "z") == FALSE]
  data_ml = double_ml_data_from_data_frame(data_iivm[[i_setting]], y_col = "y", 
                              d_cols = "d", x_cols = Xnames, z_col = "z")

  double_mliivm_obj_tuned = DoubleMLIIVM$new(data_ml, 
                                     n_folds = n_folds,
                                     ml_p = learner_pars$mlmethod$mlmethod_p,
                                     ml_mu = learner_pars$mlmethod$mlmethod_mu,
                                     ml_m = learner_pars$mlmethod$mlmethod_m,
                                     dml_procedure = dml_procedure, 
                                     score = score, 
                                     n_rep_cross_fit = n_rep_cross_fit)
  
  
  param_grid = list(param_set_p = ParamSet$new(list(
                                          ParamDbl$new("cp", lower = 0.01, upper = 0.02),
                                          ParamInt$new("minsplit", lower = 1, upper = 2))),
                    param_set_mu = ParamSet$new(list(
                                          ParamDbl$new("cp", lower = 0.01, upper = 0.02),
                                          ParamInt$new("minsplit", lower = 1, upper = 2))), 
                    param_set_m = ParamSet$new(list(
                                          ParamDbl$new("cp", lower = 0.01, upper = 0.02),
                                          ParamInt$new("minsplit", lower = 1, upper = 2)))) 
  
  double_mliivm_obj_tuned$tune(param_set = param_grid, tune_on_folds = tune_on_folds)
  double_mliivm_obj_tuned$fit()
  
  double_mliivm_obj_tuned$param_set$param_set_p = tune_ps
  double_mliivm_obj_tuned$param_set$param_set_mu = tune_ps
  double_mliivm_obj_tuned$param_set$param_set_m = tune_ps
  double_mliivm_obj_tuned$tune_settings = tune_settings

  double_mliivm_obj_tuned$tune()
  
  double_mliivm_obj_tuned$fit()
  
  theta_obj_tuned <- double_mliivm_obj_tuned$coef
  se_obj_tuned <- double_mliivm_obj_tuned$se

  # bootstrap
  # double_mlirm_obj_tuned$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj_tuned = double_mlirm_obj_tuned$boot_coef
  
  
  # restrictions to test
    # Functional (tbd) vs OOP implementation (handling randomness in param selection!?)
  expect_is(theta_obj_tuned, "numeric")
  expect_is(se_obj_tuned, "numeric")
}
)

