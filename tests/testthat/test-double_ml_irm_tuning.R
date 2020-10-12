context("Unit tests for tuning of IRM")

library("mlr3learners")
library("mlr3tuning")
library("paradox")
library('data.table')
library('mlr3')
lgr::get_logger("mlr3")$set_threshold("warn")

# settings for parameter provision
learner = c('rpart')

learner_list = list("mlmethod_m" = learner, "mlmethod_g" = learner)

tune_settings = list(n_folds_tune = 3,
                      n_rep_tune = 1, 
                      rsmp_tune = "cv", 
                      # measure_g = "regr.mse", 
                      # measure_m = "classif.ce",
                      terminator = mlr3tuning::trm("evals", n_evals = 5), 
                      algorithm = "grid_search",
                      tuning_instance_g = NULL, 
                      tuning_instance_m = NULL,
                      tuner = "grid_search",
                      resolution = 5)

test_cases = expand.grid(learner = learner,
                         dml_procedure = c('dml1', 'dml2'),
                         score = c('ATE', 'ATTE'),
                         tune_on_folds = c(FALSE, TRUE),
                         se_reestimate = c(FALSE),
                         i_setting = 1:(length(data_irm)),
                         n_rep = c(1, 3),
                         stringsAsFactors = FALSE)

test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

# skip('Skip tests for tuning')
patrick::with_parameters_test_that("Unit tests for tuning of PLR:",
                                   .cases = test_cases, {
  
  n_rep_boot = 498    
  n_folds = 2      
  
  # TBD: Functional Test Case
  
  # set.seed(i_setting)

  set.seed(i_setting)
  learner_pars <- get_default_mlmethod_irm(learner)
  
  Xnames = names(data_irm[[i_setting]])[names(data_irm[[i_setting]]) %in% c("y", "d", "z") == FALSE]
  data_ml = double_ml_data_from_data_frame(data_irm[[i_setting]], y_col = "y", 
                              d_cols = "d", x_cols = Xnames)

  double_mlirm_obj_tuned = DoubleMLIRM$new(data_ml, 
                                     n_folds = n_folds,
                                     ml_g = learner_pars$mlmethod$mlmethod_g,
                                     ml_m = learner_pars$mlmethod$mlmethod_m,
                                     dml_procedure = dml_procedure,
                                     score = score)
  
  param_grid = list(param_set_g = ParamSet$new(list(
                                          ParamDbl$new("cp", lower = 0.01, upper = 0.02),
                                          ParamInt$new("minsplit", lower = 1, upper = 2))),
                    param_set_m = ParamSet$new(list(
                                          ParamDbl$new("cp", lower = 0.01, upper = 0.02),
                                          ParamInt$new("minsplit", lower = 1, upper = 2))))
                  
  double_mlirm_obj_tuned$tune(param_set = param_grid, tune_on_folds = tune_on_folds)
  double_mlirm_obj_tuned$fit()
  
  theta_obj_tuned <- double_mlirm_obj_tuned$coef
  se_obj_tuned <- double_mlirm_obj_tuned$se
  
  # bootstrap
  # double_mlirm_obj_tuned$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj_tuned = double_mlirm_obj_tuned$boot_coef
  
  
  # restrictions to test
    # Functional (tbd) vs OOP implementation (handling randomness in param selection!?)
  expect_is(theta_obj_tuned, "numeric")
  expect_is(se_obj_tuned, "numeric")
  }
)