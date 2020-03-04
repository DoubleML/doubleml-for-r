context("Unit tests for PLR")

library("mlr3learners")
library("mlr3tuning")
library("paradox")
library('data.table')
library('mlr3')

lgr::get_logger("mlr3")$set_threshold("warn")

# settings for parameter provision
learner = c('regr.rpart')

learner_list = list("mlmethod_m" = learner, "mlmethod_g" = learner)

tune_settings = list(n_folds_tune = 3,
                      n_rep_tune = 1, 
                      rsmp_tune = "cv", 
                      measure_g = "regr.mse", 
                      measure_m = "regr.mse",
                      terminator = mlr3tuning::term("evals", n_evals = 5), 
                      algorithm = "grid_search",
                      tuning_instance_g = NULL, 
                      tuning_instance_m = NULL,
                      tuner = "grid_search",
                      resolution = 5)

test_cases = expand.grid(learner_list = learner_list,
                         dml_procedure = c('dml1', 'dml2'),
                         se_reestimate = c(FALSE),
                         inf_model = c('IV-type', 'DML2018'),
                         n_rep_cross_fit = c(1, 3),
                         i_setting = 1:(length(data_plr)),
                         stringsAsFactors = FALSE)

### TBD: Set up test cases for different ways to provide tuning settings 
###      and different settings (1-split...)

# test all params for all learners ?

test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

skip('Skip tests for tuning')
patrick::with_parameters_test_that("Unit tests for tuning of PLR:",
                                   .cases = test_cases, {
  
  n_rep_boot = 498    
  n_folds = 2      
  
  # TBD: Functional Test Case
  
  # set.seed(i_setting)
  # n_folds = 5
  # plr_hat <- DML(data_plr_multi[[i_setting]], y = "y", d = c('d1', 'd2', 'd3'),
  #                model = "plr",
  #                k = n_folds, S = 1,
  #                mlmethod = learner_pars_for_DML$mlmethod,
  #                params = learner_pars_for_DML$params,
  #                dml_procedure = dml_procedure, inf_model = inf_model,
  #                se_type = inf_model,
  #                bootstrap = "normal", nRep = n_rep_boot)
  # theta <- coef(plr_hat)
  # se <- plr_hat$se
  
  set.seed(i_setting)
  double_mlplr_obj_tuned = DoubleMLPLR$new(n_folds = n_folds,
                                     ml_learners = learner_list,
                                     dml_procedure = dml_procedure, 
                                     se_reestimate = se_reestimate,
                                     inf_model = inf_model, 
                                     n_rep_cross_fit = n_rep_cross_fit)
  
  tune_ps = ParamSet$new(list(
                          ParamDbl$new("cp", lower = 0.001, upper = 0.1),
                          ParamInt$new("minsplit", lower = 1, upper = 10)))
  
  double_mlplr_obj_tuned$param_set$param_set_g = tune_ps
  double_mlplr_obj_tuned$param_set$param_set_m = tune_ps
  
  double_mlplr_obj_tuned$tune_settings = tune_settings

  double_mlplr_obj_tuned$tune(data_plr_multi[[i_setting]], y = "y", d = c('d1', 'd2'))
  
  double_mlplr_obj_tuned$fit(data_plr_multi[[i_setting]], y = "y", d = c('d1', 'd2'))
  
  theta_obj_tuned <- double_mlplr_obj_tuned$coef
  se_obj_tuned <- double_mlplr_obj_tuned$se
  
  # bootstrap
  # double_mlplr_obj_tuned$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj_tuned = double_mlplr_obj_tuned$boot_coef
  
  
  # restrictions to test
    # Functional (tbd) vs OOP implementation (handling randomness in param selection!?)
  
  }
)

