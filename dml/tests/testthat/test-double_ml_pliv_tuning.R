context("Unit tests for tuning for PLIV")

library("mlr3learners")
library("mlr3tuning")
library("paradox")
library('data.table')
library('mlr3')

lgr::get_logger("mlr3")$set_threshold("warn")

# settings for parameter provision
learner = c('regr.rpart')

learner_list = list("mlmethod_m" = learner, "mlmethod_g" = learner, "mlmethod_r" = learner)
  
tune_settings = list(n_folds_tune = 3,
                      n_rep_tune = 1, 
                      rsmp_tune = "cv", 
                      measure_g = "regr.mse", 
                      measure_m = "regr.mse",
                      measure_r = "regr.mse",
                      terminator = mlr3tuning::term("evals", n_evals = 5), 
                      algorithm = "grid_search",
                      tuning_instance_g = NULL, 
                      tuning_instance_m = NULL,
                      tuning_instance_r = NULL,
                      tuner = "grid_search",
                      resolution = 5)

test_cases = expand.grid(learner_list = learner,
                         dml_procedure = c('dml1', 'dml2'),
                         se_reestimate = c(FALSE),
                         inf_model = c('partialling-out'),
                         i_setting = 1:(length(data_pliv)),
                         n_rep_cross_fit = c(1, 3),
                         stringsAsFactors = FALSE)

test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")
skip('Skip tests for tuning')

patrick::with_parameters_test_that("Unit tests for tuning of PLIV",
                                   .cases = test_cases, {
                                                        
  
  # TBD: Functional Test Case

  set.seed(i_setting)
  n_folds = 5
  n_rep_boot = 498
  
  # set.seed(i_setting)
  # pliv_hat <- dml_plriv(data_pliv[[i_setting]], y = "y", d = "d", z = 'z',
  #                       k = n_folds, mlmethod = learner_list,
  #                       params = learner_pars$params,
  #                       dml_procedure = dml_procedure, inf_model = inf_model,
  #                       se_type = inf_model,
  #                       bootstrap = "normal",  nRep = n_rep_boot)
  # theta <- coef(pliv_hat)
  # se <- pliv_hat$se

  
  set.seed(i_setting)
  double_mlpliv_obj_tuned = DoubleMLPLIV$new(n_folds = n_folds,
                                     ml_learners = learner_list,
                                     dml_procedure = dml_procedure, 
                                     se_reestimate = se_reestimate,
                                     inf_model = inf_model,
                                     n_rep_cross_fit = n_rep_cross_fit)
  
  tune_ps = ParamSet$new(list(
                          ParamDbl$new("cp", lower = 0.001, upper = 0.1),
                          ParamInt$new("minsplit", lower = 1, upper = 10)))
  
  double_mlpliv_obj_tuned$param_set$param_set_g = tune_ps
  double_mlpliv_obj_tuned$param_set$param_set_m = tune_ps
  double_mlpliv_obj_tuned$param_set$param_set_r = tune_ps
  
  double_mlpliv_obj_tuned$tune_settings = tune_settings
  
  double_mlpliv_obj_tuned$tune(data_pliv[[i_setting]], y = "y", d = "d", z = "z")

  double_mlpliv_obj_tuned$fit(data_pliv[[i_setting]], y = "y", d = "d", z = "z")
  
  theta_obj_exact <- double_mlpliv_obj_tuned$coef
  se_obj_exact <- double_mlpliv_obj_tuned$se
  
  # bootstrap
  # double_mlplr_obj_exact$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj_exact = double_mlplr_obj_exact$boot_coef

  
  # pass empty set of parameters (NULL, use default values)   
  set.seed(i_setting)
  double_mlpliv_obj_null = DoubleMLPLIV$new(n_folds = n_folds,
                                     ml_learners = learner_list,
                                     params = NULL,
                                     dml_procedure = dml_procedure, 
                                     se_reestimate = se_reestimate,
                                     inf_model = inf_model,
                                     n_rep_cross_fit = n_rep_cross_fit)
  
  double_mlpliv_obj_null$fit(data_pliv[[i_setting]], y = "y", d = "d", z = "z")
  
  theta_obj_null <- double_mlpliv_obj_null$coef
  se_obj_null <- double_mlpliv_obj_null$se
  
  # bootstrap
  # double_mlplr_obj_null$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj_null = double_mlplr_obj_null$boot_coef
  
  }
)

