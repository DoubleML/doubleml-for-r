context("Unit tests for PLR")

library("mlr3learners")
library("mlr3tuning")
library("paradox")
library('data.table')
library('mlr3')

lgr::get_logger("mlr3")$set_threshold("warn")

# settings for parameter provision
learner = c('regr.rpart')
learner_pars_empty <- get_default_mlmethod_plr(learner, default = TRUE)
learner_pars_once <- get_default_mlmethod_plr(learner, default = FALSE)

learner_list = list("mlmethod_m" = learner, "mlmethod_g" = learner)
  
test_cases = expand.grid(learner_list = learner_list,
                         dml_procedure = c('dml1', 'dml2'),
                         se_reestimate = c(FALSE),
                         inf_model = c('IV-type', 'DML2018'),
                         i_setting = 1:(length(data_plr)),
                         n_rep_cross_fit = c(2, 5),
                         stringsAsFactors = FALSE)
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

### TBD: Set up test cases for different ways to provide tuning settings 
###      and different settings (1-split...)

# test all params for all learners ?

test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

skip('Skip tests for tuning')
patrick::with_parameters_test_that("Unit tests for tuning of PLR with repeated cross-fitting:",
                                   .cases = test_cases, {
  
  n_rep_boot = 498    
  n_folds = 5                                                                    
  # set.seed(i_setting)
  # n_folds = 5
  # plr_hat <- DML(data_plr[[i_setting]], y = "y", d = "d",
  #                model = "plr",
  #                k = n_folds, S = n_rep_cross_fit,
  #                mlmethod = learner_pars_for_DML$mlmethod,
  #                params = learner_pars_for_DML$params,
  #                dml_procedure = dml_procedure, inf_model = inf_model,
  #                se_type = inf_model,
  #                bootstrap = "none", nRep = 500)
  # theta <- coef(plr_hat)
  # se <- plr_hat$se

  # exact parameter provision
  learner_pars_exact = rep(list(rep(list(learner_pars_once$params), 3)), n_rep_cross_fit)
  
  set.seed(i_setting)
  double_mlplr_obj_exact = DoubleMLPLR$new(n_folds = n_folds,
                                     ml_learners = learner_list,
                                     params = learner_pars_exact,
                                     dml_procedure = dml_procedure, 
                                     se_reestimate = se_reestimate,
                                     inf_model = inf_model,
                                     n_rep_cross_fit = n_rep_cross_fit)
  
  double_mlplr_obj_exact$fit(data_plr_multi[[i_setting]], y = "y", d = c('d1', 'd2', 'd3'))
  
  theta_obj_exact <- double_mlplr_obj_exact$coef
  se_obj_exact <- double_mlplr_obj_exact$se
  
  # bootstrap
  double_mlplr_obj_exact$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  boot_theta_obj_exact = double_mlplr_obj_exact$boot_coef

    
  # pass one set of parameters (recycled for every fold and rep)
  set.seed(i_setting)
  double_mlplr_obj_once = DoubleMLPLR$new(n_folds = n_folds,
                                     ml_learners = learner_list,
                                     params = learner_pars_once$params,
                                     dml_procedure = dml_procedure, 
                                     se_reestimate = se_reestimate,
                                     inf_model = inf_model,
                                     n_rep_cross_fit = n_rep_cross_fit)
  
  double_mlplr_obj_once$fit(data_plr_multi[[i_setting]], y = "y", d = c('d1', 'd2', 'd3'))
  
  theta_obj_once <- double_mlplr_obj_once$coef
  se_obj_once <- double_mlplr_obj_once$se
  
  # bootstrap
  double_mlplr_obj_once$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  boot_theta_obj_once = double_mlplr_obj_once$boot_coef

  
  # pass empty set of parameters (NULL, use default values)   
  set.seed(i_setting)
  double_mlplr_obj_null = DoubleMLPLR$new(n_folds = n_folds,
                                     ml_learners = learner_list,
                                     params = NULL,
                                     dml_procedure = dml_procedure, 
                                     se_reestimate = se_reestimate,
                                     inf_model = inf_model,
                                     n_rep_cross_fit = n_rep_cross_fit)
  
  double_mlplr_obj_null$fit(data_plr_multi[[i_setting]], y = "y", d = c('d1', 'd2', 'd3'))
  
  theta_obj_null <- double_mlplr_obj_null$coef
  se_obj_null <- double_mlplr_obj_null$se
  
  # bootstrap
  double_mlplr_obj_null$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  boot_theta_obj_null = double_mlplr_obj_null$boot_coef

  
  
  
  set.seed(i_setting)
  double_mlplr_obj_tuned = DoubleMLPLR$new(n_folds = n_folds,
                                     ml_learners = learner_list,
                                     params = learner_pars,
                                     dml_procedure = dml_procedure, 
                                     se_reestimate = se_reestimate,
                                     inf_model = inf_model)
  
  tune_ps = ParamSet$new(list(
                          ParamDbl$new("cp", lower = 0.001, upper = 0.1),
                          ParamInt$new("minsplit", lower = 1, upper = 10)))
  
  double_mlplr_obj_tuned$param_set$param_set_g = tune_ps
  double_mlplr_obj_tuned$param_set$param_set_m = tune_ps

  double_mlplr_obj_tuned$tune(data_plr_multi[[i_setting]], y = "y", d = c('d1', 'd2', 'd3'))
  
  double_mlplr_obj_tuned$fit(data_plr_multi[[i_setting]], y = "y", d = c('d1', 'd2', 'd3'))
  
  theta_obj_tuned <- double_mlplr_obj_tuned$coef
  se_obj_tuned <- double_mlplr_obj_tuned$se
  
  # bootstrap
  double_mlplr_obj_tuned$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  boot_theta_obj_tuned = double_mlplr_obj_tuned$boot_coef
  
  
  # restrictions to test?
  
  }
)

