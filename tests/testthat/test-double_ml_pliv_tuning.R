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
  
tune_settings = list(n_folds_tune = 2,
                      n_rep_tune = 1, 
                      rsmp_tune = "cv", 
                      measure_g = "regr.mse", 
                      measure_m = "regr.mse",
                      terminator = mlr3tuning::trm("evals", n_evals = 2), 
                      algorithm = "grid_search",
                      tuning_instance_g = NULL, 
                      tuning_instance_m = NULL,
                      tuner = "grid_search",
                      resolution = 5)

test_cases = expand.grid(learner_list = learner,
                         dml_procedure = c('dml1', 'dml2'),
                         se_reestimate = c(FALSE),
                         score = c('partialling out'),
                         i_setting = 1:(length(data_pliv)),
                         n_rep_cross_fit = c(1, 3),
                         tune_on_folds = c(FALSE, TRUE),
                         stringsAsFactors = FALSE)

test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

 skip('Skip tests for tuning')

patrick::with_parameters_test_that("Unit tests for tuning of PLIV",
                                   .cases = test_cases, {
                                                        
  # TBD: Functional Test Case

  set.seed(i_setting)
  n_folds = 2
  n_rep_boot = 498
  
  # set.seed(i_setting)
  # pliv_hat <- dml_plriv(data_pliv[[i_setting]], y = "y", d = "d", z = 'z',
  #                       k = n_folds, mlmethod = learner_list,
  #                       params = learner_pars$params,
  #                       dml_procedure = dml_procedure, score = score,
  #                       se_type = score,
  #                       bootstrap = "normal",  nRep = n_rep_boot)
  # theta <- coef(pliv_hat)
  # se <- pliv_hat$se

  
  set.seed(i_setting)
  Xnames = names(data_pliv[[i_setting]])[names(data_pliv[[i_setting]]) %in% c("y", "d", "z") == FALSE]
  data_ml = double_ml_data_from_data_frame(data_pliv[[i_setting]], y_col = "y", 
                              d_cols = "d", x_cols = Xnames, z_cols = "z")

  double_mlpliv_obj_tuned = DoubleMLPLIV$new(data_ml, 
                                     n_folds = n_folds,
                                     ml_g = learner_list$mlmethod_g,
                                     ml_m = learner_list$mlmethod_m,
                                     ml_r = learner_list$mlmethod_r,
                                     dml_procedure = dml_procedure, 
                                     score = score,
                                     n_rep_cross_fit = n_rep_cross_fit)
  
  param_grid = list(param_set_g = ParamSet$new(list(
                                          ParamDbl$new("cp", lower = 0.01, upper = 0.02),
                                          ParamInt$new("minsplit", lower = 1, upper = 2))),
                    param_set_m = ParamSet$new(list(
                                          ParamDbl$new("cp", lower = 0.01, upper = 0.02),
                                          ParamInt$new("minsplit", lower = 1, upper = 2))),
                    param_set_r = ParamSet$new(list( ParamDbl$new("cp", lower = 0.01, upper = 0.02),
                                          ParamInt$new("minsplit", lower = 1, upper = 2))))
  
  
  double_mlpliv_obj_tuned$tune(param_set = param_grid, tune_on_folds = tune_on_folds)
  double_mlpliv_obj_tuned$fit()
  
  theta_obj_tuned <- double_mlpliv_obj_tuned$coef
  se_obj_tuned <- double_mlpliv_obj_tuned$se
  
  # bootstrap
  # double_mlplr_obj_exact$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj_exact = double_mlplr_obj_exact$boot_coef

  expect_is(theta_obj_tuned, "numeric")
  expect_is(se_obj_tuned, "numeric")
  
  }
)

