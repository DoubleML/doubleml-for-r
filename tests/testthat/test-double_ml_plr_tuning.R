context("Unit tests for tuning of PLR")

library("mlr3learners")
library("mlr3tuning")
library("paradox")
library('data.table')
library('mlr3')

lgr::get_logger("mlr3")$set_threshold("warn")

# settings for parameter provision
learner = c('regr.rpart')

learner_list = list("mlmethod_m" = learner, "mlmethod_g" = learner)

tune_settings = list(n_folds_tune = 2,
                      n_rep_tune = 1, 
                      rsmp_tune = "cv", 
                      # measure_g = "regr.mse", 
                      # measure_m = "regr.mse",
                      terminator = mlr3tuning::trm("evals", n_evals = 2), 
                      algorithm = "grid_search",
                      tuning_instance_g = NULL, 
                      tuning_instance_m = NULL,
                      tuner = "grid_search",
                      resolution = 5)

test_cases = expand.grid(learner = learner,
                         dml_procedure = c('dml1', 'dml2'),
                         se_reestimate = c(FALSE),
                         score = c('IV-type', 'partialling out'),
                         n_rep = c(1, 3),
                         tune_on_folds = c(FALSE, TRUE),
                         i_setting = 1:(length(data_plr)),
                         stringsAsFactors = FALSE)


test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

# skip('Skip tests for tuning')
patrick::with_parameters_test_that("Unit tests for tuning of PLR:",
                                   .cases = test_cases, {
  
  n_rep_boot = 498    
  n_folds = 4
  
  # TBD: Functional Test Case
  
  # set.seed(i_setting)
  # n_folds = 5
  # plr_hat <- DML(data_plr_multi[[i_setting]], y = "y", d = c('d1', 'd2', 'd3'),
  #                model = "plr",
  #                k = n_folds, S = 1,
  #                mlmethod = learner_pars_for_DML$mlmethod,
  #                params = learner_pars_for_DML$params,
  #                dml_procedure = dml_procedure, score = score,
  #                se_type = score,
  #                bootstrap = "normal", nRep = n_rep_boot)
  # theta <- coef(plr_hat)
  # se <- plr_hat$se
  
  set.seed(i_setting)
  Xnames = names(data_plr_multi[[i_setting]])[names(data_plr_multi[[i_setting]]) %in% c("y", "d1", "d2", "z") == FALSE]
  data_ml = double_ml_data_from_data_frame(data_plr_multi[[i_setting]], y_col = "y", 
                              d_cols = c("d1", "d2"), x_cols = Xnames)

  double_mlplr_obj_tuned = DoubleMLPLR$new(data_ml, 
                                     n_folds = n_folds,
                                     ml_g = learner_list$mlmethod_g,
                                     ml_m = learner_list$mlmethod_m,
                                     dml_procedure = dml_procedure, 
                                     score = score, 
                                     n_rep = n_rep)
  
  param_grid = list(param_set_g = ParamSet$new(list(
                                          ParamDbl$new("cp", lower = 0.01, upper = 0.02),
                                          ParamInt$new("minsplit", lower = 1, upper = 2))),
                    param_set_m = ParamSet$new(list(
                                          ParamDbl$new("cp", lower = 0.01, upper = 0.02),
                                          ParamInt$new("minsplit", lower = 1, upper = 2))))
                  
  double_mlplr_obj_tuned$tune(param_set = param_grid, tune_on_folds = tune_on_folds)
  
  double_mlplr_obj_tuned$fit()
  
  theta_obj_tuned <- double_mlplr_obj_tuned$coef
  se_obj_tuned <- double_mlplr_obj_tuned$se
  
  # bootstrap
  # double_mlplr_obj_tuned$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj_tuned = double_mlplr_obj_tuned$boot_coef
  
  # restrictions to test
    # Functional (tbd) vs OOP implementation (handling randomness in param selection!?)
  
  # Test case without including "other" treatment variables
    
  expect_is(theta_obj_tuned, "numeric")
  expect_is(se_obj_tuned, "numeric")
  # 
  # data_ml$use_other_treat_as_covariate = FALSE
  # double_mlplr_obj_tuned$tune()
  # double_mlplr_obj_tuned$fit()
  # theta_obj_tuned <- double_mlplr_obj_tuned$coef
  # se_obj_tuned <- double_mlplr_obj_tuned$se
  # 
  # expect_is(theta_obj_tuned, "numeric")
  # expect_is(se_obj_tuned, "numeric")
  }
)

