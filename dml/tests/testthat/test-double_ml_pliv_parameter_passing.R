context("Unit tests for parameter passing for PLIV")

library("mlr3learners")
library("mlr3tuning")
library("paradox")
library('data.table')
library('mlr3')

lgr::get_logger("mlr3")$set_threshold("warn")

# settings for parameter provision
learner = c('regr.rpart')

learner_list = list("mlmethod_m" = learner, "mlmethod_g" = learner, "mlmethod_r" = learner)
  
test_cases = expand.grid(learner = learner,
                         dml_procedure = c('dml1', 'dml2'),
                         se_reestimate = c(FALSE),
                         inf_model = c('partialling-out'),
                         i_setting = 1:(length(data_pliv)),
                         n_rep_cross_fit = c(1, 3),
                         stringsAsFactors = FALSE)

test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")
# skip('Skip tests for tuning')

patrick::with_parameters_test_that("Unit tests for parameter passing of PLIV",
                                   .cases = test_cases, {
  
  n_rep_boot = 498    
  n_folds = 2                                                                   
  
  # TBD: Functional Test Case

  set.seed(i_setting)
  n_folds = 5
  learner_pars_once <- get_default_mlmethod_pliv(learner)
  n_rep_boot = 498
  
  set.seed(i_setting)
  pliv_hat <- dml_plriv(data_pliv[[i_setting]], y = "y", d = "d", z = 'z',
                        k = n_folds, S = n_rep_cross_fit, 
                        mlmethod = learner_list,
                        params = learner_pars_once$params,
                        dml_procedure = dml_procedure, inf_model = inf_model,
                        se_type = inf_model,
                        bootstrap = "normal",  nRep = n_rep_boot)
  theta <- coef(pliv_hat)
  se <- pliv_hat$se
  

  # exact parameter provision
  learner_pars_exact <- rep(list(rep(list(learner_pars_once$params), 1)), n_rep_cross_fit)
  
  set.seed(i_setting)
  Xnames = names(data_pliv[[i_setting]])[names(data_pliv[[i_setting]]) %in% c("y", "d", "z") == FALSE]
  
  data_ml = double_ml_data_from_data_frame(data_pliv[[i_setting]], y_col = "y", 
                              d_cols = c("d"), x_cols = Xnames, z_col = "z")
  
  double_mlpliv_obj_exact = DoubleMLPLIV$new(data_ml, 
                                     n_folds = n_folds,
                                     ml_learners = learner_list,
                                     params = learner_pars_exact,
                                     dml_procedure = dml_procedure, 
                                     se_reestimate = se_reestimate,
                                     inf_model = inf_model,
                                     n_rep_cross_fit = n_rep_cross_fit)
  
  double_mlpliv_obj_exact$fit()
  
  theta_obj_exact <- double_mlpliv_obj_exact$coef
  se_obj_exact <- double_mlpliv_obj_exact$se
  
  # bootstrap
  # double_mlplr_obj_exact$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj_exact = double_mlplr_obj_exact$boot_coef

  
  # pass empty set of parameters (NULL, use default values)   
  set.seed(i_setting)
  double_mlpliv_obj_null = DoubleMLPLIV$new(data_ml, 
                                     n_folds = n_folds,
                                     ml_learners = learner_list,
                                     params = NULL,
                                     dml_procedure = dml_procedure, 
                                     se_reestimate = se_reestimate,
                                     inf_model = inf_model,
                                     n_rep_cross_fit = n_rep_cross_fit)
  
  double_mlpliv_obj_null$fit()
  
  theta_obj_null <- double_mlpliv_obj_null$coef
  se_obj_null <- double_mlpliv_obj_null$se
  
  # bootstrap
  # double_mlplr_obj_null$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj_null = double_mlplr_obj_null$boot_coef
  
  # no parameters specified (use default values)   
  set.seed(i_setting)
  double_mlpliv_obj_default = DoubleMLPLIV$new(data_ml, 
                                     n_folds = n_folds,
                                     ml_learners = learner_list,
                                     dml_procedure = dml_procedure, 
                                     se_reestimate = se_reestimate,
                                     inf_model = inf_model,
                                     n_rep_cross_fit = n_rep_cross_fit)
  
  double_mlpliv_obj_default$fit()
  
  theta_obj_default <- double_mlpliv_obj_default$coef
  se_obj_default <- double_mlpliv_obj_default$se
  
  # bootstrap
  # double_mlplr_obj_default$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj_default = double_mlplr_obj_default$boot_coef

  expect_equal(theta_obj_null, theta_obj_exact, tolerance = 1e-8)
  expect_equal(theta_obj_null, theta_obj_default, tolerance = 1e-8)
  expect_equal(se_obj_null, se_obj_exact, tolerance = 1e-8)
  expect_equal(se_obj_null, se_obj_default, tolerance = 1e-8)

  # expect_equal(boot_theta_obj_once, boot_theta_obj_exact, tolerance = 1e-8)
  # expect_equal(boot_theta_obj_null, boot_theta_obj_exact, tolerance = 1e-8)


  
  }
)

