context("Unit tests for parameter passing for PLR")

library("mlr3learners")
library("mlr3tuning")
library("paradox")
library('data.table')
library('mlr3')

lgr::get_logger("mlr3")$set_threshold("warn")

# settings for parameter provision
learner = c('regr.rpart')

learner_list = list("mlmethod_m" = learner, "mlmethod_g" = learner)
  
test_cases = expand.grid(learner = learner,
                         dml_procedure = c('dml1', 'dml2'),
                         score = c('IV-type', 'partialling out'),
                         i_setting = 1:(length(data_plr)),
                         n_rep_cross_fit = c(1, 3),
                         stringsAsFactors = FALSE)

test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

# skip('Skip tests for tuning')

patrick::with_parameters_test_that("Unit tests for parameter passing of PLR",
                                   .cases = test_cases, {
  
  n_rep_boot = 498    
  n_folds = 2                                                                   
  
  # TBD: Functional Test Case
  learner_pars <- get_default_mlmethod_plr(learner)
  learner_pars_for_DML <- learner_pars
  learner_pars_for_DML$params$params_g = rep(list(learner_pars_for_DML$params$params_g), 2)
  learner_pars_for_DML$params$params_m = rep(list(learner_pars_for_DML$params$params_m), 2)

  set.seed(i_setting)
  plr_hat <- DML(data_plr_multi[[i_setting]], y = "y",  d = c('d1', 'd2'),
                 model = "plr",
                 k = n_folds, S = n_rep_cross_fit,
                 mlmethod = learner_pars_for_DML$mlmethod,
                 params = learner_pars_for_DML$params,
                 dml_procedure = dml_procedure, score = score,
                 se_type = score,
                 bootstrap = "none", nRep = 500)
  theta <- coef(plr_hat)
  se <- plr_hat$se

  # exact parameter provision
  learner_pars_once <- get_default_mlmethod_plr(learner, default = FALSE)
  params_g = list("d1" = learner_pars_once$params$params_g, "d2" = learner_pars_once$params$params_g)
  params_m = list("d1" = learner_pars_once$params$params_m, "d2" = learner_pars_once$params$params_m)
  
  Xnames = names(data_plr_multi[[i_setting]])[names(data_plr_multi[[i_setting]]) %in% c("y", "d1", "d2", "z") == FALSE]
  data_ml = double_ml_data_from_data_frame(data_plr_multi[[i_setting]], y_col = "y", 
                              d_cols = c("d1", "d2"), x_cols = Xnames)

  set.seed(i_setting)
  double_mlplr_obj_once = DoubleMLPLR$new(data_ml, 
                                     n_folds = n_folds,
                                     ml_g = learner_list$mlmethod_g,
                                     ml_m = learner_list$mlmethod_m,
                                     dml_procedure = dml_procedure, 
                                     score = score,
                                     n_rep_cross_fit = n_rep_cross_fit)
  
  double_mlplr_obj_once$set__ml_nuisance_params(treat_var = "d1", nuisance_part = "ml_g", params = params_g$d1)
  double_mlplr_obj_once$set__ml_nuisance_params(treat_var = "d2", nuisance_part = "ml_g", params = params_g$d2)
  double_mlplr_obj_once$set__ml_nuisance_params(treat_var = "d1", nuisance_part = "ml_m", params = params_m$d1)
  double_mlplr_obj_once$set__ml_nuisance_params(treat_var = "d2", nuisance_part = "ml_m", params = params_m$d2)

  
  double_mlplr_obj_once$fit()
  theta_obj_once <- double_mlplr_obj_once$coef
  se_obj_once <- double_mlplr_obj_once$se
  
  # Passing for non-cross-fitting case 
  
  if (n_rep_cross_fit == 1) {
    
    set.seed(i_setting)
    double_mlplr_obj_nocf = DoubleMLPLR$new(data_ml, 
                                       n_folds = n_folds,
                                       ml_g = learner_list$mlmethod_g,
                                       ml_m = learner_list$mlmethod_m,
                                       dml_procedure = dml_procedure, 
                                       score = score,
                                       n_rep_cross_fit = n_rep_cross_fit,
                                       apply_cross_fitting = FALSE)
    
    double_mlplr_obj_nocf$set__ml_nuisance_params(treat_var = "d1", nuisance_part = "ml_g", params = params_g$d1)
    double_mlplr_obj_nocf$set__ml_nuisance_params(treat_var = "d2", nuisance_part = "ml_g", params = params_g$d2)
    double_mlplr_obj_nocf$set__ml_nuisance_params(treat_var = "d1", nuisance_part = "ml_m", params = params_m$d1)
    double_mlplr_obj_nocf$set__ml_nuisance_params(treat_var = "d2", nuisance_part = "ml_m", params = params_m$d2)
  
    
    double_mlplr_obj_nocf$fit()
    theta_obj_nocf <- double_mlplr_obj_nocf$coef
    se_obj_nocf <- double_mlplr_obj_nocf$se
    
    expect_true(is.numeric(theta_obj_nocf))
    expect_true(is.numeric(se_obj_nocf))

  }
  # TBD: Exact Parameter Passing (external tuning)
  # 
  # export_params_m = list("m_params" = rep(list(list(params_m[[1]])), n_folds))
  # export_params_g = list("g_params" = rep(list(list(params_g[[1]])), n_folds))
  # learner_pars_exact_params = learner_pars_once$params
  # names(learner_pars_exact_params) = c("g_params", "m_params")
  # learner_pars_exact_params = rep(list(rep(list(learner_pars_exact_params), n_folds)), n_rep_cross_fit)
  # 
  # exact_ml_nuisance_params = list()
  # exact_ml_nuisance_params[["d1"]] = learner_pars_exact_params
  # exact_ml_nuisance_params[["d2"]] = learner_pars_exact_params
  # 
  # # learner_pars_exact <- rep(list(rep(list(rep(list(learner_pars_once$params), n_folds)), n_rep_cross_fit)), 2)
  # 
  # set.seed(i_setting)
  # 
  # # Instantiate with parameters                        
  # double_mlplr_obj_exact = DoubleMLPLR$new(data_ml, 
  #                                    n_folds = n_folds,
  #                                    ml_g = learner_list$mlmethod_g,
  #                                    ml_m = learner_list$mlmethod_m,
  #                                    dml_procedure = dml_procedure, 
  #                                    score = score,
  #                                    n_rep_cross_fit = n_rep_cross_fit)
  # 
  # double_mlplr_obj_exact$set__ml_nuisance_params(treat_var = NULL, nuisance_part = NULL, params = exact_ml_nuisance_params)
  # 
  # double_mlplr_obj_exact$fit()
  # 
  # theta_obj_exact <- double_mlplr_obj_exact$coef
  # se_obj_exact <- double_mlplr_obj_exact$se
  # 
  # # bootstrap
  # double_mlplr_obj_exact$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj_exact = double_mlplr_obj_exact$boot_coef

  # no parameters specified (use default values)   
  set.seed(i_setting)
  double_mlplr_obj_default = DoubleMLPLR$new(data_ml, 
                                     n_folds = n_folds,
                                     ml_g = learner_list$mlmethod_g,
                                     ml_m = learner_list$mlmethod_m,
                                     dml_procedure = dml_procedure, 
                                     score = score,
                                     n_rep_cross_fit = n_rep_cross_fit)
  
  double_mlplr_obj_default$fit()
  
  theta_obj_default <- double_mlplr_obj_default$coef
  se_obj_default <- double_mlplr_obj_default$se
  
  # bootstrap
  # double_mlplr_obj_default$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj_default = double_mlplr_obj_default$boot_coef

  expect_equal(theta, theta_obj_once, tolerance = 1e-8)
  # expect_equal(se, se_obj_once, tolerance = 1e-8)

  expect_equal(theta_obj_default, theta_obj_once, tolerance = 1e-8)
  expect_equal(se_obj_default, se_obj_once, tolerance = 1e-8)

  # expect_equal(boot_theta_obj_once, boot_theta_obj_exact, tolerance = 1e-8)
  # expect_equal(boot_theta_obj_null, boot_theta_obj_exact, tolerance = 1e-8)

  }
)

