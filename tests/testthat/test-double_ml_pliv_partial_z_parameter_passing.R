context("Unit tests for parameter passing for PLIV, partial_xz")

lgr::get_logger("mlr3")$set_threshold("warn")

# settings for parameter provision
learner = c('regr.rpart')

learner_list = list("mlmethod_m" = learner, "mlmethod_g" = learner, "mlmethod_r" = learner)

skip_on_cran()

test_cases = expand.grid(learner = learner,
                           dml_procedure = c('dml1', 'dml2'),
                           score = c('partialling out'),
                           i_setting = 1:(length(data_pliv)),
                           n_rep = c(1, 3),
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
  
  # TODO: Functional implementation of partialXZ and check
  # set.seed(i_setting)
  # pliv_hat <- dml_plriv(data_pliv[[i_setting]], y = "y", d = "d", z = 'z',
  #                       k = n_folds, S = n_rep, 
  #                       mlmethod = learner_list,
  #                       params = learner_pars_once$params,
  #                       dml_procedure = dml_procedure, score = score,
  #                       se_type = score,
  #                       bootstrap = "normal",  nRep = n_rep_boot)
  # theta <- coef(pliv_hat)
  # se <- pliv_hat$se
  # 

  set.seed(i_setting)
  Xnames = names(data_pliv[[i_setting]])[names(data_pliv[[i_setting]]) %in% c("y", "d", "z", "z2") == FALSE]
  data_ml = double_ml_data_from_data_frame(data_pliv[[i_setting]], y_col = "y", 
                              d_cols = "d", x_cols = Xnames, z_cols = c("z", "z2"))
  
  params_r = list("d" = learner_pars_once$params$params_r)
  
  double_mlpliv_obj_once = DoubleMLPLIV.partialZ(data_ml, 
                                                 n_folds = n_folds,
                                                 ml_r = learner_list$mlmethod_r,
                                                 dml_procedure = dml_procedure, 
                                                 score = score,
                                                 n_rep = n_rep)
  double_mlpliv_obj_once$set_ml_nuisance_params(treat_var = "d", learner = "ml_r", params = params_r$d)
  
  double_mlpliv_obj_once$fit()
  
  theta_obj_once <- double_mlpliv_obj_once$coef
  se_obj_once <- double_mlpliv_obj_once$se
  
  # Exact passing
  export_params_exact_r = rep(list(rep(params_r, n_folds)), n_rep)
  
  set.seed(i_setting)
  double_mlpliv_obj_exact = DoubleMLPLIV.partialZ(data_ml, 
                                                  n_folds = n_folds,
                                                  ml_r = learner_list$mlmethod_r,
                                                  dml_procedure = dml_procedure, 
                                                  score = score,
                                                  n_rep = n_rep)
            
  double_mlpliv_obj_exact$set_ml_nuisance_params(treat_var = "d", learner = "ml_r", params = export_params_exact_r, 
                                                  set_fold_specific = TRUE)
  
  double_mlpliv_obj_exact$fit()
  
  theta_obj_exact <- double_mlpliv_obj_exact$coef
  se_obj_exact <- double_mlpliv_obj_exact$se
  
  
  ## TBD: External parameter provision
  
  # bootstrap
  # double_mlplr_obj_exact$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj_exact = double_mlplr_obj_exact$boot_coef
  # pass empty set of parameters (NULL, use default values)   

  # no parameters specified (use default values)   
  set.seed(i_setting)
  double_mlpliv_obj_default =  DoubleMLPLIV.partialZ(data_ml, 
                                                     n_folds = n_folds,
                                                     ml_r = learner_list$mlmethod_r,
                                                     dml_procedure = dml_procedure, 
                                                     score = score,
                                                     n_rep = n_rep)
  
  double_mlpliv_obj_default$fit()
  
  theta_obj_default <- double_mlpliv_obj_default$coef
  se_obj_default <- double_mlpliv_obj_default$se
  
  # bootstrap
  # double_mlplr_obj_default$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj_default = double_mlplr_obj_default$boot_coef
  
  # check:
  # expect_equal(theta, theta_obj_default, tolerance = 1e-8)
  expect_equal(theta_obj_once, theta_obj_exact, tolerance = 1e-8)
  expect_equal(theta_obj_once, theta_obj_default, tolerance = 1e-8)
  # expect_equal(theta_obj_null, theta_obj_default, tolerance = 1e-8)
  # expect_equal(se_obj_null, se_obj_exact, tolerance = 1e-8)
  expect_equal(se_obj_once, se_obj_exact, tolerance = 1e-8)
  expect_equal(se_obj_once, se_obj_default, tolerance = 1e-8)
  # expect_equal(boot_theta_obj_once, boot_theta_obj_exact, tolerance = 1e-8)
  # expect_equal(boot_theta_obj_null, boot_theta_obj_exact, tolerance = 1e-8)


  
  }
)

