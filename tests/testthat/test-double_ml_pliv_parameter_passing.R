context("Unit tests for parameter passing for PLIV")

lgr::get_logger("mlr3")$set_threshold("warn")

# settings for parameter provision
learner = c('regr.rpart')

learner_list = list("mlmethod_m" = learner, "mlmethod_g" = learner, "mlmethod_r" = learner)
on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(learner = learner,
                           dml_procedure = c('dml2'),
                           score = c('partialling out'),
                           i_setting = 1:(length(data_pliv)),
                           n_rep = c(3),
                           stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(learner = learner,
                           dml_procedure = c('dml1', 'dml2'),
                           score = c('partialling out'),
                           i_setting = 1:(length(data_pliv)),
                           n_rep = c(1, 3),
                           stringsAsFactors = FALSE)
}

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
                        k = n_folds, S = n_rep, 
                        mlmethod = learner_list,
                        params = learner_pars_once$params,
                        dml_procedure = dml_procedure, score = score,
                        se_type = score,
                        bootstrap = "normal",  nRep = n_rep_boot)
  theta <- coef(pliv_hat)
  se <- pliv_hat$se
  

  # exact parameter provision
  # learner_pars_exact <- rep(list(rep(list(learner_pars_once$params), 1)), n_rep)
  params_g = list("d" = learner_pars_once$params$params_g)
  params_m = list("d" = learner_pars_once$params$params_m)
  params_r = list("d" = learner_pars_once$params$params_r)

  set.seed(i_setting)
  Xnames = names(data_pliv[[i_setting]])[names(data_pliv[[i_setting]]) %in% c("y", "d", "z") == FALSE]
  
  data_ml = double_ml_data_from_data_frame(data_pliv[[i_setting]], y_col = "y", 
                              d_cols = c("d"), x_cols = Xnames, z_cols = "z")
  
  double_mlpliv_obj_once = DoubleMLPLIV$new(data_ml, 
                                     n_folds = n_folds,
                                     ml_g = learner_list$mlmethod_g,
                                     ml_m = learner_list$mlmethod_m,
                                     ml_r = learner_list$mlmethod_r,
                                     dml_procedure = dml_procedure, 
                                     score = score,
                                     n_rep = n_rep)
  
  double_mlpliv_obj_once$set_ml_nuisance_params(treat_var = "d", learner = "ml_g", params = params_g$d)
  double_mlpliv_obj_once$set_ml_nuisance_params(treat_var = "d", learner = "ml_m", params = params_m$d)
  double_mlpliv_obj_once$set_ml_nuisance_params(treat_var = "d", learner = "ml_r", params = params_r$d)
  
  double_mlpliv_obj_once$fit()
  
  theta_obj_once <- double_mlpliv_obj_once$coef
  se_obj_once <- double_mlpliv_obj_once$se
  
  ## TBD: External parameter provision
  
  # bootstrap
  # double_mlplr_obj_exact$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj_exact = double_mlplr_obj_exact$boot_coef
  # pass empty set of parameters (NULL, use default values)   

  # no parameters specified (use default values)   
  set.seed(i_setting)
  double_mlpliv_obj_default = DoubleMLPLIV$new(data_ml, 
                                     n_folds = n_folds,
                                     ml_g = learner_list$mlmethod_g,
                                     ml_m = learner_list$mlmethod_m,
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

  expect_equal(theta_obj_once, theta_obj_default, tolerance = 1e-8)
  # expect_equal(theta_obj_null, theta_obj_default, tolerance = 1e-8)
  # expect_equal(se_obj_null, se_obj_exact, tolerance = 1e-8)
  expect_equal(se_obj_once, se_obj_default, tolerance = 1e-8)

  # expect_equal(boot_theta_obj_once, boot_theta_obj_exact, tolerance = 1e-8)
  # expect_equal(boot_theta_obj_null, boot_theta_obj_exact, tolerance = 1e-8)


  
  }
)

