context("Unit tests for PLIV")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")


skip_on_cran()

test_cases = expand.grid(learner = c('regr.lm', 'regr.glmnet'),
                           dml_procedure = c('dml1', 'dml2'),
                           score = c('partialling out'),
                           i_setting = 1:(length(data_pliv)),
                           stringsAsFactors = FALSE)
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for PLIV:",
                                   .cases = test_cases, {
  
  learner_pars <- get_default_mlmethod_pliv(learner)
  n_rep_boot = 498
  n_folds = 5
  n_rep = 2
  # TODO: Comparison case (functional)

  set.seed(i_setting)
  params_OOP <- rep(list(rep(list(learner_pars$params), 1)), 1)
  Xnames = names(data_pliv[[i_setting]])[names(data_pliv[[i_setting]]) %in% c("y", "d", "z", "z2") == FALSE]
  data_ml = double_ml_data_from_data_frame(data_pliv[[i_setting]], y_col = "y", 
                              d_cols = "d", x_cols = Xnames, z_cols = c("z", "z2"))

  double_mlpliv_multiz_obj = DoubleMLPLIV$new(data_ml, 
                                     n_folds = n_folds,
                                     ml_g = learner_pars$mlmethod$mlmethod_g,
                                     ml_m = learner_pars$mlmethod$mlmethod_m,
                                     ml_r = learner_pars$mlmethod$mlmethod_r,
                                     dml_procedure = dml_procedure, 
                                     score = score, 
                                     n_rep = n_rep)
  
  double_mlpliv_multiz_obj$set_ml_nuisance_params(learner = "ml_g", 
                                           treat_var = "d",
                                            params = learner_pars$params$params_g)
  double_mlpliv_multiz_obj$set_ml_nuisance_params(learner = "ml_m_z", 
                                           treat_var = "d",
                                            params = learner_pars$params$params_m)
  double_mlpliv_multiz_obj$set_ml_nuisance_params(learner = "ml_m_z2",
                                           treat_var = "d",
                                            params = learner_pars$params$params_m)
  double_mlpliv_multiz_obj$set_ml_nuisance_params(learner = "ml_r", 
                                           treat_var = "d",
                                            params = learner_pars$params$params_r)
  
  double_mlpliv_multiz_obj$fit()
  theta_multiz_obj <- double_mlpliv_multiz_obj$coef
  se_multiz_obj <- double_mlpliv_multiz_obj$se
  
  
  # Exact passing
  export_params_exact_g = rep(list(rep(list(learner_pars$params$params_g), n_folds)), n_rep)
  export_params_exact_m = rep(list(rep(list(learner_pars$params$params_m), n_folds)), n_rep)
  export_params_exact_r = rep(list(rep(list(learner_pars$params$params_r), n_folds)), n_rep)
  
  set.seed(i_setting)
  params_OOP <- rep(list(rep(list(learner_pars$params), 1)), 1)
  Xnames = names(data_pliv[[i_setting]])[names(data_pliv[[i_setting]]) %in% c("y", "d", "z", "z2") == FALSE]
  data_ml = double_ml_data_from_data_frame(data_pliv[[i_setting]], y_col = "y", 
                              d_cols = "d", x_cols = Xnames, z_cols = c("z", "z2"))

  double_mlpliv_mutliz_exact_obj = DoubleMLPLIV$new(data_ml, 
                                     n_folds = 5,
                                     ml_g = learner_pars$mlmethod$mlmethod_g,
                                     ml_m = learner_pars$mlmethod$mlmethod_m,
                                     ml_r = learner_pars$mlmethod$mlmethod_r,
                                     dml_procedure = dml_procedure, 
                                     score = score, 
                                     n_rep = n_rep)
  
    
  double_mlpliv_mutliz_exact_obj$set_ml_nuisance_params(learner = "ml_g", 
                                                        treat_var = "d",
                                                        params = export_params_exact_g, 
                                                        set_fold_specific = TRUE)
  double_mlpliv_mutliz_exact_obj$set_ml_nuisance_params(learner = "ml_m_z", 
                                                        treat_var = "d",
                                                        params = export_params_exact_m, 
                                                        set_fold_specific = TRUE)
  double_mlpliv_mutliz_exact_obj$set_ml_nuisance_params(learner = "ml_m_z2",
                                                        treat_var = "d",
                                                        params = export_params_exact_m, 
                                                        set_fold_specific = TRUE)
  double_mlpliv_mutliz_exact_obj$set_ml_nuisance_params(learner = "ml_r", 
                                                        treat_var = "d",
                                                        params = export_params_exact_r, 
                                                        set_fold_specific = TRUE)
  
  double_mlpliv_mutliz_exact_obj$fit()
  theta_mutliz_exact_obj <- double_mlpliv_mutliz_exact_obj$coef
  se_mutliz_exact_obj <- double_mlpliv_mutliz_exact_obj$se
  
  # bootstrap
  # double_mlpliv_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj = double_mlpliv_obj$boot_coef
  
  # at the moment the object result comes without a name
  expect_equal(theta_multiz_obj, theta_mutliz_exact_obj, tolerance = 1e-8)
  expect_equal(se_multiz_obj, se_mutliz_exact_obj, tolerance = 1e-8)
  # expect_equal(as.vector(pliv_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  
}
)

