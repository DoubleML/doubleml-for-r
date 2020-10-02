context("Unit tests for PLR with repeated cross-fitting")

library("mlr3learners")
library('data.table')
library('mlr3')

lgr::get_logger("mlr3")$set_threshold("warn")

test_cases = expand.grid(learner = c('regr.cv_glmnet'), # removed 'regr.glmnet' for the moment as it causes issues for matching results between the oop and functional impl due to seeds etc
                         dml_procedure = c('dml1', 'dml2'),
                         score = c('IV-type', 'partialling out'),
                         i_setting = 1:(length(data_plr)),
                         n_folds = c(2,3),
                         n_rep_cross_fit = c(1, 3),
                         stringsAsFactors = FALSE)
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for PLR:",
                                   .cases = test_cases, {
  
  learner_pars <- get_default_mlmethod_plr(learner)
  learner_pars_for_DML <- learner_pars
  learner_pars_for_DML$params$params_g = rep(list(learner_pars_for_DML$params$params_g), 1)
  learner_pars_for_DML$params$params_m = rep(list(learner_pars_for_DML$params$params_m), 1)

  set.seed(i_setting)
  params_OOP <- rep(list(rep(list(learner_pars$params), 1)), n_rep_cross_fit)
  Xnames = names(data_plr[[i_setting]])[names(data_plr[[i_setting]]) %in% c("y", "d", "z") == FALSE]
  data_ml = double_ml_data_from_data_frame(data_plr[[i_setting]], y_col = "y", 
                              d_cols = "d", x_cols = Xnames)
 
  double_mlplr_obj = DoubleMLPLR$new(data_ml, 
                                     ml_g = learner_pars_for_DML$mlmethod$mlmethod_g, 
                                     ml_m = learner_pars_for_DML$mlmethod$mlmethod_m, 
                                     dml_procedure = dml_procedure, 
                                     n_folds = n_folds,
                                     score = score, 
                                     n_rep_cross_fit = n_rep_cross_fit)
  
  # set params for nuisance part m
  double_mlplr_obj$set__ml_nuisance_params(nuisance_part = "ml_m", 
                                           treat_var = "d",
                                          params = learner_pars$params$params_m)
  
  # set params for nuisance part g
  double_mlplr_obj$set__ml_nuisance_params(nuisance_part = "ml_g", 
                                           treat_var = "d",
                                          params = learner_pars$params$params_g)
  
  double_mlplr_obj$fit()
  theta_obj <- double_mlplr_obj$coef
  se_obj <- double_mlplr_obj$se

  # External sample provision
  SAMPLES = double_mlplr_obj$smpls
  double_mlplr_obj_external = DoubleMLPLR$new(data_ml, 
                                     ml_g = learner_pars_for_DML$mlmethod$mlmethod_g, 
                                     ml_m = learner_pars_for_DML$mlmethod$mlmethod_m, 
                                     dml_procedure = dml_procedure, 
                                     score = score, 
                                     draw_sample_splitting = FALSE)
  
  double_mlplr_obj_external$set_samples(SAMPLES)
  
  # set params for nuisance part m
  double_mlplr_obj_external$set__ml_nuisance_params(nuisance_part = "ml_m", 
                                           treat_var = "d",
                                          params = learner_pars$params$params_m)
  
  # set params for nuisance part g
  double_mlplr_obj_external$set__ml_nuisance_params(nuisance_part = "ml_g", 
                                           treat_var = "d",
                                          params = learner_pars$params$params_g)
  
  double_mlplr_obj_external$fit()
  theta_obj_external <- double_mlplr_obj_external$coef
  se_obj_external <- double_mlplr_obj_external$se

  # at the moment the object result comes without a name
  # expect_equal(theta, theta_obj, tolerance = 1e-8)
  expect_equal(theta_obj, theta_obj_external, tolerance = 1e-8)
  # expect_equal(se, se_obj, tolerance = 1e-8)
  expect_equal(se_obj, se_obj_external, tolerance = 1e-8)
#  expect_equal(as.vector(plr_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
}
)

