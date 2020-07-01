context("Unit tests for PLR")

library("mlr3learners")
library('data.table')
library('mlr3')

lgr::get_logger("mlr3")$set_threshold("warn")

test_cases = expand.grid(learner = c('regr.glmnet'),
                         inf_model = c('IV-type', 'DML2018'),
                         i_setting = 1:(length(data_plr)),
                         stringsAsFactors = FALSE)
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

# skip("skip reestimate se test")
patrick::with_parameters_test_that("Unit tests for se_reestimate (PLR):",
                                   .cases = test_cases, {
  
  learner_pars <- get_default_mlmethod_plr(learner)
  learner_pars_for_DML <- learner_pars
  learner_pars_for_DML$params$params_g = rep(list(learner_pars_for_DML$params$params_g), 1)
  learner_pars_for_DML$params$params_m = rep(list(learner_pars_for_DML$params$params_m), 1)
  n_rep_boot = 498
 
  n_folds = 2
  # dml1: expect different se's
  
  params_OOP <- rep(list(rep(list(learner_pars$params), 1)), 1)
  set.seed(i_setting)
  Xnames = names(data_plr[[i_setting]])[names(data_plr[[i_setting]]) %in% c("y", "d", "z") == FALSE]
  data_ml = double_ml_data_from_data_frame(data_plr[[i_setting]], y_col = "y", 
                              d_cols = "d", x_cols = Xnames)
  
  double_mlplr_obj_dml1 = DoubleMLPLR$new(data_ml, 
                                     n_folds = n_folds,
                                     ml_learners = learner_pars$mlmethod,
                                     params = params_OOP,
                                     dml_procedure = "dml1", 
                                     se_reestimate = FALSE,
                                     inf_model = inf_model)
  double_mlplr_obj_dml1$fit()
  theta_obj_dml1 <- double_mlplr_obj_dml1$coef
  se_obj_dml1 <- double_mlplr_obj_dml1$se
  
  set.seed(i_setting)
  double_mlplr_obj_dml1_reestim = DoubleMLPLR$new(data_ml, 
                                     n_folds = n_folds,
                                     ml_learners = learner_pars$mlmethod,
                                     params = params_OOP,
                                     dml_procedure = "dml1", 
                                     se_reestimate = TRUE,
                                     inf_model = inf_model)
  double_mlplr_obj_dml1_reestim$fit()
  theta_obj_dml1_reestim <- double_mlplr_obj_dml1_reestim$coef
  se_obj_dml1_reestim <- double_mlplr_obj_dml1_reestim$se
  
  # dml2: expect same se's
  set.seed(i_setting)
  double_mlplr_obj_dml2 = DoubleMLPLR$new(data_ml, 
                                     n_folds = n_folds,
                                     ml_learners = learner_pars$mlmethod,
                                     params = params_OOP,
                                     dml_procedure = "dml2", 
                                     se_reestimate = FALSE,
                                     inf_model = inf_model)
  double_mlplr_obj_dml2$fit()
  theta_obj_dml2 <- double_mlplr_obj_dml2$coef
  se_obj_dml2 <- double_mlplr_obj_dml2$se
  
  set.seed(i_setting)
  double_mlplr_obj_dml2_reestim = DoubleMLPLR$new(data_ml, 
                                     n_folds = n_folds,
                                     ml_learners = learner_pars$mlmethod,
                                     params = params_OOP,
                                     dml_procedure = "dml2", 
                                     se_reestimate = TRUE,
                                     inf_model = inf_model)
  double_mlplr_obj_dml2_reestim$fit()
  theta_obj_dml2_reestim <- double_mlplr_obj_dml2_reestim$coef
  se_obj_dml2_reestim <- double_mlplr_obj_dml2_reestim$se
  
  
  # at the moment the object result comes without a name
  expect_equal(theta_obj_dml1, theta_obj_dml1_reestim, tolerance = 1e-8)
  expect_equal(theta_obj_dml2, theta_obj_dml2_reestim, tolerance = 1e-8)
  expect_true(se_obj_dml1 != se_obj_dml1_reestim)
  expect_equal(se_obj_dml2, se_obj_dml2_reestim, tolerance = 1e-8)
}
)

