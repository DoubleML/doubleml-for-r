context("Unit tests for PLR with repeated cross-fitting")

library("mlr3learners")
library('data.table')
library('mlr3')

lgr::get_logger("mlr3")$set_threshold("warn")

test_cases = expand.grid(learner = c('regr.lm', 'regr.glmnet'), # removed 'regr.glmnet' for the moment as it causes issues for matching results between the oop and functional impl due to seeds etc
                         dml_procedure = c('dml1', 'dml2'),
                         se_reestimate = c(FALSE),
                         inf_model = c('IV-type', 'DML2018'),
                         i_setting = 1:(length(data_plr)),
                         n_rep_cross_fit = c(2, 5),
                         stringsAsFactors = FALSE)
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for PLR:",
                                   .cases = test_cases, {
  
  learner_pars <- get_default_mlmethod_plr(learner)
  learner_pars_for_DML <- learner_pars
  learner_pars_for_DML$params$params_g = rep(list(learner_pars_for_DML$params$params_g), 1)
  learner_pars_for_DML$params$params_m = rep(list(learner_pars_for_DML$params$params_m), 1)
  n_rep_boot = 498
  
  set.seed(i_setting)
  n_folds = 5
  plr_hat <- DML(data_plr[[i_setting]], y = "y", d = "d",
                 model = "plr",
                 k = n_folds, S = n_rep_cross_fit,
                 mlmethod = learner_pars_for_DML$mlmethod,
                 params = learner_pars_for_DML$params,
                 dml_procedure = dml_procedure, inf_model = inf_model,
                 se_type = inf_model)
  theta <- coef(plr_hat)
  se <- plr_hat$se
  
  plr_hat$boot_theta <- bootstrap.DML(plr_hat, data_plr[[i_setting]], y = "y", d = "d",
                                      dml_procedure = dml_procedure,
                                      inf_model = inf_model, se_type = inf_model,
                                      bootstrap = "normal", nRep = n_rep_boot)
  
  set.seed(i_setting)
  params_OOP <- rep(list(rep(list(learner_pars$params), 1)), n_rep_cross_fit)
  Xnames = names(data_plr[[i_setting]])[names(data_plr[[i_setting]]) %in% c("y", "d", "z") == FALSE]
  data_ml = double_ml_data_from_data_frame(data_plr[[i_setting]], y_col = "y", 
                              d_cols = "d", x_cols = Xnames)
 
  double_mlplr_obj = DoubleMLPLR$new(data_ml, 
                                     n_folds = n_folds,
                                     ml_learners = learner_pars$mlmethod,
                                     params = params_OOP,
                                     dml_procedure = dml_procedure, 
                                     se_reestimate = se_reestimate, 
                                     inf_model = inf_model,
                                     n_rep_cross_fit = n_rep_cross_fit)
  double_mlplr_obj$fit()
  theta_obj <- double_mlplr_obj$coef
  se_obj <- double_mlplr_obj$se
  
  # bootstrap
  double_mlplr_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  boot_theta_obj = double_mlplr_obj$boot_coef
  
  # at the moment the object result comes without a name
  expect_equal(theta, theta_obj, tolerance = 1e-8)
  expect_equal(se, se_obj, tolerance = 1e-8)
#  expect_equal(as.vector(plr_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
}
)

