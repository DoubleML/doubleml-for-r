context("Unit tests for PLR with repeated cross-fitting")

library("mlr3learners")
library('data.table')
library('mlr3')

lgr::get_logger("mlr3")$set_threshold("warn")

test_cases = expand.grid(learner = c('regr.lm', 'regr.cv_glmnet'), # removed 'regr.glmnet' for the moment as it causes issues for matching results between the oop and functional impl due to seeds etc
                         dml_procedure = c('dml1', 'dml2'),
                         se_reestimate = c(FALSE),
                         score = c('IV-type', 'partialling out'),
                         i_setting = 1:(length(data_plr)),
                         n_rep = c(2, 5),
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
                 k = n_folds, S = n_rep,
                 mlmethod = learner_pars_for_DML$mlmethod,
                 params = learner_pars_for_DML$params,
                 dml_procedure = dml_procedure, score = score,
                 se_type = score)
  theta <- coef(plr_hat)
  se <- plr_hat$se
  
  plr_hat$boot_theta <- bootstrap.DML(plr_hat, data_plr[[i_setting]], y = "y", d = "d",
                                      dml_procedure = dml_procedure,
                                      score = score, se_type = score,
                                      bootstrap = "normal", nRep = n_rep_boot)
  
  set.seed(i_setting)
  params_OOP <- rep(list(rep(list(learner_pars$params), 1)), n_rep)
  Xnames = names(data_plr[[i_setting]])[names(data_plr[[i_setting]]) %in% c("y", "d", "z") == FALSE]
  data_ml = double_ml_data_from_data_frame(data_plr[[i_setting]], y_col = "y", 
                              d_cols = "d", x_cols = Xnames)
 
  double_mlplr_obj = DoubleMLPLR$new(data_ml, 
                                     ml_g = learner_pars_for_DML$mlmethod$mlmethod_g, 
                                     ml_m = learner_pars_for_DML$mlmethod$mlmethod_m, 
                                     dml_procedure = dml_procedure, 
                                     n_folds = n_folds,
                                     score = score, 
                                     n_rep = n_rep)
  
  # set params for nuisance part m
  double_mlplr_obj$set__ml_nuisance_params(learner = "ml_m", 
                                           treat_var = "d",
                                          params = learner_pars$params$params_m)
  
  # set params for nuisance part g
  double_mlplr_obj$set__ml_nuisance_params(learner = "ml_g", 
                                           treat_var = "d",
                                          params = learner_pars$params$params_g)
  
  double_mlplr_obj$fit()
  theta_obj <- double_mlplr_obj$coef
  se_obj <- double_mlplr_obj$se
  
  # bootstrap
  double_mlplr_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  boot_theta_obj = double_mlplr_obj$boot_t_stats
  
  # at the moment the object result comes without a name
  expect_equal(theta, theta_obj, tolerance = 1e-8)
  # expect_equal(se, se_obj, tolerance = 1e-8)
  expect_equal(as.vector(plr_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
}
)

