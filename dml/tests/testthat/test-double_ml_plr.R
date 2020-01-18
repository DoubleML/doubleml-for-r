context("Unit tests for PLR")

library("mlr3learners")
library('data.table')
library('mlr3')

lgr::get_logger("mlr3")$set_threshold("warn")

test_cases = expand.grid(learner = c('regr.lm', 'regr.glmnet'),
                         dml_procedure = c('dml1', 'dml2'),
                         inf_model = c('IV-type', 'DML2018'),
                         i_setting = 1:(length(data_plr)),
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
                 k = n_folds, S = 1,
                 mlmethod = learner_pars_for_DML$mlmethod,
                 params = learner_pars_for_DML$params,
                 dml_procedure = dml_procedure, inf_model = inf_model,
                 se_type = inf_model,
                 bootstrap = "normal", nRep = n_rep_boot)
  theta <- coef(plr_hat)
  se <- plr_hat$se
  t <- plr_hat$t
  pval <- plr_hat$pval
  ci <- confint(plr_hat, level = 0.95, joint = FALSE)
 
  set.seed(i_setting)
  double_mlplr_obj = DoubleMLPLR$new(n_folds = n_folds,
                                     ml_learners = learner_pars$mlmethod,
                                     params = learner_pars$params,
                                     dml_procedure = dml_procedure, inf_model = inf_model)
  double_mlplr_obj$fit(data_plr[[i_setting]], y = "y", d = "d")
  theta_obj <- double_mlplr_obj$coef
  se_obj <- double_mlplr_obj$se
  t_obj <- double_mlplr_obj$t
  pval_obj <- double_mlplr_obj$pval
  ci_obj <- double_mlplr_obj$confint(level = 0.95, joint = FALSE)
  
  # bootstrap
  double_mlplr_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  boot_theta_obj = double_mlplr_obj$boot_coef

  expect_equal(theta, theta_obj, tolerance = 1e-8)
  expect_equal(se, se_obj, tolerance = 1e-8)
  expect_equal(t, t_obj, tolerance = 1e-8)
  expect_equal(pval, pval_obj, tolerance = 1e-8)
  expect_equal(ci, ci_obj, tolerance = 1e-8)

  expect_equal(as.vector(plr_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  
}
)

