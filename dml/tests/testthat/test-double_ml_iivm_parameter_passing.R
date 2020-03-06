context("Unit tests for parameter passing of IIVM")

library("mlr3learners")
library("mlr3tuning")
library("paradox")
library('data.table')
library('mlr3')
lgr::get_logger("mlr3")$set_threshold("warn")

# settings for parameter provision
learner = c('rpart')

learner_list = list("mlmethod_p" = learner, "mlmethod_mu" = learner, "mlmethod_m" = learner)

test_cases = expand.grid(learner = learner,
                         dml_procedure = c('dml1', 'dml2'),
                         se_reestimate = c(FALSE),
                         inf_model = c('LATE'),
                         i_setting = 1:(length(data_iivm)),
                         n_rep_cross_fit = c(1, 3),
                         stringsAsFactors = FALSE)
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for IIVM:",
                                   .cases = test_cases, {
  
  learner_pars <- get_default_mlmethod_iivm(learner)
  n_rep_boot = 498
  n_folds = 2
  # 
  # set.seed(i_setting)
  # iivm_hat <- dml_irmiv(data_iivm[[i_setting]], y = "y", d = "d", z = "z",
  #                       k = n_folds, mlmethod = learner_pars$mlmethod,
  #                       params = learner_pars$params,
  #                       dml_procedure = dml_procedure, inf_model = inf_model,
  #                       se_type = inf_model,
  #                       bootstrap = "normal",  S = n_rep_cross_fit, 
  #                       nRep = n_rep_boot)
  # theta <- coef(iivm_hat)
  # se <- iivm_hat$se
  
  set.seed(i_setting)
  params_OOP <- rep(list(rep(list(learner_pars$params), 1)), n_rep_cross_fit)

  double_mliivm_obj_exact = DoubleMLIIVM$new(n_folds = n_folds,
                                     ml_learners = learner_pars$mlmethod,
                                     params = params_OOP,
                                     dml_procedure = dml_procedure, 
                                     se_reestimate = se_reestimate, 
                                     inf_model = inf_model, 
                                     n_rep_cross_fit = n_rep_cross_fit)
  double_mliivm_obj_exact$fit(data_iivm[[i_setting]], y = "y", d = "d", z = "z")
  theta_obj_exact <- double_mliivm_obj_exact$coef
  se_obj_exact <- double_mliivm_obj_exact$se
  
  set.seed(i_setting)
  double_mliivm_obj_null = DoubleMLIIVM$new(n_folds = n_folds,
                                     ml_learners = learner_pars$mlmethod,
                                     params = NULL,
                                     dml_procedure = dml_procedure, 
                                     se_reestimate = se_reestimate, 
                                     inf_model = inf_model, 
                                     n_rep_cross_fit = n_rep_cross_fit)
  double_mliivm_obj_null$fit(data_iivm[[i_setting]], y = "y", d = "d", z = "z")
  theta_obj_null <- double_mliivm_obj_null$coef
  se_obj_null <- double_mliivm_obj_null$se
  
  set.seed(i_setting)
  double_mliivm_obj_default = DoubleMLIIVM$new(n_folds = n_folds,
                                     ml_learners = learner_pars$mlmethod,
                                     dml_procedure = dml_procedure, 
                                     se_reestimate = se_reestimate, 
                                     inf_model = inf_model, 
                                     n_rep_cross_fit = n_rep_cross_fit)
  double_mliivm_obj_default$fit(data_iivm[[i_setting]], y = "y", d = "d", z = "z")
  theta_obj_default <- double_mliivm_obj_default$coef
  se_obj_default <- double_mliivm_obj_default$se  

  # at the moment the object result comes without a name
  # expect_equal(theta, c(d=theta_obj_exact), tolerance = 1e-8)
  expect_equal(theta_obj_null, theta_obj_exact, tolerance = 1e-8)
  expect_equal(theta_obj_null, theta_obj_default, tolerance = 1e-8)
  
  
}
)

