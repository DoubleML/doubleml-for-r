context("Unit tests for IRM")

library("mlr3learners")
library('data.table')
library('mlr3')

lgr::get_logger("mlr3")$set_threshold("warn")

test_cases = expand.grid(learner = c('glmnet'),
                         dml_procedure = c('dml1', 'dml2'),
                         inf_model = c('ATE', 'ATET'),
                         se_reestimate = c(FALSE),
                         i_setting = 1:(length(data_irm)),
                         stringsAsFactors = FALSE)
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for IRM:",
                                   .cases = test_cases, {
  
  learner_pars <- get_default_mlmethod_irm(learner)
  n_rep_boot = 498
  
  set.seed(i_setting)
  irm_hat <- dml_irm(data_irm[[i_setting]], y = "y", d = "d",
                     k = 5, mlmethod = learner_pars$mlmethod,
                     params = learner_pars$params,
                     dml_procedure = dml_procedure, inf_model = inf_model,
                     se_type = inf_model,
                     bootstrap = "normal",  nRep = n_rep_boot)
  theta <- coef(irm_hat)
  se <- irm_hat$se
  
    
  set.seed(i_setting)
  params_OOP <- rep(list(rep(list(learner_pars$params), 1)), 1)

  double_mlirm_obj = DoubleMLIRM$new(n_folds = 5,
                                     ml_learners = learner_pars$mlmethod,
                                     params = params_OOP,
                                     dml_procedure = dml_procedure, 
                                     se_reestimate = se_reestimate, inf_model = inf_model)
  double_mlirm_obj$fit(data_irm[[i_setting]], y = "y", d = "d")
  theta_obj <- double_mlirm_obj$coef
  se_obj <- double_mlirm_obj$se
  
  # bootstrap
  double_mlirm_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  boot_theta_obj = double_mlirm_obj$boot_coef
  
  # at the moment the object result comes without a name
  expect_equal(theta, c(d=theta_obj), tolerance = 1e-8)
  expect_equal(se, c(d=se_obj), tolerance = 1e-8)
  expect_equal(as.vector(irm_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  
}
)

