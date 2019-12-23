context("Unit tests for IIVM")

library("mlr3learners")
library('data.table')
library('mlr3')

lgr::get_logger("mlr3")$set_threshold("warn")

test_cases = expand.grid(learner = c('glmnet'),
                         dml_procedure = c('dml1', 'dml2'),
                         inf_model = c('LATE'),
                         i_setting = 1:(length(data_iivm)),
                         stringsAsFactors = FALSE)
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for IIVM:",
                                   .cases = test_cases, {
  
  learner_pars <- get_default_mlmethod_iivm(learner)
  n_rep_boot = 498
  
  set.seed(i_setting)
  cf <- mlr3::rsmp("cv", folds = 5)
  iivm_hat <- dml_irmiv(data_iivm[[i_setting]], y = "y", d = "d", z = "z",
                        resampling = cf, mlmethod = learner_pars$mlmethod,
                        params = learner_pars$params,
                        dml_procedure = dml_procedure, inf_model = inf_model,
                        se_type = inf_model,
                        bootstrap = "normal",  nRep = n_rep_boot)
  theta <- coef(iivm_hat)
  se <- iivm_hat$se
  
    
  set.seed(i_setting)
  double_mliivm_obj = DoubleMLIIVM$new(n_folds = 5,
                                     ml_learners = learner_pars$mlmethod,
                                     params = learner_pars$params,
                                     dml_procedure = dml_procedure, inf_model = inf_model)
  double_mliivm_obj$fit(data_iivm[[i_setting]], y = "y", d = "d", z = "z")
  theta_obj <- double_mliivm_obj$coef
  se_obj <- double_mliivm_obj$se
  
  # bootstrap
  double_mliivm_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  boot_theta_obj = double_mliivm_obj$boot_coef
  
  # at the moment the object result comes without a name
  expect_equal(theta, c(d=theta_obj), tolerance = 1e-8)
  expect_equal(se, c(d=se_obj), tolerance = 1e-8)
  expect_equal(as.vector(iivm_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  
}
)

