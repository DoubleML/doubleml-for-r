context("Unit tests for PLR (mulitple treatment case)")

library("mlr3learners")
library('data.table')
library('mlr3')

lgr::get_logger("mlr3")$set_threshold("warn")

test_cases = expand.grid(learner = c('regr.lm', 'regr.glmnet'),
                         dml_procedure = c('dml1', 'dml2'),
                         se_reestimate = c(FALSE),
                         inf_model = c('IV-type', 'DML2018'),
                         i_setting = 1:(length(data_plr)),
                         stringsAsFactors = FALSE)
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for PLR:",
                                   .cases = test_cases, {
  
  learner_pars <- get_default_mlmethod_plr(learner)
  learner_pars_for_DML <- learner_pars
  learner_pars_for_DML$params$params_g = rep(list(learner_pars_for_DML$params$params_g), 3)
  learner_pars_for_DML$params$params_m = rep(list(learner_pars_for_DML$params$params_m), 3)
  n_rep_boot = 498
  n_folds = 5
  
  set.seed(i_setting)
  # To get matching results with the oop we instatiate the resampling here in
  # order to get the same sample split for all treatment variables
  dummy_task = Task$new('dummy_resampling', 'regr', data_plr_multi[[i_setting]])
  resampling <- rsmp("cv", folds = n_folds)$instantiate(dummy_task)
  train_sets <- lapply(1:n_folds, function(x) resampling$train_set(x))
  test_sets <- lapply(1:n_folds, function(x) resampling$test_set(x))
  resampling <- rsmp("custom")$instantiate(dummy_task,
                                           train_sets,
                                           test_sets)
  
  plr_hat <- DML(data_plr_multi[[i_setting]], y = "y", d = c('d1', 'd2', 'd3'),
                 model = "plr",
                 k = n_folds, S = 1,
                 resampling = resampling,
                 mlmethod = learner_pars_for_DML$mlmethod,
                 params = learner_pars_for_DML$params,
                 dml_procedure = dml_procedure, inf_model = inf_model,
                 se_type = inf_model,
                 bootstrap = "none", nRep = n_rep_boot) # deactivate bootstrap to prevent seed issues with multi-treat as bootstrap is done in between
  theta <- coef(plr_hat)
  se <- plr_hat$se
  
  
  set.seed(i_setting)
  double_mlplr_obj = DoubleMLPLR$new(n_folds = n_folds,
                                     ml_learners = learner_pars$mlmethod,
                                     params = learner_pars$params,
                                     dml_procedure = dml_procedure, 
                                     se_reestimate = se_reestimate,
                                     inf_model = inf_model)
  double_mlplr_obj$fit(data_plr_multi[[i_setting]], y = "y", d = c('d1', 'd2', 'd3'))
  theta_obj <- double_mlplr_obj$coef
  se_obj <- double_mlplr_obj$se
  
  # at the moment the object result comes without a name
  expect_equal(theta, c(d=theta_obj), tolerance = 1e-8)
  expect_equal(se, c(d=se_obj), tolerance = 1e-8)
  
}
)

