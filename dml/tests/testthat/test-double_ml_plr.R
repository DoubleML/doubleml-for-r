context("Unit tests for PLR")

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
  
  set.seed(i_setting)
  cf <- mlr3::rsmp("cv", folds = 5)
  plr_hat <- dml_plr(data_plr[[i_setting]], y = "y", d = "d",
                     resampling = cf, mlmethod = learner_pars$mlmethod,
                     params = learner_pars$params,
                     dml_procedure = dml_procedure, inf_model = inf_model,
                     se_type = inf_model)
  theta <- coef(plr_hat)
  se <- plr_hat$se
  
    
  set.seed(i_setting)
  double_mlplr_obj = DoubleMLPLR$new(n_folds = 5,
                                     ml_learners = learner_pars$mlmethod,
                                     params = learner_pars$params,
                                     dml_procedure = dml_procedure, inf_model = inf_model)
  double_mlplr_obj$fit(data_plr[[i_setting]], y = "y", d = "d")
  theta_obj <- double_mlplr_obj$coef
  se_obj <- double_mlplr_obj$se
  
  # at the moment the object result comes without a name
  expect_equal(theta, c(d=theta_obj), tolerance = 1e-8)
  expect_equal(se, c(d=se_obj), tolerance = 1e-8)
}
)

