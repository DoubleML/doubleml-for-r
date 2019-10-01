context("Regression tests for dml estimates for partial linear regression model")

lgr::get_logger("mlr3")$set_threshold("warn")

test_cases = expand.grid(model = c('plr'),
                    learner = c('regr.lm', 'regr.glmnet'),
                    dml_procedure = c('dml1', 'dml2'),
                    inf_model = c('IV-type', 'DML2018'),
                    stringsAsFactors = FALSE)
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Regression tests for dlm estimates for partial linear regression model:",
                                   .cases = test_cases, {
  
  learner_pars <- get_default_mlmethod_plr(learner)
  
  set.seed(i_setting)
  
  all_thetas <- vector('numeric', length=n_settings)
  all_ses <- vector('numeric', length=n_settings)
  for (i_setting in 1:n_settings) {
    set.seed(1234)
    cf <- mlr3::rsmp("cv", folds = 5)
    plr_hat <- dml_plr(data_plm[[i_setting]], y = "y", d = "Var1",
                       resampling = cf, mlmethod = learner_pars$mlmethod,
                       params = list(params_m = learner_pars$params$params_m,
                         params_g =  learner_pars$params$params_g),
                       dml_procedure = dml_procedure, inf_model = inf_model,
                       se_type = inf_model)
    all_thetas[i_setting] <- coef(plr_hat)
    all_ses[i_setting] <- plr_hat$se
  }
  
  all_thetas_obj <- vector('numeric', length=n_settings)
  all_ses_obj <- vector('numeric', length=n_settings)
  for (i_setting in 1:n_settings) {
    set.seed(1234)
    double_mlplr_obj = DoubleMLPLR$new(n_folds = 5,
                                       ml_learners = learner_pars$mlmethod,
                                       params = list(params_m = learner_pars$params$params_m,
                                                   params_g =  learner_pars$params$params_g),
                                       dml_procedure = dml_procedure, inf_model = inf_model)
    double_mlplr_obj$fit(data_plm[[i_setting]], y = "y", d = "Var1")
    all_thetas_obj[i_setting] <- double_mlplr_obj$coef
    all_ses_obj[i_setting] <- double_mlplr_obj$se
  }
  
  expect_equal(all_thetas, all_thetas_obj, tolerance = 1e-8)
  expect_equal(all_ses, all_ses_obj, tolerance = 1e-8)
}
)

