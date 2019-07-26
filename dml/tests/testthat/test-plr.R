context("Regression tests for dml estimates for partial linear regression model")

test_cases = expand.grid(model = c('plr'),
                    learner = c('regr.lm', 'regr.ranger'),
                    dml_procedure = c('dml1', 'dml2'),
                    inf_model = c('IV-type', 'DML2018'),
                    stringsAsFactors = FALSE)
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Regression tests for dlm estimates for partial linear regression model:",
                                   .cases = test_cases, {

  file_name <- paste0("rds/", test_name, ".rds")

  learner_pars <- get_default_mlmethod(learner)
  cf <- mlr::makeResampleDesc("CV", iters = 5)
  mlr::configureMlr(show.learner.output = FALSE, show.info = FALSE)

  all_thetas <- vector('numeric', length=n_settings)
  for (i_setting in 1:n_settings) {
    plr_hat <- dml_plr(data_plm[[i_setting]], y = "y", d = "d",
                       resampling = cf, mlmethod = learner_pars$mlmethod,
                       params = learner_pars$params,
                       dml_procedure = dml_procedure, inf_model = inf_model)
    all_thetas[i_setting] <- plr_hat$theta
  }

  expect_known_value(all_thetas, file_name, tolerance = 1e-4)
}
)

