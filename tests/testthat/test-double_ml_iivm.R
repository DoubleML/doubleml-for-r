context("Unit tests for IIVM")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(learner = c('cv_glmnet'),
                           dml_procedure = c('dml2'),
                           score = c('LATE'),
                           i_setting = 1:(length(data_iivm)),
                           trimming_threshold = c(0),
                           stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(learner = c('cv_glmnet'),
                           dml_procedure = c('dml1', 'dml2'),
                           score = c('LATE'),
                           i_setting = 1:(length(data_iivm)),
                           trimming_threshold = c(0),
                           stringsAsFactors = FALSE)
}

test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for IIVM:",
                                   .cases = test_cases, {
  
  learner_pars <- get_default_mlmethod_iivm(learner)
  n_rep_boot = 498

  set.seed(i_setting)
  iivm_hat <- dml_irmiv(data_iivm[[i_setting]], y = "y", d = "d", z = "z",
                        k = 5, mlmethod = learner_pars$mlmethod,
                        params = learner_pars$params,
                        dml_procedure = dml_procedure, score = score,
                        se_type = score,
                        bootstrap = "normal",  nRep = n_rep_boot)
  theta <- coef(iivm_hat)
  se <- iivm_hat$se
  
    
  set.seed(i_setting)
  # params_OOP <- rep(list(rep(list(learner_pars$params), 1)), 1)
  
  Xnames = names(data_iivm[[i_setting]])[names(data_iivm[[i_setting]]) %in% c("y", "d", "z") == FALSE]
   
  data_ml = double_ml_data_from_data_frame(data_iivm[[i_setting]], y_col = "y", 
                              d_cols = "d", x_cols = Xnames, z_col = "z")

  ml_g = lrn("regr.cv_glmnet", s = "lambda.min", family = "gaussian")
  ml_prob = lrn("classif.cv_glmnet", s = "lambda.min")
  
  double_mliivm_obj = DoubleMLIIVM$new(data_ml, 
                                     n_folds = 5,
                                     ml_m =  ml_prob$clone(),
                                     ml_g = ml_g,
                                     ml_r = ml_prob$clone(),
                                     dml_procedure = dml_procedure, 
                                     trimming_threshold = trimming_threshold,
                                     score = score)
  double_mliivm_obj$fit()
  theta_obj <- double_mliivm_obj$coef
  se_obj <- double_mliivm_obj$se
  
  # bootstrap
  # double_mliivm_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj = double_mliivm_obj$boot_coef
  # 
  # at the moment the object result comes without a name
  expect_equal(theta, theta_obj, tolerance = 1e-8)
  expect_equal(se, se_obj, tolerance = 1e-8)
  # expect_equal(as.vector(iivm_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  
}
)

