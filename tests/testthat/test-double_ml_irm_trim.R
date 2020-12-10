context("Unit tests for IRM propensity score trimming")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(learner = c('rpart'),
                           dml_procedure = c('dml2'),
                           score = c('ATTE'),
                           trimming_rule = c("truncate"),
                           trimming_threshold = c(0.05),
                           i_setting = 1:(length(data_irm)),
                           stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(learner = c('rpart'),
                           dml_procedure = c('dml1', 'dml2'),
                           score = c('ATE', 'ATTE'),
                           trimming_rule = c("truncate"),
                           trimming_threshold = c(1e-12, 0.05),
                           i_setting = 1:(length(data_irm)),
                           stringsAsFactors = FALSE)
}
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for IRM:",
                                   .cases = test_cases, {
  
  learner_pars <- get_default_mlmethod_irm(learner)
  n_rep_boot = 498
  
  # set.seed(i_setting)
  # irm_hat <- dml_irm(data_irm[[i_setting]], y = "y", d = "d",
  #                    k = 5, mlmethod = learner_pars$mlmethod,
  #                    params = learner_pars$params,
  #                    dml_procedure = dml_procedure, score = score,
  #                    se_type = score,
  #                    bootstrap = "normal",  nRep = n_rep_boot)
  # theta <- coef(irm_hat)
  # se <- irm_hat$se
  # 
    
  set.seed(i_setting)
  params_OOP <- rep(list(rep(list(learner_pars$params), 1)), 1)
  Xnames = names(data_irm[[i_setting]])[names(data_irm[[i_setting]]) %in% c("y", "d", "z") == FALSE]
  data_ml = double_ml_data_from_data_frame(data_irm[[i_setting]], y_col = "y", 
                              d_cols = "d", x_cols = Xnames)
                        
  double_mlirm_obj = DoubleMLIRM$new(data_ml, 
                                     n_folds = 5,
                                     ml_g = learner_pars$mlmethod$mlmethod_g,
                                     ml_m = learner_pars$mlmethod$mlmethod_m,
                                     dml_procedure = dml_procedure, 
                                     score = score, 
                                     trimming_rule = trimming_rule, 
                                     trimming_threshold = trimming_threshold)
    # set params for nuisance part m
  double_mlirm_obj$set_ml_nuisance_params(learner = "ml_m", 
                                           treat_var = "d",
                                          params = learner_pars$params$params_m)
  # set params for nuisance part g
  double_mlirm_obj$set_ml_nuisance_params(learner = "ml_g0", 
                                           treat_var = "d",
                                          params = learner_pars$params$params_g)
  double_mlirm_obj$set_ml_nuisance_params(learner = "ml_g1", 
                                           treat_var = "d",
                                          params = learner_pars$params$params_g)
  double_mlirm_obj$fit()
  theta_obj <- double_mlirm_obj$coef
  se_obj <- double_mlirm_obj$se
  
  # bootstrap
  # double_mlirm_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj = double_mlirm_obj$boot_coef
  # 
  
  expect_true(!is.nan(theta_obj))
  expect_true(!is.nan(se_obj))
  
  # at the moment the object result comes without a name
  # expect_equal(theta, theta_obj, tolerance = 1e-8)
  # expect_equal(se, se_obj, tolerance = 1e-8)
  # expect_equal(as.vector(irm_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  
}
)

