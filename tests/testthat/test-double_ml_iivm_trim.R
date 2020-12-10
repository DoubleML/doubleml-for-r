context("Unit tests for IIVM")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(learner = c('rpart'),
                           dml_procedure = c('dml2'),
                           score = c('LATE'),
                           i_setting = 1:(length(data_iivm)),
                           trimming_rule = c("truncate"),
                           trimming_threshold = c(0.05),
                           stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(learner = c('rpart'),
                           dml_procedure = c('dml1', 'dml2'),
                           score = c('LATE'),
                           i_setting = 1:(length(data_iivm)),
                           trimming_rule = c("truncate"),
                           trimming_threshold = c(1e-12, 0.05),
                           stringsAsFactors = FALSE)
}
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for IIVM:",
                                   .cases = test_cases, {
  
  learner_pars <- get_default_mlmethod_iivm(learner)
  n_rep_boot = 498
  
  # set.seed(i_setting)
  # iivm_hat <- dml_irmiv(data_iivm[[i_setting]], y = "y", d = "d", z = "z",
  #                       k = 5, mlmethod = learner_pars$mlmethod,
  #                       params = learner_pars$params,
  #                       dml_procedure = dml_procedure, score = score,
  #                       se_type = score,
  #                       bootstrap = "normal",  nRep = n_rep_boot)
  # theta <- coef(iivm_hat)
  # se <- iivm_hat$se
  # 
    
  set.seed(i_setting)
  # params_OOP <- rep(list(rep(list(learner_pars$params), 1)), 1)
  
  Xnames = names(data_iivm[[i_setting]])[names(data_iivm[[i_setting]]) %in% c("y", "d", "z") == FALSE]
   
  data_ml = double_ml_data_from_data_frame(data_iivm[[i_setting]], y_col = "y", 
                              d_cols = "d", x_cols = Xnames, z_col = "z")

  double_mliivm_obj = DoubleMLIIVM$new(data_ml, 
                                     n_folds = 5,
                                     ml_m = learner_pars$mlmethod$mlmethod_p,
                                     ml_g = learner_pars$mlmethod$mlmethod_mu,
                                     ml_r = learner_pars$mlmethod$mlmethod_m,
                                     dml_procedure = dml_procedure, 
                                     trimming_threshold = trimming_threshold,
                                     score = score)
  
  double_mliivm_obj$set_ml_nuisance_params(learner = "ml_m", 
                                           treat_var = "d",
                                            params = learner_pars$params$params_p)
  double_mliivm_obj$set_ml_nuisance_params(learner = "ml_g0", 
                                           treat_var = "d",
                                            params = learner_pars$params$params_mu)
  double_mliivm_obj$set_ml_nuisance_params(learner = "ml_g1", 
                                           treat_var = "d",
                                            params = learner_pars$params$params_mu)
  
  double_mliivm_obj$set_ml_nuisance_params(learner = "ml_r0", 
                                           treat_var = "d",
                                            params = learner_pars$params$params_m)
  double_mliivm_obj$set_ml_nuisance_params(learner = "ml_r1", 
                                           treat_var = "d",
                                            params = learner_pars$params$params_m)
  
  double_mliivm_obj$fit()
  theta_obj <- double_mliivm_obj$coef
  se_obj <- double_mliivm_obj$se
  
  # bootstrap
  # double_mliivm_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj = double_mliivm_obj$boot_coef
  # 
  # at the moment the object result comes without a name
  expect_true(!is.nan(theta_obj))
  expect_true(!is.nan(se_obj))
  # expect_equal(as.vector(iivm_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  
}
)

