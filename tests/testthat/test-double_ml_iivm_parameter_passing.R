context("Unit tests for parameter passing of IIVM")

lgr::get_logger("mlr3")$set_threshold("warn")

# settings for parameter provision
learner = c('rpart')

learner_list = list("mlmethod_p" = learner, "mlmethod_mu" = learner, "mlmethod_m" = learner)

on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(learner = learner,
                           dml_procedure = c('dml2'),
                           score = c('LATE'),
                           i_setting = 1:(length(data_iivm)),
                           n_rep = c(1),
                           stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(learner = learner,
                           dml_procedure = c('dml1', 'dml2'),
                           score = c('LATE'),
                           i_setting = 1:(length(data_iivm)),
                           n_rep = c(1, 3),
                           stringsAsFactors = FALSE)
}
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
  #                       dml_procedure = dml_procedure, score = score,
  #                       se_type = score,
  #                       bootstrap = "normal",  S = n_rep, 
  #                       nRep = n_rep_boot)
  # theta <- coef(iivm_hat)
  # se <- iivm_hat$se
  
  set.seed(i_setting)

  Xnames = names(data_iivm[[i_setting]])[names(data_iivm[[i_setting]]) %in% c("y", "d", "z") == FALSE]
  data_ml = double_ml_data_from_data_frame(data_iivm[[i_setting]], y_col = "y", 
                              d_cols = "d", x_cols = Xnames, z_col = "z")
  
  double_mliivm_obj_once =  DoubleMLIIVM$new(data_ml, 
                                     n_folds = 5,
                                     n_rep = n_rep,
                                     ml_m = learner_pars$mlmethod$mlmethod_p,
                                     ml_g = learner_pars$mlmethod$mlmethod_mu,
                                     ml_r = learner_pars$mlmethod$mlmethod_m,
                                     dml_procedure = dml_procedure, 
                                     score = score)
  
  double_mliivm_obj_once$set_ml_nuisance_params(learner = "ml_m", 
                                           treat_var = "d",
                                            params = learner_pars$params$params_p)
  double_mliivm_obj_once$set_ml_nuisance_params(learner = "ml_g0", 
                                           treat_var = "d",
                                            params = learner_pars$params$params_mu)
  double_mliivm_obj_once$set_ml_nuisance_params(learner = "ml_g1", 
                                           treat_var = "d",
                                            params = learner_pars$params$params_mu)
  double_mliivm_obj_once$set_ml_nuisance_params(learner = "ml_r0", 
                                           treat_var = "d",
                                            params = learner_pars$params$params_m)
  double_mliivm_obj_once$set_ml_nuisance_params(learner = "ml_r1", 
                                           treat_var = "d",
                                            params = learner_pars$params$params_m)
  
  double_mliivm_obj_once$fit()
  theta_obj_once <- double_mliivm_obj_once$coef
  se_obj_once <- double_mliivm_obj_once$se
  
  # Exact parameter passing (foldwise)
  export_params_exact_g = rep(list(rep(list(learner_pars$params$params_mu), 5)), n_rep)
  export_params_exact_r = rep(list(rep(list(learner_pars$params$params_m), 5)), n_rep)
  export_params_exact_m = rep(list(rep(list(learner_pars$params$params_p), 5)), n_rep)
  
  set.seed(i_setting)
  double_mliivm_obj_exact =  DoubleMLIIVM$new(data_ml, 
                                       n_folds = 5,
                                       n_rep = n_rep,
                                       ml_m = learner_pars$mlmethod$mlmethod_p,
                                       ml_g = learner_pars$mlmethod$mlmethod_mu,
                                       ml_r = learner_pars$mlmethod$mlmethod_m,
                                       dml_procedure = dml_procedure, 
                                       score = score)
  
  double_mliivm_obj_exact$set_ml_nuisance_params(treat_var = "d", learner = "ml_m", 
                                                params = export_params_exact_m, 
                                                set_fold_specific = TRUE)
  double_mliivm_obj_exact$set_ml_nuisance_params(treat_var = "d", learner = "ml_r0", 
                                                params = export_params_exact_r, 
                                                set_fold_specific = TRUE)
  double_mliivm_obj_exact$set_ml_nuisance_params(treat_var = "d", learner = "ml_r1", 
                                                params = export_params_exact_r, 
                                                set_fold_specific = TRUE)
  double_mliivm_obj_exact$set_ml_nuisance_params(treat_var = "d", learner = "ml_g0", 
                                                params = export_params_exact_g, 
                                                set_fold_specific = TRUE)
  double_mliivm_obj_exact$set_ml_nuisance_params(treat_var = "d", learner = "ml_g1", 
                                                params = export_params_exact_g, 
                                                set_fold_specific = TRUE)
  
  double_mliivm_obj_exact$fit()
  theta_obj_exact <- double_mliivm_obj_exact$coef
  se_obj_exact <- double_mliivm_obj_exact$se
  
  set.seed(i_setting)
  double_mliivm_obj_default = DoubleMLIIVM$new(data_ml, 
                                     n_folds = 5,
                                     n_rep = n_rep,
                                     ml_m = learner_pars$mlmethod$mlmethod_p,
                                     ml_g = learner_pars$mlmethod$mlmethod_mu,
                                     ml_r = learner_pars$mlmethod$mlmethod_m,
                                     dml_procedure = dml_procedure, 
                                     score = score)
  double_mliivm_obj_default$fit()
  theta_obj_default <- double_mliivm_obj_default$coef
  se_obj_default <- double_mliivm_obj_default$se  

  # at the moment the object result comes without a name
  # expect_equal(theta, c(d=theta_obj_once), tolerance = 1e-8)
  expect_equal(theta_obj_default, theta_obj_once, tolerance = 1e-8)
  expect_equal(theta_obj_exact, theta_obj_once, tolerance = 1e-8)
  
}
)

