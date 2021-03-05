context("Unit tests for parameter passing of IRM")

lgr::get_logger("mlr3")$set_threshold("warn")

# settings for parameter provision
learner = c('rpart')

learner_list = list("mlmethod_m" = learner, "mlmethod_g" = learner)

on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
test_cases = expand.grid(learner = learner,
                         dml_procedure = c('dml2'),
                         score = c('ATE'),
                         i_setting = 1:(length(data_irm)),
                         n_rep = c(1),
                         trimming_threshold = 0,
                         stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(learner = learner,
                           dml_procedure = c('dml1', 'dml2'),
                           score = c('ATE', 'ATTE'),
                           i_setting = 1:(length(data_irm)),
                           n_rep = c(1, 3),
                           trimming_threshold = 0,
                           stringsAsFactors = FALSE)
}
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for parameter passing of IRM:",
                                   .cases = test_cases, {
  
  n_rep_boot = 498
  learner_pars <- get_default_mlmethod_irm(learner)

  if (n_rep == 1) {
    set.seed(i_setting)
    irm_hat <- dml_irm(data_irm[[i_setting]], y = "y", d = "d",
                       k = 5,
                       mlmethod = learner_pars$mlmethod,
                       params = learner_pars$params,
                       dml_procedure = dml_procedure, score = score,
                       se_type = score,
                       bootstrap = "normal",  nRep = n_rep_boot)
    theta <- coef(irm_hat)
    se <- irm_hat$se
  }
  
  set.seed(i_setting)
  params_OOP <- rep(list(rep(list(learner_pars$params), 1)), n_rep)

  Xnames = names(data_irm[[i_setting]])[names(data_irm[[i_setting]]) %in% c("y", "d") == FALSE]
  
  data_ml = double_ml_data_from_data_frame(data_irm[[i_setting]], y_col = "y", 
                              d_cols = c("d"), x_cols = Xnames)

  double_mlirm_obj_once = DoubleMLIRM$new(data_ml, 
                                     n_folds = 5,
                                     ml_g = lrn(learner_pars$mlmethod$mlmethod_g),
                                     ml_m = lrn(learner_pars$mlmethod$mlmethod_m),
                                     dml_procedure = dml_procedure, 
                                     score = score,
                                     n_rep = n_rep, trimming_threshold = trimming_threshold)
  
  # set params for nuisance part m
  double_mlirm_obj_once$set_ml_nuisance_params(learner = "ml_m", 
                                           treat_var = "d",
                                          params = learner_pars$params$params_m)  
  # set params for nuisance part g
  double_mlirm_obj_once$set_ml_nuisance_params(learner = "ml_g0", 
                                           treat_var = "d",
                                          params = learner_pars$params$params_g)
  double_mlirm_obj_once$set_ml_nuisance_params(learner = "ml_g1", 
                                           treat_var = "d",
                                          params = learner_pars$params$params_g)
  
  double_mlirm_obj_once$fit()
  theta_obj_once <- double_mlirm_obj_once$coef
  se_obj_once <- double_mlirm_obj_once$se
  
  # bootstrap
  # double_mlirm_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj = double_mlirm_obj$boot_coef
  export_params_exact_g = rep(list(rep(list(learner_pars$params$params_g), 5)), n_rep)
  export_params_exact_m = rep(list(rep(list(learner_pars$params$params_m), 5)), n_rep)


  set.seed(i_setting)
  double_mlirm_obj_exact = DoubleMLIRM$new(data_ml, 
                                     n_folds = 5,
                                     ml_g = lrn(learner_pars$mlmethod$mlmethod_g),
                                     ml_m = lrn(learner_pars$mlmethod$mlmethod_m),
                                     dml_procedure = dml_procedure, 
                                     score = score,
                                     n_rep = n_rep, trimming_threshold = trimming_threshold)
  
  # set params for nuisance part m
  double_mlirm_obj_exact$set_ml_nuisance_params(treat_var = "d", learner = "ml_m", 
                                                params = export_params_exact_m, 
                                                set_fold_specific = TRUE)
  # set params for nuisance part g
  double_mlirm_obj_exact$set_ml_nuisance_params(treat_var = "d", learner = "ml_g0", 
                                                params = export_params_exact_g, 
                                                set_fold_specific = TRUE)
  double_mlirm_obj_exact$set_ml_nuisance_params(treat_var = "d", learner = "ml_g1", 
                                                params = export_params_exact_g, 
                                                set_fold_specific = TRUE)
  # set params for nuisance part g
  # set params for nuisance part g
  
  double_mlirm_obj_exact$fit()
  theta_obj_exact <- double_mlirm_obj_exact$coef
  se_obj_exact <- double_mlirm_obj_exact$se
  
  
  set.seed(i_setting)
  double_mlirm_obj_default = DoubleMLIRM$new(data_ml, 
                                     n_folds = 5,
                                     ml_g = learner_pars$mlmethod$mlmethod_g,
                                     ml_m = learner_pars$mlmethod$mlmethod_m,
                                     dml_procedure = dml_procedure, 
                                     score = score,
                                     n_rep = n_rep, trimming_threshold = trimming_threshold)
  
  double_mlirm_obj_default$fit()
  theta_obj_default <- double_mlirm_obj_default$coef
  se_obj_default <- double_mlirm_obj_default$se
  
  # at the moment the object result comes without a name
  if (n_rep == 1) {
    expect_equal(theta, theta_obj_once, tolerance = 1e-8)
  }
  expect_equal(theta_obj_default, theta_obj_once, tolerance = 1e-8)
  expect_equal(theta_obj_once, theta_obj_exact, tolerance = 1e-8)
  # expect_equal(se, c(d=se_obj), tolerance = 1e-8)
  # expect_equal(as.vector(irm_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  
}
)

