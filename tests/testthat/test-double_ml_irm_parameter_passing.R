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
                         stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(learner = learner,
                           dml_procedure = c('dml1', 'dml2'),
                           score = c('ATE', 'ATTE'),
                           i_setting = 1:(length(data_irm)),
                           n_rep = c(1, 3),
                           stringsAsFactors = FALSE)
}
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for parameter passing of IRM:",
                                   .cases = test_cases, {
  
  n_rep_boot = 498
  n_folds = 2

  learner_pars <- get_default_mlmethod_irm(learner)

  
  # # TBD: Functional Test Case
  # set.seed(i_setting)
  # irm_hat <- dml_irm(data_irm[[i_setting]], y = "y", d = "d",
  #                    k = n_folds, mlmethod = learner_pars$mlmethod,
  #                    params = learner_pars$params,
  #                    dml_procedure = dml_procedure, score = score,
  #                    se_type = score,
  #                    bootstrap = "normal",  S = n_rep,
  #                    nRep = n_rep_boot)
  # theta <- coef(irm_hat)
  # se <- irm_hat$se

  set.seed(i_setting)
  params_OOP <- rep(list(rep(list(learner_pars$params), 1)), n_rep)

  Xnames = names(data_irm[[i_setting]])[names(data_irm[[i_setting]]) %in% c("y", "d") == FALSE]
  
  data_ml = double_ml_data_from_data_frame(data_irm[[i_setting]], y_col = "y", 
                              d_cols = c("d"), x_cols = Xnames)

  double_mlirm_obj_once = DoubleMLIRM$new(data_ml, 
                                     n_folds = 5,
                                     ml_g = learner_pars$mlmethod$mlmethod_g,
                                     ml_m = learner_pars$mlmethod$mlmethod_m,
                                     dml_procedure = dml_procedure, 
                                     score = score,
                                     n_rep = n_rep)
  
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
  
  set.seed(i_setting)
  double_mlirm_obj_default = DoubleMLIRM$new(data_ml, 
                                     n_folds = 5,
                                     ml_g = learner_pars$mlmethod$mlmethod_g,
                                     ml_m = learner_pars$mlmethod$mlmethod_m,
                                     dml_procedure = dml_procedure, 
                                     score = score,
                                     n_rep = n_rep)
  
  double_mlirm_obj_default$fit()
  theta_obj_default <- double_mlirm_obj_default$coef
  se_obj_default <- double_mlirm_obj_default$se
  
  # at the moment the object result comes without a name
  # expect_equal(theta, c(d=theta_obj_once), tolerance = 1e-8)
  expect_equal(theta_obj_default, theta_obj_once, tolerance = 1e-8)
  # expect_equal(se, c(d=se_obj), tolerance = 1e-8)
  # expect_equal(as.vector(irm_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  
}
)

