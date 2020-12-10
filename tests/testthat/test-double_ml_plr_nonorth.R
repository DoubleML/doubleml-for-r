context("Unit tests for PLR")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

non_orth_score = function(y, d, g_hat, m_hat, smpls) {
  u_hat = y - g_hat
  psi_a = -1*d*d
  psi_b = d*u_hat
  psis = list(psi_a = psi_a, psi_b = psi_b)
  return(psis)
  }

on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(learner = c('regr.cv_glmnet'),
                           dml_procedure = c('dml1'),
                           score = c(non_orth_score),
                           n_folds = c(3), 
                           n_rep = c(2),
                           i_setting = 1:(length(data_plr)),
                           stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(learner = c('regr.lm', 'regr.cv_glmnet'),
                           dml_procedure = c('dml1', 'dml2'),
                           score = c(non_orth_score),
                           n_folds = c(2,3), 
                           n_rep = c(1,2),
                           i_setting = 1:(length(data_plr)),
                           stringsAsFactors = FALSE)
}
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for PLR:",
                                   .cases = test_cases, {
  
  learner_pars <- get_default_mlmethod_plr(learner)
  learner_pars_for_DML <- learner_pars
  learner_pars_for_DML$params$params_g = rep(list(learner_pars_for_DML$params$params_g), 1)
  learner_pars_for_DML$params$params_m = rep(list(learner_pars_for_DML$params$params_m), 1)
  n_rep_boot = 498
  set.seed(i_setting)
  params_OOP <- rep(list(rep(list(learner_pars$params), 1)), 1)
  Xnames = names(data_plr[[i_setting]])[names(data_plr[[i_setting]]) %in% c("y", "d", "z") == FALSE]
  data_ml = double_ml_data_from_data_frame(data_plr[[i_setting]], y_col = "y", 
                              d_cols = "d", x_cols = Xnames)
                  
  double_mlplr_obj = DoubleMLPLR$new(data = data_ml, 
                                     ml_g = learner_pars_for_DML$mlmethod$mlmethod_g, 
                                     ml_m = learner_pars_for_DML$mlmethod$mlmethod_m, 
                                     dml_procedure = dml_procedure, 
                                     n_folds = n_folds,
                                     score = score)
  
  # set params for nuisance part m
  double_mlplr_obj$set_ml_nuisance_params(learner = "ml_m", 
                                           treat_var = "d",
                                           params = learner_pars$params$params_m)
  
  # set params for nuisance part g
  double_mlplr_obj$set_ml_nuisance_params(learner = "ml_g", 
                                           treat_var = "d",
                                           params = learner_pars$params$params_g)

  double_mlplr_obj$fit()
  theta_obj <- double_mlplr_obj$coef
  se_obj <- double_mlplr_obj$se
  t_obj <- double_mlplr_obj$t_stat
  pval_obj <- double_mlplr_obj$pval
  ci_obj <- double_mlplr_obj$confint(level = 0.95, joint = FALSE)
  

  expect_is(theta_obj, "numeric")  
  expect_is(se_obj, "numeric")  
  expect_is(t_obj, "numeric")  
  expect_is(pval_obj, "numeric")  
  expect_is(ci_obj, "matrix")  


  if (n_folds == 2 & n_rep == 1) {
    double_mlplr_nocf = DoubleMLPLR$new(data = data_ml, 
                                     ml_g = learner_pars_for_DML$mlmethod$mlmethod_g, 
                                     ml_m = learner_pars_for_DML$mlmethod$mlmethod_m, 
                                     dml_procedure = dml_procedure, 
                                     n_folds = n_folds,
                                     score = score, 
                                     apply_cross_fitting = FALSE)
  
    # set params for nuisance part m
    double_mlplr_nocf$set_ml_nuisance_params(learner = "ml_m", 
                                             treat_var = "d",
                                             params = learner_pars$params$params_m)
    
    # set params for nuisance part g
    double_mlplr_nocf$set_ml_nuisance_params(learner = "ml_g", 
                                             treat_var = "d",
                                             params = learner_pars$params$params_g)
    double_mlplr_nocf$fit()
    theta_nocf <- double_mlplr_nocf$coef
    se_nocf <- double_mlplr_nocf$se
    t_nocf <- double_mlplr_nocf$t_stat
    pval_nocf <- double_mlplr_nocf$pval
    ci_nocf <- double_mlplr_nocf$confint(level = 0.95, joint = FALSE)
  
    expect_is(theta_nocf, "numeric")  
    expect_is(se_nocf, "numeric")  
    expect_is(t_nocf, "numeric")  
    expect_is(pval_nocf, "numeric")  
    expect_is(ci_nocf, "matrix")  

  }
  
  # expect_equal(as.vector(plr_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  
}
)

