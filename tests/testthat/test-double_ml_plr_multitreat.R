context("Unit tests for PLR (mulitple treatment case)")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(learner = c('regr.lm'),
                           dml_procedure = c('dml2'),
                           score = c('partialling out'),
                           i_setting = 1:(length(data_plr)),
                           stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(learner = c('regr.lm', 'regr.cv_glmnet'),
                           dml_procedure = c('dml1', 'dml2'),
                           score = c('IV-type', 'partialling out'),
                           i_setting = 1:(length(data_plr)),
                           stringsAsFactors = FALSE)
}

test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for PLR:",
                                   .cases = test_cases, {
  
  learner_pars <- get_default_mlmethod_plr(learner)
  
  learner_pars_for_DML <- learner_pars
  learner_pars_for_DML$params$params_g = rep(list(learner_pars_for_DML$params$params_g), 3)
  learner_pars_for_DML$params$params_m = rep(list(learner_pars_for_DML$params$params_m), 3)
  n_rep_boot = 498
  
  n_folds = 5
  
  set.seed(i_setting)
  plr_hat <- DML(data_plr_multi[[i_setting]], y = "y", d = c('d1', 'd2', 'd3'),
                 model = "plr",
                 k = n_folds, S = 1,
                 mlmethod = learner_pars_for_DML$mlmethod,
                 params = learner_pars_for_DML$params,
                 dml_procedure = dml_procedure, score = score,
                 se_type = score)
  theta <- coef(plr_hat)
  se <- plr_hat$se
  ci_ptwise <- confint(plr_hat, joint = FALSE, level = 0.95)
  
  set.seed(i_setting)
  plr_hat$boot_theta <- bootstrap.DML(plr_hat, data_plr_multi[[i_setting]], y = "y", 
                                      d = c('d1', 'd2', 'd3'), 
                                      dml_procedure = dml_procedure,
                                      score = score, se_type = score,
                                      bootstrap = "normal", nRep = n_rep_boot)
  ci_joint <- confint(plr_hat, joint = TRUE, level = 0.95)
  
  set.seed(i_setting)
  params_OOP <- rep(list(rep(list(learner_pars$params), 3)), 1)

  Xnames = names(data_plr_multi[[i_setting]])[names(data_plr_multi[[i_setting]]) %in% c("y", "d1", "d2", "d3", "z") == FALSE]
  data_ml = double_ml_data_from_data_frame(data_plr_multi[[i_setting]], y_col = "y", 
                              d_cols = c("d1", "d2", "d3"), x_cols = Xnames)
   
  if (learner == "regr.lm") {
    DoubleML_learner = lrn(learner)
  } else if (learner == "regr.cv_glmnet") {
    DoubleML_learner = lrn(learner, s = "lambda.min", family = "gaussian")
  }
  
  double_mlplr_obj = DoubleMLPLR$new(data_ml, 
                                     ml_g = DoubleML_learner$clone(), 
                                     ml_m = DoubleML_learner$clone(), 
                                     dml_procedure = dml_procedure, 
                                     n_folds = n_folds,
                                     score = score)
          
  double_mlplr_obj$fit()
  theta_obj <- double_mlplr_obj$coef
  se_obj <- double_mlplr_obj$se
  
  # bootstrap
  set.seed(i_setting)
  double_mlplr_obj$bootstrap(method = 'normal',  n_rep_boot = n_rep_boot)
  boot_theta_obj = double_mlplr_obj$boot_t_stat
  
  # joint confint
  ci_ptwise_obj = double_mlplr_obj$confint(joint = FALSE, level = 0.95)
  ci_joint_obj = double_mlplr_obj$confint(joint = TRUE, level = 0.95)
  
  # at the moment the object result comes without a name
  expect_equal(theta, c(theta_obj), tolerance = 1e-8)
  expect_equal(se, c(se_obj), tolerance = 1e-8)
  expect_equal(as.vector(plr_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  expect_equal(ci_ptwise, ci_ptwise_obj)
  expect_equal(ci_joint, ci_joint_obj)

}
)

