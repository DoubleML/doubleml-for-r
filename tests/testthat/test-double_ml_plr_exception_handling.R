context("Unit tests for exception handling if fit() or bootstrap() was not run yet; uses PLR")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")
on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(learner = c('regr.lm'),
                           dml_procedure = c("dml1"),
                           score = c('IV-type'),
                           set_params = c(FALSE),
                           n_folds = c(4),
                           n_rep = c(1),
                           apply_cross_fitting = c(TRUE),
                           i_setting = c(1),
                           stringsAsFactors = FALSE)
} else {
  
  test_cases = expand.grid(learner = c('regr.cv_glmnet'),
                           dml_procedure = c("dml1"),
                           score = c('IV-type', 'partialling out'),
                           set_params = c(TRUE, FALSE),
                           n_folds = c(1,5),
                           n_rep = c(1,2),
                           apply_cross_fitting = c(TRUE, FALSE),
                           i_setting = c(1),
                           stringsAsFactors = FALSE)
}
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for exception handling of PLR:",
                                   .cases = test_cases, {
  
  learner_pars <- get_default_mlmethod_plr(learner)
  learner_pars_for_DML <- learner_pars
  learner_pars_for_DML$params$params_g = rep(list(learner_pars_for_DML$params$params_g), 1)
  learner_pars_for_DML$params$params_m = rep(list(learner_pars_for_DML$params$params_m), 1)
  n_rep_boot = 498
  
  params_OOP <- rep(list(rep(list(learner_pars$params), 1)), 1)

  data_ml = double_ml_data_from_data_frame(data_plr[[i_setting]], y_col = "y", 
                              d_cols = c("d", "X1"))
                  
  expect_error(DoubleML$new())
  
  
  if ((n_folds > 1 & !apply_cross_fitting) | 
      (n_rep > 1 & !apply_cross_fitting) |
      (n_rep > 1 & n_folds == 1 & apply_cross_fitting)) {
    expect_error(DoubleMLPLR$new(data = data_ml, 
                                     ml_g = learner_pars_for_DML$mlmethod$mlmethod_g, 
                                     ml_m = mlr3::lrn(learner_pars_for_DML$mlmethod$mlmethod_m),
                                     dml_procedure = dml_procedure, 
                                     n_folds = n_folds,
                                     n_rep = n_rep,
                                     score = score, 
                                     apply_cross_fitting = apply_cross_fitting))
  } else {
  
    double_mlplr_obj = DoubleMLPLR$new(data = data_ml, 
                                       ml_g = learner_pars_for_DML$mlmethod$mlmethod_g, 
                                       ml_m = mlr3::lrn(learner_pars_for_DML$mlmethod$mlmethod_m),
                                       dml_procedure = dml_procedure, 
                                       n_folds = n_folds,
                                       n_rep = n_rep,
                                       score = score, 
                                       apply_cross_fitting = apply_cross_fitting)
  
    if (set_params) {
      # set params for nuisance part m
      double_mlplr_obj$set_ml_nuisance_params(learner = "ml_m", 
                                               treat_var = "d",
                                              params = learner_pars$params$params_m)
      
      # set params for nuisance part g
      double_mlplr_obj$set_ml_nuisance_params(learner = "ml_g", 
                                               treat_var = "d",
                                              params = learner_pars$params$params_g)
    }
  
    # currently, no warning or message printed
    utils::capture.output(double_mlplr_obj$summary(), file = NULL)
    expect_error(double_mlplr_obj$bootstrap(method = 'normal',  n_rep_boot = n_rep_boot))
  
    double_mlplr_obj$fit()
    
    utils::capture.output(double_mlplr_obj$print(), file = NULL)
    utils::capture.output(expect_is(double_mlplr_obj$summary(), "matrix"), file = NULL)
    
    expect_error(double_mlplr_obj$confint(level = 1.2))
    expect_error(double_mlplr_obj$confint(joint = TRUE, level = 0.95))
  }

  # expect_equal(as.vector(plr_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  
}
)

