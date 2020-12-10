context("Unit tests for PLR, no cross-fitting")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(learner = c('regr.lm'),
                           dml_procedure = c('dml2'),
                           score = c('partialling out'),
                           i_setting = 1:(length(data_plr)),
                           apply_cross_fitting = FALSE,
                           n_folds = c(1,2),
                           stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(learner = c('regr.lm'),
                           dml_procedure = c('dml1', 'dml2'),
                           score = c('IV-type', 'partialling out'),
                           i_setting = 1:(length(data_plr)),
                           apply_cross_fitting = FALSE,
                           n_folds = c(1,2),
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
                                     score = score, 
                                     apply_cross_fitting = apply_cross_fitting)
  
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
  
  
  if (n_folds == 2) {
    dml_plr_obj_external = DoubleMLPLR$new(data = data_ml, 
                                       ml_g = learner_pars_for_DML$mlmethod$mlmethod_g, 
                                       ml_m = learner_pars_for_DML$mlmethod$mlmethod_m, 
                                       dml_procedure = dml_procedure, 
                                       n_folds = n_folds,
                                       score = score, 
                                       draw_sample_splitting = FALSE, apply_cross_fitting = FALSE)
  
    set.seed(i_setting)
    # set up a task and cross-validation resampling scheme in mlr3
    my_task = Task$new("help task", "regr", data_plr[[i_setting]])
    my_sampling = rsmp("holdout", ratio = 0.5)$instantiate(my_task)
    train_ids = list("train_ids" = my_sampling$train_set(1))
    test_ids = list("test_ids" = my_sampling$test_set(1))
    
    smpls = list(list(train_ids = train_ids, test_ids = test_ids))
    
    dml_plr_obj_external$set_sample_splitting(smpls)
    dml_plr_obj_external$fit()
  
    theta_external <- dml_plr_obj_external$coef
    se_external <- dml_plr_obj_external$se
    t_external <- dml_plr_obj_external$t_stat
    pval_external <- dml_plr_obj_external$pval
    ci_external <- dml_plr_obj_external$confint(level = 0.95, joint = FALSE)
    
    expect_identical(double_mlplr_obj$smpls, dml_plr_obj_external$smpls)  
    expect_equal(theta_external, theta_obj, tolerance = 1e-8)
    expect_equal(se_external, se_obj, tolerance = 1e-8)
    expect_equal(t_external, t_obj, tolerance = 1e-8)
    expect_equal(pval_external, pval_obj, tolerance = 1e-8)
    expect_equal(ci_external, ci_obj, tolerance = 1e-8)
  
  } else {
    expect_true(is.numeric(theta_obj))
    expect_true(is.numeric(se_obj))
    
  }

  # expect_equal(as.vector(plr_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  
}
)

