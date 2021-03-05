context("Unit tests for PLR with a classifier for ml_m")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(g_learner = c('regr.cv_glmnet', 'classif.cv_glmnet'),
                           m_learner = c('classif.cv_glmnet'),
                           dml_procedure = c('dml2'),
                           score = c('partialling out'),
                           i_setting = 1:(length(data_plr)),
                           stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(g_learner = c('regr.cv_glmnet'),
                           m_learner = c('classif.cv_glmnet'),
                           dml_procedure = c('dml1', 'dml2'),
                           score = c('IV-type', 'partialling out'),
                           i_setting = 1:(length(data_plr)),
                           stringsAsFactors = FALSE)
}
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for PLR with classifier for ml_m:",
                                   .cases = test_cases, {
  n_rep_boot = 498
  n_folds = 3
                                     
  if (g_learner == "regr.cv_glmnet") {  
    learner_pars <- get_default_mlmethod_plr(g_learner)
    learner_pars_for_DML <- learner_pars
    learner_pars_for_DML$params$params_g = rep(list(learner_pars_for_DML$params$params_g), 1)
    learner_pars_for_DML$params$params_m = rep(list(learner_pars_for_DML$params$params_m), 1)

    set.seed(i_setting)
    params_OOP <- rep(list(rep(list(learner_pars$params), 1)), 1)
    Xnames = names(data_irm[[i_setting]])[names(data_plr[[i_setting]]) %in% c("y", "d", "z") == FALSE]
    data_ml = double_ml_data_from_data_frame(data_irm[[i_setting]], y_col = "y", 
                                d_cols = "d", x_cols = Xnames)
                    
    double_mlplr_obj = DoubleMLPLR$new(data = data_ml, 
                                       ml_g = lrn(g_learner), 
                                       ml_m = lrn(m_learner), 
                                       dml_procedure = dml_procedure, 
                                       n_folds = n_folds,
                                       score = score)
    double_mlplr_obj$fit()
    theta_obj <- double_mlplr_obj$coef
    se_obj <- double_mlplr_obj$se
    t_obj <- double_mlplr_obj$t_stat
    pval_obj <- double_mlplr_obj$pval
    ci_obj <- double_mlplr_obj$confint(level = 0.95, joint = FALSE)
    
    expect_is(theta_obj, "numeric")
    expect_is(se_obj, "numeric")
    expect_is(t_obj, "numeric")
    expect_is( pval_obj, "numeric")
    expect_is(ci_obj, "matrix")
    
  } else if (g_learner == "classif.cv_glmnet") 
    
    expect_error(DoubleMLPLR$new(data = data_ml, 
                                       ml_g = lrn(g_learner), 
                                       ml_m = lrn(m_learner), 
                                       dml_procedure = dml_procedure, 
                                       n_folds = n_folds,
                                       score = score))
}
)

