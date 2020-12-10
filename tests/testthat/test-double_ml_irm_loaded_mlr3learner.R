context("Unit tests for IRM")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(dml_procedure = c('dml1'),
                           score = c('ATTE'),
                           i_setting = 1:(length(data_irm)),
                           trimming_threshold = 0,
                           stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(dml_procedure = c('dml1', 'dml2'),
                           score = c('ATE', 'ATTE'),
                           i_setting = 1:(length(data_irm)),
                           trimming_threshold = 0,
                           stringsAsFactors = FALSE)
}
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for IRM:",
                                   .cases = test_cases, {
  
  set.seed(i_setting)
  Xnames = names(data_irm[[i_setting]])[names(data_irm[[i_setting]]) %in% c("y", "d", "z") == FALSE]
  data_ml = double_ml_data_from_data_frame(data_irm[[i_setting]], y_col = "y", 
                              d_cols = "d", x_cols = Xnames)

  # unloaded learners (access by name)
  learner_regr_name = "regr.ranger"
  regr_params = list("num.trees" = 10, "max.depth" = 2)
  learner_classif_name = "classif.ranger"
  classif_params = list("num.trees" = 10, "max.depth" = 2)
  # learner_regr_name = "regr.rpart"
  # regr_params = list("cp" = 0.01, "minsplit" = 20)
  # learner_classif_name = "classif.rpart"
  # classif_params = list("cp" = 0.01, "minsplit" = 20)   
  # learner_regr_name = "regr.cv_glmnet"
  # regr_params = list("s" = "lambda.min", "family" = "gaussian", "nfolds" = 5)
  # learner_classif_name = "classif.cv_glmnet"
  # classif_params = list("s" = "lambda.min", "nfolds" = 5)
  
  # loaded learners (mlr3)
  loaded_regr_learner = mlr3::lrn("regr.ranger", "num.trees" = 10, "max.depth" = 2)
  loaded_classif_learner = mlr3::lrn("classif.ranger", "num.trees" = 10, "max.depth" = 2)
  # loaded_regr_learner = mlr3::lrn("regr.rpart", "cp" = 0.1, "minsplit" = 20)
  # loaded_classif_learner = mlr3::lrn("classif.rpart", "cp" = 0.1, "minsplit" = 20)
  # loaded_regr_learner = mlr3::lrn("regr.cv_glmnet", "s" = "lambda.min", "family" = "gaussian", "nfolds" = 5)
  # loaded_classif_learner = mlr3::lrn("classif.cv_glmnet", "s" = "lambda.min", "nfolds" = 5)

  set.seed(2)
  double_mlirm = DoubleMLIRM$new(data_ml, 
                                     n_folds = 5,
                                     ml_g = learner_regr_name ,
                                     ml_m = learner_classif_name,
                                     dml_procedure = dml_procedure, 
                                     score = score, 
                                     trimming_threshold = trimming_threshold)
  # set params for nuisance part m
  double_mlirm$set_ml_nuisance_params(learner = "ml_m", 
                                           treat_var = "d",
                                          params = classif_params)
  # set params for nuisance part g
  double_mlirm$set_ml_nuisance_params(learner = "ml_g0", 
                                           treat_var = "d",
                                          params = regr_params)
  double_mlirm$set_ml_nuisance_params(learner = "ml_g1", 
                                           treat_var = "d",
                                          params = regr_params)
  double_mlirm$fit()
  theta <- double_mlirm$coef
  se <- double_mlirm$se
  
  

  set.seed(2)
  double_mlirm_loaded = DoubleMLIRM$new(data_ml, 
                                     n_folds = 5,
                                     ml_g = loaded_regr_learner ,
                                     ml_m = loaded_classif_learner,
                                     dml_procedure = dml_procedure, 
                                     score = score, 
                                     trimming_threshold = trimming_threshold)
  double_mlirm_loaded$fit()
  theta_loaded <- double_mlirm_loaded$coef
  se_loaded <- double_mlirm_loaded$se
  
  # bootstrap
  # double_mlirm_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
  # boot_theta_obj = double_mlirm_obj$boot_coef
  # 
  # at the moment the object result comes without a name
  expect_equal(theta, theta_loaded, tolerance = 1e-8)
  expect_equal(se, se_loaded, tolerance = 1e-8)
  # expect_equal(as.vector(irm_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  
}
)

