context("Unit tests for PLR (p_adjust)")

library("mlr3learners")
library('data.table')
library('mlr3')

lgr::get_logger("mlr3")$set_threshold("warn")

test_cases = expand.grid(learner = c('regr.cv_glmnet'),
                         dml_procedure = c('dml1', 'dml2'),
                         se_reestimate = c(FALSE),
                         score = c('IV-type', 'partialling out'),
                         method = c("romano-wolf", "bonferroni"),
                         i_setting = 1:(length(data_plr)),
                         stringsAsFactors = FALSE)
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Unit tests for PLR:",
                                   .cases = test_cases, {
  
  learner_pars <- get_default_mlmethod_plr(learner)
  learner_pars_for_DML <- learner_pars
  learner_pars_for_DML$params$params_g = rep(list(learner_pars_for_DML$params$params_g), 3)
  learner_pars_for_DML$params$params_m = rep(list(learner_pars_for_DML$params$params_m), 3)
  n_rep_boot = 498
  n_folds = 5
  
  set.seed(1)
  n = 100 #sample size
  p = 25 # number of variables
  s = 3 # nubmer of non-zero variables
  X = matrix(rnorm(n*p), ncol=p)
  colnames(X) = paste("X", 1:p, sep="")
  beta = c(rep(3,s), rep(0,p-s))
  y = 1 + X%*%beta + rnorm(n)
  data = data.frame(cbind(y,X))
  colnames(data)[1] <- "y"
  
  # index for hypoth testing
  k = 10
  data_ml = double_ml_data_from_data_frame(data, x_cols = colnames(X)[(k+1):p], 
                                                 y_col = "y", 
                                                 d_cols = colnames(X)[1:k]) 
  double_mlplr_obj = DoubleMLPLR$new(data_ml, 
                                     ml_g = learner_pars_for_DML$mlmethod$mlmethod_g, 
                                     ml_m = learner_pars_for_DML$mlmethod$mlmethod_m, 
                                     dml_procedure = dml_procedure, 
                                     n_folds = n_folds,
                                     score = score)
  lapply(data_ml$d_cols, function(x) {
          # set params for nuisance part m
          double_mlplr_obj$set__ml_nuisance_params(learner = "ml_m", 
                                                   treat_var = x,
                                                   params = learner_pars$params$params_m)
          # set params for nuisance part g
          double_mlplr_obj$set__ml_nuisance_params(learner = "ml_g", 
                                                   treat_var = x,
                                                  params = learner_pars$params$params_g)})
  double_mlplr_obj$fit()
  double_mlplr_obj$bootstrap()
  double_mlplr_obj$p_adjust(method = method)
  expect_true(is.matrix(double_mlplr_obj$p_adjust(method = method)))
}
)

