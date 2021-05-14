context("Unit tests for PLIV.partialX")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

skip_on_cran()

test_cases = expand.grid(
  learner = c("regr.lm", "regr.glmnet"),
  dml_procedure = c("dml1", "dml2"),
  score = "partialling out",
  i_setting = 1:(length(data_pliv_partialX)),
  stringsAsFactors = FALSE)
test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for PLIV.partialX:",
  .cases = test_cases, {
    learner = get_default_mlmethod_pliv(learner)
    n_rep_boot = 498

    set.seed(i_setting)
    dim_z = 5
    pliv_hat = dml_pliv_partial_x(data_pliv_partialX[[i_setting]]$df,
      y = "y", d = "d", z = paste0("Z", 1:dim_z),
      n_folds = 5,
      ml_g = learner$ml_g$clone(),
      ml_m = learner$ml_m$clone(),
      ml_r = learner$ml_r$clone(),
      dml_procedure = dml_procedure, score = score)
    theta = pliv_hat$coef
    se = pliv_hat$se
    
    set.seed(i_setting)
    boot_theta = bootstrap_pliv_partial_x(pliv_hat$thetas, pliv_hat$ses,
                                 data_pliv_partialX[[i_setting]]$df,
                                 y = "y", d = "d", z = paste0("Z", 1:dim_z),
                                 n_folds = 5, smpls = pliv_hat$smpls,
                                 all_preds= pliv_hat$all_preds,
                                 bootstrap = "normal", n_rep_boot = n_rep_boot)$boot_coef

    set.seed(i_setting)
    double_mlpliv_obj = DoubleMLPLIV.partialX(data_pliv_partialX[[i_setting]]$dml_data,
                                              ml_g = learner$ml_g$clone(),
                                              ml_m = learner$ml_m$clone(),
                                              ml_r = learner$ml_r$clone(),
                                              n_folds = 5,
                                              score = score,
                                              dml_procedure = dml_procedure)

    double_mlpliv_obj$fit()
    theta_obj = double_mlpliv_obj$coef
    se_obj = double_mlpliv_obj$se

    # bootstrap
    set.seed(i_setting)
    double_mlpliv_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    boot_theta_obj = double_mlpliv_obj$boot_coef

    # at the moment the object result comes without a name
    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)
    expect_equal(as.vector(boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)
