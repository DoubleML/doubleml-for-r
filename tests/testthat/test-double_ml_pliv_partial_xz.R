context("Unit tests for PLIV.partialXZ")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

skip_on_cran()

test_cases = expand.grid(
  learner = c("regr.lm", "regr.glmnet"),
  dml_procedure = c("dml1", "dml2"),
  score = "partialling out",
  stringsAsFactors = FALSE)
test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for PLIV.partialXZ:",
  .cases = test_cases, {
    learner_pars = get_default_mlmethod_pliv(learner)
    n_rep_boot = 498

    set.seed(3141)
    dim_z = 150
    pliv_hat = dml_pliv_partial_xz(data_pliv_partialXZ$df,
      y = "y", d = "d", z = paste0("Z", 1:dim_z),
      n_folds = 5,
      ml_g = learner_pars$ml_g$clone(),
      ml_m = learner_pars$ml_m$clone(),
      ml_r = learner_pars$ml_r$clone(),
      dml_procedure = dml_procedure, score = score)
    theta = pliv_hat$coef
    se = pliv_hat$se

    boot_theta = bootstrap_pliv_partial_xz(pliv_hat$thetas, pliv_hat$ses,
                                 data_pliv_partialXZ$df,
                                 y = "y", d = "d", z = paste0("Z", 1:dim_z),
                                 n_folds = 5, smpls = pliv_hat$smpls,
                                 all_preds= pliv_hat$all_preds,
                                 bootstrap = "normal", n_rep_boot = n_rep_boot)$boot_coef

    set.seed(3141)
    double_mlpliv_obj = DoubleMLPLIV.partialXZ(data_pliv_partialXZ$dml_data,
                                               ml_g = learner_pars$ml_g$clone(),
                                               ml_m = learner_pars$ml_m$clone(),
                                               ml_r = learner_pars$ml_r$clone(),
                                               n_folds = 5,
                                               score = score,
                                               dml_procedure = dml_procedure)

    double_mlpliv_obj$fit(store_predictions=TRUE)
    theta_obj = double_mlpliv_obj$coef
    se_obj = double_mlpliv_obj$se

    # bootstrap
    double_mlpliv_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    boot_theta_obj = double_mlpliv_obj$boot_coef

    # at the moment the object result comes without a name
    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)
    expect_equal(as.vector(boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)

test_that("Unit tests for PLIV.partialXZ invalid score", {
  msg = paste("Callable score not implemented for DoubleMLPLIV with",
              "partialX=TRUE and partialZ=TRUE.")
  double_mlplr_obj <- DoubleMLPLIV.partialXZ(
    data_pliv_partialXZ$dml_data,
    ml_g = mlr3::lrn('regr.rpart'),
    ml_m = mlr3::lrn('regr.rpart'),
    ml_r = mlr3::lrn('regr.rpart'),
    score = function(x) return(mean(x)))
  expect_error(double_mlplr_obj$fit(),
               regexp = msg)
}
)

