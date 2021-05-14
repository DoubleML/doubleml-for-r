context("Unit tests for parameter passing of IRM")

lgr::get_logger("mlr3")$set_threshold("warn")

# settings for parameter provision
learner = "rpart"

learner_list = list("mlmethod_m" = learner, "mlmethod_g" = learner)

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = learner,
    dml_procedure = "dml2",
    score = "ATE",
    i_setting = 1:(length(data_irm)),
    trimming_threshold = 1e-5,
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = learner,
    dml_procedure = c("dml1", "dml2"),
    score = c("ATE", "ATTE"),
    i_setting = 1:(length(data_irm)),
    trimming_threshold = 1e-5,
    stringsAsFactors = FALSE)
}
test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for parameter passing of IRM (oop vs fun):",
  .cases = test_cases, {
    n_rep_boot = 498
    n_folds = 2
    n_rep = 3
  
    learner_pars = get_default_mlmethod_irm(learner)
    
    set.seed(i_setting)
    irm_hat = dml_irm(data_irm[[i_setting]],
                      y = "y", d = "d",
                      n_folds = n_folds,
                      n_rep = n_rep,
                      ml_g = mlr3::lrn(learner_pars$mlmethod$mlmethod_g),
                      ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m, predict_type = "prob"),
                      params_g = learner_pars$params$params_g,
                      params_m = learner_pars$params$params_m,
                      dml_procedure = dml_procedure, score = score,
                      trimming_threshold = trimming_threshold)
    theta = irm_hat$coef
    se = irm_hat$se
    
    boot_theta = bootstrap_irm(irm_hat$thetas, irm_hat$ses,
                               data_irm[[i_setting]],
                               y = "y", d = "d",
                               n_folds = n_folds, n_rep = n_rep,
                               smpls = irm_hat$smpls,
                               all_preds= irm_hat$all_preds,
                               score = score,
                               bootstrap = "normal", n_rep_boot = n_rep_boot,
                               trimming_threshold = trimming_threshold)$boot_coef

    set.seed(i_setting)
    Xnames = names(data_irm[[i_setting]])[names(data_irm[[i_setting]]) %in% c("y", "d") == FALSE]

    data_ml = double_ml_data_from_data_frame(data_irm[[i_setting]],
      y_col = "y",
      d_cols = c("d"), x_cols = Xnames)

    double_mlirm_obj = DoubleMLIRM$new(data_ml,
      n_folds = n_folds,
      ml_g = lrn(learner_pars$mlmethod$mlmethod_g),
      ml_m = lrn(learner_pars$mlmethod$mlmethod_m, predict_type = "prob"),
      dml_procedure = dml_procedure,
      score = score,
      n_rep = n_rep, trimming_threshold = trimming_threshold)

    # set params for nuisance part m
    double_mlirm_obj$set_ml_nuisance_params(
      learner = "ml_m",
      treat_var = "d",
      params = learner_pars$params$params_m)
    # set params for nuisance part g
    double_mlirm_obj$set_ml_nuisance_params(
      learner = "ml_g0",
      treat_var = "d",
      params = learner_pars$params$params_g)
    double_mlirm_obj$set_ml_nuisance_params(
      learner = "ml_g1",
      treat_var = "d",
      params = learner_pars$params$params_g)

    double_mlirm_obj$fit()
    theta_obj = double_mlirm_obj$coef
    se_obj = double_mlirm_obj$se

    # bootstrap
    double_mlirm_obj$bootstrap(method = 'normal',  n_rep = n_rep_boot)
    boot_theta_obj = double_mlirm_obj$boot_coef

    expect_equal(theta, theta_obj, tolerance = 1e-8)
    expect_equal(se, se_obj, tolerance = 1e-8)

    expect_equal(as.vector(boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)

