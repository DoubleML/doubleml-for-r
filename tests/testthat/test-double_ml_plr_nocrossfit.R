context("Unit tests for PLR, no cross-fitting")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.lm",
    dml_procedure = "dml2",
    score = "partialling out",
    apply_cross_fitting = FALSE,
    n_folds = c(1, 2),
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = "regr.lm",
    dml_procedure = c("dml1", "dml2"),
    score = c("IV-type", "partialling out"),
    apply_cross_fitting = FALSE,
    n_folds = c(1, 2),
    stringsAsFactors = FALSE)
}
test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for PLR:",
  .cases = test_cases, {
    learner_pars = get_default_mlmethod_plr(learner)
    n_rep_boot = 498
    
    set.seed(3141)
    df = data_plr$df
    if (n_folds == 2) {
      my_task = Task$new("help task", "regr", df)
      my_sampling = rsmp("holdout", ratio = 0.5)$instantiate(my_task)
      train_ids = list(my_sampling$train_set(1))
      test_ids = list(my_sampling$test_set(1))
      
      smpls = list(list(train_ids = train_ids, test_ids = test_ids))
    } else {
      smpls = list(list(train_ids = list(seq(nrow(df))),
                        test_ids = list(seq(nrow(df)))))
    }
    plr_hat = dml_plr(df,
                      y = "y", d = "d",
                      n_folds = 1,
                      ml_g = learner_pars$ml_g$clone(),
                      ml_m = learner_pars$ml_m$clone(),
                      dml_procedure = dml_procedure, score = score,
                      smpls=smpls)
    theta = plr_hat$coef
    se = plr_hat$se
    t = plr_hat$t
    pval = plr_hat$pval

    set.seed(3141)
    double_mlplr_obj = DoubleMLPLR$new(
      data = data_plr$dml_data,
      ml_g = learner_pars$ml_g$clone(),
      ml_m = learner_pars$ml_m$clone(),
      dml_procedure = dml_procedure,
      n_folds = n_folds,
      score = score,
      apply_cross_fitting = apply_cross_fitting)

    double_mlplr_obj$fit(store_predictions=TRUE)
    theta_obj = double_mlplr_obj$coef
    se_obj = double_mlplr_obj$se
    t_obj = double_mlplr_obj$t_stat
    pval_obj = double_mlplr_obj$pval
    ci_obj = double_mlplr_obj$confint(level = 0.95, joint = FALSE)


    if (n_folds == 2) {
      dml_plr_obj_external = DoubleMLPLR$new(
        data = data_plr$dml_data,
        ml_g = learner_pars$ml_g$clone(),
        ml_m = learner_pars$ml_m$clone(),
        dml_procedure = dml_procedure,
        n_folds = n_folds,
        score = score,
        draw_sample_splitting = FALSE, apply_cross_fitting = FALSE)

      set.seed(3141)
      # set up a task and cross-validation resampling scheme in mlr3
      my_task = Task$new("help task", "regr", df)
      my_sampling = rsmp("holdout", ratio = 0.5)$instantiate(my_task)
      train_ids = list(my_sampling$train_set(1))
      test_ids = list(my_sampling$test_set(1))

      smpls = list(list(train_ids = train_ids, test_ids = test_ids))

      dml_plr_obj_external$set_sample_splitting(smpls)
      dml_plr_obj_external$fit()

      theta_external = dml_plr_obj_external$coef
      se_external = dml_plr_obj_external$se
      t_external = dml_plr_obj_external$t_stat
      pval_external = dml_plr_obj_external$pval
      ci_external = dml_plr_obj_external$confint(level = 0.95, joint = FALSE)
      
      expect_identical(double_mlplr_obj$smpls, dml_plr_obj_external$smpls)
      expect_equal(theta_external, theta_obj, tolerance = 1e-8)
      expect_equal(se_external, se_obj, tolerance = 1e-8)
      expect_equal(t_external, t_obj, tolerance = 1e-8)
      expect_equal(pval_external, pval_obj, tolerance = 1e-8)
      expect_equal(ci_external, ci_obj, tolerance = 1e-8)

      expect_equal(theta, theta_obj, tolerance = 1e-8)
      expect_equal(se, se_obj, tolerance = 1e-8)
      expect_equal(t, t_obj, tolerance = 1e-8)
      expect_equal(pval, pval_obj, tolerance = 1e-8)

    } else {
      expect_equal(theta, theta_obj, tolerance = 1e-8)
      expect_equal(se, se_obj, tolerance = 1e-8)
      expect_equal(t, t_obj, tolerance = 1e-8)
      expect_equal(pval, pval_obj, tolerance = 1e-8)
    }

    # expect_equal(as.vector(plr_hat$boot_theta), as.vector(boot_theta_obj), tolerance = 1e-8)
  }
)
