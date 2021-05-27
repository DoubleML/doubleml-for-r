context("Unit tests for exception handling if fit() or bootstrap() was not run yet; uses PLR")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")
on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.lm",
    dml_procedure = "dml1",
    score = "IV-type",
    set_params = FALSE,
    n_folds = c(4),
    n_rep = c(1),
    apply_cross_fitting = c(TRUE),
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = "regr.cv_glmnet",
    dml_procedure = "dml1",
    score = c("IV-type", "partialling out"),
    set_params = c(TRUE, FALSE),
    n_folds = c(1, 5),
    n_rep = c(1, 2),
    apply_cross_fitting = c(TRUE, FALSE),
    stringsAsFactors = FALSE)
}
test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for exception handling of PLR:",
  .cases = test_cases, {
    learner_pars = get_default_mlmethod_plr(learner)
    n_rep_boot = 498

    data_ml = double_ml_data_from_data_frame(data_plr$df,
      y_col = "y",
      d_cols = c("d", "X1"))

    msg = "DoubleML is an abstract class that can't be initialized."
    expect_error(DoubleML$new(), regexp = msg)


    if ((n_folds > 1 & !apply_cross_fitting) |
      (n_rep > 1 & !apply_cross_fitting) |
      (n_rep > 1 & n_folds == 1 & apply_cross_fitting)) {
      if (n_folds > 2 & !apply_cross_fitting) {
        msg = "Estimation without cross-fitting not supported for n_folds > 2."
      } else {
        msg = "Assertion on 'i' failed: Element 1 is not <= 1."
      }
      expect_error(DoubleMLPLR$new(
        data = data_ml,
        ml_g = learner_pars$mlmethod$mlmethod_g,
        ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
        dml_procedure = dml_procedure,
        n_folds = n_folds,
        n_rep = n_rep,
        score = score,
        apply_cross_fitting = apply_cross_fitting),
        regexp = msg)
    } else {
      double_mlplr_obj = DoubleMLPLR$new(
        data = data_ml,
        ml_g = learner_pars$mlmethod$mlmethod_g,
        ml_m = mlr3::lrn(learner_pars$mlmethod$mlmethod_m),
        dml_procedure = dml_procedure,
        n_folds = n_folds,
        n_rep = n_rep,
        score = score,
        apply_cross_fitting = apply_cross_fitting)

      if (set_params) {
        # set params for nuisance part m
        double_mlplr_obj$set_ml_nuisance_params(
          learner = "ml_m",
          treat_var = "d",
          params = learner_pars$params$params_m)

        # set params for nuisance part g
        double_mlplr_obj$set_ml_nuisance_params(
          learner = "ml_g",
          treat_var = "d",
          params = learner_pars$params$params_g)
      }

      # currently, no warning or message printed
      utils::capture.output(double_mlplr_obj$summary(), file = NULL)
      msg = "Apply fit\\(\\) before bootstrap\\(\\)."
      expect_error(double_mlplr_obj$bootstrap(method = "normal", n_rep_boot = n_rep_boot),
                   regexp = msg)

      double_mlplr_obj$fit()

      utils::capture.output(double_mlplr_obj$print(), file = NULL)
      utils::capture.output(expect_is(double_mlplr_obj$summary(), "matrix"), file = NULL)

      msg = "'level' must be > 0 and < 1."
      expect_error(double_mlplr_obj$confint(level = 1.2),
                   regexp = msg)
      msg = "Multiplier bootstrap has not yet been performed. First call bootstrap\\(\\) and then try confint\\(\\) again."
      expect_error(double_mlplr_obj$confint(joint = TRUE, level = 0.95),
                   regexp = msg)
    }
  }
)
