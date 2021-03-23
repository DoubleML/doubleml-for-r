context("Unit tests for PLR (p_adjust)")

library("mlr3learners")

lgr::get_logger("mlr3")$set_threshold("warn")

on_cran = !identical(Sys.getenv("NOT_CRAN"), "true")
if (on_cran) {
  test_cases = expand.grid(
    learner = "regr.cv_glmnet",
    dml_procedure = "dml1",
    score = "partialling out",
    method = c("romano-wolf"),
    apply_cross_fitting = c(TRUE),
    stringsAsFactors = FALSE)
} else {
  test_cases = expand.grid(
    learner = "regr.cv_glmnet",
    dml_procedure = c("dml1", "dml2"),
    score = c("IV-type", "partialling out"),
    method = c("romano-wolf", "bonferroni"),
    apply_cross_fitting = c(TRUE, FALSE),
    stringsAsFactors = FALSE)
}
test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for PLR:",
  .cases = test_cases, {
    ml_m = lrn(learner, s = "lambda.min")
    ml_g = lrn(learner, s = "lambda.min")

    n_rep_boot = 498

    if (!apply_cross_fitting) {
      n_folds = 2
    } else {
      n_folds = 5
    }

    set.seed(1)
    n = 100 # sample size
    p = 25 # number of variables
    s = 3 # nubmer of non-zero variables
    X = matrix(rnorm(n * p), ncol = p)
    colnames(X) = paste("X", 1:p, sep = "")
    beta = c(rep(3, s), rep(0, p - s))
    y = 1 + X %*% beta + rnorm(n)
    data = data.frame(cbind(y, X))
    colnames(data)[1] = "y"

    # index for hypoth testing
    k = 10
    data_ml = double_ml_data_from_data_frame(data,
      x_cols = colnames(X)[(k + 1):p],
      y_col = "y",
      d_cols = colnames(X)[1:k])
    double_mlplr_obj = DoubleMLPLR$new(data_ml,
      ml_g = ml_g,
      ml_m = ml_m,
      dml_procedure = dml_procedure,
      n_folds = n_folds,
      score = score,
      apply_cross_fitting = apply_cross_fitting)
    double_mlplr_obj$fit()
    double_mlplr_obj$bootstrap()
    double_mlplr_obj$p_adjust(method = method)
    expect_true(is.matrix(double_mlplr_obj$p_adjust(method = method)))
  }
)
