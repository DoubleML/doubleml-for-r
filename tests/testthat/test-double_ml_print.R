context("Unit tests for print methods")

lgr::get_logger("mlr3")$set_threshold("warn")

set.seed(3141)
dml_data = make_plr_CCDDHNR2018(n_obs = 100)
dml_cluster_data = make_pliv_multiway_cluster_CKMS2021(N = 10, M = 10, dim_X = 5)

ml_g = ml_m = ml_r = "regr.rpart"
dml_plr = DoubleMLPLR$new(dml_data, ml_g, ml_m, n_folds = 2)
dml_pliv = DoubleMLPLIV$new(dml_cluster_data, ml_g, ml_m, ml_r, n_folds = 2)
dml_plr$fit()
dml_pliv$fit()

test_that("DoubleMLData print method", {
  verify_output(test_path("print_outputs/dml_data.txt"), {
    print(dml_data)
  })
})

test_that("DoubleMLClusterData print method", {
  verify_output(test_path("print_outputs/dml_cluster_data.txt"), {
    print(dml_cluster_data)
  })
})

test_that("DoubleMLPLR print method", {
  verify_output(test_path("print_outputs/dml_plr.txt"), {
    print(dml_plr)
  })
})

test_that("DoubleMLPLIV print method", {
  verify_output(test_path("print_outputs/dml_pliv.txt"), {
    print(dml_pliv)
  })
})
