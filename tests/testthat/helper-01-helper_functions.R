extract_test_pred = function(x) {
  pred = x$data$response
  return(pred)
}


se_repeated = function(se_s, coefficients, theta_s, aggreg_median) {
  if (aggreg_median) {
    se = sqrt(stats::median(se_s^2 + (theta_s - coefficients)^2))
  }

  if (!aggreg_median) {
    se = sqrt(mean(se_s^2 + (theta_s - coefficients)^2))
  }

  return(se)
}


sample_splitting = function(k, data) {

  resampling = mlr3::ResamplingCV$new()
  resampling$param_set$values$folds = k

  dummy_task = mlr3::Task$new("dummy_resampling", "regr", data)
  resampling = resampling$instantiate(dummy_task)

  n_iters = resampling$iters
  train_ids = lapply(1:n_iters, function(x) resampling$train_set(x))
  test_ids = lapply(1:n_iters, function(x) resampling$test_set(x))

  return(list(train_ids = train_ids, test_ids = test_ids))
}


draw_bootstrap_weights = function(bootstrap, n_rep_boot, n_obs) {
  if (bootstrap == "Bayes") {
    weights = stats::rexp(n_rep_boot * n_obs, rate = 1) - 1
  } else if (bootstrap == "normal") {
    weights = stats::rnorm(n_rep_boot * n_obs)
  } else if (bootstrap == "wild") {
    weights = stats::rnorm(n_rep_boot * n_obs) / sqrt(2) + (stats::rnorm(n_rep_boot * n_obs)^2 - 1) / 2
  } else {
    stop("invalid boot method")
  }
  weights = matrix(weights, nrow = n_rep_boot, ncol = n_obs, byrow = TRUE)
  
  return(weights)
}


functional_bootstrap = function(theta, se, psi, psi_a, k, smpls, dml_procedure,
                                n_rep_boot, weights) {
  if (dml_procedure == "dml1") {
    test_ids = smpls$test_ids
    boot_coef = matrix(NA, nrow = k, ncol = n_rep_boot)
    boot_t_stat = matrix(NA, nrow = k, ncol = n_rep_boot)
    ii = 0
    for (i_fold in 1:k) {
      test_index = test_ids[[i_fold]]
      
      score = psi[test_index]
      J = mean(psi_a[test_index])
      
      n_obs_in_fold = length(test_index)
      for (i in seq(n_rep_boot)) {
        boot_coef[i_fold, i] = mean(weights[i, (ii + 1):(ii + n_obs_in_fold)] * 1 / J * score)
        boot_t_stat[i_fold, i] = boot_coef[i_fold, i] / se
      }
      ii = ii + n_obs_in_fold
    }
    boot_coef = colMeans(boot_coef)
    boot_t_stat = colMeans(boot_t_stat)
  } else {
    # DML2
    score = psi
    J = mean(psi_a)
    boot_coef = matrix(NA, nrow = 1, ncol = n_rep_boot)
    boot_t_stat = matrix(NA, nrow = 1, ncol = n_rep_boot)
    for (i in seq(n_rep_boot)) {
      boot_coef[1, i] = mean(weights[i, ] * 1 / J * score)
      boot_t_stat[1, i] = boot_coef[1, i] / se
    }
  }
  
  res = list(boot_coef = boot_coef, boot_t_stat = boot_t_stat)
  return(res)
}
