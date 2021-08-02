se_repeated = function(se_s, coefficients, theta_s) {
    se = sqrt(stats::median(se_s^2 + (theta_s - coefficients)^2))
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


functional_bootstrap = function(theta, se, psi, psi_a, k, smpls,
                                n_rep_boot, weights) {
  score = psi
  J = mean(psi_a)
  boot_coef = matrix(NA_real_, nrow = 1, ncol = n_rep_boot)
  boot_t_stat = matrix(NA_real_, nrow = 1, ncol = n_rep_boot)
  for (i in seq(n_rep_boot)) {
    boot_coef[1, i] = mean(weights[i, ] * 1 / J * score)
    boot_t_stat[1, i] = boot_coef[1, i] / se
  }

  res = list(boot_coef = boot_coef, boot_t_stat = boot_t_stat)
  return(res)
}

trim_vec = function(values, trimming_threshold) {
  if (trimming_threshold > 0) {
    values[values < trimming_threshold] = trimming_threshold
    values[values > 1 - trimming_threshold] = 1 - trimming_threshold
  }
  return(values)
}
