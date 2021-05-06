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
