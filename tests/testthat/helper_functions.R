#' Helper Functions
#'
#' Functions needed in package code.
#'
#' @param x some object.
#' @name helpers
NULL


#' @rdname helpers
extract_test_pred = function(x) {
  pred = x$data$response
  return(pred)
}

#' @param probs probability.
#' @param digits number of digits.
#' @rdname helpers
# format.perc = function (probs, digits) {
#   paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
#         "%" ) }

#' @param se_s Standard errors from repeated cross-fitting.
#' @param coefficients Final estimators aggregated over all repetitions.
#' @param theta_s Estimators from all repetitions.
#' @inheritParams DML
#' @rdname helpers
se_repeated = function(se_s, coefficients, theta_s, aggreg_median) {
  if (aggreg_median) {
    se = sqrt(stats::median(se_s^2 - (theta_s - coefficients)^2))
  }

  if (!aggreg_median) {
    se = sqrt(mean(se_s^2 - (theta_s - coefficients)^2))
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

draw_bootstrap_weights = function(bootstrap, nRep, n_obs) {
  if (bootstrap == "Bayes") {
    weights = stats::rexp(nRep * n_obs, rate = 1) - 1
  } else if (bootstrap == "normal") {
    weights = stats::rnorm(nRep * n_obs)
  } else if (bootstrap == "wild") {
    weights = stats::rnorm(nRep * n_obs) / sqrt(2) + (stats::rnorm(nRep * n_obs)^2 - 1) / 2
  } else {
    stop("invalid boot method")
  }
  weights = matrix(weights, nrow = nRep, ncol = n_obs, byrow = TRUE)

  return(weights)
}

functional_bootstrap = function(theta, se, psi, psi_a, k, smpls, dml_procedure,
                                bootstrap, nRep) {
  n = length(psi)
  weights = draw_bootstrap_weights(bootstrap, nRep, n)
  if (dml_procedure == "dml1") {
    test_ids = smpls$test_ids
    boot_coef = matrix(NA, nrow = k, ncol = nRep)
    boot_t_stat = matrix(NA, nrow = k, ncol = nRep)
    ii = 0
    for (i_fold in 1:k) {
      test_index = test_ids[[i_fold]]
      
      score = psi[test_index]
      J = mean(psi_a[test_index])
      
      n_obs_in_fold = length(test_index)
      for (i in seq(nRep)) {
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
    boot_coef = matrix(NA, nrow = 1, ncol = nRep)
    boot_t_stat = matrix(NA, nrow = 1, ncol = nRep)
    for (i in seq(nRep)) {
      boot_coef[1, i] = mean(weights[i, ] * 1 / J * score)
      boot_t_stat[1, i] = boot_coef[1, i] / se
    }
  }
  
  res = list(boot_coef = boot_coef, boot_t_stat = boot_t_stat)
  return(res)
}
