# Double Machine Learning for Interactive Instrumental Variable Regression Model.
dml_irmiv = function(data, y, d, z,
  n_folds,
  ml_g, ml_m, ml_r,
  dml_procedure, score,
  always_takers = TRUE, never_takers = TRUE,
  n_rep = 1, smpls = NULL,
  trimming_threshold = 1e-12,
  params_g = NULL, params_m = NULL, params_r = NULL) {

  if (is.null(smpls)) {
    smpls = lapply(1:n_rep, function(x) sample_splitting(n_folds, data))
  }

  all_thetas = all_ses = rep(NA_real_, n_rep)
  all_preds = list()

  for (i_rep in 1:n_rep) {
    this_smpl = smpls[[i_rep]]
    train_ids = this_smpl$train_ids
    test_ids = this_smpl$test_ids

    all_preds[[i_rep]] = fit_nuisance_iivm(
      data, y, d, z,
      ml_g, ml_m, ml_r,
      train_ids, test_ids,
      always_takers, never_takers,
      params_g, params_m, params_r)
    res = extract_iivm_preds(data, y, d, z, n_folds,
      this_smpl, all_preds[[i_rep]],
      trimming_threshold = trimming_threshold)
    m_hat = res$m_hat
    g0_hat = res$g0_hat
    g1_hat = res$g1_hat
    r0_hat = res$r0_hat
    r1_hat = res$r1_hat
    D = data[, d]
    Y = data[, y]
    Z = data[, z]

    # DML 1
    if (dml_procedure == "dml1") {
      thetas = vars = rep(NA_real_, n_folds)
      for (i in 1:n_folds) {
        test_index = test_ids[[i]]
        orth_est = orth_irmiv_dml(
          m_hat = m_hat[test_index],
          g0_hat = g0_hat[test_index], g1_hat = g1_hat[test_index],
          r0_hat = r0_hat[test_index], r1_hat = r1_hat[test_index],
          d = D[test_index], y = Y[test_index], z = Z[test_index],
          score = score)
        thetas[i] = orth_est$theta
      }
      all_thetas[i_rep] = mean(thetas, na.rm = TRUE)
      if (length(train_ids) == 1) {
        D = D[test_index]
        Y = Y[test_index]
        Z = Z[test_index]
        m_hat = m_hat[test_index]
        g0_hat = g0_hat[test_index]
        g1_hat = g1_hat[test_index]
        r0_hat = r0_hat[test_index]
        r1_hat = r1_hat[test_index]
      }
    }
    if (dml_procedure == "dml2") {
      orth_est = orth_irmiv_dml(
        m_hat = m_hat, g0_hat = g0_hat,
        g1_hat = g1_hat,
        r0_hat = r0_hat, r1_hat = r1_hat,
        d = D, y = Y, z = Z,
        score = score)
      all_thetas[i_rep] = orth_est$theta
    }

    all_ses[i_rep] = sqrt(var_irmiv(
      theta = all_thetas[i_rep], m_hat = m_hat, g0_hat = g0_hat,
      g1_hat = g1_hat, r0_hat = r0_hat, r1_hat = r1_hat,
      d = D, y = Y, z = Z, score = score))
  }

  theta = stats::median(all_thetas)
  if (length(this_smpl$train_ids) > 1) {
    n = nrow(data)
  } else {
    n = length(this_smpl$test_ids[[1]])
  }
  se = se_repeated(all_ses * sqrt(n), all_thetas, theta) / sqrt(n)

  t = theta / se
  pval = 2 * stats::pnorm(-abs(t))

  names(theta) = names(se) = d
  res = list(
    coef = theta, se = se, t = t, pval = pval,
    thetas = all_thetas, ses = all_ses,
    all_preds = all_preds, smpls = smpls)

  return(res)
}


fit_nuisance_iivm = function(data, y, d, z,
  ml_g, ml_m, ml_r,
  train_ids, test_ids,
  always_takers, never_takers,
  params_g, params_m, params_r) {

  # Set up task_m first to get resampling (test and train ids) scheme based on full sample
  # nuisance m

  m_indx = names(data) != y & names(data) != d
  data_m = data[, m_indx, drop = FALSE]

  # tbd: handle case with classif vs. regr. for task_m
  # if (grepl("regr.", mlmethod$mlmethod_p )) {
  #  # task_m = mlr3::TaskRegr$new(id = paste0("nuis_m_", z), backend = data_m, target = z)
  #   task_m = mlr3::TaskRegr$new(id = paste0("nuis_m_", z), backend = data_m, target = z)
  #   # task_m = mlr3::tsk(id = paste0("nuis_m_", z), backend )
  # }

  # if (grepl("classif.", mlmethod$mlmethod_m )) {
  data_m[, z] = factor(data_m[, z])
  task_m = mlr3::TaskClassif$new(
    id = paste0("nuis_m_", z), backend = data_m,
    target = z, positive = "1")
  # }

  resampling_m = mlr3::rsmp("custom")
  resampling_m$instantiate(task_m, train_ids, test_ids)
  n_iters = resampling_m$iters

  # in each fold, select those with z = 0
  train_ids_0 = lapply(1:n_iters, function(x) {
    resampling_m$train_set(x)[data[resampling_m$train_set(x), z] == 0]
  })
  # in each fold, select those with d = 0
  train_ids_1 = lapply(1:n_iters, function(x) {
    resampling_m$train_set(x)[data[resampling_m$train_set(x), z] == 1]
  })

  if (!is.null(params_m)) {
    ml_m$param_set$values = params_m
  }
  r_m = mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
  m_hat_list = lapply(r_m$predictions(), function(x) x$prob[, "1"])

  # nuisance g0: E[Y|Z=0, X]
  g_indx = names(data) != d & names(data) != z
  data_g = data[, g_indx, drop = FALSE]
  task_g0 = mlr3::TaskRegr$new(id = paste0("nuis_g0_", z), backend = data_g, target = y)
  ml_g0 = ml_g$clone()
  if (!is.null(params_g)) {
    ml_g0$param_set$values = params_g
  }
  resampling_g0 = mlr3::rsmp("custom")
  # Train on subset with z == 0 (in each fold) only, predict for all test obs
  resampling_g0$instantiate(task_g0, train_ids_0, test_ids)
  train_ids_g0 = lapply(1:n_iters, function(x) resampling_g0$train_set(x))
  test_ids_g0 = lapply(1:n_iters, function(x) resampling_g0$test_set(x))

  r_g0 = mlr3::resample(task_g0, ml_g0, resampling_g0, store_models = TRUE)
  g0_hat_list = lapply(r_g0$predictions(), function(x) x$response)

  # nuisance g1: E[Y|Z=1, X]
  task_g1 = mlr3::TaskRegr$new(id = paste0("nuis_g1_", z), backend = data_g, target = y)
  ml_g1 = ml_g$clone()
  if (!is.null(params_g)) {
    ml_g1$param_set$values = params_g
  }
  resampling_g1 = mlr3::rsmp("custom")
  # Train on subset with z == 1 (in each fold) only, predict for all test obs
  resampling_g1$instantiate(task_g1, train_ids_1, test_ids)
  train_ids_g1 = lapply(1:n_iters, function(x) resampling_g1$train_set(x))
  test_ids_g1 = lapply(1:n_iters, function(x) resampling_g1$test_set(x))

  r_g1 = mlr3::resample(task_g1, ml_g1, resampling_g1, store_models = TRUE)
  # g1_hat_list = lapply(r_g1$data$prediction, function(x) x$test$response)
  g1_hat_list = lapply(r_g1$predictions(), function(x) x$response)

  # nuisance r0: E[D|Z=0, X]
  r_indx = names(data) != y & names(data) != z
  data_r = data[, r_indx, drop = FALSE]
  data_r[, d] = factor(data_r[, d])

  if (always_takers == FALSE & never_takers == FALSE) {
    message("If there are no always-takers and no never-takers, ATE is estimated")
  }

  if (always_takers == FALSE) {
    lengths = lapply(test_ids, length)
    r0_hat_list = lapply(lengths, function(x) rep(0, x))
  }

  if (always_takers == TRUE) {
    task_r0 = mlr3::TaskClassif$new(
      id = paste0("nuis_r0_", d), backend = data_r,
      target = d, positive = "1")
    ml_r0 = ml_r$clone()
    if (!is.null(params_r)) {
      ml_r0$param_set$values = params_r
    }

    resampling_r0 = mlr3::rsmp("custom")
    # Train on subset with z == 0 (in each fold) only, predict for all test obs
    resampling_r0$instantiate(task_r0, train_ids_0, test_ids)
    train_ids_r0 = lapply(1:n_iters, function(x) resampling_r0$train_set(x))
    test_ids_r0 = lapply(1:n_iters, function(x) resampling_r0$test_set(x))
    r_r0 = mlr3::resample(task_r0, ml_r0, resampling_r0, store_models = TRUE)
    r0_hat_list = lapply(r_r0$predictions(), function(x) x$prob[, "1"])
  }

  if (never_takers == FALSE) {
    lengths = lapply(test_ids, length)
    r1_hat_list = lapply(lengths, function(x) rep(1, x))
  }

  if (never_takers == TRUE) {
    # nuisance m1: E[E|Z=1, 0]
    task_r1 = mlr3::TaskClassif$new(
      id = paste0("nuis_r1_", d), backend = data_r,
      target = d, positive = "1")
    ml_r1 = ml_r$clone()
    if (!is.null(params_r)) {
      ml_r1$param_set$values = params_r
    }

    resampling_r1 = mlr3::rsmp("custom")
    # Train on subset with z == 0 (in each fold) only, predict for all test obs
    resampling_r1$instantiate(task_r1, train_ids_1, test_ids)
    train_ids_r1 = lapply(1:n_iters, function(x) resampling_r1$train_set(x))
    test_ids_r1 = lapply(1:n_iters, function(x) resampling_r1$test_set(x))
    r_r1 = mlr3::resample(task_r1, ml_r1, resampling_r1, store_models = TRUE)
    r1_hat_list = lapply(r_r1$predictions(), function(x) x$prob[, "1"])
  }

  all_preds = list(
    m_hat_list = m_hat_list,
    g0_hat_list = g0_hat_list,
    g1_hat_list = g1_hat_list,
    r0_hat_list = r0_hat_list,
    r1_hat_list = r1_hat_list)

  return(all_preds)
}


extract_iivm_preds = function(data, y, d, z, n_folds, smpls, all_preds,
  trimming_threshold) {

  test_ids = smpls$test_ids

  m_hat_list = all_preds$m_hat_list
  g0_hat_list = all_preds$g0_hat_list
  g1_hat_list = all_preds$g1_hat_list
  r0_hat_list = all_preds$r0_hat_list
  r1_hat_list = all_preds$r1_hat_list

  n = nrow(data)
  D = data[, d]
  Y = data[, y]
  Z = data[, z]
  m_hat = g0_hat = g1_hat = r0_hat = r1_hat = rep(NA_real_, n)

  for (i in 1:n_folds) {
    test_index = test_ids[[i]]

    m_hat[test_index] = m_hat_list[[i]]
    g0_hat[test_index] = g0_hat_list[[i]]
    g1_hat[test_index] = g1_hat_list[[i]]
    r0_hat[test_index] = r0_hat_list[[i]]
    r1_hat[test_index] = r1_hat_list[[i]]
  }

  m_hat = trim_vec(m_hat, trimming_threshold)

  res = list(
    m_hat = m_hat, g0_hat = g0_hat, g1_hat = g1_hat,
    r0_hat = r0_hat, r1_hat = r1_hat)
  return(res)
}


# Orthogonalized Estimation of Coefficient in irm
orth_irmiv_dml = function(m_hat, g0_hat, g1_hat, r0_hat, r1_hat, d, y, z, score) {
  theta = NA_real_

  if (score == "LATE" | score == "partialling out") {
    theta = 1 / mean(r1_hat - r0_hat + z * (d - r1_hat) / m_hat - ((1 - z) * (d - r0_hat) / (1 - m_hat))) *
      mean(g1_hat - g0_hat + z * (y - g1_hat) / m_hat - ((1 - z) * (y - g0_hat) / (1 - m_hat)))
  }
  else {
    stop("Inference framework for orthogonal estimation unknown")
  }

  res = list(theta = theta)
  return(res)
}


# Variance estimation for DML estimator in the Interactive Instrumental Variable Regression Model
var_irmiv = function(theta, m_hat, g0_hat, g1_hat, r0_hat, r1_hat, d, y, z, score) {
  n = length(d)
  if (score == "LATE") {
    var = 1 / n * 1 / (mean((r1_hat - r0_hat + z * (d - r1_hat) / m_hat - (1 - z) * (d - r0_hat) / (1 - m_hat))))^2 *
      mean((g1_hat - g0_hat + z * (y - g1_hat) / m_hat - (1 - z) * (y - g0_hat) / (1 - m_hat) -
        (r1_hat - r0_hat + z * (d - r1_hat) / m_hat - (1 - z) * (d - r0_hat) / (1 - m_hat)) * theta)^2)
  } else {
    stop("Inference framework for variance estimation unknown")
  }
  return(c(var))
}


# Bootstrap Implementation for Interactive Instrumental Variable Regression Model
bootstrap_irmiv = function(theta, se, data, y, d, z, n_folds, smpls, all_preds,
  score, bootstrap, n_rep_boot,
  n_rep = 1, trimming_threshold = 1e-12) {
  for (i_rep in 1:n_rep) {
    res = extract_iivm_preds(data, y, d, z, n_folds,
      smpls[[i_rep]], all_preds[[i_rep]],
      trimming_threshold = trimming_threshold)
    m_hat = res$m_hat
    g0_hat = res$g0_hat
    g1_hat = res$g1_hat
    r0_hat = res$r0_hat
    r1_hat = res$r1_hat
    D = data[, d]
    Y = data[, y]
    Z = data[, z]

    if (score == "LATE") {

      psi = g1_hat - g0_hat + Z * (Y - g1_hat) / m_hat - (1 - Z) * (Y - g0_hat) / (1 - m_hat) -
        (r1_hat - r0_hat + Z * (D - r1_hat) / m_hat - (1 - Z) * (D - r0_hat) / (1 - m_hat)) * theta[i_rep]

      psi_a = -(r1_hat - r0_hat + Z * (D - r1_hat) / m_hat
        - (1 - Z) * (D - r0_hat) / (1 - m_hat))
    } else {
      stop("Inference framework for multiplier bootstrap unknown")
    }

    n = length(psi)
    weights = draw_bootstrap_weights(bootstrap, n_rep_boot, n)
    this_res = functional_bootstrap(
      theta[i_rep], se[i_rep], psi, psi_a, n_folds,
      smpls[[i_rep]],
      n_rep_boot, weights)
    if (i_rep == 1) {
      boot_res = this_res
    } else {
      boot_res$boot_coef = cbind(boot_res$boot_coef, this_res$boot_coef)
      boot_res$boot_t_stat = cbind(boot_res$boot_t_stat, this_res$boot_t_stat)
    }
  }
  return(boot_res)
}
