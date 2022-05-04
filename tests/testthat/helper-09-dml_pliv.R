# Double Machine Learning for Partially Linear Instrumental Variable Regression.
dml_pliv = function(data, y, d, z,
  n_folds,
  ml_l, ml_m, ml_r,
  params, dml_procedure, score,
  n_rep = 1, smpls = NULL,
  params_l = NULL, params_m = NULL, params_r = NULL) {

  if (is.null(smpls)) {
    smpls = lapply(1:n_rep, function(x) sample_splitting(n_folds, data))
  }

  all_thetas = all_ses = rep(NA_real_, n_rep)
  all_preds = list()

  for (i_rep in 1:n_rep) {
    this_smpl = smpls[[i_rep]]
    train_ids = this_smpl$train_ids
    test_ids = this_smpl$test_ids

    all_preds[[i_rep]] = fit_nuisance_pliv(
      data, y, d, z,
      ml_l, ml_m, ml_r,
      this_smpl,
      params_l, params_m, params_r)

    residuals = compute_pliv_residuals(
      data, y, d, z, n_folds, this_smpl,
      all_preds[[i_rep]])
    u_hat = residuals$u_hat
    v_hat = residuals$v_hat
    w_hat = residuals$w_hat

    # DML 1
    if (dml_procedure == "dml1") {
      thetas = vars = rep(NA_real_, n_folds)
      for (i in 1:n_folds) {
        test_index = test_ids[[i]]
        orth_est = orth_pliv_dml(
          u_hat = u_hat[test_index],
          v_hat = v_hat[test_index],
          w_hat = w_hat[test_index],
          score = score)
        thetas[i] = orth_est$theta
      }
      all_thetas[i_rep] = mean(thetas, na.rm = TRUE)
      if (length(train_ids) == 1) {
        u_hat = u_hat[test_index]
        v_hat = v_hat[test_index]
        w_hat = w_hat[test_index]
      }
    }
    if (dml_procedure == "dml2") {
      orth_est = orth_pliv_dml(
        u_hat = u_hat, v_hat = v_hat, w_hat = w_hat,
        score = score)
      all_thetas[i_rep] = orth_est$theta
    }

    all_ses[i_rep] = sqrt(var_pliv(
      theta = all_thetas[i_rep], u_hat = u_hat, v_hat = v_hat,
      w_hat = w_hat, score = score))
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

fit_nuisance_pliv = function(data, y, d, z,
  ml_l, ml_m, ml_r,
  smpls,
  params_l, params_m, params_r) {

  train_ids = smpls$train_ids
  test_ids = smpls$test_ids

  # nuisance l: E[Y|X]
  l_indx = names(data) != d & names(data) != z
  data_l = data[, l_indx, drop = FALSE]
  task_l = mlr3::TaskRegr$new(id = paste0("nuis_l_", d), backend = data_l, target = y)

  resampling_l = mlr3::rsmp("custom")
  resampling_l$instantiate(task_l, train_ids, test_ids)

  if (!is.null(params_l)) {
    ml_l$param_set$values = params_l
  }

  r_l = mlr3::resample(task_l, ml_l, resampling_l, store_models = TRUE)
  l_hat_list = lapply(r_l$predictions(), function(x) x$response)

  # nuisance m: E[Z|X]
  m_indx = names(data) != y & names(data) != d
  data_m = data[, m_indx, drop = FALSE]
  task_m = mlr3::TaskRegr$new(id = paste0("nuis_m_", z), backend = data_m, target = z)
  if (!is.null(params_m)) {
    ml_m$param_set$values = params_m
  }

  resampling_m = mlr3::rsmp("custom")
  resampling_m$instantiate(task_m, train_ids, test_ids)

  r_m = mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
  m_hat_list = lapply(r_m$predictions(), function(x) x$response)

  # nuisance r: E[D|X]
  r_indx = names(data) != y & names(data) != z
  data_r = data[, r_indx, drop = FALSE]
  task_r = mlr3::TaskRegr$new(id = paste0("nuis_r_", d), backend = data_r, target = d)
  if (!is.null(params_r)) {
    ml_r$param_set$values = params_r
  }

  resampling_r = mlr3::rsmp("custom")
  resampling_r$instantiate(task_r, train_ids, test_ids)

  r_r = mlr3::resample(task_r, ml_r, resampling_r, store_models = TRUE)
  r_hat_list = lapply(r_r$predictions(), function(x) x$response)

  all_preds = list(
    m_hat_list = m_hat_list,
    l_hat_list = l_hat_list,
    r_hat_list = r_hat_list)

  return(all_preds)
}

compute_pliv_residuals = function(data, y, d, z, n_folds, smpls, all_preds) {

  test_ids = smpls$test_ids

  m_hat_list = all_preds$m_hat_list
  l_hat_list = all_preds$l_hat_list
  r_hat_list = all_preds$r_hat_list

  n = nrow(data)
  D = data[, d]
  Y = data[, y]
  Z = data[, z]

  v_hat = u_hat = w_hat = rep(NA_real_, n)

  for (i in 1:n_folds) {
    test_index = test_ids[[i]]

    m_hat = m_hat_list[[i]]
    l_hat = l_hat_list[[i]]
    r_hat = r_hat_list[[i]]

    v_hat[test_index] = D[test_index] - r_hat
    u_hat[test_index] = Y[test_index] - l_hat
    w_hat[test_index] = Z[test_index] - m_hat
  }
  residuals = list(u_hat = u_hat, v_hat = v_hat, w_hat = w_hat)

  return(residuals)
}

# Orthogonalized Estimation of Coefficient in PLR
orth_pliv_dml = function(u_hat, v_hat, w_hat, score) {
  if (score == "partialling out") {
    theta = mean(u_hat * w_hat) / mean(v_hat * w_hat)
  } else {
    stop("Inference framework for orthogonal estimation unknown")
  }
  res = list(theta = theta)
  return(res)
}

# Variance estimation for DML estimator in the partially linear regression model
var_pliv = function(theta, u_hat, v_hat, w_hat, score) {
  if (score == "partialling out") {
    var = mean(1 / length(u_hat) * 1 / (mean(v_hat * w_hat))^2 *
      mean(((u_hat - v_hat * theta) * w_hat)^2))
  } else {
    stop("Inference framework for variance estimation unknown")
  }
  return(c(var))
}

# Bootstrap Implementation for Partially Linear Regression Model
bootstrap_pliv = function(theta, se, data, y, d, z, n_folds, smpls,
  all_preds, bootstrap, n_rep_boot,
  n_rep = 1) {
  for (i_rep in 1:n_rep) {
    residuals = compute_pliv_residuals(
      data, y, d, z, n_folds,
      smpls[[i_rep]], all_preds[[i_rep]])
    u_hat = residuals$u_hat
    v_hat = residuals$v_hat
    w_hat = residuals$w_hat

    psi = (u_hat - v_hat * theta[i_rep]) * w_hat
    psi_a = -v_hat * w_hat

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
