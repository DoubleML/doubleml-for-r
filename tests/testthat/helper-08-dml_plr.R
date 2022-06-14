# Double Machine Learning for Partially Linear Regression.
dml_plr = function(data, y, d,
  n_folds, ml_l, ml_m, ml_g,
  dml_procedure, score,
  n_rep = 1, smpls = NULL,
  params_l = NULL, params_m = NULL, params_g = NULL) {

  if (is.null(smpls)) {
    smpls = lapply(1:n_rep, function(x) sample_splitting(n_folds, data))
  }

  all_thetas = all_ses = rep(NA_real_, n_rep)
  all_preds = list()

  for (i_rep in 1:n_rep) {
    this_smpl = smpls[[i_rep]]
    stopifnot(length(this_smpl$train_ids) == length(this_smpl$test_ids))
    if (length(this_smpl$train_ids) == 1) {
      dml_procedure = "dml1"
    }

    res_single_split = fit_plr_single_split(
      data, y, d,
      n_folds, ml_l, ml_m, ml_g,
      dml_procedure, score,
      this_smpl,
      params_l, params_m, params_g)

    all_preds[[i_rep]] = res_single_split$all_preds
    all_thetas[i_rep] = res_single_split$theta
    all_ses[i_rep] = res_single_split$se
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

  names(theta) = names(se) = names(t) = names(pval) = d
  res = list(
    coef = theta, se = se, t = t, pval = pval,
    thetas = all_thetas, ses = all_ses,
    all_preds = all_preds, smpls = smpls)

  return(res)
}


dml_plr_multitreat = function(data, y, d,
  n_folds, ml_l, ml_m, ml_g,
  dml_procedure, score,
  n_rep = 1, smpls = NULL,
  params_l = NULL, params_m = NULL, params_g = NULL) {

  if (is.null(smpls)) {
    smpls = lapply(1:n_rep, function(x) sample_splitting(n_folds, data))
  }

  all_preds = all_thetas = all_ses = list()
  n_d = length(d)

  for (i_rep in 1:n_rep) {
    this_smpl = smpls[[i_rep]]
    thetas_this_rep = ses_this_rep = rep(NA_real_, n_d)
    all_preds_this_rep = list()

    for (i_d in seq(n_d)) {
      if (!is.null(params_l)) {
        this_params_l = params_l[[i_d]]
      } else {
        this_params_l = NULL
      }
      if (!is.null(params_g)) {
        this_params_g = params_g[[i_d]]
      } else {
        this_params_g = NULL
      }
      if (!is.null(params_m)) {
        this_params_m = params_m[[i_d]]
      } else {
        this_params_m = NULL
      }
      res_single_split = fit_plr_single_split(
        data, y, d[i_d],
        n_folds, ml_l, ml_m, ml_g,
        dml_procedure, score,
        this_smpl,
        this_params_l, this_params_m, this_params_g)

      all_preds_this_rep[[i_d]] = res_single_split$all_preds
      thetas_this_rep[i_d] = res_single_split$theta
      ses_this_rep[i_d] = res_single_split$se
    }

    all_preds[[i_rep]] = all_preds_this_rep
    all_thetas[[i_rep]] = thetas_this_rep
    all_ses[[i_rep]] = ses_this_rep

  }

  theta = se = t = pval = rep(NA_real_, n_d)
  if (length(this_smpl$train_ids) > 1) {
    n = nrow(data)
  } else {
    n = length(smpls[[1]]$test_ids[[1]])
  }
  for (i_d in seq(n_d)) {
    theta_vec = unlist(lapply(all_thetas, function(x) x[i_d]))
    se_vec = unlist(lapply(all_ses, function(x) x[i_d]))
    theta[i_d] = stats::median(theta_vec)
    se[i_d] = se_repeated(se_vec * sqrt(n), theta_vec, theta[i_d]) / sqrt(n)
    t[i_d] = theta[i_d] / se[i_d]
    pval[i_d] = 2 * stats::pnorm(-abs(t[i_d]))
  }

  names(theta) = names(se) = names(t) = names(pval) = d
  res = list(
    coef = theta, se = se, t = t, pval = pval,
    thetas = all_thetas, ses = all_ses,
    all_preds = all_preds, smpls = smpls)

  return(res)
}


fit_plr_single_split = function(data, y, d,
  n_folds, ml_l, ml_m, ml_g,
  dml_procedure, score, smpl,
  params_l, params_m, params_g) {

  train_ids = smpl$train_ids
  test_ids = smpl$test_ids

  fit_g = (score == "IV-type") | is.function(score)
  all_preds = fit_nuisance_plr(
    data, y, d,
    ml_l, ml_m, ml_g,
    n_folds, smpl, fit_g,
    params_l, params_m, params_g)

  residuals = compute_plr_residuals(
    data, y, d, n_folds, smpl,
    all_preds)
  y_minus_l_hat = residuals$y_minus_l_hat
  d_minus_m_hat = residuals$d_minus_m_hat
  y_minus_g_hat = residuals$y_minus_g_hat
  D = data[, d]
  Y = data[, y]

  # DML 1
  if (dml_procedure == "dml1") {
    thetas = rep(NA_real_, n_folds)
    for (i in 1:n_folds) {
      test_index = test_ids[[i]]

      orth_est = orth_plr_dml(
        y_minus_l_hat = y_minus_l_hat[test_index],
        d_minus_m_hat = d_minus_m_hat[test_index],
        y_minus_g_hat = y_minus_g_hat[test_index],
        d = D[test_index],
        score = score)
      thetas[i] = orth_est$theta
    }
    theta = mean(thetas, na.rm = TRUE)
    if (length(train_ids) == 1) {
      D = D[test_index]
      y_minus_l_hat = y_minus_l_hat[test_index]
      d_minus_m_hat = d_minus_m_hat[test_index]
      y_minus_g_hat = y_minus_g_hat[test_index]
    }
  }

  if (dml_procedure == "dml2") {
    orth_est = orth_plr_dml(
      y_minus_l_hat = y_minus_l_hat,
      d_minus_m_hat = d_minus_m_hat,
      y_minus_g_hat = y_minus_g_hat,
      d = D, score = score)
    theta = orth_est$theta
  }

  se = sqrt(var_plr(
    theta = theta, d = D,
    y_minus_l_hat = y_minus_l_hat,
    d_minus_m_hat = d_minus_m_hat,
    y_minus_g_hat = y_minus_g_hat,
    score = score))

  res = list(
    theta = theta, se = se,
    all_preds = all_preds)

  return(res)
}


fit_nuisance_plr = function(data, y, d,
  ml_l, ml_m, ml_g,
  n_folds, smpls, fit_g,
  params_l, params_m, params_g) {

  train_ids = smpls$train_ids
  test_ids = smpls$test_ids

  # nuisance l
  l_indx = names(data) != d
  data_l = data[, l_indx, drop = FALSE]
  task_l = mlr3::TaskRegr$new(
    id = paste0("nuis_l_", d),
    backend = data_l, target = y)

  resampling_l = mlr3::rsmp("custom")
  resampling_l$instantiate(task_l, train_ids, test_ids)

  if (!is.null(params_l)) {
    ml_l$param_set$values = params_l
  }

  r_l = mlr3::resample(task_l, ml_l, resampling_l, store_models = TRUE)
  l_hat_list = lapply(r_l$predictions(), function(x) x$response)

  # nuisance m
  if (!is.null(params_m)) {
    ml_m$param_set$values = params_m
  }
  m_indx = names(data) != y
  data_m = data[, m_indx, drop = FALSE]

  if (ml_m$task_type == "regr") {
    task_m = mlr3::TaskRegr$new(id = paste0("nuis_m_", d), backend = data_m, target = d)

    resampling_m = mlr3::rsmp("custom")
    resampling_m$instantiate(task_m, train_ids, test_ids)

    r_m = mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
    m_hat_list = lapply(r_m$predictions(), function(x) x$response)
  } else if ((ml_m$task_type == "classif")) {
    ml_m$predict_type = "prob"
    data_m[[d]] = factor(data_m[[d]])
    task_m = mlr3::TaskClassif$new(
      id = paste0("nuis_m_", d), backend = data_m,
      target = d, positive = "1")

    resampling_m = mlr3::rsmp("custom")
    resampling_m$instantiate(task_m, train_ids, test_ids)

    r_m = mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
    m_hat_list = lapply(r_m$predictions(), function(x) as.data.table(x)$prob.1)
  }

  if (fit_g) {
    # nuisance g
    residuals = compute_plr_residuals(
      data, y, d, n_folds,
      smpls, list(
        l_hat_list = l_hat_list,
        g_hat_list = NULL,
        m_hat_list = m_hat_list))
    y_minus_l_hat = residuals$y_minus_l_hat
    d_minus_m_hat = residuals$d_minus_m_hat
    psi_a = -d_minus_m_hat * d_minus_m_hat
    psi_b = d_minus_m_hat * y_minus_l_hat
    theta_initial = -mean(psi_b, na.rm = TRUE) / mean(psi_a, na.rm = TRUE)

    D = data[, d]
    Y = data[, y]
    g_indx = names(data) != y & names(data) != d
    y_minus_theta_d = Y - theta_initial * D
    data_g = cbind(data[, g_indx, drop = FALSE], y_minus_theta_d)

    task_g = mlr3::TaskRegr$new(
      id = paste0("nuis_g_", d), backend = data_g,
      target = "y_minus_theta_d")

    resampling_g = mlr3::rsmp("custom")
    resampling_g$instantiate(task_g, train_ids, test_ids)

    if (!is.null(params_g)) {
      ml_g$param_set$values = params_g
    }

    r_g = mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
    g_hat_list = lapply(r_g$predictions(), function(x) x$response)
  } else {
    g_hat_list = NULL
  }

  all_preds = list(
    l_hat_list = l_hat_list,
    m_hat_list = m_hat_list,
    g_hat_list = g_hat_list)

  return(all_preds)
}

compute_plr_residuals = function(data, y, d, n_folds, smpls, all_preds) {

  test_ids = smpls$test_ids

  l_hat_list = all_preds$l_hat_list
  m_hat_list = all_preds$m_hat_list
  g_hat_list = all_preds$g_hat_list

  n = nrow(data)
  D = data[, d]
  Y = data[, y]

  y_minus_l_hat = d_minus_m_hat = y_minus_g_hat = rep(NA_real_, n)

  for (i in 1:n_folds) {
    test_index = test_ids[[i]]

    l_hat = l_hat_list[[i]]
    m_hat = m_hat_list[[i]]

    y_minus_l_hat[test_index] = Y[test_index] - l_hat
    d_minus_m_hat[test_index] = D[test_index] - m_hat

    if (!is.null(g_hat_list)) {
      g_hat = g_hat_list[[i]]
      y_minus_g_hat[test_index] = Y[test_index] - g_hat
    }
  }
  residuals = list(
    y_minus_l_hat = y_minus_l_hat,
    d_minus_m_hat = d_minus_m_hat,
    y_minus_g_hat = y_minus_g_hat)

  return(residuals)
}


# Orthogonalized Estimation of Coefficient in PLR
orth_plr_dml = function(y_minus_l_hat, d_minus_m_hat, y_minus_g_hat, d, score) {
  theta = NA_real_

  if (score == "partialling out") {
    res_fit = stats::lm(y_minus_l_hat ~ 0 + d_minus_m_hat)
    theta = stats::coef(res_fit)
  }

  else if (score == "IV-type") {
    theta = mean(d_minus_m_hat * y_minus_g_hat) / mean(d_minus_m_hat * d)
  }

  else {
    stop("Inference framework for orthogonal estimation unknown")
  }

  res = list(theta = theta)
  return(res)
}


# Variance estimation for DML estimator in the partially linear regression model
var_plr = function(theta, d, y_minus_l_hat, d_minus_m_hat, y_minus_g_hat, score) {
  n = length(d)
  if (score == "partialling out") {
    var = 1 / n * 1 / (mean(d_minus_m_hat^2))^2 *
      mean(((y_minus_l_hat - d_minus_m_hat * theta) * d_minus_m_hat)^2)
  }
  else if (score == "IV-type") {
    var = 1 / n * 1 / mean(d_minus_m_hat * d)^2 *
      mean(((y_minus_g_hat - d * theta) * d_minus_m_hat)^2)
  }
  return(c(var))
}


# Bootstrap Implementation for Partially Linear Regression Model
bootstrap_plr = function(thetas, ses, data, y, d,
  n_folds, smpls, all_preds,
  bootstrap, n_rep_boot, score,
  n_rep = 1) {
  for (i_rep in 1:n_rep) {
    n = nrow(data)
    weights = draw_bootstrap_weights(bootstrap, n_rep_boot, n)
    this_res = boot_plr_single_split(
      thetas[i_rep], ses[i_rep],
      data, y, d, n_folds, smpls[[i_rep]],
      all_preds[[i_rep]],
      weights, n_rep_boot, score)
    if (i_rep == 1) {
      boot_res = this_res
    } else {
      boot_res$boot_coef = cbind(boot_res$boot_coef, this_res$boot_coef)
      boot_res$boot_t_stat = cbind(boot_res$boot_t_stat, this_res$boot_t_stat)
    }
  }
  return(boot_res)
}


boot_plr_multitreat = function(thetas, ses, data, y, d,
  n_folds, smpls, all_preds,
  bootstrap, n_rep_boot, score,
  n_rep = 1) {
  n_d = length(d)
  for (i_rep in 1:n_rep) {
    n = nrow(data)
    weights = draw_bootstrap_weights(bootstrap, n_rep_boot, n)
    boot_theta = boot_t_stat = matrix(NA_real_, nrow = n_d, ncol = n_rep_boot)
    for (i_d in seq(n_d)) {
      this_res = boot_plr_single_split(
        thetas[[i_rep]][i_d], ses[[i_rep]][i_d],
        data, y, d[i_d], n_folds, smpls[[i_rep]],
        all_preds[[i_rep]][[i_d]],
        weights, n_rep_boot, score)
      boot_theta[i_d, ] = this_res$boot_coef
      boot_t_stat[i_d, ] = this_res$boot_t_stat
    }
    this_res = list(
      boot_coef = boot_theta,
      boot_t_stat = boot_t_stat)
    if (i_rep == 1) {
      boot_res = this_res
    } else {
      boot_res$boot_coef = cbind(boot_res$boot_coef, this_res$boot_coef)
      boot_res$boot_t_stat = cbind(boot_res$boot_t_stat, this_res$boot_t_stat)
    }
  }
  return(boot_res)
}


boot_plr_single_split = function(theta, se, data, y, d,
  n_folds, smpl, all_preds,
  weights, n_rep_boot, score) {

  residuals = compute_plr_residuals(
    data, y, d, n_folds,
    smpl, all_preds)
  y_minus_l_hat = residuals$y_minus_l_hat
  d_minus_m_hat = residuals$d_minus_m_hat
  y_minus_g_hat = residuals$y_minus_g_hat

  D = data[, d]

  if (score == "partialling out") {
    psi = (y_minus_l_hat - d_minus_m_hat * theta) * d_minus_m_hat
    psi_a = -d_minus_m_hat * d_minus_m_hat
  }
  else if (score == "IV-type") {
    psi = (y_minus_g_hat - D * theta) * d_minus_m_hat
    psi_a = -d_minus_m_hat * D
  }

  res = functional_bootstrap(
    theta, se,
    psi, psi_a, n_folds,
    smpl,
    n_rep_boot, weights)
  return(res)
}
