dml_pliv_partial_x = function(data, y, d, z,
                              n_folds,
                              ml_g, ml_m, ml_r,
                              params, dml_procedure, score,
                              n_rep = 1, smpls=NULL,
                              params_g = NULL, params_m = NULL, params_r = NULL) {
  stopifnot(length(z) > 1)
  if (is.null(smpls)) {
    smpls = lapply(1:n_rep, function(x) sample_splitting(n_folds, data))
  }
  
  all_thetas = all_ses = rep(NA_real_, n_rep)
  all_preds = list()
  
  for (i_rep in 1:n_rep) {
    this_smpl = smpls[[i_rep]]
    
    all_preds[[i_rep]] = fit_nuisance_pliv_partial_x(data, y, d, z,
                                                     ml_g, ml_m, ml_r,
                                                     this_smpl,
                                                     params_g, params_m, params_r)
    
    residuals = compute_pliv_partial_x_residuals(data, y, d, z, n_folds,
                                                 this_smpl,
                                                 all_preds[[i_rep]])
    u_hat = residuals$u_hat
    w_hat = residuals$w_hat
    r_hat_tilde = residuals$r_hat_tilde
    
    # DML 1
    if (dml_procedure == "dml1") {
      thetas = vars = rep(NA_real_, n_folds)
      for (i in 1:n_folds) {
        test_index = this_smpl$test_ids[[i]]
        orth_est = orth_pliv_partial_x_dml(
          u_hat = u_hat[test_index],
          w_hat = w_hat[test_index],
          r_hat_tilde = r_hat_tilde[test_index],
          score = score)
        thetas[i] = orth_est$theta
      }
      all_thetas[i_rep] = mean(thetas, na.rm = TRUE)
    }
    if (dml_procedure == "dml2") {
      orth_est = orth_pliv_partial_x_dml(
        u_hat = u_hat, w_hat = w_hat, r_hat_tilde = r_hat_tilde,
        score = score)
      all_thetas[i_rep] = orth_est$theta
    }
    
    all_ses[i_rep] = sqrt(var_pliv_partial_x(
      theta = all_thetas[i_rep], u_hat = u_hat, w_hat = w_hat,
      r_hat_tilde = r_hat_tilde, score = score))
  }
  
  theta = stats::median(all_thetas)
  if (length(this_smpl$train_ids) > 1) {
    n = nrow(data)
  } else {
    n = length(this_smpl$test_ids[[1]])
  }
  se = se_repeated(all_ses*sqrt(n), all_thetas, theta)/sqrt(n)

  t = theta / se
  pval = 2 * stats::pnorm(-abs(t))

  names(theta) = names(se) = d
  res = list(
    coef = theta, se = se, t = t, pval = pval,
    thetas = all_thetas, ses = all_ses,
    all_preds=all_preds, smpls=smpls)

  return(res)
}

fit_nuisance_pliv_partial_x = function(data, y, d, z,
                                       ml_g, ml_m, ml_r,
                                       smpls,
                                       params_g, params_m, params_r) {
  train_ids = smpls$train_ids
  test_ids = smpls$test_ids

  # nuisance g: E[Y|X]
  g_indx = names(data) != d & (names(data) %in% z == FALSE)
  data_g = data[, g_indx, drop = FALSE]
  task_g = mlr3::TaskRegr$new(id = paste0("nuis_g_", d), backend = data_g, target = y)
  
  resampling_g = mlr3::rsmp("custom")
  resampling_g$instantiate(task_g, train_ids, test_ids)
  
  if (!is.null(params_g)) {
    ml_g$param_set$values = params_g
  }
  
  r_g = mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
  g_hat_list = lapply(r_g$predictions(), function(x) x$response)
  
  # nuisance m: E[Z|X]
  n_z = length(z)
  m_hat_list = list()
  for (i_z in seq(n_z)) {
    m_indx = (names(data) != y) & (names(data) != d) & (names(data) %in% z[-i_z] == FALSE)
    data_m = data[, m_indx, drop = FALSE]
    task_m = mlr3::TaskRegr$new(id = paste0("nuis_m_", z[i_z]), backend = data_m, target = z[i_z])
    this_ml_m = ml_m$clone()
    if (!is.null(params_m)) {
      this_ml_m$param_set$values = params_m
    }
    
    resampling_m = mlr3::rsmp("custom")
    resampling_m$instantiate(task_m, train_ids, test_ids)
    
    r_m = mlr3::resample(task_m, this_ml_m, resampling_m, store_models = TRUE)
    m_hat_list[[i_z]] = lapply(r_m$predictions(), function(x) x$response)
  }

  # nuisance r: E[D|X]
  r_indx = names(data) != y & (names(data) %in% z == FALSE)
  data_r = data[, r_indx, drop = FALSE]
  task_r = mlr3::TaskRegr$new(id = paste0("nuis_r_", d), backend = data_r, target = d)
  if (!is.null(params_r)) {
    ml_g$param_set$values = params_r
  }
  
  resampling_r = mlr3::rsmp("custom")
  resampling_r$instantiate(task_r, train_ids, test_ids)
  
  r_r = mlr3::resample(task_r, ml_r, resampling_r, store_models = TRUE)
  r_hat_list = lapply(r_r$predictions(), function(x) x$response)
  
  n = nrow(data)
  r_hat_array = rep(NA_real_, n)
  m_hat_array = matrix(NA_real_, nrow = n, ncol = n_z)
  
  for (i_fold in seq_len(length(test_ids))) {
    test_index = test_ids[[i_fold]]
    r_hat_array[test_index] = r_hat_list[[i_fold]]
    for (i_z in seq(n_z)) {
      m_hat_array[test_index, i_z] = m_hat_list[[i_z]][[i_fold]]
    }
  }
  D = data[, d]
  Z = data[, z]
  r_hat_tilde = predict(lm(D - r_hat_array ~ 1 + as.matrix(Z - m_hat_array)),
                        Z - m_hat_array)
  
  all_preds = list(
    g_hat_list = g_hat_list,
    r_hat_list = r_hat_list,
    r_hat_tilde = r_hat_tilde)

  return(all_preds)
}

compute_pliv_partial_x_residuals = function(data, y, d, z, n_folds, smpls,
                                            all_preds) {
  test_ids = smpls$test_ids

  g_hat_list = all_preds$g_hat_list
  r_hat_list = all_preds$r_hat_list
  r_hat_tilde = all_preds$r_hat_tilde
  
  n = nrow(data)
  D = data[, d]
  Y = data[, y]
  
  u_hat = w_hat = rep(NA_real_, n)
  
  for (i in 1:n_folds) {
    test_index = test_ids[[i]]
    
    g_hat = g_hat_list[[i]]
    r_hat = r_hat_list[[i]]

    u_hat[test_index] = Y[test_index] - g_hat
    w_hat[test_index] = D[test_index] - r_hat
  }
  residuals = list(u_hat=u_hat, w_hat=w_hat, r_hat_tilde=r_hat_tilde)

  return(residuals)
}


orth_pliv_partial_x_dml = function(u_hat, w_hat, r_hat_tilde, score) {
  stopifnot(score == "partialling out")
  theta = mean(r_hat_tilde * u_hat) / mean(r_hat_tilde * w_hat)
  res = list(theta = theta)
  return(res)
}


var_pliv_partial_x = function(theta, u_hat, w_hat, r_hat_tilde, score) {
  stopifnot(score == "partialling out")
  var = mean(1 / length(u_hat) * 1 / (mean(r_hat_tilde * w_hat))^2 *
               mean(((u_hat - w_hat * theta) * r_hat_tilde)^2))
  return(c(var))
}


bootstrap_pliv_partial_x = function(theta, se, data, y, d, z, n_folds, smpls,
                                    all_preds, bootstrap,
                                    n_rep_boot, n_rep=1) {
  for (i_rep in 1:n_rep) {
    residuals = compute_pliv_partial_x_residuals(data, y, d, z, n_folds,
                                                 smpls[[i_rep]],
                                                 all_preds[[i_rep]])
    u_hat = residuals$u_hat
    w_hat = residuals$w_hat
    r_hat_tilde = residuals$r_hat_tilde
    
    psi = (u_hat - w_hat * theta[i_rep]) * r_hat_tilde
    psi_a = - r_hat_tilde * w_hat

    n = length(psi)
    weights = draw_bootstrap_weights(bootstrap, n_rep_boot, n)
    this_res = functional_bootstrap(theta[i_rep], se[i_rep], psi, psi_a, n_folds,
                                    smpls[[i_rep]],
                                    n_rep_boot, weights)
    if (i_rep==1) {
      boot_res = this_res
    } else {
      boot_res$boot_coef = cbind(boot_res$boot_coef, this_res$boot_coef)
      boot_res$boot_t_stat = cbind(boot_res$boot_t_stat, this_res$boot_t_stat)
    }
  }
  return(boot_res)
}
