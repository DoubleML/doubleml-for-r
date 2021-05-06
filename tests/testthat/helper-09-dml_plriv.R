# Double Machine Learning for Partially Linear Instrumental Variable Regression.
dml_plriv = function(data, y, d, z,
                     n_folds, mlmethod,
                     params, dml_procedure, score,
                     n_rep = 1, smpls=NULL) {
  
  if (is.null(smpls)) {
    smpls = lapply(1:n_rep, function(x) sample_splitting(n_folds, data))
  }
  
  all_thetas = all_ses = rep(NA, n_rep)
  all_preds = list()
  
  for (i_rep in 1:n_rep) {
    this_smpl = smpls[[i_rep]]
    train_ids = this_smpl$train_ids
    test_ids = this_smpl$test_ids
    
    all_preds[[i_rep]] = fit_nuisance_pliv(data, y, d, z,
                                           mlmethod, params,
                                           this_smpl)
    
    residuals = compute_plriv_residuals(data, y, d, z, n_folds, this_smpl,
                                        all_preds[[i_rep]])
    u_hat = residuals$u_hat
    v_hat = residuals$v_hat
    w_hat = residuals$w_hat
    
    # DML 1
    if (dml_procedure == "dml1") {
      thetas = vars = rep(NA, n_folds)
      for (i in 1:n_folds) {
        test_index = test_ids[[i]]
        orth_est = orth_plriv_dml(
          u_hat = u_hat[test_index],
          v_hat = v_hat[test_index],
          w_hat = w_hat[test_index],
          score = score)
        thetas[i] = orth_est$theta
      }
      all_thetas[i_rep] = mean(thetas, na.rm = TRUE)
    }
    if (dml_procedure == "dml2") {
      orth_est = orth_plriv_dml(
        u_hat = u_hat, v_hat = v_hat, w_hat = w_hat,
        score = score)
      all_thetas[i_rep] = orth_est$theta
    }
    
    all_ses[i_rep] = sqrt(var_plriv(
      theta = all_thetas[i_rep], u_hat = u_hat, v_hat = v_hat,
      w_hat = w_hat, score = score))
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

fit_nuisance_pliv = function(data, y, d, z,
                             mlmethod, params,
                             smpls) {
  train_ids = smpls$train_ids
  test_ids = smpls$test_ids

  # nuisance g: E[Y|X]
  g_indx = names(data) != d & names(data) != z
  data_g = data[, g_indx, drop = FALSE]
  task_g = mlr3::TaskRegr$new(id = paste0("nuis_g_", d), backend = data_g, target = y)
  
  resampling_g = mlr3::rsmp("custom")
  resampling_g$instantiate(task_g, train_ids, test_ids)
  
  ml_g = mlr3::lrn(mlmethod$mlmethod_g)
  ml_g$param_set$values = params$params_g
  
  r_g = mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
  g_hat_list = lapply(r_g$data$predictions(), function(x) x$response)
  
  # nuisance m: E[Z|X]
  m_indx = names(data) != y & names(data) != d
  data_m = data[, m_indx, drop = FALSE]
  task_m = mlr3::TaskRegr$new(id = paste0("nuis_m_", z), backend = data_m, target = z)
  ml_m = mlr3::lrn(mlmethod$mlmethod_m)
  ml_m$param_set$values = params$params_m
  
  resampling_m = mlr3::rsmp("custom")
  resampling_m$instantiate(task_m, train_ids, test_ids)
  
  r_m = mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
  m_hat_list = lapply(r_m$data$predictions(), function(x) x$response)

  # nuisance r: E[D|X]
  r_indx = names(data) != y & names(data) != z
  data_r = data[, r_indx, drop = FALSE]
  task_r = mlr3::TaskRegr$new(id = paste0("nuis_r_", d), backend = data_r, target = d)
  ml_r = mlr3::lrn(mlmethod$mlmethod_r)
  ml_r$param_set$values = params$params_r
  
  resampling_r = mlr3::rsmp("custom")
  resampling_r$instantiate(task_r, train_ids, test_ids)
  
  r_r = mlr3::resample(task_r, ml_r, resampling_r, store_models = TRUE)
  r_hat_list = lapply(r_r$data$predictions(), function(x) x$response)
  
  all_preds = list(
    m_hat_list = m_hat_list,
    g_hat_list = g_hat_list,
    r_hat_list = r_hat_list)

  return(all_preds)
}

compute_plriv_residuals = function(data, y, d, z, n_folds, smpls, all_preds) {
  test_ids = smpls$test_ids

  m_hat_list = all_preds$m_hat_list
  g_hat_list = all_preds$g_hat_list
  r_hat_list = all_preds$r_hat_list
  
  n = nrow(data)
  D = data[, d]
  Y = data[, y]
  Z = data[, z]
  
  v_hat = u_hat = w_hat = rep(NA, n)
  
  for (i in 1:n_folds) {
    test_index = test_ids[[i]]
    
    m_hat = m_hat_list[[i]]
    g_hat = g_hat_list[[i]]
    r_hat = r_hat_list[[i]]
    
    v_hat[test_index] = D[test_index] - r_hat
    u_hat[test_index] = Y[test_index] - g_hat
    w_hat[test_index] = Z[test_index] - m_hat
  }
  residuals = list(u_hat=u_hat, v_hat=v_hat, w_hat=w_hat)

  return(residuals)
}

# Orthogonalized Estimation of Coefficient in PLR
orth_plriv_dml = function(u_hat, v_hat, w_hat, score) {
  if (score == "partialling out") {
    theta = mean(u_hat * w_hat) / mean(v_hat * w_hat)
  } else {
    stop("Inference framework for orthogonal estimation unknown")
  }
  res = list(theta = theta)
  return(res)
}

# Variance estimation for DML estimator in the partially linear regression model
var_plriv = function(theta, u_hat, v_hat, w_hat, score) {
  if (score == "partialling out") {
    var = mean(1 / length(u_hat) * 1 / (mean(v_hat * w_hat))^2 *
      mean(((u_hat - v_hat * theta) * w_hat)^2))
  } else {
    stop("Inference framework for variance estimation unknown")
  }
  return(c(var))
}

# Bootstrap Implementation for Partially Linear Regression Model
bootstrap_plriv = function(theta, se, data, y, d, z, n_folds, smpls,
                           all_preds, dml_procedure, bootstrap, n_rep_boot,
                           n_rep=1) {
  for (i_rep in 1:n_rep) {
    residuals = compute_plriv_residuals(data, y, d, z, n_folds,
                                        smpls[[i_rep]], all_preds[[i_rep]])
    u_hat = residuals$u_hat
    v_hat = residuals$v_hat
    w_hat = residuals$w_hat
    
    psi = (u_hat - v_hat * theta) * w_hat
    psi_a = - v_hat * w_hat
    
    n = length(psi)
    weights = draw_bootstrap_weights(bootstrap, n_rep_boot, n)
    this_res = functional_bootstrap(theta, se, psi, psi_a, n_folds,
                                    smpls[[i_rep]],
                                    dml_procedure, n_rep_boot, weights)
    if (i_rep==1) {
      boot_res = this_res
    } else {
      boot_res$boot_coef = cbind(boot_res$boot_coef, this_res$boot_coef)
      boot_res$boot_t_stat = cbind(boot_res$boot_t_stat, this_res$boot_t_stat)
    }
  }
  return(boot_res)
}
