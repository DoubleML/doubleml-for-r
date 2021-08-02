# Double Machine Learning for Interactive Regression Model.
dml_irm = function(data, y, d,
                   n_folds, ml_g, ml_m,
                   dml_procedure, score,
                   n_rep = 1, smpls = NULL,
                   trimming_threshold = 1e-12,
                   params_g = NULL, params_m = NULL) {
  
  if (is.null(smpls)) {
    smpls = lapply(1:n_rep, function(x) sample_splitting(n_folds, data))
  }
  
  all_thetas = all_ses = rep(NA_real_, n_rep)
  all_preds = list()
  
  for (i_rep in 1:n_rep) {
    this_smpl = smpls[[i_rep]]
    train_ids = this_smpl$train_ids
    test_ids = this_smpl$test_ids
    
    all_preds[[i_rep]] = fit_nuisance_irm(data, y, d,
                                          ml_g, ml_m,
                                          train_ids, test_ids, score,
                                          params_g, params_m)
    res = extract_irm_residuals(data, y, d, n_folds, this_smpl,
                                all_preds[[i_rep]], score,
                                trimming_threshold=trimming_threshold)
    u0_hat = res$u0_hat
    u1_hat = res$u1_hat
    m_hat = res$m_hat
    p_hat = res$p_hat
    g0_hat = res$g0_hat
    g1_hat = res$g1_hat
    D = data[, d]
    Y = data[, y]
    
    # DML 1
    if (dml_procedure == "dml1") {
      thetas = vars = rep(NA_real_, n_folds)
      
      for (i in 1:n_folds) {
        test_index = test_ids[[i]]
        orth_est = orth_irm_dml(
          g0_hat = g0_hat[test_index], g1_hat = g1_hat[test_index],
          u0_hat = u0_hat[test_index], u1_hat = u1_hat[test_index],
          d = D[test_index], p_hat = p_hat[test_index], m = m_hat[test_index],
          y = Y[test_index],
          score = score)
        thetas[i] = orth_est$theta
      }
      all_thetas[i_rep] = mean(thetas, na.rm = TRUE)
      if (length(train_ids) == 1) {
        D = D[test_index]
        Y = Y[test_index]
        g0_hat = g0_hat[test_index]
        g1_hat = g1_hat[test_index]
        u0_hat = u0_hat[test_index]
        u1_hat = u1_hat[test_index]
        p_hat = p_hat[test_index]
        m_hat = m_hat[test_index]
      }
    }
    if (dml_procedure == "dml2") {
      orth_est = orth_irm_dml(
        g0_hat = g0_hat, g1_hat = g1_hat,
        u0_hat = u0_hat, u1_hat = u1_hat, d = D, p_hat = p_hat,
        y = Y, m = m_hat, score = score)
      all_thetas[i_rep] = orth_est$theta
    }
    
    all_ses[i_rep] = sqrt(var_irm(
      theta = all_thetas[i_rep], g0_hat = g0_hat, g1_hat = g1_hat,
      u0_hat = u0_hat, u1_hat = u1_hat,
      d = D, p_hat = p_hat, m = m_hat, y = Y, score = score))
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
    all_preds = all_preds, smpls=smpls)

  return(res)
}

fit_nuisance_irm = function(data, y, d,
                            ml_g, ml_m,
                            train_ids, test_ids, score,
                            params_g, params_m) {
  # Set up task_m first to get resampling (test and train ids) scheme based on full sample
  # nuisance m
  m_indx = names(data) != y
  data_m = data[, m_indx, drop = FALSE]
  
  # tbd: handle case with classif vs. regr. for task_p
  data_m[, d] = factor(data_m[, d])
  task_m = mlr3::TaskClassif$new(
    id = paste0("nuis_p_", d), backend = data_m,
    target = d, positive = "1")
  
  resampling_m = mlr3::rsmp("custom")
  resampling_m$instantiate(task_m, train_ids, test_ids)
  n_iters = resampling_m$iters
  
  # train and test ids according to status of d
  # in each fold, select those with d = 0
  train_ids_0 = lapply(1:n_iters, function(x) {
    resampling_m$train_set(x)[data[resampling_m$train_set(x), d] == 0]
  })
  # in each fold, select those with d = 0
  train_ids_1 = lapply(1:n_iters, function(x) {
    resampling_m$train_set(x)[data[resampling_m$train_set(x), d] == 1]
  })

  if (!is.null(params_m)) {
    ml_m$param_set$values = params_m
  }
  r_m = mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
  m_hat_list = lapply(r_m$predictions(), function(x) x$prob[, "1"])

  # nuisance g0: E[Y|D=0, X]
  g_indx = names(data) != d
  data_g = data[, g_indx, drop = FALSE]
  task_g0 = mlr3::TaskRegr$new(id = paste0("nuis_g0_", d), backend = data_g, target = y)
  ml_g0 = ml_g$clone()
  if (!is.null(params_g)) {
    ml_g0$param_set$values = params_g
  }
  resampling_g0 = mlr3::rsmp("custom")
  # Train on subset with d == 0 (in each fold) only, predict for all test obs
  resampling_g0$instantiate(task_g0, train_ids_0, test_ids)
  train_ids_g0 = lapply(1:n_iters, function(x) resampling_g0$train_set(x))
  test_ids_g0 = lapply(1:n_iters, function(x) resampling_g0$test_set(x))
  
  r_g0 = mlr3::resample(task_g0, ml_g0, resampling_g0, store_models = TRUE)
  g0_hat_list = lapply(r_g0$predictions(), function(x) x$response)
  
  # nuisance g1: E[Y|D=1, X]
  if (score == "ATE") {
    task_g1 = mlr3::TaskRegr$new(id = paste0("nuis_g1_", d), backend = data_g, target = y)
    ml_g1 = ml_g$clone()
    if (!is.null(params_g)) {
      ml_g1$param_set$values = params_g
    }
    resampling_g1 = mlr3::rsmp("custom")
    resampling_g1$instantiate(task_g1, train_ids_1, test_ids)
    train_ids_g1 = lapply(1:n_iters, function(x) resampling_g1$train_set(x))
    test_ids_g1 = lapply(1:n_iters, function(x) resampling_g1$test_set(x))
    
    r_g1 = mlr3::resample(task_g1, ml_g1, resampling_g1, store_models = TRUE)
    g1_hat_list = lapply(r_g1$predictions(), function(x) x$response)
  } else {
    g1_hat_list = NULL
  }

  all_preds = list(
    m_hat_list = m_hat_list,
    g0_hat_list = g0_hat_list,
    g1_hat_list = g1_hat_list)

  return(all_preds)
}

extract_irm_residuals = function(data, y, d, n_folds, smpls, all_preds, score,
                                 trimming_threshold) {
  test_ids = smpls$test_ids
  
  m_hat_list = all_preds$m_hat_list
  g0_hat_list = all_preds$g0_hat_list
  g1_hat_list = all_preds$g1_hat_list
  
  n = nrow(data)
  D = data[, d]
  Y = data[, y]
  
  g0_hat = g1_hat = u0_hat = u1_hat = m_hat = p_hat = rep(NA_real_, n)
  
  for (i in 1:n_folds) {
    test_index = test_ids[[i]]
    
    m_hat[test_index] = m_hat_list[[i]]
    p_hat[test_index] = mean(D[test_index])
    g0_hat[test_index] = g0_hat_list[[i]]
    
    if (score == "ATE") {
      g1_hat[test_index] = g1_hat_list[[i]]
    }
    
    u0_hat[test_index] = Y[test_index] - g0_hat[test_index]
    
    if (score == "ATE") {
      u1_hat[test_index] = Y[test_index] - g1_hat[test_index]
    }
  }
  
  m_hat = trim_vec(m_hat, trimming_threshold)
  
  res = list(u0_hat=u0_hat, u1_hat=u1_hat,
             m_hat=m_hat, p_hat=p_hat,
             g0_hat=g0_hat, g1_hat=g1_hat)
  
  return(res)
}

# Orthogonalized Estimation of Coefficient in irm
orth_irm_dml = function(g0_hat, g1_hat, u0_hat, u1_hat, d, p_hat, m, y, score) {

  if (score == "ATE") {
    theta = mean(g1_hat - g0_hat + d * (u1_hat) / m - (1 - d) * u0_hat / (1 - m))
  }
  else if (score == "ATTE") {
    theta = mean(d * (y - g0_hat) / p_hat - m * (1 - d) * u0_hat / (p_hat * (1 - m))) / mean(d / p_hat)
  }
  else {
    stop("Inference framework for orthogonal estimation unknown")
  }

  res = list(theta = theta)
  return(res)
}


# Variance estimation for DML estimator in the interactive regression model
var_irm = function(theta, g0_hat, g1_hat, u0_hat, u1_hat, d, p_hat, m, y, score) {
  n = length(d)
  if (score == "ATE") {
    var = 1 / n * mean(((g1_hat - g0_hat + d * (u1_hat) / m - (1 - d) * u0_hat / (1 - m) - theta)^2))
  }
  else if (score == "ATTE") {
    var = 1 / n * mean((d * (y - g0_hat) / p_hat - m * (1 - d) * u0_hat / (p_hat * (1 - m)) - d / p_hat * theta)^2) / (mean(d/p_hat)^2)
  }

  return(c(var))
}

# Bootstrap Implementation for Interactive Regression Model
bootstrap_irm = function(theta, se, data, y, d, n_folds, smpls, all_preds,
                         score, bootstrap, n_rep_boot,
                         n_rep=1, trimming_threshold = 1e-12) {
  for (i_rep in 1:n_rep) {
    res = extract_irm_residuals(data, y, d, n_folds,
                                smpls[[i_rep]], all_preds[[i_rep]], score,
                                trimming_threshold=trimming_threshold)
    u0_hat = res$u0_hat
    u1_hat = res$u1_hat
    m_hat = res$m_hat
    p_hat = res$p_hat
    g0_hat = res$g0_hat
    g1_hat = res$g1_hat
    D = data[, d]
    
    if (score == "ATE") {
      psi = g1_hat - g0_hat + D * u1_hat / m_hat - (1 - D) * u0_hat / (1 - m_hat) - theta[i_rep]
      psi_a = rep(-1, length(D))
    }
    else if (score == "ATTE") {
      psi = D * u0_hat / p_hat - m_hat * (1 - D) * u0_hat / (p_hat * (1 - m_hat)) - D / p_hat * theta[i_rep]
      psi_a = -D / p_hat
    }
    
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
