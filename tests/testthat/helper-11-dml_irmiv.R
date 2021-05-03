# Double Machine Learning for Interactive Instrumental Variable Regression Model.
dml_irmiv = function(data, y, d, z,
                     n_folds, mlmethod,
                     params, dml_procedure, score,
                     always_takers = TRUE, never_takers = TRUE,
                     n_rep = 1, smpls = NULL) {
  
  if (is.null(smpls)) {
    smpls = lapply(1:n_rep, function(x) sample_splitting(n_folds, data))
  }
  
  all_thetas = all_ses = rep(NA, n_rep)
  all_preds = list()
  
  for (i_rep in 1:n_rep) {
    this_smpl = smpls[[i_rep]]
    train_ids = this_smpl$train_ids
    test_ids = this_smpl$test_ids
    
    all_preds[[i_rep]] = fit_nuisance_iivm(data, y, d, z,
                                           mlmethod, params,
                                           train_ids, test_ids,
                                           always_takers, never_takers)
    res = extract_iivm_preds(data, y, d, z, n_folds,
                             this_smpl, all_preds[[i_rep]])
    p_hat = res$p_hat
    mu0_hat = res$mu0_hat
    mu1_hat = res$mu1_hat
    m0_hat = res$m0_hat
    m1_hat = res$m1_hat
    D = data[, d]
    Y = data[, y]
    Z = data[, z]
    
    # DML 1
    if (dml_procedure == "dml1") {
      thetas = vars = rep(NA, n_folds)
      for (i in 1:n_folds) {
        test_index = test_ids[[i]]
        orth_est = orth_irmiv_dml(
          p_hat = p_hat[test_index],
          mu0_hat = mu0_hat[test_index], mu1_hat = mu1_hat[test_index],
          m0_hat = m0_hat[test_index], m1_hat = m1_hat[test_index],
          d = D[test_index], y = Y[test_index], z = Z[test_index],
          score = score)
        thetas[i] = orth_est$theta
      }
      all_thetas[i_rep] = mean(thetas, na.rm = TRUE)
    }
    if (dml_procedure == "dml2") {
      orth_est = orth_irmiv_dml(
        p_hat = p_hat, mu0_hat = mu0_hat,
        mu1_hat = mu1_hat,
        m0_hat = m0_hat, m1_hat = m1_hat,
        d = D, y = Y, z = Z,
        score = score)
      all_thetas[i_rep] = orth_est$theta
    }
    
    all_ses[i_rep] = sqrt(var_irmiv(
      theta = all_thetas[i_rep], p_hat = p_hat, mu0_hat = mu0_hat,
      mu1_hat = mu1_hat, m0_hat = m0_hat, m1_hat = m1_hat,
      d = D, y = Y, z = Z, score = score))
  }
  
  theta = stats::median(all_thetas)
  se = se = sqrt(stats::median(all_ses^2 + (all_thetas - theta)^2))
  
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
                             mlmethod, params,
                             train_ids, test_ids,
                             always_takers, never_takers) {

  # Set up task_m first to get resampling (test and train ids) scheme based on full sample
  # nuisance m
  p_indx = names(data) != y & names(data) != d
  data_p = data[, p_indx, drop = FALSE]
  
  # tbd: handle case with classif vs. regr. for task_p
  # if (grepl("regr.", mlmethod$mlmethod_p )) {
  #  # task_p = mlr3::TaskRegr$new(id = paste0("nuis_p_", z), backend = data_p, target = z)
  #   task_p = mlr3::TaskRegr$new(id = paste0("nuis_p_", z), backend = data_p, target = z)
  #   # task_p = mlr3::tsk(id = paste0("nuis_p_", z), backend )
  # }

  # if (grepl("classif.", mlmethod$mlmethod_p )) {
  data_p[, z] = factor(data_p[, z])
  task_p = mlr3::TaskClassif$new(
    id = paste0("nuis_p_", z), backend = data_p,
    target = z, positive = "1")
  # }
  
  resampling_p = mlr3::rsmp("custom")
  resampling_p$instantiate(task_p, train_ids, test_ids)
  n_iters = resampling_p$iters

  # in each fold, select those with z = 0
  train_ids_0 = lapply(1:n_iters, function(x) {
    resampling_p$train_set(x)[data[resampling_p$train_set(x), z] == 0]
  })
  # in each fold, select those with d = 0
  train_ids_1 = lapply(1:n_iters, function(x) {
    resampling_p$train_set(x)[data[resampling_p$train_set(x), z] == 1]
  })

  ml_p = mlr3::lrn(mlmethod$mlmethod_p, predict_type = "prob")
  ml_p$param_set$values = params$params_p
  r_p = mlr3::resample(task_p, ml_p, resampling_p, store_models = TRUE)
  p_hat_list = lapply(r_p$data$predictions(), function(x) x$prob[, "1"])

  # nuisance mu0: E[Y|Z=0, X]
  mu_indx = names(data) != d & names(data) != z
  data_mu = data[, mu_indx, drop = FALSE]
  task_mu0 = mlr3::TaskRegr$new(id = paste0("nuis_mu0_", z), backend = data_mu, target = y)
  ml_mu0 = mlr3::lrn(mlmethod$mlmethod_mu)
  ml_mu0$param_set$values = params$params_mu
  resampling_mu0 = mlr3::rsmp("custom")
  # Train on subset with z == 0 (in each fold) only, predict for all test obs
  resampling_mu0$instantiate(task_mu0, train_ids_0, test_ids)
  train_ids_mu0 = lapply(1:n_iters, function(x) resampling_mu0$train_set(x))
  test_ids_mu0 = lapply(1:n_iters, function(x) resampling_mu0$test_set(x))
  
  r_mu0 = mlr3::resample(task_mu0, ml_mu0, resampling_mu0, store_models = TRUE)
  mu0_hat_list = lapply(r_mu0$data$predictions(), function(x) x$response)
  
  # nuisance g1: E[Y|Z=1, X]
  task_mu1 = mlr3::TaskRegr$new(id = paste0("nuis_mu1_", z), backend = data_mu, target = y)
  ml_mu1 = mlr3::lrn(mlmethod$mlmethod_mu)
  ml_mu1$param_set$values = params$params_mu
  resampling_mu1 = mlr3::rsmp("custom")
  # Train on subset with z == 1 (in each fold) only, predict for all test obs
  resampling_mu1$instantiate(task_mu1, train_ids_1, test_ids)
  train_ids_mu1 = lapply(1:n_iters, function(x) resampling_mu1$train_set(x))
  test_ids_mu1 = lapply(1:n_iters, function(x) resampling_mu1$test_set(x))

  r_mu1 = mlr3::resample(task_mu1, ml_mu1, resampling_mu1, store_models = TRUE)
  # mu1_hat_list = lapply(r_mu1$data$prediction, function(x) x$test$response)
  mu1_hat_list = lapply(r_mu1$data$predictions(), function(x) x$response)

  # nuisance m0: E[D|Z=0, X]
  m_indx = names(data) != y & names(data) != z
  data_m = data[, m_indx, drop = FALSE]
  data_m[, d] = factor(data_m[, d])

  if (always_takers == FALSE & never_takers == FALSE) {
    message("If there are no always-takers and no never-takers, ATE is estimated")
  }
  
  if (always_takers == FALSE) {
    lengths = lapply(test_ids, length)
    m0_hat_list = lapply(lengths, function(x) rep(0, x))
  }
  
  if (always_takers == TRUE) {
    task_m0 = mlr3::TaskClassif$new(
      id = paste0("nuis_m0_", d), backend = data_m,
      target = d, positive = "1")
    ml_m0 = mlr3::lrn(mlmethod$mlmethod_m, predict_type = "prob")
    ml_m0$param_set$values = params$params_m
    
    resampling_m0 = mlr3::rsmp("custom")
    # Train on subset with z == 0 (in each fold) only, predict for all test obs
    resampling_m0$instantiate(task_m0, train_ids_0, test_ids)
    train_ids_m0 = lapply(1:n_iters, function(x) resampling_m0$train_set(x))
    test_ids_m0 = lapply(1:n_iters, function(x) resampling_m0$test_set(x))
    r_m0 = mlr3::resample(task_m0, ml_m0, resampling_m0, store_models = TRUE)
    m0_hat_list = lapply(r_m0$data$predictions(), function(x) x$prob[, "1"])
  }
  
  if (never_takers == FALSE) {
    lengths = lapply(test_ids, length)
    m1_hat_list = lapply(lengths, function(x) rep(1, x))
  }
  
  if (never_takers == TRUE) {
    # nuisance m1: E[E|Z=1, 0]
    task_m1 = mlr3::TaskClassif$new(
      id = paste0("nuis_m1_", d), backend = data_m,
      target = d, positive = "1")
    ml_m1 = mlr3::lrn(mlmethod$mlmethod_m, predict_type = "prob")
    ml_m1$param_set$values = params$params_m
    
    resampling_m1 = mlr3::rsmp("custom")
    # Train on subset with z == 0 (in each fold) only, predict for all test obs
    resampling_m1$instantiate(task_m1, train_ids_1, test_ids)
    train_ids_m1 = lapply(1:n_iters, function(x) resampling_m1$train_set(x))
    test_ids_m1 = lapply(1:n_iters, function(x) resampling_m1$test_set(x))
    r_m1 = mlr3::resample(task_m1, ml_m1, resampling_m1, store_models = TRUE)
    m1_hat_list = lapply(r_m1$data$predictions(), function(x) x$prob[, "1"])
  }

  all_preds = list(
    p_hat_list = p_hat_list,
    mu0_hat_list = mu0_hat_list,
    mu1_hat_list = mu1_hat_list,
    m0_hat_list = m0_hat_list,
    m1_hat_list = m1_hat_list)

  return(all_preds)
}


extract_iivm_preds = function(data, y, d, z, n_folds, smpls, all_preds) {
  test_ids = smpls$test_ids
  
  p_hat_list = all_preds$p_hat_list
  mu0_hat_list = all_preds$mu0_hat_list
  mu1_hat_list = all_preds$mu1_hat_list
  m0_hat_list = all_preds$m0_hat_list
  m1_hat_list = all_preds$m1_hat_list
  
  n = nrow(data)
  D = data[, d]
  Y = data[, y]
  Z = data[, z]
  p_hat = mu0_hat = mu1_hat = m0_hat = m1_hat = rep(NA, n)
  
  for (i in 1:n_folds) {
    test_index = test_ids[[i]]
    
    p_hat[test_index] = p_hat_list[[i]]
    mu0_hat[test_index] = mu0_hat_list[[i]]
    mu1_hat[test_index] = mu1_hat_list[[i]]
    m0_hat[test_index] = m0_hat_list[[i]]
    m1_hat[test_index] = m1_hat_list[[i]]
  }
  
  res = list(p_hat=p_hat, mu0_hat=mu0_hat, mu1_hat=mu1_hat,
             m0_hat=m0_hat, m1_hat=m1_hat)
  return(res)
}


# Orthogonalized Estimation of Coefficient in irm
orth_irmiv_dml = function(p_hat, mu0_hat, mu1_hat, m0_hat, m1_hat, d, y, z, score) {
  theta = NA

  if (score == "LATE" | score == "partialling out") {
    theta = 1 / mean(m1_hat - m0_hat + z * (d - m1_hat) / p_hat - ((1 - z) * (d - m0_hat) / (1 - p_hat))) *
      mean(mu1_hat - mu0_hat + z * (y - mu1_hat) / p_hat - ((1 - z) * (y - mu0_hat) / (1 - p_hat)))
  }
  else {
    stop("Inference framework for orthogonal estimation unknown")
  }

  res = list(theta = theta)
  return(res)
}


# Variance estimation for DML estimator in the Interactive Instrumental Variable Regression Model
var_irmiv = function(theta, p_hat, mu0_hat, mu1_hat, m0_hat, m1_hat, d, y, z, score) {
  n = length(d)
  if (score == "LATE") {
    var = 1 / n * 1 / (mean((m1_hat - m0_hat + z * (d - m1_hat) / p_hat - (1 - z) * (d - m0_hat) / (1 - p_hat))))^2 *
      mean((mu1_hat - mu0_hat + z * (y - mu1_hat) / p_hat - (1 - z) * (y - mu0_hat) / (1 - p_hat) -
        (m1_hat - m0_hat + z * (d - m1_hat) / p_hat - (1 - z) * (d - m0_hat) / (1 - p_hat)) * theta)^2)
  } else {
    stop("Inference framework for variance estimation unknown")
  }
  return(c(var))
}


# Bootstrap Implementation for Interactive Instrumental Variable Regression Model
bootstrap_irmiv = function(theta, se, data, y, d, z, n_folds, smpls, all_preds,
                           dml_procedure, score, bootstrap, n_rep_boot,
                           n_rep=1) {
  for (i_rep in 1:n_rep) {
    res = extract_iivm_preds(data, y, d, z, n_folds,
                             smpls[[i_rep]], all_preds[[i_rep]])
    p_hat = res$p_hat
    mu0_hat = res$mu0_hat
    mu1_hat = res$mu1_hat
    m0_hat = res$m0_hat
    m1_hat = res$m1_hat
    D = data[, d]
    Y = data[, y]
    Z = data[, z]
    
    if (score == "LATE") {
      
      psi = mu1_hat - mu0_hat + Z * (Y - mu1_hat) / p_hat - (1 - Z) * (Y - mu0_hat) / (1 - p_hat) -
        (m1_hat - m0_hat + Z * (D - m1_hat) / p_hat - (1 - Z) * (D - m0_hat) / (1 - p_hat)) * theta
      
      psi_a = -(m1_hat - m0_hat + Z * (D - m1_hat) / p_hat
                - (1 - Z) * (D - m0_hat) / (1 - p_hat))
    } else {
      stop("Inference framework for multiplier bootstrap unknown")
    }
    
    this_res = functional_bootstrap(theta, se, psi, psi_a, n_folds,
                                    smpls[[i_rep]],
                                    dml_procedure, bootstrap, n_rep_boot)
    if (i_rep==1) {
      boot_res = this_res
    } else {
      boot_res$boot_coef = cbind(boot_res$boot_coef, this_res$boot_coef)
      boot_res$boot_t_stat = cbind(boot_res$boot_t_stat, this_res$boot_t_stat)
    }
  }
  return(boot_res)
}
