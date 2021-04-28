# Double Machine Learning for Interactive Regression Model.
dml_irm = function(data, y, d,
                   k, mlmethod,
                   params, dml_procedure, score,
                   smpls = NULL) {

  if (is.null(smpls)) {
    smpls = sample_splitting(k, data)
  }
  train_ids = smpls$train_ids
  test_ids = smpls$test_ids

  all_preds = fit_nuisance_irm(data, y, d,
                               mlmethod, params,
                               train_ids, test_ids)
  m_hat_list = all_preds$m_hat_list
  g0_hat_list = all_preds$g0_hat_list
  g1_hat_list = all_preds$g1_hat_list
  
  n = nrow(data)
  theta = se = te = pval = NA
  n_k = vapply(test_ids, length, double(1L))

  D = data[, d]
  Y = data[, y]

  # DML 1
  if (dml_procedure == "dml1") {
    thetas = vars = rep(NA, k)
    se_i = NA

    g0_hat = g1_hat = u0_hat = u1_hat = m_hat_k = d_k = p_hat_k = y_k = matrix(NA, nrow = max(n_k), ncol = k)
    g0_hat_se = g1_hat_se = u0_hat_se = u1_hat_se = m_hat_k_se = p_hat_k_se = matrix(NA, nrow = n, ncol = 1)

    for (i in 1:k) {
      # test_index = test_index_list[[i]]
      test_index = test_ids[[i]]
      g0_hat[, i] = g0_hat_se[test_index, 1] = g0_hat_list[[i]]
      g1_hat[, i] = g1_hat_se[test_index, 1] = g1_hat_list[[i]]

      m_hat_k[, i] = m_hat_k_se[test_index, 1] = m_hat_list[[i]]
      d_k[, i] = D[test_index]
      p_hat_k[, i] = p_hat_k_se[test_index, 1] = mean(D[test_index])
      y_k[, i] = Y[test_index]
      u0_hat[, i] = u0_hat_se[test_index, 1] = Y[test_index] - g0_hat[, i]
      u1_hat[, i] = u1_hat_se[test_index, 1] = Y[test_index] - g1_hat[, i]


      orth_est = orth_irm_dml(
        g0_hat = g0_hat[, i], g1_hat = g1_hat[, i],
        u0_hat = u0_hat[, i], u1_hat = u1_hat[, i],
        d = d_k[, i], p_hat = p_hat_k[, i], m = m_hat_k[, i],
        y = y_k[, i],
        score = score)
      thetas[i] = orth_est$theta
    }

    theta = mean(thetas, na.rm = TRUE)

    se = sqrt(var_irm(
      theta = theta, g0_hat = g0_hat_se, g1_hat = g1_hat_se,
      u0_hat = u0_hat_se, u1_hat = u1_hat_se,
      d = D, p_hat = p_hat_k_se, m = m_hat_k_se, y = Y, score = score))

    t = theta / se

    pval = 2 * stats::pnorm(-abs(t))

  }

  if (dml_procedure == "dml2") {

    g0_hat = g1_hat = u0_hat = u1_hat = m_hat = p_hat = matrix(NA, nrow = n, ncol = 1)

    for (i in 1:k) {

      # test_index = test_index_list[[i]]
      test_index = test_ids[[i]]

      m_hat[test_index, 1] = m_hat_list[[i]]
      p_hat[test_index, 1] = mean(D[test_index])
      g0_hat[test_index, 1] = g0_hat_list[[i]]
      g1_hat[test_index, 1] = g1_hat_list[[i]]

      u0_hat[test_index, 1] = Y[test_index] - g0_hat[test_index, 1]
      u1_hat[test_index, 1] = Y[test_index] - g1_hat[test_index, 1]

    }

    orth_est = orth_irm_dml(
      g0_hat = g0_hat, g1_hat = g1_hat,
      u0_hat = u0_hat, u1_hat = u1_hat, d = D, p_hat = p_hat,
      y = Y, m = m_hat, score = score)

    theta = orth_est$theta
    se = sqrt(var_irm(
      theta = theta, g0_hat = g0_hat, g1_hat = g1_hat,
      u0_hat = u0_hat, u1_hat = u1_hat,
      d = D, p_hat = p_hat, m = m_hat, y = Y, score = score))

    t = theta / se

    pval = 2 * stats::pnorm(-abs(t))
  }

  names(theta) = names(se) = d
  res = list(
    coefficients = theta, se = se, t = t, pval = pval,
    all_preds = all_preds)

  class(res) = "DML"
  return(res)
}

fit_nuisance_irm = function(data, y, d,
                            mlmethod, params,
                            train_ids, test_ids) {
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
  
  ml_m = mlr3::lrn(mlmethod$mlmethod_m, predict_type = "prob")
  ml_m$param_set$values = params$params_m
  r_m = mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
  m_hat_list = lapply(r_m$data$predictions(), function(x) x$prob[, "1"])
  
  # nuisance g0: E[Y|D=0, X]
  g_indx = names(data) != d
  data_g = data[, g_indx, drop = FALSE]
  task_g0 = mlr3::TaskRegr$new(id = paste0("nuis_g0_", d), backend = data_g, target = y)
  ml_g0 = mlr3::lrn(mlmethod$mlmethod_g)
  ml_g0$param_set$values = params$params_g
  resampling_g0 = mlr3::rsmp("custom")
  # Train on subset with d == 0 (in each fold) only, predict for all test obs
  resampling_g0$instantiate(task_g0, train_ids_0, test_ids)
  train_ids_g0 = lapply(1:n_iters, function(x) resampling_g0$train_set(x))
  test_ids_g0 = lapply(1:n_iters, function(x) resampling_g0$test_set(x))
  
  r_g0 = mlr3::resample(task_g0, ml_g0, resampling_g0, store_models = TRUE)
  g0_hat_list = lapply(r_g0$data$predictions(), function(x) x$response)
  
  # nuisance g1: E[Y|D=1, X]
  task_g1 = mlr3::TaskRegr$new(id = paste0("nuis_g1_", d), backend = data_g, target = y)
  ml_g1 = mlr3::lrn(mlmethod$mlmethod_g)
  ml_g1$param_set$values = params$params_g
  resampling_g1 = mlr3::rsmp("custom")
  resampling_g1$instantiate(task_g1, train_ids_1, test_ids)
  train_ids_g1 = lapply(1:n_iters, function(x) resampling_g1$train_set(x))
  test_ids_g1 = lapply(1:n_iters, function(x) resampling_g1$test_set(x))
  
  r_g1 = mlr3::resample(task_g1, ml_g1, resampling_g1, store_models = TRUE)
  g1_hat_list = lapply(r_g1$data$predictions(), function(x) x$response)

  all_preds = list(
    m_hat_list = m_hat_list,
    g0_hat_list = g0_hat_list,
    g1_hat_list = g1_hat_list)

  return(all_preds)
}

# Orthogonalized Estimation of Coefficient in irm
orth_irm_dml = function(g0_hat, g1_hat, u0_hat, u1_hat, d, p_hat, m, y, score) {

  obj_list = list(g0_hat, g1_hat, u0_hat, u1_hat, d, p_hat, m, y)

  if (any(lapply(obj_list, length) != length(g0_hat))) {
    stop("dimensions don't match.")
  }
  theta = NA

  if (score == "ATE") {
    theta = mean(g1_hat - g0_hat + d * (u1_hat) / m - (1 - d) * u0_hat / (1 - m), na.rm = TRUE)
  }

  else if (score == "ATTE") {
    theta = mean(d * (y - g0_hat) / p_hat - m * (1 - d) * u0_hat / (p_hat * (1 - m)), na.rm = TRUE) / mean(d / p_hat, na.rm = TRUE)
  }

  else {
    stop("Inference framework for orthogonal estimation unknown")
  }

  res = list(theta = theta)
  return(res)
}


# Variance estimation for DML estimator in the interactive regression model
var_irm = function(theta, g0_hat, g1_hat, u0_hat, u1_hat, d, p_hat, m, y, score) {
  obj_list = list(g0_hat, g1_hat, u0_hat, u1_hat, d, p_hat, m, y)

  if (any(lapply(obj_list, length) != length(g0_hat))) {
    stop("dimensions don't match.")
  }
  var = NA

  if (score == "ATE") {

    # var = mean( 1/length(d) * colMeans(((g1_hat - g0_hat + d*(u1_hat)/m + (1-d)*u0_hat/(1-m))^2), na.rm = TRUE))

    var = mean(1 / length(d) * colMeans(((g1_hat - g0_hat + d * (u1_hat) / m - (1 - d) * u0_hat / (1 - m) - theta)^2), na.rm = TRUE))

  }

  else if (score == "ATTE") {

    var = mean(1 / length(d) * colMeans((d * (y - g0_hat) / p_hat - m * (1 - d) * u0_hat / (p_hat * (1 - m)) - d / p_hat * theta)^2, na.rm = TRUE))

  }

  return(c(var))
}



# Bootstrap Implementation for Interactive Regression Model
bootstrap_irm = function(theta, g0_hat, g1_hat, u0_hat, u1_hat, d, p_hat, m, y, score, se, bootstrap, nRep) {

  boot_var = NA

  if (score == "ATE") {

    psi = g1_hat - g0_hat + d * (u1_hat) / m - (1 - d) * u0_hat / (1 - m) - theta
    J = -1
    if (!is.vector(psi)) {
      J = rep(J, ncol(psi))
    }
  }

  if (score == "ATTE") {

    psi = d * (y - g0_hat) / p_hat - m * (1 - d) * u0_hat / (p_hat * (1 - m)) - d / p_hat * theta

    # note that the J here is by construction always equal to -1
    J = -colMeans(d / p_hat, na.rm = TRUE)

  }

  n = length(d)
  pertub = matrix(NA, nrow = 1, ncol = nRep)

  if (!is.vector(psi)) {
    J = matrix(rep(J, each = nrow(psi)), nrow = nrow(psi))
  }

  for (i in seq(nRep)) {

    if (bootstrap == "Bayes") {
      weights = stats::rexp(n, rate = 1) - 1
    }

    if (bootstrap == "normal") {
      weights = stats::rnorm(n)
    }

    if (bootstrap == "wild") {
      weights = stats::rnorm(n) / sqrt(2) + (stats::rnorm(n)^2 - 1) / 2
    }

    pertub[1, i] = mean(colMeans(weights * 1 / se * 1 / J * psi, na.rm = TRUE))

  }

  res = list(boot_theta = pertub)
  return(c(res))
}
