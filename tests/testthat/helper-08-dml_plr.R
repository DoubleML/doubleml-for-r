# Double Machine Learning for Partially Linear Regression.
dml_plr = function(data, y, d,
                   n_folds, mlmethod,
                   params, dml_procedure, score,
                   smpls = NULL) {

  if (is.null(smpls)) {
    smpls = sample_splitting(n_folds, data)
  }
  train_ids = smpls$train_ids
  test_ids = smpls$test_ids
  
  all_preds = fit_nuisance_plr(data, y, d,
                               mlmethod, params,
                               smpls)
  
  residuals = compute_plr_residuals(data, y, d, n_folds, smpls, all_preds)
  u_hat = residuals$u_hat
  v_hat = residuals$v_hat
  D = data[, d]
  Y = data[, y]
  v_hatd =  v_hat * D
  
  theta = se = te = pval = NA

  # DML 1
  if (dml_procedure == "dml1") {
    thetas = vars = rep(NA, n_folds)
    for (i in 1:n_folds) {
      test_index = test_ids[[i]]

      orth_est = orth_plr_dml(
        u_hat = u_hat[test_index], v_hat = v_hat[test_index],
        v_hatd = v_hatd[test_index],
        score = score)
      thetas[i] = orth_est$theta
    }
    theta = mean(thetas, na.rm = TRUE)
  }

  if (dml_procedure == "dml2") {
    orth_est = orth_plr_dml(u_hat = u_hat, v_hat = v_hat,
                            v_hatd = v_hatd, score = score)
    theta = orth_est$theta
  }

  se = sqrt(var_plr(
    theta = theta, d = D, u_hat = u_hat, v_hat = v_hat,
    v_hatd = v_hatd, score = score,
    dml_procedure = dml_procedure))

  t = theta / se
  pval = 2 * stats::pnorm(-abs(t))
  
  ci = c()

  names(theta) = names(se) = names(t) = names(pval) =d
  res = list(
    coef = theta, se = se, t = t, pval = pval,
    all_preds = all_preds, smpls=smpls)

  return(res)
}

fit_nuisance_plr = function(data, y, d,
                            mlmethod, params,
                            smpls) {
  train_ids = smpls$train_ids
  test_ids = smpls$test_ids
  
  # nuisance g
  g_indx = names(data) != d
  data_g = data[, g_indx, drop = FALSE]
  task_g = mlr3::TaskRegr$new(id = paste0("nuis_g_", d), backend = data_g, target = y)
  
  resampling_g = mlr3::rsmp("custom")
  resampling_g$instantiate(task_g, train_ids, test_ids)
  
  ml_g = mlr3::lrn(mlmethod$mlmethod_g)
  ml_g$param_set$values = params$params_g
  
  r_g = mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
  g_hat_list = lapply(r_g$data$predictions(), function(x) x$response)
  
  # nuisance m
  ml_m = mlr3::lrn(mlmethod$mlmethod_m)
  ml_m$param_set$values = params$params_m
  m_indx = names(data) != y
  data_m = data[, m_indx, drop = FALSE]
  
  if (checkmate::test_class(ml_m, "LearnerRegr")) {
    task_m = mlr3::TaskRegr$new(id = paste0("nuis_m_", d), backend = data_m, target = d)
    
    resampling_m = mlr3::rsmp("custom")
    resampling_m$instantiate(task_m, train_ids, test_ids)
    
    r_m = mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
    m_hat_list = lapply(r_m$data$predictions(), function(x) x$response)
  } else if (checkmate::test_class(ml_m, "LearnerClassif")) {
    ml_m$predict_type = "prob"
    data_m[[d]] = factor(data_m[[d]])
    task_m = mlr3::TaskClassif$new(id = paste0("nuis_m_", d), backend = data_m,
                                   target = d, positive = "1")
    
    resampling_m = mlr3::rsmp("custom")
    resampling_m$instantiate(task_m, train_ids, test_ids)
    
    r_m = mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
    m_hat_list = lapply(r_m$data$predictions(), function(x) as.data.table(x)$prob.1)
  }
  
  all_preds = list(
    m_hat_list = m_hat_list,
    g_hat_list = g_hat_list)
  
  return(all_preds)
}

compute_plr_residuals = function(data, y, d, n_folds, smpls, all_preds) {
  test_ids = smpls$test_ids
  
  g_hat_list = all_preds$g_hat_list
  m_hat_list = all_preds$m_hat_list
  
  n = nrow(data)
  D = data[, d]
  Y = data[, y]
  
  v_hat = u_hat = w_hat = rep(NA, n)
  
  for (i in 1:n_folds) {
    test_index = test_ids[[i]]
    
    g_hat = g_hat_list[[i]]
    m_hat = m_hat_list[[i]]
    
    u_hat[test_index] = Y[test_index] - g_hat
    v_hat[test_index] = D[test_index] - m_hat
  }
  residuals = list(u_hat=u_hat, v_hat=v_hat)
  
  return(residuals)
}

dml_plr_boot = function(data, y, d, theta, se, all_preds, dml_procedure = "dml2",
  score = "IV-type",
  weights = weights, nRep = 500) {

  m_hat_list = all_preds$m_hat_list
  g_hat_list = all_preds$g_hat_list

  smpls = all_preds$smpls
  train_ids = smpls$train_ids
  test_ids = smpls$test_ids

  n_iters = length(test_ids)
  n_k = vapply(test_ids, length, double(1L))

  n = nrow(data)
  D = data[, d]
  Y = data[, y]

  # DML 1
  if (dml_procedure == "dml1") {
    v_hat = u_hat = v_hatd = d_k = matrix(NA, nrow = max(n_k), ncol = n_iters)

    for (i in 1:n_iters) {
      test_index = test_ids[[i]]

      m_hat = m_hat_list[[i]]
      g_hat = g_hat_list[[i]]

      d_k[, i] = D[test_index]
      v_hat[, i] = D[test_index] - m_hat
      u_hat[, i] = Y[test_index] - g_hat
      v_hatd[, i] = v_hat[, i] * D[test_index]
    }

    boot = bootstrap_plr(
      theta = theta, d = d_k, u_hat = u_hat, v_hat = v_hat,
      v_hatd = v_hatd, score = score, se = se,
      weights = weights, nRep = nRep)
    boot_theta = boot$boot_theta
  }

  if (dml_procedure == "dml2") {

    v_hat = u_hat = v_hatd = matrix(NA, nrow = n, ncol = 1)

    for (i in 1:n_iters) {
      test_index = test_ids[[i]]

      m_hat = m_hat_list[[i]]
      g_hat = g_hat_list[[i]]

      v_hat[test_index, 1] = D[test_index] - m_hat
      u_hat[test_index, 1] = Y[test_index] - g_hat
      v_hatd[test_index, 1] = v_hat[test_index] * D[test_index]

    }

    boot = bootstrap_plr(
      theta = theta, d = D, u_hat = u_hat, v_hat = v_hat,
      v_hatd = v_hatd, score = score, se = se,
      weights = weights, nRep = nRep)
    boot_theta = boot$boot_theta
  }

  return(boot_theta)
}


# Orthogonalized Estimation of Coefficient in PLR
orth_plr_dml = function(u_hat, v_hat, v_hatd, score) {
  theta = NA

  if (score == "partialling out") {
    res_fit = stats::lm(u_hat ~ 0 + v_hat)
    theta = stats::coef(res_fit)
  }

  else if (score == "IV-type") {
    theta = mean(v_hat * u_hat) / mean(v_hatd)
    # se = 1/(mean(u_hat)^2) * mean((v_hat - theta*u_hat)*u_hat)^2
  }

  else {
    stop("Inference framework for orthogonal estimation unknown")
  }

  res = list(theta = theta)
  return(res)
}


# Variance estimation for DML estimator in the partially linear regression model
var_plr = function(theta, d, u_hat, v_hat, v_hatd, score, dml_procedure) {
  n = length(d)
  if (score == "partialling out") {
    var = 1 / n * 1 / (mean(v_hat^2))^2 *
      mean(((u_hat - v_hat * theta) * v_hat)^2)
  }
  else if (score == "IV-type") {
    var = 1 / n * 1 / mean(v_hatd)^2 *
      mean(((u_hat - d * theta) * v_hat)^2)
  }
  return(c(var))
}


# Bootstrap Implementation for Partially Linear Regression Model
bootstrap_plr = function(theta, se, data, y, d,
                         n_folds, smpls, all_preds, dml_procedure,
                         bootstrap, nRep, score) {
  residuals = compute_plr_residuals(data, y, d, n_folds, smpls, all_preds)
  u_hat = residuals$u_hat
  v_hat = residuals$v_hat
  D = data[, d]
  v_hatd =  v_hat * D

  if (score == "partialling out") {
    psi = (u_hat - v_hat * theta) * v_hat
    psi_a = -v_hat * v_hat
  }
  else if (score == "IV-type") {
    psi = (u_hat - D * theta) * v_hat
    psi_a = -v_hatd
  }

  res = functional_bootstrap(theta, se, psi, psi_a, n_folds, smpls, dml_procedure, bootstrap, nRep)
  return(res)
}
