# Double Machine Learning for Partially Linear Regression.
dml_plr = function(data, y, d,
                   n_folds, mlmethod,
                   params, dml_procedure, score,
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
    
    all_preds[[i_rep]] = fit_nuisance_plr(data, y, d,
                                          mlmethod, params,
                                          this_smpl)
    
    residuals = compute_plr_residuals(data, y, d, n_folds, this_smpl,
                                      all_preds[[i_rep]])
    u_hat = residuals$u_hat
    v_hat = residuals$v_hat
    D = data[, d]
    Y = data[, y]
    v_hatd =  v_hat * D
  
    # DML 1
    if (dml_procedure == "dml1") {
      thetas = rep(NA, n_folds)
      for (i in 1:n_folds) {
        test_index = test_ids[[i]]
  
        orth_est = orth_plr_dml(
          u_hat = u_hat[test_index], v_hat = v_hat[test_index],
          v_hatd = v_hatd[test_index],
          score = score)
        thetas[i] = orth_est$theta
      }
      all_thetas[i_rep] = mean(thetas, na.rm = TRUE)
    }
  
    if (dml_procedure == "dml2") {
      orth_est = orth_plr_dml(u_hat = u_hat, v_hat = v_hat,
                              v_hatd = v_hatd, score = score)
      all_thetas[i_rep] = orth_est$theta
    }
  
    all_ses[i_rep] = sqrt(var_plr(
      theta = all_thetas[i_rep], d = D, u_hat = u_hat, v_hat = v_hat,
      v_hatd = v_hatd, score = score,
      dml_procedure = dml_procedure))
  }

  theta = stats::median(all_thetas)
  se = se = sqrt(stats::median(all_ses^2 + (all_thetas - theta)^2))

  t = theta / se
  pval = 2 * stats::pnorm(-abs(t))
  
  ci = c()

  names(theta) = names(se) = names(t) = names(pval) =d
  res = list(
    coef = theta, se = se, t = t, pval = pval,
    thetas = all_thetas, ses = all_ses,
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
bootstrap_plr = function(thetas, ses, data, y, d,
                         n_folds, smpls, all_preds, dml_procedure,
                         bootstrap, n_rep_boot, score,
                         n_rep=1) {
  for (i_rep in 1:n_rep) {
    residuals = compute_plr_residuals(data, y, d, n_folds,
                                      smpls[[i_rep]], all_preds[[i_rep]])
    u_hat = residuals$u_hat
    v_hat = residuals$v_hat
    D = data[, d]
    v_hatd =  v_hat * D
    
    if (score == "partialling out") {
      psi = (u_hat - v_hat * thetas[i_rep]) * v_hat
      psi_a = -v_hat * v_hat
    }
    else if (score == "IV-type") {
      psi = (u_hat - D * thetas[i_rep]) * v_hat
      psi_a = -v_hatd
    }
    
    this_res = functional_bootstrap(thetas[i_rep], ses[i_rep],
                                    psi, psi_a, n_folds,
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
