# Double Machine Learning for Partially Linear Regression.
dml_plr = function(data, y, d,
                   n_folds, ml_g, ml_m,
                   dml_procedure, score,
                   n_rep = 1, smpls = NULL,
                   params_g = NULL, params_m = NULL) {

  if (is.null(smpls)) {
    smpls = lapply(1:n_rep, function(x) sample_splitting(n_folds, data))
  }

  all_thetas = all_ses = rep(NA, n_rep)
  all_preds = list()

  for (i_rep in 1:n_rep) {
    this_smpl = smpls[[i_rep]]
    stopifnot(length(this_smpl$train_ids) == length(this_smpl$test_ids))
    if (length(this_smpl$train_ids) == 1) {
      dml_procedure = 'dml1'
    }
    
    res_single_split = fit_plr_single_split(data, y, d,
                                            n_folds, ml_g, ml_m,
                                            dml_procedure, score,
                                            this_smpl,
                                            params_g, params_m)

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
  se = se_repeated(all_ses*sqrt(n), all_thetas, theta)/sqrt(n)

  t = theta / se
  pval = 2 * stats::pnorm(-abs(t))

  names(theta) = names(se) = names(t) = names(pval) =d
  res = list(
    coef = theta, se = se, t = t, pval = pval,
    thetas = all_thetas, ses = all_ses,
    all_preds = all_preds, smpls=smpls)

  return(res)
}


dml_plr_multitreat = function(data, y, d,
                              n_folds, ml_g, ml_m,
                              dml_procedure, score,
                              n_rep = 1, smpls = NULL,
                              params_g = NULL, params_m = NULL) {
  
  if (is.null(smpls)) {
    smpls = lapply(1:n_rep, function(x) sample_splitting(n_folds, data))
  }
  
  all_preds = all_thetas = all_ses = list()
  n_d = length(d)
  
  for (i_rep in 1:n_rep) {
    this_smpl = smpls[[i_rep]]
    thetas_this_rep = ses_this_rep = rep(NA, n_d)
    all_preds_this_rep = list()
    
    for (i_d in seq(n_d)) {
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
      res_single_split = fit_plr_single_split(data, y, d[i_d],
                                              n_folds, ml_g, ml_m,
                                              dml_procedure, score,
                                              this_smpl,
                                              this_params_g, this_params_m)
      
      all_preds_this_rep[[i_d]] = res_single_split$all_preds
      thetas_this_rep[i_d] = res_single_split$theta
      ses_this_rep[i_d] = res_single_split$se
    }
    
    all_preds[[i_rep]] = all_preds_this_rep
    all_thetas[[i_rep]] = thetas_this_rep
    all_ses[[i_rep]] = ses_this_rep
    
  }
  
  theta = se = t = pval = rep(NA, n_d)
  if (length(this_smpl$train_ids) > 1) {
    n = nrow(data)
  } else {
    n = length(smpls[[1]]$test_ids[[1]])
  }
  for (i_d in seq(n_d)) {
    theta_vec = unlist(lapply(all_thetas, function(x) x[i_d]))
    se_vec = unlist(lapply(all_ses, function(x) x[i_d]))
    theta[i_d] = stats::median(theta_vec)
    se[i_d] = se_repeated(se_vec*sqrt(n), theta_vec, theta[i_d])/sqrt(n)
    t[i_d] = theta[i_d] / se[i_d]
    pval[i_d] = 2 * stats::pnorm(-abs(t[i_d]))
  }
  
  names(theta) = names(se) = names(t) = names(pval) = d
  res = list(
    coef = theta, se = se, t = t, pval = pval,
    thetas = all_thetas, ses = all_ses,
    all_preds = all_preds, smpls=smpls)
  
  return(res)
}


fit_plr_single_split = function(data, y, d,
                                n_folds, ml_g, ml_m,
                                dml_procedure, score, smpl,
                                params_g, params_m) {
  
  train_ids = smpl$train_ids
  test_ids = smpl$test_ids
  
  all_preds = fit_nuisance_plr(data, y, d,
                               ml_g, ml_m,
                               smpl,
                               params_g, params_m)
  
  residuals = compute_plr_residuals(data, y, d, n_folds, smpl,
                                    all_preds)
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
    theta = mean(thetas, na.rm = TRUE)
    if (length(train_ids) == 1) {
      D = D[test_index]
      u_hat = u_hat[test_index]
      v_hat = v_hat[test_index]
      v_hatd = v_hatd[test_index]
    }
  }
  
  if (dml_procedure == "dml2") {
    orth_est = orth_plr_dml(u_hat = u_hat, v_hat = v_hat,
                            v_hatd = v_hatd, score = score)
    theta = orth_est$theta
  }
  
  se = sqrt(var_plr(
    theta = theta, d = D, u_hat = u_hat, v_hat = v_hat,
    v_hatd = v_hatd, score = score))
  
  res = list(
    theta = theta, se = se,
    all_preds = all_preds)
  
  return(res)
}


fit_nuisance_plr = function(data, y, d,
                            ml_g, ml_m,
                            smpls,
                            params_g, params_m) {
  train_ids = smpls$train_ids
  test_ids = smpls$test_ids
  
  # nuisance g
  g_indx = names(data) != d
  data_g = data[, g_indx, drop = FALSE]
  task_g = mlr3::TaskRegr$new(id = paste0("nuis_g_", d), backend = data_g, target = y)
  
  resampling_g = mlr3::rsmp("custom")
  resampling_g$instantiate(task_g, train_ids, test_ids)
  
  if (!is.null(params_g)) {
    ml_g$param_set$values = params_g
  }
  
  r_g = mlr3::resample(task_g, ml_g, resampling_g, store_models = TRUE)
  g_hat_list = lapply(r_g$predictions(), function(x) x$response)
  
  # nuisance m
  if (!is.null(params_m)) {
    ml_m$param_set$values = params_m
  }
  m_indx = names(data) != y
  data_m = data[, m_indx, drop = FALSE]
  
  if (checkmate::test_class(ml_m, "LearnerRegr")) {
    task_m = mlr3::TaskRegr$new(id = paste0("nuis_m_", d), backend = data_m, target = d)
    
    resampling_m = mlr3::rsmp("custom")
    resampling_m$instantiate(task_m, train_ids, test_ids)
    
    r_m = mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
    m_hat_list = lapply(r_m$predictions(), function(x) x$response)
  } else if (checkmate::test_class(ml_m, "LearnerClassif")) {
    ml_m$predict_type = "prob"
    data_m[[d]] = factor(data_m[[d]])
    task_m = mlr3::TaskClassif$new(id = paste0("nuis_m_", d), backend = data_m,
                                   target = d, positive = "1")
    
    resampling_m = mlr3::rsmp("custom")
    resampling_m$instantiate(task_m, train_ids, test_ids)
    
    r_m = mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
    m_hat_list = lapply(r_m$predictions(), function(x) as.data.table(x)$prob.1)
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
var_plr = function(theta, d, u_hat, v_hat, v_hatd, score) {
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
                         n_folds, smpls, all_preds,
                         bootstrap, n_rep_boot, score,
                         n_rep=1) {
  for (i_rep in 1:n_rep) {
    n = nrow(data)
    weights = draw_bootstrap_weights(bootstrap, n_rep_boot, n)
    this_res = boot_plr_single_split(thetas[i_rep], ses[i_rep],
                                     data, y, d, n_folds, smpls[[i_rep]],
                                     all_preds[[i_rep]],
                                     weights, n_rep_boot, score)
    if (i_rep==1) {
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
                               n_rep=1) {
  n_d = length(d)
  for (i_rep in 1:n_rep) {
    n = nrow(data)
    weights = draw_bootstrap_weights(bootstrap, n_rep_boot, n)
    boot_theta = boot_t_stat = matrix(NA, nrow = n_d, ncol = n_rep_boot)
    for (i_d in seq(n_d)) {
      this_res = boot_plr_single_split(thetas[[i_rep]][i_d], ses[[i_rep]][i_d],
                                       data, y, d[i_d], n_folds, smpls[[i_rep]],
                                       all_preds[[i_rep]][[i_d]],
                                       weights, n_rep_boot, score)
      boot_theta[i_d, ] = this_res$boot_coef
      boot_t_stat[i_d, ] = this_res$boot_t_stat
    }
    this_res = list(boot_coef=boot_theta,
                    boot_t_stat=boot_t_stat)
    if (i_rep==1) {
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
  residuals = compute_plr_residuals(data, y, d, n_folds,
                                    smpl, all_preds)
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
  
  res = functional_bootstrap(theta, se,
                             psi, psi_a, n_folds,
                             smpl,
                             n_rep_boot, weights)
  return(res)
}
