dml_pliv_partial_z = function(data, y, d, z,
                              n_folds,
                              ml_r,
                              dml_procedure, score,
                              n_rep = 1, smpls=NULL,
                              params_r = NULL) {
  if (is.null(smpls)) {
    smpls = lapply(1:n_rep, function(x) sample_splitting(n_folds, data))
  }
  
  all_thetas = all_ses = rep(NA_real_, n_rep)
  all_preds = list()
  
  for (i_rep in 1:n_rep) {
    this_smpl = smpls[[i_rep]]
    
    all_preds[[i_rep]] = fit_nuisance_pliv_partial_z(data, y, d, z,
                                                     ml_r,
                                                     this_smpl,
                                                     params_r)
    
    residuals = compute_pliv_partial_z_residuals(data, y, d, z, n_folds,
                                                 this_smpl,
                                                 all_preds[[i_rep]])
    r_hat = residuals$r_hat
    D = data[, d]
    Y = data[, y]
    
    # DML 1
    if (dml_procedure == "dml1") {
      thetas = vars = rep(NA_real_, n_folds)
      for (i in 1:n_folds) {
        test_index = this_smpl$test_ids[[i]]
        orth_est = orth_pliv_partial_z_dml(
          r_hat = r_hat[test_index],
          y = Y[test_index],
          d = D[test_index],
          score = score)
        thetas[i] = orth_est$theta
      }
      all_thetas[i_rep] = mean(thetas, na.rm = TRUE)
      if (length(this_smpl$train_ids) == 1) {
        r_hat = r_hat[test_index]
        Y = Y[test_index]
        D = D[test_index]
      }
    }
    if (dml_procedure == "dml2") {
      orth_est = orth_pliv_partial_z_dml(
        r_hat = r_hat, y = Y, d = D,
        score = score)
      all_thetas[i_rep] = orth_est$theta
    }
    
    all_ses[i_rep] = sqrt(var_pliv_partial_z(
      theta = all_thetas[i_rep], r_hat = r_hat, y = Y, d = D,
      score = score))
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

fit_nuisance_pliv_partial_z = function(data, y, d, z,
                                       ml_r,
                                       smpls,
                                       params_r) {
  train_ids = smpls$train_ids
  test_ids = smpls$test_ids

  # nuisance r: E[D|X]
  r_indx = names(data) != y
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
    r_hat_list = r_hat_list)

  return(all_preds)
}

compute_pliv_partial_z_residuals = function(data, y, d, z, n_folds, smpls,
                                            all_preds) {
  test_ids = smpls$test_ids

  r_hat_list = all_preds$r_hat_list
  n = nrow(data)
  r_hat = rep(NA_real_, n)
  
  for (i in 1:n_folds) {
    test_index = test_ids[[i]]

    r_hat[test_index] = r_hat_list[[i]]
  }
  residuals = list(r_hat=r_hat)

  return(residuals)
}


orth_pliv_partial_z_dml = function(r_hat, y, d, score) {
  stopifnot(score == "partialling out")
  theta = mean(r_hat * y) / mean(r_hat * d)
  res = list(theta = theta)
  return(res)
}


var_pliv_partial_z = function(theta, r_hat, y, d, score) {
  stopifnot(score == "partialling out")
  var = mean(1 / length(r_hat) * 1 / (mean(r_hat * d))^2 *
               mean(((y - d * theta) * r_hat)^2))
  return(c(var))
}


bootstrap_pliv_partial_z = function(theta, se, data, y, d, z, n_folds, smpls,
                                    all_preds, bootstrap,
                                    n_rep_boot, n_rep=1) {
  for (i_rep in 1:n_rep) {
    residuals = compute_pliv_partial_z_residuals(data, y, d, z, n_folds,
                                                 smpls[[i_rep]],
                                                 all_preds[[i_rep]])
    r_hat = residuals$r_hat
    D = data[, d]
    Y = data[, y]
    
    psi = (Y - D * theta[i_rep]) * r_hat
    psi_a = - r_hat * D

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
