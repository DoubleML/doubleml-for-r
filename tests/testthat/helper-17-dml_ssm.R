# Double Machine Learning for Sample Selection Models
dml_ssm = function(data, y, d, z, s,
  n_folds, ml_pi, ml_m, ml_g,
  dml_procedure, score,
  n_rep = 1, smpls = NULL,
  trimming_threshold = 1e-12,
  normalize_ipw = FALSE,
  params_pi = NULL, params_m = NULL, params_g = NULL) {

  if (is.null(smpls)) {
    smpls = lapply(1:n_rep, function(x) sample_splitting(n_folds, data))
  }

  all_thetas = all_ses = rep(NA_real_, n_rep)
  all_preds = list()

  for (i_rep in 1:n_rep) {
    this_smpl = smpls[[i_rep]]
    train_ids = this_smpl$train_ids
    test_ids = this_smpl$test_ids

    all_preds[[i_rep]] = fit_nuisance_ssm(
      data, y, d, z, s,
      ml_pi, ml_m, ml_g,
      this_smpl, score,
      params_pi, params_m, params_g
    )

    res = extract_ssm_preds(data = data, n_folds = n_folds, smpls = this_smpl,
      all_preds = all_preds[[i_rep]], trimming_threshold = trimming_threshold
    )
    pi_hat = res$pi_hat
    m_hat = res$m_hat
    g_hat_d0 = res$g_hat_d0
    g_hat_d1 = res$g_hat_d1
    Y = data[, y]
    D = data[, d]
    S = data[, s]

    # DML 1
    if (dml_procedure == "dml1") {
      thetas = vars = rep(NA_real_, n_folds)
      psi_a = rep(NA_real_, length(data$d))
      psi_b = rep(NA_real_, length(data$d))

      for (i in 1:n_folds) {
        test_index = test_ids[[i]]
        orth_est = orth_ssm_dml(
          pi_hat = pi_hat[test_index],
          m_hat = m_hat[test_index],
          g_hat_d0 = g_hat_d0[test_index],
          g_hat_d1 = g_hat_d1[test_index],
          y = Y[test_index],
          d = D[test_index],
          s = S[test_index],
          score = score,
          normalize_ipw = normalize_ipw
        )
        thetas[i] = -mean(orth_est$psi_b) / mean(orth_est$psi_a)
        psi_a[test_index] = orth_est$psi_a
        psi_b[test_index] = orth_est$psi_b
      }
      all_thetas[i_rep] = mean(thetas, na.rm = TRUE)

      if (length(train_ids) == 1) {
        pi_hat = pi_hat[test_index]
        m_hat = m_hat[test_index]
        g_hat_d0 = g_hat_d0[test_index]
        g_hat_d1 = g_hat_d1[test_index]
        y = Y[test_index]
        d = D[test_index]
        s = S[test_index]
      }
    }

    # DML2
    if (dml_procedure == "dml2") {
      orth_est = orth_ssm_dml(
        pi_hat = pi_hat,
        m_hat = m_hat,
        g_hat_d0 = g_hat_d0,
        g_hat_d1 = g_hat_d1,
        y = data[, y],
        d = data[, d],
        s = data[, s],
        score = score,
        normalize_ipw = normalize_ipw
      )
      psi_a = orth_est$psi_a
      psi_b = orth_est$psi_b
      all_thetas[i_rep] = -mean(psi_b) / mean(psi_a)
    }

    all_ses[i_rep] = sqrt(var_ssm(
      theta = all_thetas[i_rep],
      psi_a = psi_a,
      psi_b = psi_b,
      d = D
    ))
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

  names(theta) = names(se) = d
  res = list(
    coef = theta, se = se, t = t, pval = pval,
    thetas = all_thetas, ses = all_ses,
    all_preds = all_preds, smpls = smpls
  )

  return(res)
}

fit_nuisance_ssm = function(data, y, d, z, s,
  ml_pi, ml_m, ml_g,
  this_smpl,
  score = score,
  params_pi, params_m, params_g) {

  if (score == "missing-at-random") {
    train_ids = this_smpl$train_ids
    test_ids = this_smpl$test_ids

    # nuisance pi
    pi_indx = names(data) != y
    data_pi = data[, pi_indx, drop = FALSE]
    data_pi[, s] = factor(data_pi[, s])
    task_pi = mlr3::TaskClassif$new(
      id = paste0("nuis_pi_", s), backend = data_pi,
      target = s, positive = "1"
    )
    resampling_pi = mlr3::rsmp("custom")
    resampling_pi$instantiate(task_pi, train_ids, test_ids)

    if (!is.null(params_pi)) {
      ml_pi$param_set$values = params_pi
    }
    r_pi = mlr3::resample(task_pi, ml_pi, resampling_pi, store_models = TRUE)
    pi_hat_list = lapply(r_pi$predictions(), function(x) x$prob[, "1"])

    # nuisance m
    m_indx = names(data) != y & names(data) != s
    data_m = data[, m_indx, drop = FALSE]
    data_m[, d] = factor(data_m[, d])
    task_m = mlr3::TaskClassif$new(
      id = paste0("nuis_m_", d), backend = data_m,
      target = d, positive = "1"
    )
    resampling_m = mlr3::rsmp("custom")
    resampling_m$instantiate(task_m, train_ids, test_ids)

    if (!is.null(params_m)) {
      ml_m$param_set$values = params_m
    }
    r_m = mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
    m_hat_list = lapply(r_m$predictions(), function(x) x$prob[, "1"])

    # nuisance g_d0
    train_ids_d0_s1 = lapply(seq_len(length(train_ids)), function(x) {
      train_ids[[x]][data$d[train_ids[[x]]] == 0 & data$s[train_ids[[x]]] == 1]
    })
    train_ids_d1_s1 = lapply(seq_len(length(train_ids)), function(x) {
      train_ids[[x]][data$d[train_ids[[x]]] == 1 & data$s[train_ids[[x]]] == 1]
    })

    g_indx = names(data) != d & names(data) != s
    data_g = data[, g_indx, drop = FALSE]

    task_g_d0 = mlr3::TaskRegr$new(id = paste0("nuis_g_d0_", y), backend = data_g, target = y)
    ml_g_d0 = ml_g$clone()
    if (!is.null(params_g)) {
      ml_g_d0$param_set$values = params_g
    }
    resampling_g_d0 = mlr3::rsmp("custom")
    resampling_g_d0$instantiate(task_g_d0, train_ids_d0_s1, test_ids)

    r_g_d0 = mlr3::resample(task_g_d0, ml_g_d0, resampling_g_d0, store_models = TRUE)
    g_hat_d0_list = lapply(r_g_d0$predictions(), function(x) x$response)

    # nuisance g_d1
    task_g_d1 = mlr3::TaskRegr$new(id = paste0("nuis_g_d1_", y), backend = data_g, target = y)
    ml_g_d1 = ml_g$clone()
    if (!is.null(params_g)) {
      ml_g_d1$param_set$values = params_g
    }
    resampling_g_d1 = mlr3::rsmp("custom")
    resampling_g_d1$instantiate(task_g_d1, train_ids_d1_s1, test_ids)
    r_g_d1 = mlr3::resample(task_g_d1, ml_g_d1, resampling_g_d1, store_models = TRUE)
    g_hat_d1_list = lapply(r_g_d1$predictions(), function(x) x$response)

  } else { # nonignorable

    pi_hat_list = list()
    m_hat_list = list()
    g_hat_d0_list = list()
    g_hat_d1_list = list()

    data$strata = data$d + 2 * data$s
    n_folds = length(this_smpl$train_ids)

    for (i_fold in 1:n_folds) {
      train_ids = this_smpl$train_ids[[i_fold]]
      test_ids = this_smpl$test_ids[[i_fold]]

      # split train_ids into 2 sets
      dummy_train_task = Task$new("dummy", "regr", data)
      dummy_train_task$set_col_roles("strata", c("target", "stratum"))
      dummy_train_resampling = rsmp("holdout", ratio = 0.5)$instantiate(dummy_train_task$filter(train_ids))
      train1 = dummy_train_resampling$train_set(1)
      train2 = dummy_train_resampling$test_set(1)

      # nuisance pi_prelim and pi
      pi_indx = names(data) != y & names(data) != "strata" & names(data) != "pi_hat_prelim"
      data_pi = data[, pi_indx, drop = FALSE]
      data_pi[, s] = factor(data_pi[, s])

      task_pi_prelim = mlr3::TaskClassif$new(
        id = paste0("nuis_pi_", s), backend = data_pi,
        target = s, positive = "1"
      )

      resampling_pi_prelim = mlr3::rsmp("custom")
      resampling_pi_prelim$instantiate(task_pi_prelim, list(train1), list(1:nrow(data)))

      ml_pi_prelim = ml_pi$clone()
      if (!is.null(params_pi)) {
        ml_pi_prelim$param_set$values = params_pi
      }

      r_pi_prelim = mlr3::resample(task_pi_prelim, ml_pi_prelim, resampling_pi_prelim, store_models = TRUE)
      preds_pi_hat_prelim = r_pi_prelim$predictions()[[1]]$prob[, "1"]

      data$pi_hat_prelim = preds_pi_hat_prelim
      pi_hat_list[[i_fold]] = preds_pi_hat_prelim[test_ids]

      # nuisance m
      m_indx = names(data) != y & names(data) != s & names(data) != z & names(data) != "strata"
      data_m = data[, m_indx, drop = FALSE]
      data_m[, d] = factor(data_m[, d])
      task_m = mlr3::TaskClassif$new(
        id = paste0("nuis_m_", d), backend = data_m,
        target = d, positive = "1"
      )
      resampling_m = mlr3::rsmp("custom")
      resampling_m$instantiate(task_m, list(train2), list(test_ids))

      if (!is.null(params_m)) {
        ml_m$param_set$values = params_m
      }
      r_m = mlr3::resample(task_m, ml_m, resampling_m, store_models = TRUE)
      m_hat_list[[i_fold]] = r_m$predictions()[[1]]$prob[, "1"]

      # nuisance g_d0
      train2_d0_s1 = train2[data[train2, ]$d == 0 & data[train2, ]$s == 1]

      g_indx = names(data) != d & names(data) != s & names(data) != z & names(data) != "strata"
      data_g = data[, g_indx, drop = FALSE]

      task_g_d0 = mlr3::TaskRegr$new(id = paste0("nuis_g_d0_", y), backend = data_g, target = y)
      ml_g_d0 = ml_g$clone()
      if (!is.null(params_g)) {
        ml_g_d0$param_set$values = params_g
      }

      resampling_g_d0 = mlr3::rsmp("custom")
      resampling_g_d0$instantiate(task_g_d0, list(train2_d0_s1), list(test_ids))
      r_g_d0 = mlr3::resample(task_g_d0, ml_g_d0, resampling_g_d0, store_models = TRUE)
      g_hat_d0_list[[i_fold]] = r_g_d0$predictions()[[1]]$response

      # nuisance g_d1
      train2_d1_s1 = train2[data[train2, ]$d == 1 & data[train2, ]$s == 1]

      task_g_d1 = mlr3::TaskRegr$new(id = paste0("nuis_g_d1_", y), backend = data_g, target = y)
      ml_g_d1 = ml_g$clone()
      if (!is.null(params_g)) {
        ml_g_d1$param_set$values = params_g
      }

      resampling_g_d1 = mlr3::rsmp("custom")
      resampling_g_d1$instantiate(task_g_d1, list(train2_d1_s1), list(test_ids))
      r_g_d1 = mlr3::resample(task_g_d1, ml_g_d1, resampling_g_d1, store_models = TRUE)
      g_hat_d1_list[[i_fold]] = r_g_d1$predictions()[[1]]$response

    }

  }

  all_preds = list(
    pi_hat_list = pi_hat_list,
    m_hat_list = m_hat_list,
    g_hat_d0_list = g_hat_d0_list,
    g_hat_d1_list = g_hat_d1_list
  )

  return(all_preds)
}

extract_ssm_preds = function(data, n_folds, smpls, all_preds, trimming_threshold) {

  test_ids = smpls$test_ids

  pi_hat_list = all_preds$pi_hat_list
  m_hat_list = all_preds$m_hat_list
  g_hat_d0_list = all_preds$g_hat_d0_list
  g_hat_d1_list = all_preds$g_hat_d1_list

  n = nrow(data)

  pi_hat = m_hat = g_hat_d0 = g_hat_d1 = rep(NA_real_, n)

  for (i in 1:n_folds) {
    test_index = test_ids[[i]]

    pi_hat[test_index] = pi_hat_list[[i]]
    m_hat[test_index] = m_hat_list[[i]]
    g_hat_d0[test_index] = g_hat_d0_list[[i]]
    g_hat_d1[test_index] = g_hat_d1_list[[i]]

  }

  m_hat = trim_vec(m_hat, trimming_threshold)

  res = list(
    pi_hat = pi_hat, m_hat = m_hat,
    g_hat_d0 = g_hat_d0, g_hat_d1 = g_hat_d1
  )

  return(res)
}

# Orthogonalized estimation of coefficient in SSM
orth_ssm_dml = function(pi_hat, m_hat, g_hat_d0, g_hat_d1, y, d, s, score, normalize_ipw) {

  dtreat = (d == 1)
  dcontrol = (d == 0)

  if (score == "missing-at-random" | score == "nonignorable") {

    psi_a = -1
    if (normalize_ipw == TRUE) {
      weight_treat = sum(dtreat) / sum((dtreat * s) / (pi_hat * m_hat))
      weight_control = sum(dcontrol) / sum((dcontrol * s) / (pi_hat * (1 - m_hat)))

      psi_b1 = weight_treat * ((dtreat * s * (y - g_hat_d1)) / (m_hat * pi_hat)) + g_hat_d1
      psi_b0 = weight_control * ((dcontrol * s * (y - g_hat_d0)) / ((1 - m_hat) * pi_hat)) + g_hat_d0

    } else {
      psi_b1 = (dtreat * s * (y - g_hat_d1)) / (m_hat * pi_hat) + g_hat_d1
      psi_b0 = (dcontrol * s * (y - g_hat_d0)) / ((1 - m_hat) * pi_hat) + g_hat_d0

    }
    psi_b = psi_b1 - psi_b0

  } else {
    stop("Inference framework for orthogonal estimation unknown")
  }

  res = list(psi_a = psi_a, psi_b = psi_b)

  return(res)
}

# Variance estimation for DML estimator in SSM
var_ssm = function(theta, psi_a, psi_b, d) {
  n = length(d)
  J = mean(psi_a)
  var = mean((psi_a * theta + psi_b)^2) / J^2 / n

  return(c(var))
}

# Bootstrap Implementation for SSM
bootstrap_ssm = function(theta, se, data, y, d, s, n_folds, smpls, all_preds,
  score, bootstrap, n_rep_boot,
  n_rep = 1, trimming_threshold = 1e-12, normalize_ipw = FALSE) {
  for (i_rep in 1:n_rep) {
    res = extract_ssm_preds(data = data, n_folds = n_folds, smpls = smpls[[i_rep]], all_preds = all_preds[[i_rep]],
      trimming_threshold = trimming_threshold)

    pi_hat = res$pi_hat
    m_hat = res$m_hat
    g_hat_d0 = res$g_hat_d0
    g_hat_d1 = res$g_hat_d1
    y = data[, "y"]
    d = data[, "d"]
    s = data[, "s"]

    dtreat = (d == 1)
    dcontrol = (d == 0)

    psi_a = rep(-1, length(d))

    if (normalize_ipw == TRUE) {
      weight_treat = sum(dtreat) / sum((dtreat * s) / (pi_hat * m_hat))
      weight_control = sum(dcontrol) / sum((dcontrol * s) / (pi_hat * (1 - m_hat)))

      psi_b1 = weight_treat * ((dtreat * s * (y - g_hat_d1)) / (m_hat * pi_hat)) + g_hat_d1
      psi_b0 = weight_control * ((dcontrol * s * (y - g_hat_d0)) / ((1 - m_hat) * pi_hat)) + g_hat_d0

    } else {
      psi_b1 = (dtreat * s * (y - g_hat_d1)) / (m_hat * pi_hat) + g_hat_d1
      psi_b0 = (dcontrol * s * (y - g_hat_d0)) / ((1 - m_hat) * pi_hat) + g_hat_d0

    }
    psi_b = psi_b1 - psi_b0
    psi = psi_a * theta[i_rep] + psi_b


    n = length(psi)
    weights = draw_bootstrap_weights(bootstrap, n_rep_boot, n)
    this_res = functional_bootstrap(
      theta[i_rep], se[i_rep], psi, psi_a, n_folds,
      smpls[[i_rep]],
      n_rep_boot, weights
    )
    if (i_rep == 1) {
      boot_res = this_res
    } else {
      boot_res$boot_coef = cbind(boot_res$boot_coef, this_res$boot_coef)
      boot_res$boot_t_stat = cbind(boot_res$boot_t_stat, this_res$boot_t_stat)
    }
  }
  return(boot_res)
}
