# Double Machine Learning for Interactive Instrumental Variable Regression Model.
dml_irmiv = function(data, y, d, z,
                     k, mlmethod,
                     params, dml_procedure, score,
                     always_takers = TRUE, never_takers = TRUE,
                     smpls = NULL) {

  if (is.null(smpls)) {
    smpls = sample_splitting(k, data)
  }
  train_ids = smpls$train_ids
  test_ids = smpls$test_ids

  all_preds = fit_nuisance_iivm(data, y, d, z,
                                mlmethod, params,
                                train_ids, test_ids,
                                always_takers, never_takers)
  p_hat_list = all_preds$p_hat_list
  mu0_hat_list = all_preds$mu0_hat_list
  mu1_hat_list = all_preds$mu1_hat_list
  m0_hat_list = all_preds$m0_hat_list
  m1_hat_list = all_preds$m1_hat_list

  n = nrow(data)
  theta = se = te = pval = NA
  n_k = vapply(test_ids, length, double(1L))

  D = data[, d]
  Y = data[, y]
  Z = data[, z]

  # DML 1
  if (dml_procedure == "dml1") {
    thetas = vars = rep(NA, k)
    se_i = NA

    p_hat = mu0_hat = mu1_hat = m0_hat = m1_hat = d_k = y_k = z_k = matrix(NA, nrow = max(n_k), ncol = k)
    p_hat_se = mu0_hat_se = mu1_hat_se = m0_hat_se = m1_hat_se = matrix(NA, nrow = n, ncol = 1)

    for (i in 1:k) {
      test_index = test_ids[[i]]

      p_hat[, i] = p_hat_se[test_index, 1] = p_hat_list[[i]]
      mu0_hat[, i] = mu0_hat_se[test_index, 1] = mu0_hat_list[[i]]
      mu1_hat[, i] = mu1_hat_se[test_index, 1] = mu1_hat_list[[i]]
      m0_hat[, i] = m0_hat_se[test_index, 1] = m0_hat_list[[i]]
      m1_hat[, i] = m1_hat_se[test_index, 1] = m1_hat_list[[i]]
      d_k[, i] = D[test_index]
      y_k[, i] = Y[test_index]
      z_k[, i] = Z[test_index]

      orth_est = orth_irmiv_dml(
        p_hat = p_hat[, i], mu0_hat = mu0_hat[, i],
        mu1_hat = mu1_hat[, i],
        m0_hat = m0_hat[, i], m1_hat = m1_hat[, i],
        d = d_k[, i], y = y_k[, i], z = z_k[, i],
        score = score)
      thetas[i] = orth_est$theta

    }

    theta = mean(thetas, na.rm = TRUE)

    se = sqrt(var_irmiv(
      theta = theta, p_hat = p_hat_se, mu0_hat = mu0_hat_se,
      mu1_hat = mu1_hat_se, m0_hat = m0_hat_se, m1_hat = m1_hat_se,
      d = D, y = Y, z = Z, score = score))

    t = theta / se

    pval = 2 * stats::pnorm(-abs(t))
  }

  if (dml_procedure == "dml2") {

    p_hat = mu0_hat = mu1_hat = m0_hat = m1_hat = matrix(NA, nrow = n, ncol = 1)

    for (i in 1:k) {
      test_index = test_ids[[i]]

      p_hat[test_index, 1] = p_hat_list[[i]]
      mu0_hat[test_index, 1] = mu0_hat_list[[i]]
      mu1_hat[test_index, 1] = mu1_hat_list[[i]]
      m0_hat[test_index, 1] = m0_hat_list[[i]]
      m1_hat[test_index, 1] = m1_hat_list[[i]]

    }

    orth_est = orth_irmiv_dml(
      p_hat = p_hat, mu0_hat = mu0_hat,
      mu1_hat = mu1_hat,
      m0_hat = m0_hat, m1_hat = m1_hat,
      d = D, y = Y, z = Z,
      score = score)

    theta = orth_est$theta
    se = sqrt(var_irmiv(
      theta = theta, p_hat = p_hat, mu0_hat = mu0_hat,
      mu1_hat = mu1_hat, m0_hat = m0_hat, m1_hat = m1_hat,
      d = D, y = Y, z = Z, score = score))

    t = theta / se

    pval = 2 * stats::pnorm(-abs(t))
  }

  names(theta) = names(se) = d
  res = list(
    coef = theta, se = se, t = t, pval = pval,
    all_preds = all_preds)

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

# Orthogonalized Estimation of Coefficient in irm
orth_irmiv_dml = function(p_hat, mu0_hat, mu1_hat, m0_hat, m1_hat, d, y, z, score) {
  theta = NA

  if (score == "LATE" | score == "partialling out") {
    theta = 1 / mean(m1_hat - m0_hat + z * (d - m1_hat) / p_hat - ((1 - z) * (d - m0_hat) / (1 - p_hat))) *
      mean(mu1_hat - mu0_hat + z * (y - mu1_hat) / p_hat - ((1 - z) * (y - mu0_hat) / (1 - p_hat)))
  }

  else if (score == "LATTE") {

    # tbd: LATTE

    # Ep = mean(d)

    # theta = mean( d*(y - g0_hat)/Ep - m*(1-d)*u0_hat/(Ep*(1-m))) / mean(d/Ep)
  }

  else {
    stop("Inference framework for orthogonal estimation unknown")
  }

  res = list(theta = theta)
  return(res)
}


# Variance estimation for DML estimator in the Interactive Instrumental Variable Regression Model
var_irmiv = function(theta, p_hat, mu0_hat, mu1_hat, m0_hat, m1_hat, d, y, z, score) {
  var = NA

  if (score == "LATE") {

    var = mean(1 / length(d) * 1 / (colMeans((m1_hat - m0_hat + z * (d - m1_hat) / p_hat - (1 - z) * (d - m0_hat) / (1 - p_hat)), na.rm = TRUE))^2 *
      colMeans((mu1_hat - mu0_hat + z * (y - mu1_hat) / p_hat - (1 - z) * (y - mu0_hat) / (1 - p_hat) -
        (m1_hat - m0_hat + z * (d - m1_hat) / p_hat - (1 - z) * (d - m0_hat) / (1 - p_hat)) * theta)^2, na.rm = TRUE))

  }

  # else if (score == "partialling out") {
  #
  #      score_mat = 1/(m1_hat - m0_hat + z*(d-m1_hat)/p_hat - ((1-z)*(d-m0_hat)/(1-p_hat)))*
  #                     (mu1_hat - mu0_hat + z*(y - mu1_hat)/p_hat - ((1-z)*(y - mu0_hat)/(1-p_hat)))
  #
  #      var = 1/length(d) * apply(score_mat, 2, function(x) var(x, na.rm = TRUE))
  #   }
  #
  else if (score == "LATTE") {

    # tbd: LATTE

    #    if (is.numeric(d)) {
    #      d = as.matrix(d)
    #    }
    #
    #  Ep = colMeans(d)
    #
    # var = mean( 1/length(d) * colMeans( (d*(y - g0_hat)/Ep - m*(1-d)*u0_hat/(Ep*(1-m)) - d/Ep * theta)^2, na.rm = TRUE))
    #

    message("LATTE not yet implemented.")
  } else {
    stop("Inference framework for variance estimation unknown")
  }
  return(c(var))
}



# Bootstrap Implementation for Interactive Instrumental Variable Regression Model
bootstrap_irmiv = function(theta, p_hat, mu0_hat, mu1_hat, m0_hat, m1_hat,
  d, y, z, score, se, bootstrap, nRep) {

  boot_var = NA

  if (score == "LATE") {

    score = mu1_hat - mu0_hat + z * (y - mu1_hat) / p_hat - (1 - z) * (y - mu0_hat) / (1 - p_hat) -
      (m1_hat - m0_hat + z * (d - m1_hat) / p_hat - (1 - z) * (d - m0_hat) / (1 - p_hat)) * theta

    J = -colMeans(m1_hat - m0_hat + z * (d - m1_hat) / p_hat
      - (1 - z) * (d - m0_hat) / (1 - p_hat), na.rm = TRUE)
  }

  else if (score == "LATTE") {

    # if (is.numeric(d)) {
    #     d = as.matrix(d)
    #   }
    #
    # Ep = colMeans(d)
    #
    # score = d*(y - g0_hat)/Ep - m*(1-d)*u0_hat/(Ep*(1-m)) - d/Ep * theta

  } else {
    stop("Inference framework for multiplier bootstrap unknown")
  }

  n = length(d)
  pertub = matrix(NA, nrow = 1, ncol = nRep)

  if (!is.vector(score)) {
    J = matrix(rep(J, each = nrow(score)), nrow = nrow(score))
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

    pertub[1, i] = mean(colMeans(weights * 1 / se * 1 / J * score, na.rm = TRUE))

  }

  res = list(boot_theta = pertub)
  return(c(res))
}
