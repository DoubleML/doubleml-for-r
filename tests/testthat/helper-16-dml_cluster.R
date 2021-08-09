est_one_way_cluster_dml2 = function(psi_a, psi_b,
  cluster_var,
  smpls_one_split) {
  test_ids = smpls_one_split$test_ids
  n_folds = length(test_ids)
  psi_a_subsample = 0
  psi_b_subsample = 0
  for (i in 1:n_folds) {
    test_index = test_ids[[i]]
    I_k = unique(cluster_var[test_index])
    const = 1 / length(I_k)
    psi_a_subsample = psi_a_subsample + const * sum(psi_a[test_index])
    psi_b_subsample = psi_b_subsample + const * sum(psi_b[test_index])
  }
  theta = -psi_b_subsample / psi_a_subsample
  return(theta)
}

var_one_way_cluster = function(psi, psi_a,
  cluster_var,
  smpls_one_split) {

  test_ids = smpls_one_split$test_ids
  n_folds = length(test_ids)
  gamma_hat = 0
  j_hat = 0
  for (i_fold in 1:n_folds) {
    test_index = test_ids[[i_fold]]
    I_k = unique(cluster_var[test_index])
    const = 1 / length(I_k)
    for (i in I_k) {
      ind = (cluster_var == i)
      for (val_i in psi[ind]) {
        for (val_j in psi[ind]) {
          gamma_hat = gamma_hat + const * val_i * val_j
        }
      }
      j_hat = j_hat + const * sum(psi_a[ind])
    }
  }
  gamma_hat = gamma_hat / n_folds
  j_hat = j_hat / n_folds
  var = gamma_hat / (j_hat^2) / length(unique(cluster_var))
  return(var)
}

est_two_way_cluster_dml2 = function(psi_a, psi_b,
  cluster_var1,
  cluster_var2,
  smpls_one_split) {

  test_ids = smpls_one_split$test_ids
  n_folds = length(test_ids)
  psi_a_subsample = 0
  psi_b_subsample = 0
  for (i in 1:n_folds) {
    test_index = test_ids[[i]]
    I_k = unique(cluster_var1[test_index])
    J_l = unique(cluster_var2[test_index])
    const = 1 / (length(I_k) * length(J_l))
    psi_a_subsample = psi_a_subsample + const * sum(psi_a[test_index])
    psi_b_subsample = psi_b_subsample + const * sum(psi_b[test_index])
  }
  theta = -psi_b_subsample / psi_a_subsample
  return(theta)
}

var_two_way_cluster = function(psi, psi_a,
  cluster_var1,
  cluster_var2,
  smpls_one_split) {

  test_ids = smpls_one_split$test_ids
  n_folds = length(test_ids)
  gamma_hat = 0
  j_hat = 0
  for (i_fold in 1:n_folds) {
    test_index = test_ids[[i_fold]]
    I_k = unique(cluster_var1[test_index])
    J_l = unique(cluster_var2[test_index])
    const = min(length(I_k), length(J_l)) / (length(I_k) * length(J_l))^2
    for (i in I_k) {
      for (j in J_l) {
        for (j_ in J_l) {
          ind1 = (cluster_var1 == i) & (cluster_var2 == j)
          ind2 = (cluster_var1 == i) & (cluster_var2 == j_)
          gamma_hat = gamma_hat + const * psi[ind1] * psi[ind2]
        }
      }
    }
    for (j in J_l) {
      for (i in I_k) {
        for (i_ in I_k) {
          ind1 = (cluster_var1 == i) & (cluster_var2 == j)
          ind2 = (cluster_var1 == i_) & (cluster_var2 == j)
          gamma_hat = gamma_hat + const * psi[ind1] * psi[ind2]
        }
      }
    }
    j_hat = j_hat + sum(psi_a[test_index]) / (length(I_k) * length(J_l))
  }
  gamma_hat = gamma_hat / n_folds
  j_hat = j_hat / n_folds
  n_clusters1 = length(unique(cluster_var1))
  n_clusters2 = length(unique(cluster_var2))
  var_scaling_factor = min(n_clusters1, n_clusters2)
  var = gamma_hat / (j_hat^2) / var_scaling_factor
  return(var)
}
