
extract_prediction = function(obj_resampling) {
  f_hat <- as.data.table(obj_resampling$prediction())
  setorder(f_hat, 'row_id')
  f_hat <- f_hat$response
  
  return(f_hat)
}


extract_prob_prediction = function(obj_resampling) {
  f_hat <- as.data.table(obj_resampling$prediction())
  setorder(f_hat, 'row_id')
  f_hat <- f_hat$prob.1
  
  return(f_hat)
}

