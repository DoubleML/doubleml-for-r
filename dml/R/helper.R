
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


initiate_learner = function(mlmethod, params) {
  learner <- mlr3::lrn(mlmethod)
  learner$param_set$values <- params
  
  return(learner)
}


initiate_prob_learner = function(mlmethod, params) {
  learner <- mlr3::lrn(mlmethod,
                       predict_type = 'prob')
  learner$param_set$values <- params
  
  return(learner)
}


initiate_regr_task = function(id, data, skip_cols, target) {
  indx <- !(names(data) %in% skip_cols)
  data_sel <- data[ , indx, drop = FALSE]
  task <- mlr3::TaskRegr$new(id = id, backend = data_sel, target = target)
  
  return(task)
}


initiate_classif_task = function(id, data, skip_cols, target) {
  indx <- !(names(data) %in% skip_cols)
  data_sel <- data[ , indx, drop = FALSE]
  data_sel[, target] <- factor(data_sel[, target])
  task <- mlr3::TaskClassif$new(id = id, backend = data_sel,
                                target = target, positive = "1")
  
  return(task)
}

