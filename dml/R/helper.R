
extract_prediction = function(obj_resampling) {
  f_hat <- as.data.table(obj_resampling$prediction())
  setorder(f_hat, 'row_id')
  f_hat <- as.data.table(list("row_id" = f_hat$row_id, "response" = f_hat$response))
  
  return(f_hat)
}

rearrange_prediction = function(prediction_list){
    predictions <- rbindlist(prediction_list)
    setorder(predictions, 'row_id')
    predictions <- predictions$response
    return(predictions)
}


extract_prob_prediction = function(obj_resampling) {
  f_hat <- as.data.table(obj_resampling$prediction())
  setorder(f_hat, 'row_id')
  f_hat <- as.data.table(list("row_id" = f_hat$row_id, "prob.1" = f_hat$prob.1))
  
  return(f_hat)
}

rearrange_prob_prediction = function(prediction_list){
    predictions <- rbindlist(prediction_list)
    setorder(predictions, 'row_id')
    predictions <- predictions$prob.1
    return(predictions)
}

initiate_learner = function(mlmethod, params) {
  learner <- mlr3::lrn(mlmethod)
  
  if (!is.null(params) & length(params) != 0){
  learner$param_set$values <- params
  }
  
  else if (is.null(params) | length(params) == 0){
    message("No parameters provided for learners. Default values are used.")
  }
  
  return(learner)
}


initiate_prob_learner = function(mlmethod, params) {
  learner <- mlr3::lrn(mlmethod,
                       predict_type = 'prob')
  
  if (!is.null(params) & length(params) != 0){
  learner$param_set$values <- params
  }
  
  else if (is.null(params) | length(params) == 0){
    message("No parameters provided for learners. Default values are used.")
  }
  
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


extract_training_data = function(data, smpls) {
  data_train = data[smpls, , drop = FALSE]
  
  return(data_train)
}

tune_instance = function(tuner, tuning_instance){
  tuning_result = tuner$tune(tuning_instance)
  tuning_archive = tuning_instance$archive()
  params = tuning_instance$result$params
  
  tuning_results = list(tuning_result = tuning_result, 
                        tuning_archive = tuning_archive, 
                        params = params)
  return(tuning_results)
}
  
extract_tuned_params = function(param_results) {
  params = lapply(param_results, function(x) x$params)
  return(params)
}


initiate_resampling = function(task, train_ids, test_ids){
    stopifnot(length(train_ids) == length(test_ids))
    resampling = lapply(1:length(train_ids), function(x)
                                              mlr3::rsmp("custom")$instantiate(task,
                                                          list(train_ids[[x]]),
                                                          list(test_ids[[x]])))
    return(resampling)
}



resample_dml = function(task, learner, resampling, store_models = FALSE){
    task = mlr3::assert_task(as_task(task, clone = TRUE))
    checkmate::check_list(learner)
    checkmate::check_list(resampling)
    learner = lapply(learner, function(x) mlr3::assert_learner(as_learner(x, clone = TRUE)))
    resampling = lapply(resampling, function(x) mlr3::assert_resampling(as_resampling(x)))
    # mlr3::assert_flag(store_models)
    instance = lapply(resampling, function(x) x$clone(deep = TRUE))
    
    # TBD: handle non-instantiated resampling (but should not be necessary -> initiate_resampling)
    # if (!any(instance$is_instantiated)) {
    #     instance = lapply(instance, function(x) x$instantiate(task))
    # }

    res = lapply(1:length(learner), function(x) mlr3::resample(task, learner[[x]], 
                                                    resampling[[x]], store_models = store_models))
      
    return(res)

}


format.perc <- function (probs, digits) {
  paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
        "%" ) }


# Take x (vector or matrix) as input and return it as a matrix
assure_matrix = function(x){
  if (is.vector(x)){
    x = matrix(x, ncol = 1)
  }
  
  else {
    checkmate::check_matrix(x)
  }
  
  return(x)
  
}

# Check if matrices in a list have the same number of rows
check_matrix_row = function(mat_list){
  checkmate::check_list(mat_list)
  
  n_rows = vapply(mat_list, nrow, integer(1L))
 
  if ( isFALSE(all(n_rows == n_rows[1]))){
    stop("Matrices do not have same number of rows.")
  }
}


# resample_dml = function(task, learner, resampling, store_models = FALSE){
#     task = mlr3::assert_task(as_task(task, clone = TRUE))
#     checkmate::check_list(learner)
#     learner = lapply(learner, function(x) mlr3::assert_learner(as_learner(x, clone = TRUE)))
#     resampling = mlr3::assert_resampling(as_resampling(resampling))
#     mlr3::assert_flag(store_models)
#     # lapply(learner, function(x) mlr3::assert_learnable(task, learner = x))
#     instance = resampling$clone(deep = TRUE)
# resample_dml = function(task, learner, resampling, store_models = FALSE){
    # task = mlr3::assert_task(as_task(task, clone = TRUE))
    # checkmate::check_list(learner)
    # learner = lapply(learner, function(x) mlr3::assert_learner(as_learner(x, clone = TRUE)))
    # resampling = mlr3::assert_resampling(as_resampling(resampling))
    # mlr3::assert_flag(store_models)
    # # lapply(learner, function(x) mlr3::assert_learnable(task, learner = x))
    # instance = resampling$clone(deep = TRUE)
#     if (!instance$is_instantiated) {
#         instance = instance$instantiate(task)
#     }
#     n = instance$iters
#     if (use_future()) {
#         lg$debug("Running resample() via future with %i iterations", 
#             n)
#         res = future.apply::future_lapply(seq_len(n), workhorse, ### how to proceed here?
#             task = task, learner = learner, resampling = instance, 
#             store_models = store_models, lgr_threshold = lg$threshold, 
#             future.globals = FALSE, future.scheduling = structure(TRUE, 
#                 ordering = "random"), future.packages = "mlr3")
#     }
#     else {
#         lg$debug("Running resample() sequentially with %i iterations", 
#             n)
#         res = lapply(learner, function(x) workhorse, task = task, learner = x,
#             resampling = instance, store_models = store_models)
#     }
#     res = map_dtr(res, reassemble, learner = learner)
#     res[, `:=`(c("task", "resampling", "iteration"), 
#         list(list(task), list(instance), seq_len(n)))]
#     ResampleResult$new(res)
# }
# <bytecode: 0x000001f8c340bb18>
# <environment: namespace:mlr3> 
# }
# 
# 



