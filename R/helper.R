
extract_prediction = function(obj_resampling, return_train_preds = FALSE) {
  if (!return_train_preds) {
    f_hat_aux = data.table("row_id" = 1:obj_resampling$task$backend$nrow)
    f_hat = as.data.table(obj_resampling$prediction())
    f_hat = data.table::merge.data.table(f_hat_aux, f_hat, by = "row_id", all = TRUE)
    data.table::setorder(f_hat, 'row_id')
    f_hat = as.data.table(list("row_id" = f_hat$row_id, "response" = f_hat$response)) # tbd: optimize
    return(f_hat)
    
  } else {
    # Access in-sample predictions
    iters = obj_resampling$resampling$iters
    f_hat_aux = data.table("row_id" = 1:obj_resampling$task$backend$nrow)
    f_hat_list = lapply(1:iters, function(x) as.data.table(obj_resampling$predictions()[[x]]))
    f_hat_list = lapply(1:iters, function(x) data.table::merge.data.table(f_hat_aux, f_hat_list[[x]], 
                                                                            by = "row_id", all = TRUE))
    f_hat_list = lapply(f_hat_list, function(x) data.table::setorder(x, "row_id"))
    f_hat_list = lapply(f_hat_list, function(x) as.data.table(list("row_id" = x$row_id, 
                                                                   "response"= x$response)))
    return(f_hat_list)
  }
}

rearrange_prediction = function(prediction_list, test_ids, keep_rowids = FALSE){
    
    # if (length(test_ids) > 1) {
      # Note: length(test_ids) = 1 if apply_cross_fitting == FALSE)  
    prediction_list = lapply(1:length(test_ids), function(x) prediction_list[[x]][test_ids[[x]], ])
    predictions = data.table::rbindlist(prediction_list)
    data.table::setorder(predictions, 'row_id')
    if (!keep_rowids) {
      predictions = predictions$response
    }
    return(predictions)
}


extract_prob_prediction = function(obj_resampling) {
  f_hat_aux = data.table("row_id" = 1:obj_resampling$task$backend$nrow)
  f_hat = as.data.table(obj_resampling$prediction())
  f_hat = data.table::merge.data.table(f_hat_aux, f_hat, by = "row_id", all = TRUE)
  data.table::setorder(f_hat, 'row_id')
  f_hat = as.data.table(list("row_id" = f_hat$row_id, "prob.1" = f_hat$prob.1))
  
  return(f_hat)
}

rearrange_prob_prediction = function(prediction_list, test_ids){
    prediction_list = lapply(1:length(test_ids), function(x) prediction_list[[x]][test_ids[[x]], ])
    predictions = data.table::rbindlist(prediction_list)
    data.table::setorder(predictions, 'row_id')
    predictions = predictions$prob.1
    return(predictions)
}

initiate_learner = function(mlmethod, params) {
  if (any(class(mlmethod) == "LearnerRegr")) {
    learner = mlmethod$clone()
    if (!is.null(params)){
      learner$param_set$values = params
    }
  } else {
    learner = lrn(mlmethod)
    
    if (!is.null(params) & length(params) != 0){
    learner$param_set$values = params
    }
    
    else if (is.null(params) | length(params) == 0){
      message("No parameters provided for learners. Default values are used.")
    }
  }
  return(learner)
}


initiate_prob_learner = function(mlmethod, params) {
  if (any(class(mlmethod) == "LearnerClassif")) {
    learner = mlmethod$clone()
    learner$predict_type = "prob"
    if (!is.null(params)){
      learner$param_set$values = params
    }
  } else {
    learner = lrn(mlmethod,
                         predict_type = 'prob')
    
    if (!is.null(params) & length(params) != 0){
    learner$param_set$values = params
    }
    
    else if (is.null(params) | length(params) == 0){
      message("No parameters provided for learners. Default values are used.")
    }
  }
  return(learner)
}


initiate_regr_task = function(id, data, select_cols, target) {
  if (!is.null(select_cols)){
    indx = (names(data) %in% c(select_cols, target))
    data_sel = data[ , indx, with = FALSE]
    task = mlr3::TaskRegr$new(id = id, backend = data_sel, target = target)
  } else {
    task = mlr3::TaskRegr$new(id = id, backend = data, target = target)
  }
  
  return(task)
}


initiate_classif_task = function(id, data, select_cols, target) {
  indx = (names(data) %in% c(select_cols, target))
  data_sel = data[ , indx, with = FALSE]
  data_sel[[target]] = factor(data_sel[[target]])
  task = mlr3::TaskClassif$new(id = id, backend = data_sel,
                                target = target, positive = "1")
  return(task)
}


extract_training_data = function(data, smpls) {
  data_train = data[smpls, , drop = FALSE]
  
  return(data_train)
}

tune_instance = function(tuner, tuning_instance){
  tuning_result = tuner$optimize(tuning_instance)
  tuning_archive = tuning_instance$archive$data()
  params = tuning_instance$result$params
  
  tuning_results = list(tuning_result = tuning_result, 
                        tuning_archive = tuning_archive, 
                        params = params)
  return(tuning_results)
}
  
extract_tuned_params = function(param_results) {
  params = vapply(param_results, function(x) x$tuning_result$learner_param_vals, list(1L))
  return(params)
}

initiate_resampling = function(task, train_ids, test_ids){
    stopifnot(length(train_ids) == length(test_ids))
    resampling = lapply(1:length(train_ids), function(x)
                                              rsmp("custom")$instantiate(task,
                                                          list(train_ids[[x]]),
                                                          list(test_ids[[x]])))
    return(resampling)
}

resample_dml = function(task, learner, resampling, store_models = FALSE){
    task = mlr3::assert_task(mlr3::as_task(task, clone = TRUE))
    checkmate::check_list(learner)
    checkmate::check_list(resampling)
    
    # if (length(resampling) > 1) {
      # Note: length(resampling) = 1 only if apply_cross_fitting == FALSE
    learner = lapply(learner, function(x) mlr3::assert_learner(mlr3::as_learner(x, clone = TRUE)))
    resampling = lapply(resampling, function(x) mlr3::assert_resampling(mlr3::as_resampling(x)))
    # mlr3::assert_flag(store_models)
    # instance = lapply(resampling, function(x) x$clone(deep = TRUE))
    res = lapply(1:length(learner), function(x) mlr3::resample(task, learner[[x]], 
                                                    resampling[[x]], store_models = store_models))
    # } else {
    #     
    #   }
    
    # TBD: handle non-instantiated resampling (but should not be necessary -> initiate_resampling)
    # if (!any(instance$is_instantiated)) {
    #     instance = lapply(instance, function(x) x$instantiate(task))
    # }
    return(res)

}


# helper to draw weights in multiplier bootstrap
draw_weights = function(method, n_rep_boot, n_obs) {
  if (method == "Bayes") {
    weights = stats::rexp(n_rep_boot * n_obs, rate = 1) - 1
  } else if (method == "normal") {
    weights = stats::rnorm(n_rep_boot * n_obs)
  } else if (method == "wild") {
    weights = stats::rnorm(n_rep_boot * n_obs)/sqrt(2) + (stats::rnorm(n_rep_boot * n_obs)^2 - 1)/2
  } else {
    stop("invalid boot method")
  }
  weights = matrix(weights, nrow = n_rep_boot, ncol = n_obs, byrow=TRUE)
  return(weights)
}





format.perc = function (probs, digits) {
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



