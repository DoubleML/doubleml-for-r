
dml_cv_predict = function(learner, X_cols, y_col,
                          data_model, nuisance_id, 
                          smpls = NULL, est_params = NULL, 
                          return_train_preds = FALSE, learner_class = NULL, 
                          fold_specific_params = FALSE) {
  
  # TODO: Asserts 
  if (fold_specific_params) {
    stopifnot(length(smpls$train_ids) == length(smpls$test_ids))
  }
  
  fold_specific_target = (all(class(data_model) == "list"))
                      
  if (!fold_specific_target) {
    task_pred = initiate_task(id = nuisance_id, data = data_model, 
                              target = y_col,
                              select_cols = X_cols,  
                              learner_class = learner_class)
    
    if (!fold_specific_params) {
      ml_learner = initiate_learner(learner, learner_class, est_params)
      resampling_smpls = rsmp("custom")$instantiate(task_pred, smpls$train_ids,
                                                               smpls$test_ids)
      resampling_pred = resample(task_pred, ml_learner, resampling_smpls, store_models = TRUE)
      preds = extract_prediction(resampling_pred, learner_class, return_train_preds = FALSE)
      
      if (return_train_preds) {
        resampling_smpls_train = rsmp("custom")$instantiate(task_pred, smpls$train_ids,
                                                                       smpls$train_ids)
        resampling_pred_train = resample(task_pred, ml_learner, resampling_smpls_train, store_models = TRUE)
        train_preds = extract_prediction(resampling_pred_train, learner_class, return_train_preds = TRUE)
      }
    } else {
      # learners initiated according to fold-specific learners, proceed foldwise
      ml_learners = lapply(est_params, function(x) initiate_learner(learner, learner_class, x))
      resampling_smpls = lapply(1:length(smpls$train_ids), function(x) 
                                                      rsmp("custom")$instantiate(task_pred, list(smpls$train_ids[[x]]),
                                                                                            list(smpls$test_ids[[x]])))
      
      resampling_pred = lapply(1:length(ml_learners), function(x) resample(task_pred, ml_learners[[x]], 
                                                                           resampling_smpls[[x]], store_models = TRUE))
      
      preds = extract_prediction_list(resampling_pred, learner_class, smpls$test_ids, return_train_preds = FALSE, 
                                      fold_specific_params = fold_specific_params)
      
      if (return_train_preds) {
        resampling_smpls_train = lapply(1:length(smpls$train_ids), function(x) 
                                                      rsmp("custom")$instantiate(task_pred, list(smpls$train_ids[[x]]),
                                                                                            list(smpls$train_ids[[x]])))
      
        resampling_pred_train = lapply(1:length(ml_learners), function(x) resample(task_pred, ml_learners[[x]], 
                                                                                   resampling_smpls_train[[x]], store_models = TRUE))
        train_preds = extract_prediction_list(resampling_pred_train, learner_class, smpls$test_ids, return_train_preds = TRUE, 
                                              fold_specific_params = fold_specific_params)
      }
    }
  } else {
    task_pred = lapply(data_model, function(x) 
                                    initiate_task(id = nuisance_id, data = x, 
                                                  target = y_col,
                                                  select_cols = X_cols,  
                                                  learner_class = learner_class))
    # fold_specific_target == TRUE; only required for pliv_partialXZ
     if (!fold_specific_params) {
       ml_learner = initiate_learner(learner, learner_class, est_params)
       
       resampling_smpls = lapply(1:length(data_model), function(x) 
                                                        rsmp("custom")$instantiate(task_pred[[x]], 
                                                                                    list(smpls$train_ids[[x]]), 
                                                                                    list(smpls$test_ids[[x]])))
       resampling_pred = lapply(1:length(data_model), function(x) resample(task_pred[[x]], ml_learner, 
                                                                           resampling_smpls[[x]], store_models = TRUE))
       preds = extract_prediction_list(resampling_pred, learner_class, smpls$test_ids, return_train_preds = FALSE,
                                       fold_specific_params = fold_specific_params)
     } else { 
       # learners initiated according to fold-specific learners, proceed foldwise
       ml_learners = lapply(est_params, function(x) initiate_learner(learner, learner_class, x))
       resampling_smpls = lapply(1:length(smpls$train_ids), function(x) 
                                                               rsmp("custom")$instantiate(task_pred[[x]], list(smpls$train_ids[[x]]),
                                                                                                          list(smpls$test_ids[[x]])))
       resampling_pred = lapply(1:length(ml_learners), function(x) resample(task_pred[[x]], ml_learners[[x]], 
                                                                             resampling_smpls[[x]], store_models = TRUE))
       preds = extract_prediction_list(resampling_pred, learner_class, smpls$test_ids, return_train_preds = FALSE, 
                                       fold_specific_params = fold_specific_params)
     }
  }
  if (return_train_preds) {
    return(list("preds" = preds, "train_preds" = train_preds))
  } else {
    return(preds)
  }
}

dml_tune = function(learner, X_cols, y_col, data_tune_list, 
                    nuisance_id, param_set, tune_settings, measure, learner_class) {
  
  tuner = tnr(tune_settings$algorithm, resolution = tune_settings$resolution)
  task_tune = lapply(data_tune_list, function(x) initiate_task(id = nuisance_id, 
                                                               data = x, 
                                                               target = y_col, 
                                                               select_cols = X_cols, 
                                                               learner_class = learner_class))
  ml_learner = initiate_learner(learner, learner_class, params = list())
  tuning_instance = lapply(task_tune, function(x) 
                                        TuningInstanceSingleCrit$new(task = x, 
                                                                     learner = ml_learner, 
                                                                     resampling = tune_settings$rsmp_tune, 
                                                                     measure = measure, 
                                                                     search_space = param_set, 
                                                                     terminator = tune_settings$terminator))
  tuning_result = lapply(tuning_instance, function(x) tune_instance(tuner, x))
  params = vapply(tuning_result, function(x) x$tuning_result$learner_param_vals, list(1L))
  
  return(list("tuning_result" = tuning_result,
              "params" = params))
}


extract_prediction = function(obj_resampling, learner_class, return_train_preds = FALSE, return_type = "numeric", fold_specific_params = FALSE) {
  if (learner_class == "LearnerRegr") {
    if (!return_train_preds) {
      f_hat_aux = data.table("row_id" = 1:obj_resampling$task$backend$nrow)
      f_hat = as.data.table(obj_resampling$prediction())
      f_hat = data.table::merge.data.table(f_hat_aux, f_hat, by = "row_id", all = TRUE)
      data.table::setorder(f_hat, 'row_id')
      f_hat = as.data.table(list("row_id" = f_hat$row_id, "response" = f_hat$response)) # TODO: optimize
      preds = f_hat$response
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
      
      if (fold_specific_params) {
        f_hat = f_hat_list[[1]]
      }
      preds = lapply(f_hat_list, function(x) x$response)
    }
  } else if (learner_class == "LearnerClassif") {
    f_hat_aux = data.table("row_id" = 1:obj_resampling$task$backend$nrow)
    f_hat = as.data.table(obj_resampling$prediction())
    f_hat = data.table::merge.data.table(f_hat_aux, f_hat, by = "row_id", all = TRUE)
    data.table::setorder(f_hat, 'row_id')
    f_hat = as.data.table(list("row_id" = f_hat$row_id, "prob.1" = f_hat$prob.1))
    preds = f_hat$prob.1
  }
  
  if (return_type == "numeric") {
    return(preds)
  } else if (return_type == "data.table") {
    return(f_hat)
  }
}

extract_prediction_list = function(obj_resampling, learner_class, test_ids = NULL, return_train_preds = FALSE, fold_specific_params = FALSE) {
  checkmate::assert_list(obj_resampling)
  
  prediction_list = lapply(obj_resampling, function(x) extract_prediction(x, learner_class, 
                                                                          return_train_preds = return_train_preds,
                                                                          return_type = "data.table", 
                                                                          fold_specific_params = fold_specific_params))
  if (return_train_preds & fold_specific_params) {
    f_hat_aux = lapply(obj_resampling, function(x) data.table("row_id" = 1:x$task$backend$nrow))
    f_hat_list = lapply(obj_resampling, function(x) as.data.table(x$prediction()))
    f_hat_list = lapply(1:length(obj_resampling), function(x) data.table::merge.data.table(f_hat_aux[[x]], f_hat_list[[x]], 
                                                                            by = "row_id", all = TRUE))
    f_hat_list = lapply(f_hat_list, function(x) data.table::setorder(x, "row_id"))
    f_hat_list = lapply(f_hat_list, function(x) as.data.table(list("row_id" = x$row_id, 
                                                                   "response"= x$response)))
    preds = lapply(f_hat_list, function(x) x$response)
  } else {
  
    prediction_list = lapply(1:length(test_ids), function(x) prediction_list[[x]][test_ids[[x]], ])
    predictions = data.table::rbindlist(prediction_list)
    data.table::setorder(predictions, 'row_id')
    
    if (learner_class == "LearnerRegr") {
      preds = predictions$response
    } else if (learner_class == "LearnerClassif") {
      preds = predictions$prob.1
    }
  }
  return(preds)
}



initiate_learner = function(learner, learner_class, params) {
  ml_learner = learner$clone()
  
  if (!is.null(params)) {
    ml_learner$param_set$values = params
  } else if (is.null(params) | length(params) == 0) {
    message("No parameters provided for learners. Default values are used.")
  }
  
  if (learner_class == "LearnerClassif") {
    ml_learner$predict_type = 'prob'
  }
  return(ml_learner)
}

# Function to initialize task (regression or classification)
initiate_task = function(id, data, target, select_cols, learner_class) {
  if (!is.null(select_cols)){
    indx = (names(data) %in% c(select_cols, target))
    data = data[ , indx, with = FALSE]
  }
  if (learner_class == "LearnerRegr") {
    task = mlr3::TaskRegr$new(id = id, backend = data, target = target)
  } else if (learner_class == "LearnerClassif") {
    data[[target]] = factor(data[[target]])
    task = mlr3::TaskClassif$new(id = id, backend = data, target = target, 
                                 positive = "1")
  }
  return(task)
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

get_cond_samples = function(smpls, D) {
  train_ids_0 = lapply(1:length(smpls$train_ids), function(x)
                                                      smpls$train_ids[[x]][D[smpls$train_ids[[x]]] == 0])
  train_ids_1 =  lapply(1:length(smpls$test_ids), function(x) 
                                                      smpls$train_ids[[x]][D[smpls$train_ids[[x]]] == 1])
  return(list(smpls_0 = list("train_ids" = train_ids_0,
                             "test_ids" = smpls$test_ids), 
              smpls_1 = list("train_ids" = train_ids_1, 
                             "test_ids" = smpls$test_ids)))
}

set_default_measure = function(measure_in = NA, learner_class) {
  
  if (is.na(measure_in)) {
    if (learner_class == "LearnerRegr") {
      measure = msr("regr.mse")
    } else if (learner_class == "LearnerClassif") {
      measure = msr("classif.ce")
    }
  } else if (is.character(measure_in)) {
    measure = msr(measure_in)
  }
  return(measure)
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

extract_training_data = function(data, smpls) {
  data_train = data[smpls, , drop = FALSE]
  
  return(data_train)
}

tune_instance = function(tuner, tuning_instance){
  tuning_result = tuner$optimize(tuning_instance)
  if (utils::compareVersion(as.character(utils::packageVersion('mlr3tuning')), '0.6.0') < 0) {
    tuning_archive = tuning_instance$archive$data()
  } else {
    tuning_archive = tuning_instance$archive$data
  }
  tuning_results = list(tuning_result = tuning_result, 
                        tuning_archive = tuning_archive, 
                        params = tuning_instance$result$params)
  return(tuning_results)
}
