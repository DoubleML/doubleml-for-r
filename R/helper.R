
dml_cv_predict = function(learner, X_cols, y_col,
  data_model, nuisance_id,
  smpls = NULL, est_params = NULL,
  return_train_preds = FALSE, task_type = NULL,
  fold_specific_params = FALSE) {

  valid_task_type = c("regr", "classif")
  assertChoice(task_type, valid_task_type)
  # TODO: extend asserts

  if (fold_specific_params) {
    stopifnot(length(smpls$train_ids) == length(smpls$test_ids))
  }

  fold_specific_target = (all(class(data_model) == "list"))

  if (!fold_specific_target) {
    n_obs = nrow(data_model)
    task_pred = initiate_task(
      id = nuisance_id, data = data_model,
      target = y_col,
      select_cols = X_cols,
      task_type = task_type)

    if (!fold_specific_params) {
      ml_learner = initiate_learner(
        learner, task_type,
        est_params, return_train_preds)
      resampling_smpls = rsmp("custom")$instantiate(
        task_pred, smpls$train_ids,
        smpls$test_ids)
      resampling_pred = resample(task_pred, ml_learner, resampling_smpls,
        store_models = TRUE)
      preds = extract_prediction(resampling_pred, task_type, n_obs)
      models = extract_models(resampling_pred)
      if (return_train_preds) {
        train_preds = extract_prediction(resampling_pred, task_type, n_obs,
          return_train_preds = TRUE)
      }
    } else {
      # learners initiated according to fold-specific learners, proceed foldwise
      ml_learners = lapply(
        est_params,
        function(x) {
          initiate_learner(
            learner,
            task_type, x,
            return_train_preds)
        })
      resampling_smpls = lapply(
        seq_len(length(smpls$train_ids)),
        function(x) {
          rsmp("custom")$instantiate(
            task_pred, list(smpls$train_ids[[x]]),
            list(smpls$test_ids[[x]]))
        })

      resampling_pred = lapply(seq_len(length(ml_learners)), function(x) {
        resample(task_pred, ml_learners[[x]],
          resampling_smpls[[x]],
          store_models = TRUE)
      })

      preds = extract_prediction(resampling_pred, task_type, n_obs)
      models = extract_models(resampling_pred)
      if (return_train_preds) {
        train_preds = extract_prediction(resampling_pred, task_type,
          n_obs,
          return_train_preds = TRUE)
      }
    }
  } else {
    n_obs = nrow(data_model[[1]])
    task_pred = lapply(data_model, function(x) {
      initiate_task(
        id = nuisance_id, data = x,
        target = y_col,
        select_cols = X_cols,
        task_type = task_type)
    })
    # fold_specific_target == TRUE; only required for pliv_partialXZ
    if (!fold_specific_params) {
      ml_learner = initiate_learner(learner, task_type, est_params)

      resampling_smpls = lapply(
        seq_len(length(data_model)),
        function(x) {
          rsmp("custom")$instantiate(
            task_pred[[x]],
            list(smpls$train_ids[[x]]),
            list(smpls$test_ids[[x]]))
        })
      resampling_pred = lapply(
        seq_len(length(data_model)),
        function(x) {
          resample(task_pred[[x]], ml_learner,
            resampling_smpls[[x]],
            store_models = TRUE)
        })
      preds = extract_prediction(resampling_pred, task_type, n_obs)
      models = extract_models(resampling_pred)
    } else {
      # learners initiated according to fold-specific learners, proceed foldwise
      ml_learners = lapply(
        est_params,
        function(x) initiate_learner(learner, task_type, x))
      resampling_smpls = lapply(seq_len(length(smpls$train_ids)), function(x) {
        rsmp("custom")$instantiate(
          task_pred[[x]], list(smpls$train_ids[[x]]),
          list(smpls$test_ids[[x]]))
      })
      resampling_pred = lapply(seq_len(length(ml_learners)), function(x) {
        resample(task_pred[[x]], ml_learners[[x]],
          resampling_smpls[[x]],
          store_models = TRUE)
      })
      preds = extract_prediction(resampling_pred, task_type, n_obs)
      models = extract_models(resampling_pred)
    }
  }
  if (return_train_preds) {
    return(list(
      "preds" = preds,
      "train_preds" = train_preds,
      "models" = models))
  } else {
    return(list(
      "preds" = preds,
      "models" = models))
  }
}

dml_tune = function(learner, X_cols, y_col, data_tune_list,
  nuisance_id, param_set, tune_settings, measure, task_type) {

  task_tune = lapply(data_tune_list, function(x) {
    initiate_task(
      id = nuisance_id,
      data = x,
      target = y_col,
      select_cols = X_cols,
      task_type = task_type)
  })
  valid_task_type = c("regr", "classif")
  assertChoice(task_type, valid_task_type)

  ml_learner = initiate_learner(learner, task_type, params = learner$param_set$values)
  tuning_instance = lapply(task_tune, function(x) {
    TuningInstanceSingleCrit$new(
      task = x,
      learner = ml_learner,
      resampling = tune_settings$rsmp_tune,
      measure = measure,
      search_space = param_set,
      terminator = tune_settings$terminator)
  })
  tuning_result = lapply(
    tuning_instance,
    function(x) tune_instance(tune_settings$tuner, x))
  params = vapply(
    tuning_result,
    function(x) x$tuning_result$learner_param_vals, list(1L))

  return(list(
    "tuning_result" = tuning_result,
    "params" = params))
}

extract_prediction = function(obj_resampling, task_type, n_obs,
  return_train_preds = FALSE) {

  valid_task_type = c("regr", "classif")
  assertChoice(task_type, valid_task_type)

  if (compareVersion(as.character(packageVersion("mlr3")), "0.11.0") < 0) {
    ind_name = "row_id"
  } else {
    ind_name = "row_ids"
  }
  if (task_type == "regr") {
    resp_name = "response"
  } else if (task_type == "classif") {
    resp_name = "prob.1"
  }

  if (return_train_preds) {
    if (testR6(obj_resampling, classes = "ResampleResult")) {
      n_iters = obj_resampling$resampling$iters
      preds = vector("list", n_iters)
      f_hat_list = lapply(
        1:n_iters,
        function(x) as.data.table(obj_resampling$predictions("train")[[x]]))
      for (i_iter in 1:n_iters) {
        preds_vec = rep(NA_real_, n_obs)
        f_hat = f_hat_list[[i_iter]]
        preds_vec[f_hat[[ind_name]]] = f_hat[[resp_name]]
        preds[[i_iter]] = preds_vec
      }
    } else {
      n_obj_rsmp = length(obj_resampling)
      preds = vector("list", n_obj_rsmp)
      for (i_obj_rsmp in 1:n_obj_rsmp) {
        preds_vec = vector("numeric", length = n_obs)
        f_hat = as.data.table(obj_resampling[[i_obj_rsmp]]$prediction("train"))
        preds_vec[f_hat[[ind_name]]] = f_hat[[resp_name]]
        preds[[i_obj_rsmp]] = preds_vec
      }
    }
  } else {
    preds = rep(NA_real_, n_obs)
    if (testR6(obj_resampling, classes = "ResampleResult")) {
      obj_resampling = list(obj_resampling)
    }
    n_obj_rsmp = length(obj_resampling)
    for (i_obj_rsmp in 1:n_obj_rsmp) {
      f_hat = as.data.table(obj_resampling[[i_obj_rsmp]]$prediction("test"))
      preds[f_hat[[ind_name]]] = f_hat[[resp_name]]
    }
  }

  return(preds)
}

extract_from_list = function(x) {
  learner = x$score()$learner
  stopifnot(length(learner) == 1)
  return(learner[[1]])
}

extract_models = function(obj_resampling) {
  if (testR6(obj_resampling, classes = "ResampleResult")) {
    models = obj_resampling$score()$learner
  } else {
    models = lapply(obj_resampling, extract_from_list)
  }
  return(models)
}

initiate_learner = function(learner, task_type, params, return_train_preds = FALSE) {

  valid_task_type = c("regr", "classif")
  assertChoice(task_type, valid_task_type)

  ml_learner = learner$clone()

  if (!is.null(params)) {
    ml_learner$param_set$values = insert_named(
      ml_learner$param_set$values,
      params)
  } # else if (is.null(params) | length(params) == 0) {
  # message("No parameters provided for learners. Default values are used.")
  # }

  if (task_type == "classif") {
    ml_learner$predict_type = "prob"
  }
  if (return_train_preds) {
    ml_learner$predict_sets = c("test", "train")
  }
  return(ml_learner)
}

# Function to initialize task (regression or classification)
initiate_task = function(id, data, target, select_cols, task_type) {
  valid_task_type = c("regr", "classif")
  assertChoice(task_type, valid_task_type)

  if (!is.null(select_cols)) {
    indx = (names(data) %in% c(select_cols, target))
    data = data[, indx, with = FALSE]
  }
  if (task_type == "regr") {
    task = TaskRegr$new(id = id, backend = data, target = target)
  } else if (task_type == "classif") {
    data[[target]] = factor(data[[target]])
    assert_set_equal(
      levels(data[[target]]),
      c("0", "1"))
    task = TaskClassif$new(
      id = id, backend = data, target = target,
      positive = "1")
  }
  return(task)
}


# helper to draw weights in multiplier bootstrap
draw_weights = function(method, n_rep_boot, n_obs) {
  if (method == "Bayes") {
    weights = rexp(n_rep_boot * n_obs, rate = 1) - 1
  } else if (method == "normal") {
    weights = rnorm(n_rep_boot * n_obs)
  } else if (method == "wild") {
    weights = rnorm(n_rep_boot * n_obs) / sqrt(2) +
      (stats::rnorm(n_rep_boot * n_obs)^2 - 1) / 2
  } else {
    stop("invalid boot method")
  }
  weights = matrix(weights, nrow = n_rep_boot, ncol = n_obs, byrow = TRUE)
  return(weights)
}

get_cond_samples = function(smpls, D) {
  train_ids_0 = lapply(seq_len(length(smpls$train_ids)), function(x) {
    smpls$train_ids[[x]][D[smpls$train_ids[[x]]] == 0]
  })
  train_ids_1 = lapply(seq_len(length(smpls$test_ids)), function(x) {
    smpls$train_ids[[x]][D[smpls$train_ids[[x]]] == 1]
  })
  return(list(
    smpls_0 = list(
      "train_ids" = train_ids_0,
      "test_ids" = smpls$test_ids),
    smpls_1 = list(
      "train_ids" = train_ids_1,
      "test_ids" = smpls$test_ids)))
}

set_default_measure = function(measure_in = NA, task_type) {
  valid_task_type = c("regr", "classif")
  assertChoice(task_type, valid_task_type)

  if (is.null(measure_in)) {
    if (task_type == "regr") {
      measure = msr("regr.mse")
    } else if (task_type == "classif") {
      measure = msr("classif.ce")
    }
  } else if (is.character(measure_in)) {
    measure = msr(measure_in)
  }
  return(measure)
}


format.perc = function(probs, digits) {
  paste(
    format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
    "%")
}


# Take x (vector or matrix) as input and return it as a matrix
assure_matrix = function(x) {
  if (is.vector(x)) {
    x = matrix(x, ncol = 1)
  }
  else {
    check_matrix(x)
  }
  return(x)

}

# Check if matrices in a list have the same number of rows
check_matrix_row = function(mat_list) {
  check_list(mat_list)

  n_rows = vapply(mat_list, nrow, integer(1L))

  if (isFALSE(all(n_rows == n_rows[1]))) {
    stop("Matrices do not have same number of rows.")
  }
}

extract_training_data = function(data, smpls) {
  data_train = data[smpls, , drop = FALSE]

  return(data_train)
}

tune_instance = function(tuner, tuning_instance) {
  tuning_result = tuner$optimize(tuning_instance)
  if (compareVersion(as.character(packageVersion("mlr3tuning")), "0.6.0") < 0) {
    tuning_archive = tuning_instance$archive$data()
  } else {
    tuning_archive = tuning_instance$archive$data
  }
  tuning_results = list(
    tuning_result = tuning_result,
    tuning_archive = tuning_archive,
    params = tuning_instance$result$params)
  return(tuning_results)
}

check_is_partition = function(ind, n_obs) {
  ind = unlist(ind)
  if (length(ind) != n_obs) {
    return(FALSE)
  } else {
    hit = rep(0, n_obs)
    hit[ind] = 1
    if (sum(hit) < n_obs) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}

check_smpl_split = function(smpl, n_obs, check_intersect = FALSE) {

  assert_list(smpl, names = "named")
  assert_set_equal(names(smpl), c("train_ids", "test_ids"))
  assert_list(smpl$train_ids, names = "unnamed")
  assert_list(smpl$test_ids, names = "unnamed")
  if (length(smpl$train_ids) != length(smpl$test_ids)) {
    stop("Number of folds for train and test samples do not match.")
  }
  lapply(smpl$train_ids, function(train_ids) {
    assert_vector(train_ids,
      any.missing = FALSE, all.missing = FALSE,
      unique = TRUE, max.len = n_obs)
  })
  lapply(smpl$train_ids, function(train_ids) {
    assert_subset(train_ids, seq(n_obs))
  })
  lapply(smpl$test_ids, function(test_ids) {
    assert_vector(test_ids,
      any.missing = FALSE, all.missing = FALSE,
      unique = TRUE, max.len = n_obs)
  })
  lapply(smpl$test_ids, function(test_ids) {
    assert_subset(test_ids, seq(n_obs))
  })
  if (check_intersect) {
    for (i_fold in seq(length(length(smpl$train_ids))))
    {
      assert_disjunct(smpl$train_ids[[i_fold]], smpl$test_ids[[i_fold]])
    }
  }
  return(TRUE)
}
