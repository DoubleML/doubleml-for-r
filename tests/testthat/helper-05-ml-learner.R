get_default_mlmethod_plr = function(learner, default = FALSE) {
  if (default == FALSE) {
    if (learner == "regr.lm") {
      mlmethod = list(
        mlmethod_l = learner,
        mlmethod_m = learner,
        mlmethod_g = learner
      )
      params = list(
        params_l = list(),
        params_m = list(),
        params_g = list()
      )

    }
    else if (learner == "regr.ranger") {
      mlmethod = list(
        mlmethod_l = learner,
        mlmethod_m = learner,
        mlmethod_g = learner
      )

      params = list(
        params_l = list(num.trees = 60),
        params_m = list(num.trees = 120),
        params_g = list(num.trees = 100)
      )

    }
    else if (learner == "regr.rpart") {
      mlmethod = list(
        mlmethod_l = learner,
        mlmethod_m = learner,
        mlmethod_g = learner
      )

      params = list(
        params_l = list(cp = 0.013, minsplit = 18),
        params_m = list(cp = 0.01, minsplit = 20),
        params_g = list(cp = 0.005, minsplit = 10)
      )

    }

    # else if (learner == 'regr.glmnet') {
    #   mlmethod = list(mlmethod_m = learner,
    #                    mlmethod_g = learner)
    #
    #   params = list( params_m = list(lambda = 0.01583237,
    #                                   s = 0.01583237),
    #                   params_g = list(lambda = 0.09463488,
    #                                    s = 0.09463488))
    #
    # }
    else if (learner == "regr.cv_glmnet") {
      mlmethod = list(
        mlmethod_l = learner,
        mlmethod_m = learner,
        mlmethod_g = learner
      )

      params = list(
        params_l = list(
          s = "lambda.min",
          family = "gaussian"
        ),
        params_m = list(
          s = "lambda.min",
          family = "gaussian"
        ),
        params_g = list(
          s = "lambda.min",
          family = "gaussian"))
    }
  }

  else if (default == TRUE) {
    mlmethod = list(
      mlmethod_l = learner,
      mlmethod_m = learner,
      mlmethod_g = learner
    )
    params = list(
      params_l = list(),
      params_m = list(),
      params_g = list())
  }

  if (learner == "graph_learner") {
    # pipeline learner
    pipe_learner = mlr3pipelines::po("learner",
      lrn("regr.glmnet"),
      lambda = 0.01,
      family = "gaussian")
    mlmethod = list(
      mlmethod_l = "graph_learner",
      mlmethod_m = "graph_learner",
      mlmethod_g = "graph_learner")
    params = list(
      params_g = list(),
      params_m = list())
    ml_l = mlr3::as_learner(pipe_learner)
    ml_m = mlr3::as_learner(pipe_learner)
    ml_g = mlr3::as_learner(pipe_learner)
  } else {
    ml_l = mlr3::lrn(mlmethod$mlmethod_l)
    ml_l$param_set$values = params$params_l
    ml_m = mlr3::lrn(mlmethod$mlmethod_m)
    ml_m$param_set$values = params$params_m
    ml_g = mlr3::lrn(mlmethod$mlmethod_g)
    ml_g$param_set$values = params$params_g
  }

  return(list(
    mlmethod = mlmethod, params = params,
    ml_l = ml_l, ml_m = ml_m, ml_g = ml_g
  ))
}

get_default_mlmethod_pliv = function(learner) {
  if (learner == "regr.lm") {
    mlmethod = list(
      mlmethod_l = learner,
      mlmethod_m = learner,
      mlmethod_r = learner,
      mlmethod_g = learner
    )
    params = list(
      params_l = list(),
      params_m = list(),
      params_r = list(),
      params_g = list()
    )

  }
  else if (learner == "regr.ranger") {
    mlmethod = list(
      mlmethod_l = learner,
      mlmethod_m = learner,
      mlmethod_r = learner,
      mlmethod_g = learner
    )

    params = list(
      params_l = list(num.trees = 100),
      params_m = list(num.trees = 120),
      params_r = list(num.trees = 100),
      params_g = list(num.trees = 100)
    )

  }
  else if (learner == "regr.rpart") {
    mlmethod = list(
      mlmethod_l = learner,
      mlmethod_m = learner,
      mlmethod_r = learner,
      mlmethod_g = learner
    )

    params = list(
      params_l = list(cp = 0.01, minsplit = 20),
      params_m = list(cp = 0.01, minsplit = 20),
      params_r = list(cp = 0.01, minsplit = 20),
      params_g = list(cp = 0.01, minsplit = 20)
    )

  }
  else if (learner == "regr.cv_glmnet") {
    mlmethod = list(
      mlmethod_l = learner,
      mlmethod_m = learner,
      mlmethod_r = learner,
      mlmethod_g = learner
    )

    params = list(
      params_l = list(
        s = "lambda.min",
        family = "gaussian"
      ),
      params_m = list(
        s = "lambda.min",
        family = "gaussian"
      ),
      params_r = list(
        s = "lambda.min",
        family = "gaussian"
      ),
      params_g = list(
        s = "lambda.min",
        family = "gaussian"
      )
    )

  } else if (learner == "regr.glmnet") {
    mlmethod = list(
      mlmethod_l = learner,
      mlmethod_m = learner,
      mlmethod_r = learner,
      mlmethod_g = learner
    )

    params = list(
      params_l = list(
        lambda = 0.01,
        family = "gaussian"
      ),
      params_m = list(
        lambda = 0.01,
        family = "gaussian"
      ),
      params_r = list(
        lambda = 0.01,
        family = "gaussian"
      ),
      params_g = list(
        lambda = 0.01,
        family = "gaussian"
      )
    )

  }

  if (learner == "graph_learner") {
    # pipeline learner
    pipe_learner = mlr3pipelines::po("learner",
      lrn("regr.glmnet"),
      lambda = 0.01,
      family = "gaussian")
    mlmethod = list(
      mlmethod_l = "graph_learner",
      mlmethod_m = "graph_learner",
      mlmethod_r = "graph_learner",
      mlmethod_g = "graph_learner")
    params = list(
      params_l = list(),
      params_m = list(),
      params_r = list(),
      params_g = list())
    ml_l = mlr3::as_learner(pipe_learner)
    ml_m = mlr3::as_learner(pipe_learner)
    ml_r = mlr3::as_learner(pipe_learner)
    ml_g = mlr3::as_learner(pipe_learner)
  } else {
    ml_l = mlr3::lrn(mlmethod$mlmethod_l)
    ml_l$param_set$values = params$params_l
    ml_m = mlr3::lrn(mlmethod$mlmethod_m)
    ml_m$param_set$values = params$params_m
    ml_r = mlr3::lrn(mlmethod$mlmethod_r)
    ml_r$param_set$values = params$params_r
    ml_g = mlr3::lrn(mlmethod$mlmethod_g)
    ml_g$param_set$values = params$params_g
  }

  return(list(
    mlmethod = mlmethod, params = params,
    ml_l = ml_l, ml_m = ml_m, ml_r = ml_r, ml_g = ml_g
  ))
}

get_default_mlmethod_irm = function(learner) {
  if (learner == "cv_glmnet") {
    mlmethod = list(
      mlmethod_m = paste0("classif.", learner),
      mlmethod_g = paste0("regr.", learner)
    )
    slambda = "lambda.min"
    family = "gaussian"

    params = list(
      params_m = list(s = slambda),
      params_g = list(s = slambda, family = family)
    )
  }

  else if (learner == "rpart") {
    mlmethod = list(
      mlmethod_m = paste0("classif.", learner),
      mlmethod_g = paste0("regr.", learner)
    )

    params = list(
      params_g = list(cp = 0.01, minsplit = 20),
      params_m = list(cp = 0.01, minsplit = 20)
    )

  }

  if (learner == "graph_learner") {
    # pipeline learner
    pipe_learner = mlr3pipelines::po("learner",
      lrn("regr.rpart"),
      cp = 0.01, minsplit = 20)
    pipe_learner_classif = mlr3pipelines::po("learner",
      lrn("classif.rpart",
        predict_type = "prob"),
      cp = 0.01, minsplit = 20)
    mlmethod = list(
      mlmethod_m = "graph_learner",
      mlmethod_g = "graph_learner")
    params = list(
      params_g = list(),
      params_m = list())
    ml_g = mlr3::as_learner(pipe_learner)
    ml_m = mlr3::as_learner(pipe_learner_classif)
  } else {
    ml_g = mlr3::lrn(mlmethod$mlmethod_g)
    ml_g$param_set$values = params$params_g
    ml_m = mlr3::lrn(mlmethod$mlmethod_m, predict_type = "prob")
    ml_m$param_set$values = params$params_m
  }
  return(list(
    mlmethod = mlmethod, params = params,
    ml_g = ml_g, ml_m = ml_m
  ))
}

get_default_mlmethod_iivm = function(learner) {
  if (learner == "cv_glmnet") {
    mlmethod = list(
      mlmethod_m = paste0("classif.", learner),
      mlmethod_g = paste0("regr.", learner),
      mlmethod_r = paste0("classif.", learner)
    )
    slambda = "lambda.min"
    family = "gaussian"

    params = list(
      params_m = list(s = slambda),
      params_g = list(s = slambda, family = family),
      params_r = list(s = slambda)
    )
  }

  else if (learner == "rpart") {
    mlmethod = list(
      mlmethod_m = paste0("classif.", learner),
      mlmethod_g = paste0("regr.", learner),
      mlmethod_r = paste0("classif.", learner)
    )

    params = list(
      params_m = list(cp = 0.01, minsplit = 20),
      params_g = list(cp = 0.01, minsplit = 20),
      params_r = list(cp = 0.01, minsplit = 20)
    )

  }

  if (learner == "graph_learner") {
    # pipeline learner
    pipe_learner = mlr3pipelines::po("learner",
      lrn("regr.rpart"),
      cp = 0.01, minsplit = 20)
    pipe_learner_classif = mlr3pipelines::po("learner",
      lrn("classif.rpart",
        predict_type = "prob"),
      cp = 0.01, minsplit = 20)
    mlmethod = list(
      mlmethod_m = "graph_learner",
      mlmethod_g = "graph_learner",
      mlmethod_r = "graph_learner")
    params = list(
      params_g = list(),
      params_m = list(),
      params_r = list())
    ml_g = mlr3::as_learner(pipe_learner)
    ml_m = mlr3::as_learner(pipe_learner_classif)
    ml_r = mlr3::as_learner(pipe_learner_classif)
  } else {
    ml_g = mlr3::lrn(mlmethod$mlmethod_g)
    ml_g$param_set$values = params$params_g
    ml_m = mlr3::lrn(mlmethod$mlmethod_m, predict_type = "prob")
    ml_m$param_set$values = params$params_m
    ml_r = mlr3::lrn(mlmethod$mlmethod_r, predict_type = "prob")
    ml_r$param_set$values = params$params_r
  }

  return(list(
    mlmethod = mlmethod, params = params,
    ml_g = ml_g, ml_m = ml_m, ml_r = ml_r
  ))
}

get_default_mlmethod_irm_binary = function(learner) {
  if (learner == "cv_glmnet") {
    mlmethod = list(
      mlmethod_m = paste0("classif.", learner),
      mlmethod_g = paste0("classif.", learner)
    )
    slambda = "lambda.min"

    params = list(
      params_m = list(s = slambda),
      params_g = list(s = slambda)
    )
  }

  else if (learner == "rpart") {
    mlmethod = list(
      mlmethod_m = paste0("classif.", learner),
      mlmethod_g = paste0("classif.", learner)
    )

    params = list(
      params_g = list(cp = 0.01, minsplit = 20),
      params_m = list(cp = 0.01, minsplit = 20)
    )

  }

  if (learner == "graph_learner") {
    # pipeline learner
    pipe_learner_classif = mlr3pipelines::po("learner",
      lrn("classif.rpart",
        predict_type = "prob"),
      cp = 0.01, minsplit = 20)
    mlmethod = list(
      mlmethod_m = "graph_learner",
      mlmethod_g = "graph_learner")
    params = list(
      params_g = list(),
      params_m = list())
    ml_g = mlr3::as_learner(pipe_learner_classif)
    ml_m = mlr3::as_learner(pipe_learner_classif)
  } else {
    ml_g = mlr3::lrn(mlmethod$mlmethod_m, predict_type = "prob")
    ml_g$param_set$values = params$params_g
    ml_m = mlr3::lrn(mlmethod$mlmethod_m, predict_type = "prob")
    ml_m$param_set$values = params$params_m
  }
  return(list(
    mlmethod = mlmethod, params = params,
    ml_g = ml_g, ml_m = ml_m
  ))
}

get_default_mlmethod_iivm_binary = function(learner) {
  if (learner == "cv_glmnet") {
    mlmethod = list(
      mlmethod_m = paste0("classif.", learner),
      mlmethod_g = paste0("classif.", learner),
      mlmethod_r = paste0("classif.", learner)
    )
    slambda = "lambda.min"

    params = list(
      params_m = list(s = slambda),
      params_g = list(s = slambda),
      params_r = list(s = slambda)
    )
  }

  else if (learner == "rpart") {
    mlmethod = list(
      mlmethod_m = paste0("classif.", learner),
      mlmethod_g = paste0("classif.", learner),
      mlmethod_r = paste0("classif.", learner)
    )

    params = list(
      params_m = list(cp = 0.01, minsplit = 20),
      params_g = list(cp = 0.01, minsplit = 20),
      params_r = list(cp = 0.01, minsplit = 20)
    )

  }

  else if (learner == "log_reg") {
    mlmethod = list(
      mlmethod_m = paste0("classif.", learner),
      mlmethod_g = paste0("classif.", learner),
      mlmethod_r = paste0("classif.", learner)
    )

    params = list(
      params_m = list(),
      params_g = list(),
      params_r = list()
    )

  }

  if (learner == "graph_learner") {
    # pipeline learner
    pipe_learner_classif = mlr3pipelines::po("learner",
      lrn("classif.rpart",
        predict_type = "prob"),
      cp = 0.01, minsplit = 20)
    mlmethod = list(
      mlmethod_m = "graph_learner",
      mlmethod_g = "graph_learner",
      mlmethod_r = "graph_learner")
    params = list(
      params_g = list(),
      params_m = list(),
      params_r = list())
    ml_g = mlr3::as_learner(pipe_learner_classif)
    ml_m = mlr3::as_learner(pipe_learner_classif)
    ml_r = mlr3::as_learner(pipe_learner_classif)
  } else {
    ml_g = mlr3::lrn(mlmethod$mlmethod_g, predict_type = "prob")
    ml_g$param_set$values = params$params_g
    ml_m = mlr3::lrn(mlmethod$mlmethod_m, predict_type = "prob")
    ml_m$param_set$values = params$params_m
    ml_r = mlr3::lrn(mlmethod$mlmethod_r, predict_type = "prob")
    ml_r$param_set$values = params$params_r
  }
  return(list(
    mlmethod = mlmethod, params = params,
    ml_g = ml_g, ml_m = ml_m, ml_r = ml_r
  ))
}

get_default_mlmethod_ssm = function(learner) {
  if (learner == "cv_glmnet") {
    mlmethod = list(
      mlmethod_pi = paste0("classif.", learner),
      mlmethod_m = paste0("classif.", learner),
      mlmethod_g = paste0("regr.", learner)
    )
    slambda = "lambda.min"

    params = list(
      params_pi = list(s = slambda),
      params_m = list(s = slambda),
      params_g = list(s = slambda)
    )
  }

  if (learner == "graph_learner") {
    # pipeline learner
    pipe_learner_classif = mlr3pipelines::po("learner",
      lrn("classif.cv_glmnet", predict_type = "prob"),
      s = "lambda.min")
    pipe_learner = mlr3pipelines::po("learner",
      lrn("regr.cv_glmnet"),
      s = "lambda.min")
    mlmethod = list(
      mlmethod_pi = "graph_learner",
      mlmethod_m = "graph_learner",
      mlmethod_g = "graph_learner")
    params = list(
      params_pi = list(),
      params_m = list(),
      params_g = list())
    ml_pi = mlr3::as_learner(pipe_learner_classif)
    ml_m = mlr3::as_learner(pipe_learner_classif)
    ml_g = mlr3::as_learner(pipe_learner)
  } else {
    ml_pi = mlr3::lrn(mlmethod$mlmethod_pi, predict_type = "prob")
    ml_pi$param_set$values = params$params_pi
    ml_m = mlr3::lrn(mlmethod$mlmethod_m, predict_type = "prob")
    ml_m$param_set$values = params$params_m
    ml_g = mlr3::lrn(mlmethod$mlmethod_g)
    ml_g$param_set$values = params$params_g
  }

  return(list(
    mlmethod = mlmethod, params = params,
    ml_pi = ml_pi, ml_m = ml_m, ml_g = ml_g
  ))
}
