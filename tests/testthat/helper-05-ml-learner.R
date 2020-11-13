get_default_mlmethod_plr <- function(learner, default = FALSE) {
  
  if (default == FALSE) {
    if (learner == 'regr.lm') {
      mlmethod <- list(mlmethod_m = learner,
                       mlmethod_g = learner)
      params <- list(params_g = list(),
                     params_m = list())
      
    }
    else if (learner == 'regr.ranger') {
      mlmethod <- list(mlmethod_m = learner,
                       mlmethod_g = learner)
      
      params <- list(params_g = list(num.trees = 100),
                     params_m = list(num.trees = 120))
      
    }
    else if (learner == 'regr.rpart') {
      mlmethod <- list(mlmethod_m = learner,
                       mlmethod_g = learner)
      
      params <- list(params_g = list(cp = 0.01, minsplit = 20),
                     params_m = list(cp = 0.01, minsplit = 20))
    
  }
    
    # else if (learner == 'regr.glmnet') {
    #   mlmethod <- list(mlmethod_m = learner,
    #                    mlmethod_g = learner)
    # 
    #   params <- list( params_m = list(lambda = 0.01583237,
    #                                   s = 0.01583237),
    #                   params_g = list(lambda = 0.09463488,
    #                                    s = 0.09463488))
    # 
    # }
    else if (learner == 'regr.cv_glmnet') {
      mlmethod <- list(mlmethod_m = learner,
                       mlmethod_g = learner)
      
      params <- list( params_m = list(s = "lambda.min", 
                                      family = "gaussian"),
                      params_g = list(s = "lambda.min",
                                      family = "gaussian"))
      
    }
  
  }
  
  else if (default == TRUE) {
     mlmethod <- list(mlmethod_m = learner,
                     mlmethod_g = learner)
     params <- list(params_g = list(),
                   params_m = list())
    
  }
  return(list(mlmethod=mlmethod, params=params))
}

get_default_mlmethod_pliv <- function(learner) {
  if (learner == 'regr.lm') {
    mlmethod <- list(mlmethod_m = learner,
                     mlmethod_g = learner,
                     mlmethod_r = learner)
    params <- list(params_g = list(),
                   params_m = list(),
                   params_r = list())
    
  }
  else if (learner == 'regr.ranger') {
    mlmethod <- list(mlmethod_m = learner,
                     mlmethod_g = learner,
                     mlmethod_r = learner)
    
    params <- list(params_g = list(num.trees = 100),
                   params_m = list(num.trees = 120),
                   params_r = list(num.trees = 100))
    
  }
   else if (learner == 'regr.rpart') {
    mlmethod <- list(mlmethod_m = learner,
                     mlmethod_g = learner,
                     mlmethod_r = learner)
    
    params <- list(params_g = list(cp = 0.01, minsplit = 20),
                   params_m = list(cp = 0.01, minsplit = 20),
                   params_r = list(cp = 0.01, minsplit = 20))
    
  }
  else if (learner == 'regr.cv_glmnet') {
    mlmethod <- list(mlmethod_m = learner,
                     mlmethod_g = learner,
                     mlmethod_r = learner)
    
    params <- list( params_m = list(s = "lambda.min", 
                                    family = "gaussian"),
                    params_g = list(s = "lambda.min",
                                    family = "gaussian"),
                    params_r = list(s = "lambda.min",
                                    family = "gaussian"))
    
  }   else if (learner == 'regr.glmnet') {
    mlmethod <- list(mlmethod_m = learner,
                     mlmethod_g = learner,
                     mlmethod_r = learner)
    
    params <- list( params_m = list(lambda = 0.01, 
                                    family = "gaussian"),
                    params_g = list(lambda = 0.01,
                                    family = "gaussian"),
                    params_r = list(lambda = 0.01,
                                    family = "gaussian"))
    
  }
  
  return(list(mlmethod=mlmethod, params=params))
}

get_default_mlmethod_irm <- function(learner) {
  if (learner == 'cv_glmnet') {
    mlmethod <- list(mlmethod_m = paste0("classif.", learner),
                     mlmethod_g = paste0("regr.", learner))
    slambda = "lambda.min"
    family = "gaussian"
    
    params <- list(params_m = list(s = slambda),
                   params_g = list(s = slambda, family = family))
  }  
  
  else if (learner == 'rpart') {
    mlmethod <- list(mlmethod_m = paste0("classif.", learner),
                     mlmethod_g = paste0("regr.", learner))

    params <- list(params_g = list(cp = 0.01, minsplit = 20),
                   params_m = list(cp = 0.01, minsplit = 20))
    
  }

  return(list(mlmethod=mlmethod, params=params))
}

get_default_mlmethod_iivm <- function(learner) {
  if (learner == 'cv_glmnet') {
    mlmethod <- list(mlmethod_p = paste0("classif.", learner),
                     mlmethod_mu =  paste0("regr.", learner),
                     mlmethod_m = paste0("classif.", learner))
    slambda = "lambda.min"
    family = "gaussian"
     
    params <- list(params_p = list(s = slambda),
                   params_mu = list(s = slambda, family = family),
                   params_m = list(s = slambda))
  }
  
  else if (learner == 'rpart') {
    mlmethod <- list(mlmethod_p = paste0("classif.", learner),
                     mlmethod_mu = paste0("regr.", learner), 
                     mlmethod_m = paste0("classif.", learner))

    params <- list(params_p = list(cp = 0.01, minsplit = 20),
                   params_mu = list(cp = 0.01, minsplit = 20),
                   params_m = list(cp = 0.01, minsplit = 20))
    
  }
  
  return(list(mlmethod=mlmethod, params=params))
}

