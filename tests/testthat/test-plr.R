context("Regression tests for dml estimates for partial linear regression model, 'IV-type'")

test_cases = expand.grid(model = c('plr'),
                    learner = c('regr.lm', 'regr.glmnet'),
                    dml_procedure = c('dml1', 'dml2'),
                    score = c('IV-type'),
                    stringsAsFactors = FALSE)
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

skip('Skip old regression tests')
patrick::with_parameters_test_that("Regression tests for dlm estimates for partial linear regression model:",
                                   .cases = test_cases, {

  file_name <- paste0("rds/", test_name, ".rds")

  learner_pars <- get_default_mlmethod_plr(learner)

  set.seed(i_setting)
  #cf <- mlr::makeResampleDesc("CV", iters = 5)
  
  #mlr::configureMlr(show.learner.output = FALSE, show.info = FALSE)

  all_thetas <- vector('numeric', length=n_settings)
  for (i_setting in 1:n_settings) {
    set.seed(1234)
    cf <- mlr3::rsmp("cv", folds = 5)
    plr_hat <- dml_plr(data_plm[[i_setting]], y = "y", d = "Var1",
                       resampling = cf, mlmethod = learner_pars$mlmethod,
                       params = list(params_m = learner_pars$params$params_m,
                         params_g =  learner_pars$params$params_g),
                       dml_procedure = dml_procedure, score = score)
    all_thetas[i_setting] <- coef(plr_hat)
  }

  expect_known_value(all_thetas, file_name, tolerance = 1e-4)
}
)


context("Regression tests for dml estimates for partial linear regression model, 'partialling out'")

test_cases = expand.grid(model = c('plr'),
                    learner = c('regr.lm', 'regr.glmnet'),
                    dml_procedure = c('dml1', 'dml2'),
                    score = c('partialling out'),
                    stringsAsFactors = FALSE)
test_cases['test_name'] = apply(test_cases, 1, paste, collapse="_")

patrick::with_parameters_test_that("Regression tests for dlm estimates for partial linear regression model:",
                                   .cases = test_cases, {

  file_name <- paste0("rds/", test_name, ".rds")

  learner_pars <- get_default_mlmethod_plr(learner)

  set.seed(1234)
  #cf <- mlr::makeResampleDesc("CV", iters = 5)
  cf <- mlr3::rsmp("cv", folds = 5)
  #mlr::configureMlr(show.learner.output = FALSE, show.info = FALSE)

  all_thetas <- vector('numeric', length=n_settings)
  for (i_setting in 1:n_settings) {
    set.seed(i_setting)
    plr_hat <- dml_plr(data_plm[[i_setting]], y = "y", d = "Var1",
                       resampling = cf, mlmethod = learner_pars$mlmethod,
                       params = learner_pars$params,
                       dml_procedure = dml_procedure, score = score)
    all_thetas[i_setting] <- coef(plr_hat)
  }

  expect_known_value(all_thetas, file_name, tolerance = 1e-4)
}
)


test_that("Regression tests for dlm estimates for partial linear regression model:", {

  model = 'plr'
  learner = 'regr.lm'
  dml_procedure = 'dml1'
  score = 'IV-type'

  learner_pars <- get_default_mlmethod_plr(learner)

  set.seed(1234)
  # cf <- mlr::makeResampleDesc("CV", iters = 5)
  # mlr::configureMlr(show.learner.output = FALSE, show.info = FALSE)

  all_thetas <- vector('numeric', length=n_settings)
  
 # Instances <- list()
  
  for (i_setting in 1:n_settings) {
    set.seed(i_setting)
    #Instances[[i_setting]] <- mlr::makeResampleInstance(cf, size = nrow(data_plm[[i_setting]]))
    cf <- mlr3::rsmp("cv", folds = 5)

    plr_hat <- dml_plr(data_plm[[i_setting]], y = "y", d = "Var1",
                       # ResampleInstance = Instances[[i_setting]], 
                       resampling = cf, mlmethod = learner_pars$mlmethod,
                       params = learner_pars$params,
                       dml_procedure = dml_procedure, score = score)
    all_thetas[i_setting] <- coef(plr_hat)
  }


  # all_thetas_manual <- vector('numeric', length=n_settings)
  # for (i_setting in 1:n_settings) {
  #   this_data = data_plm[[i_setting]]
  #   set.seed(i_setting)
  #   cf <- mlr3::rsmp("cv", folds = 5)
  # 
  #   g_indx <- names(this_data) != "Var1"
  #   data_g <- this_data[ , g_indx, drop = FALSE]
  #   task_g <- mlr3::TaskRegr$new(id = "nuis_g", backend = data_g, target = "y")
  # 
  #   ml_g <- mlr3::lrn(learner_pars$mlmethod$mlmethod_g)
  #   
  #   resampling <- cf$instantiate(task_g)
  #   r_g <- mlr3::resample(task_g, ml_g, resampling, store_models = TRUE)
  #   
  #   n_iters <- resampling$iters
  #   train_ids <- lapply(1:n_iters, function(x) resampling$train_set(x))
  #   test_ids <- lapply(1:n_iters, function(x) resampling$test_set(x))
  # 
  #   # rin <- Instances[[i_setting]]
  #   # rin <- mlr::makeResampleInstance(cf, size = nrow(this_data))
  # 
  #   # f1 <- as.formula(paste("d ~ -1 + ", paste(names(this_data)[grep("X", names(this_data))], collapse = " + ")))
  #   # f2 <- as.formula(paste("y ~ -1 + ", paste(names(this_data)[grep("X", names(this_data))], collapse = " + ")))
  #   # 
  #   # data_nuis <- dplyr::select(this_data, - Var1, -y)
  #   # f1 <- as.formula(paste("Var1 ~ 1 + ", paste(names(data_nuis), collapse = " + ")))
  #   # f2 <- as.formula(paste("y ~ 1 + ", paste(names(data_nuis), collapse = " + ")))
  #   # 
  #   # n_iters <- cf$iters
  #   this_thetas_vec <- vector('numeric', length=n_iters)
  #   for (i in 1:n_iters) {
  #     
  #     # this_train_inds = rin$train.inds[[i]]
  #     # this_test_inds = rin$test.inds[[i]]
  #     this_train_inds <- train_ids[[i]]
  #     this_test_inds <- test_ids[[i]]
  # 
  #     this_train_data = this_data[this_train_inds, ]
  #     this_test_data = this_data[this_test_inds, ]
  # 
  #     # m0 <- lm(f1, this_train_data)
  #     # g0 <- lm(f2, this_train_data)
  #     
  #     # vhat <- this_test_data$Var1 - predict(m0, newdata = this_test_data, type = "response")
  #     # uhat <- this_test_data$y - predict(g0, newdata = this_test_data, type = "response")
  # 
  #     this_train_data_X <- dplyr::select(this_train_data, -y, -Var1)
  #     this_test_data_X <- dplyr::select(this_test_data, -y, -Var1)
  # 
  #     X_train_X <- as.matrix(this_train_data_X)
  #     X_test_X <- as.matrix(this_test_data_X)
  # 
  #     # m0 <- glmnet::cv.glmnet(X_train_X, this_train_data$Var1, lambda = learner_pars$params$params_m$lambda)
  #     # g0 <- glmnet::cv.glmnet(X_train_X, this_train_data$y, lambda = learner_pars$params$params_g$lambda)
  #     # m0 <- glmnet::cv.glmnet(X_train_X, this_train_data$Var1)
  #     # g0 <- glmnet::cv.glmnet(X_train_X, this_train_data$y) 
  #     
  #     m0 <- stats::lm(this_train_data$Var1 ~ ., data = data.frame(X_train_X))
  #     g0 <- stats::lm(this_train_data$y ~ ., data = data.frame(X_train_X))
  #     
  #     # vhat <- this_test_data$Var1 - predict(m0, newx =  X_test_X, type = "response", s = learner_pars$params$params_m$s)
  #     # uhat <- this_test_data$y - predict(g0, newx =  X_test_X, type = "response", s = learner_pars$params$params_g$s)
  # 
  #     vhat <- this_test_data$Var1 - predict(m0, newdata = data.frame(X_test_X))
  #     uhat <- this_test_data$y - predict(g0, newdata =  data.frame(X_test_X))
  #     
  #     this_thetas_vec[i] <- mean(vhat * uhat) / mean(vhat * this_test_data$Var1)
  # 
  #   }
  #   all_thetas_manual[i_setting] <- mean(this_thetas_vec)
  # }

  expect_equal(all_thetas, all_thetas_manual, tolerance = 1e-4)
}
)

