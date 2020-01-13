### Debug R6 classes
The documentation for R6 class debugging can be found here: https://r6.r-lib.org/articles/Debugging.html.
1. Debugging with the first approach [Enabling debugging for all future instances of a class](https://r6.r-lib.org/articles/Debugging.html#enabling-debugging-for-all-future-instances-of-a-class) does not work right now with inherited classes.
2. Debugging with the second approach [Debugging methods in individual objects](https://r6.r-lib.org/articles/Debugging.html#debugging-methods-in-individual-objects) works via
```R
# Generate Data
theta <- 0.5
N <- 1000
k <- 20
b <- 1/(1:k)
sigma <- clusterGeneration::genPositiveDefMat(k,"unifcorrmat")$Sigma

X <- mvtnorm::rmvnorm(N, sigma=sigma)
G <- g(as.vector(X%*%b))
M <- m(as.vector(X%*%b))
d <- M + rnorm(N)
y <- theta * d + G + rnorm(N)

data <- data.frame(y, d,X)

# Instatiate an object of the DoubleMLPLR class
ml_learners = list(mlmethod_m = 'regr.glmnet',
                   mlmethod_g = 'regr.glmnet')

params <- list(params_m = list(s = "lambda.min", 
                               family = "gaussian"),
               params_g = list(s = "lambda.min",
                               family = "gaussian"))
double_mlplr_obj = DoubleMLPLR$new(n_folds = 3,
                                   ml_learners = ml_learners,
                                   params = params,
                                   dml_procedure = 'dml1',
                                   inf_model = 'IV-type')

# Enable debugging for the current instance double_mlplr_obj and method fit
debug(double_mlplr_obj$fit)

# Call fit and arrive in debug prompt
double_mlplr_obj$fit(data, y = "y", d = "d")
# [Debugging prompt]
```

