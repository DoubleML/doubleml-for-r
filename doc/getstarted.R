## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(eval = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  remotes::install_github("DoubleML/doubleml-for-r")

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(DoubleML)

## -----------------------------------------------------------------------------
library(DoubleML)

# Load bonus data
df_bonus = fetch_bonus(return_type="data.table")
head(df_bonus)

# Simulate data
set.seed(3141)
n_obs = 500
n_vars = 100
theta = 3
X = matrix(rnorm(n_obs*n_vars), nrow=n_obs, ncol=n_vars)
d = X[,1:3]%*%c(5,5,5) + rnorm(n_obs)
y = theta*d + X[, 1:3]%*%c(5,5,5) + rnorm(n_obs)

## -----------------------------------------------------------------------------
# Specify the data and variables for the causal model
dml_data_bonus = DoubleMLData$new(df_bonus,
                             y_col = "inuidur1",
                             d_cols = "tg",
                             x_cols = c("female", "black", "othrace", "dep1", "dep2",
                                        "q2", "q3", "q4", "q5", "q6", "agelt35", "agegt54",
                                          "durable", "lusd", "husd"))
print(dml_data_bonus)

# matrix interface to DoubleMLData
dml_data_sim = double_ml_data_from_matrix(X=X, y=y, d=d)
dml_data_sim

## -----------------------------------------------------------------------------
library(mlr3)
library(mlr3learners)
# surpress messages from mlr3 package during fitting
lgr::get_logger("mlr3")$set_threshold("warn")

learner = lrn("regr.ranger", num.trees=500, mtry=floor(sqrt(n_vars)), max.depth=5, min.node.size=2)
ml_g_bonus = learner$clone()
ml_m_bonus = learner$clone()

learner = lrn("regr.glmnet", lambda = sqrt(log(n_vars)/(n_obs)))
ml_g_sim = learner$clone()
ml_m_sim = learner$clone()

## -----------------------------------------------------------------------------
set.seed(3141)
obj_dml_plr_bonus = DoubleMLPLR$new(dml_data_bonus, ml_g=ml_g_bonus, ml_m=ml_m_bonus)
obj_dml_plr_bonus$fit()
print(obj_dml_plr_bonus)

obj_dml_plr_sim = DoubleMLPLR$new(dml_data_sim, ml_g=ml_g_sim, ml_m=ml_m_sim)
obj_dml_plr_sim$fit()
print(obj_dml_plr_sim)

