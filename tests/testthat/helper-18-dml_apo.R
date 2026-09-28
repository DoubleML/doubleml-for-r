# nolint start: object_usage_linter.
# Package internals and DoubleMLData are loaded by the testthat harness.
# Balanced nominal assignment and Y(d) = d_index + d_index * X + epsilon.
# Numeric label values do not enter the outcome model.
make_apo_data = function(
  n_obs = 60L, treatment_levels = c("control", "filter", "boiling"), seed = 314L
) {
  treatment_levels = get_apo_levels(treatment_levels, treatment_levels)
  n_levels = length(treatment_levels)
  checkmate::assert_count(n_obs)
  if (n_levels < 2L || n_obs < 2L * n_levels) {
    stop("APO fixtures require at least two levels and two observations per level.")
  }
  # Restore the caller's RNG state so fixtures cannot affect unrelated tests.
  has_seed = exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (has_seed) {
    old_seed = get(".Random.seed", envir = .GlobalEnv)
  }
  on.exit({
    if (has_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  set.seed(seed)
  level_index = rep(seq_len(n_levels), length.out = n_obs)
  x = rnorm(n_obs)
  conditional_means = outer(x, seq_len(n_levels), "*") +
    matrix(rep(seq_len(n_levels), each = n_obs), nrow = n_obs)
  colnames(conditional_means) = as.character(treatment_levels)
  df = data.frame(
    y = conditional_means[cbind(seq_len(n_obs), level_index)] + rnorm(n_obs),
    d = treatment_levels[level_index], x = x
  )
  # Alternating occurrences within each level keep both classes in every fold.
  fold = ((seq_len(n_obs) - 1L) %/% n_levels) %% 2L + 1L
  test_ids = lapply(1:2, function(k) which(fold == k))
  train_ids = lapply(test_ids, function(ids) setdiff(seq_len(n_obs), ids))
  list(
    df = df,
    dml_data = DoubleMLData$new(df, y_col = "y", d_cols = "d", x_cols = "x"),
    treatment_levels = treatment_levels,
    conditional_means = conditional_means,
    propensity = matrix(1 / n_levels,
      nrow = n_obs, ncol = n_levels,
      dimnames = list(NULL, as.character(treatment_levels))
    ),
    true_apo = setNames(seq_len(n_levels), as.character(treatment_levels)),
    smpls = list(list(train_ids = train_ids, test_ids = test_ids))
  )
}

# Independent test oracle: use direct equality rather than production label helpers.
apo_score_from_predictions = function(
  y, d, treatment_level, g_hat, p_hat,
  trimming_threshold = 0, normalize_ipw = FALSE
) {
  p_hat = pmin(pmax(p_hat, trimming_threshold), 1 - trimming_threshold)
  weights = as.integer(d == treatment_level) / p_hat
  if (normalize_ipw) {
    weights = weights / mean(weights)
  }
  psi_b = g_hat + weights * (y - g_hat)
  theta = mean(psi_b)
  psi = psi_b - theta
  list(
    psi_a = rep(-1, length(y)), psi_b = psi_b, theta = theta, psi = psi,
    se = sqrt(mean(psi^2) / length(y))
  )
}
# nolint end: object_usage_linter.

# Independent sensitivity oracle. It deliberately does not call production
# sensitivity helpers, so sign, normalization and centering errors are visible.
# nolint start: object_length_linter.
apo_sensitivity_from_predictions = function(
  y, indicator, g_selected, g_complement, p,
  trimming_threshold = 0, normalize_ipw = FALSE
) {
  p = pmin(pmax(p, trimming_threshold), 1 - trimming_threshold)
  normalization = 1
  if (normalize_ipw) normalization = mean(indicator / p)
  effective_p = p * normalization
  riesz_rep = indicator / effective_p
  observed_g = indicator * g_selected + (1 - indicator) * g_complement
  sigma_score = (y - observed_g)^2
  sigma2 = mean(sigma_score)
  psi_sigma2 = sigma_score - sigma2
  nu_score = 2 / effective_p - riesz_rep^2
  nu2 = mean(nu_score)
  psi_nu2 = nu_score - nu2
  max_bias = sqrt(sigma2 * nu2)
  psi_max_bias = (sigma2 * psi_nu2 + nu2 * psi_sigma2) / (2 * max_bias)
  list(
    sigma2 = sigma2, nu2 = nu2,
    psi_sigma2 = psi_sigma2, psi_nu2 = psi_nu2,
    riesz_rep = riesz_rep, max_bias = max_bias,
    psi_max_bias = psi_max_bias
  )
}
# nolint end: object_length_linter.

ApoSensitivityRegr = R6::R6Class("ApoSensitivityRegr",
  inherit = mlr3::LearnerRegr,
  public = list(
    selected_level = NULL,
    expected_levels = NULL,
    initialize = function(selected_level, expected_levels) {
      self$selected_level = as.character(selected_level)
      self$expected_levels = as.character(expected_levels)
      super$initialize(id = "regr.apo_sensitivity",
        feature_types = c("numeric", "integer", "factor"),
        predict_types = "response")
    }
  ),
  private = list(
    .train = function(task) {
      complement = "d" %in% task$feature_names
      if (complement) {
        d = task$data(cols = "d")$d
        if (!is.factor(d)) stop("Complement treatment feature must be a factor.")
        if (!identical(levels(d), self$expected_levels)) {
          stop("Complement treatment factor must retain all observed levels.")
        }
        if (any(as.character(d) == self$selected_level)) {
          stop("Selected rows leaked into complement outcome training.")
        }
      }
      list(
        train_ids = task$row_ids, complement = complement,
        expected_levels = self$expected_levels
      )
    },
    .predict = function(task) {
      if (any(task$row_ids %in% self$model$train_ids)) {
        stop("In-sample sensitivity outcome prediction.")
      }
      x = task$data(cols = "x")$x
      response = if (self$model$complement) {
        d = task$data(cols = "d")$d
        if (!is.factor(d) || !identical(levels(d), self$model$expected_levels)) {
          stop("Complement prediction lost nominal treatment levels.")
        }
        match(as.character(d), self$model$expected_levels) + x / 1000
      } else {
        .5 + x / 1000
      }
      list(response = response)
    }
  )
)

# Small mlr3 learners expose APO's training/prediction boundary without fitting noise.
ApoLeakageRegr = R6::R6Class("ApoLeakageRegr",
  inherit = mlr3::LearnerRegr,
  public = list(
    initialize = function() {
      super$initialize(id = "regr.apo_leakage", feature_types = "numeric",
        predict_types = "response")
    }
  ),
  private = list(
    .train = function(task) {
      list(train_ids = task$row_ids, log = new.env(parent = emptyenv()))
    },
    .predict = function(task) {
      if (any(task$row_ids %in% self$model$train_ids)) stop("In-sample APO prediction.")
      self$model$log$test_ids = c(self$model$log$test_ids, task$row_ids)
      list(response = as.numeric(task$row_ids))
    }
  )
)

ApoLeakageClassif = R6::R6Class("ApoLeakageClassif",
  inherit = mlr3::LearnerClassif,
  public = list(
    reject_training = FALSE,
    zero_propensity = FALSE,
    initialize = function(reject_training = FALSE, zero_propensity = FALSE) {
      self$reject_training = reject_training
      self$zero_propensity = zero_propensity
      super$initialize(id = "classif.apo_leakage", feature_types = "numeric",
        predict_types = "prob", properties = "twoclass")
    }
  ),
  private = list(
    .train = function(task) {
      if (self$reject_training) stop("APO training was reached before support validation.")
      if (!identical(task$positive, "1")) stop("Wrong positive APO class.")
      list(train_ids = task$row_ids, target = task$truth(),
        log = new.env(parent = emptyenv()))
    },
    .predict = function(task) {
      if (any(task$row_ids %in% self$model$train_ids)) stop("In-sample APO prediction.")
      self$model$log$test_ids = c(self$model$log$test_ids, task$row_ids)
      p = if (self$zero_propensity) rep(0, task$nrow) else .2 + task$row_ids / 1000
      # Return class 0 first to catch selection by position instead of class name.
      list(prob = cbind("0" = 1 - p, "1" = p))
    }
  )
)

# nolint start: object_usage_linter.
# The package harness loads model classes; R6 supplies self and super bindings.
# Deterministic nuisances vary across folds and repetitions. Feature x is a
# stable observation ID, exposing conditional training and complement coding.
apo_irm_equivalence_fixture = function(threshold) {
  outcome = R6::R6Class("ApoIrmEquivalenceRegr",
    inherit = mlr3::LearnerRegr,
    public = list(
      initialize = function() {
        super$initialize(id = "regr.apo_irm_equivalence", feature_types = c("numeric", "integer"),
          predict_types = "response")
      }
    ),
    private = list(
      .train = function(task) {
        x = task$data(cols = "x")$x
        level = unique((x - 1) %% 2)
        if (length(level) != 1L) stop("Outcome training must be conditional on treatment.")
        list(level = level, offset = mean(x) / 100, train_ids = x)
      },
      .predict = function(task) {
        x = task$data(cols = "x")$x
        if (any(x %in% self$model$train_ids)) stop("In-sample outcome prediction.")
        list(response = .3 + x / 20 + self$model$level * (1 + x / 10) +
          self$model$offset)
      }
    )
  )
  propensity = R6::R6Class("ApoIrmEquivalenceClassif",
    inherit = mlr3::LearnerClassif,
    public = list(
      initialize = function() {
        super$initialize(id = "classif.apo_irm_equivalence",
          feature_types = c("numeric", "integer"), predict_types = "prob", properties = "twoclass")
      }
    ),
    private = list(
      .train = function(task) {
        x = task$data(cols = "x")$x
        d = (x - 1) %% 2
        target = as.integer(as.character(task$truth()))
        if (!all(target == d) && !all(target == 1 - d)) stop("Unexpected treatment coding.")
        list(complement = all(target == 1 - d), offset = mean(x) / 1000, train_ids = x)
      },
      .predict = function(task) {
        x = task$data(cols = "x")$x
        if (any(x %in% self$model$train_ids)) stop("In-sample propensity prediction.")
        p = .3 + .1 * ((x - 1) %% 5) + self$model$offset
        p[x == 1] = .02
        p[x == 2] = .98
        if (self$model$complement) p = 1 - p
        list(prob = cbind("0" = 1 - p, "1" = p))
      }
    )
  )
  x = seq_len(48L)
  d = rep(0:1, 24L)
  data = DoubleMLData$new(data.frame(
    y = .5 + x / 17 + d * (2 + x / 30) + cos(x) / 3, d = d, x = x
  ), y_col = "y", d_cols = "d", x_cols = "x")
  smpls = lapply(c(2L, 4L), function(block_size) {
    fold = ((x - 1L) %/% block_size) %% 2L
    tests = lapply(0:1, function(k) which(fold == k))
    list(train_ids = lapply(tests, function(ids) setdiff(x, ids)), test_ids = tests)
  })
  ml_g = outcome$new()
  ml_m = propensity$new()
  irm = DoubleMLIRM$new(data, ml_g$clone(deep = TRUE), ml_m$clone(deep = TRUE),
    n_folds = 2, n_rep = 2, score = "ATE", dml_procedure = "dml2",
    trimming_threshold = threshold, draw_sample_splitting = FALSE)
  apos = lapply(0:1, function(level) {
    DoubleMLAPO$new(data, ml_g$clone(deep = TRUE), ml_m$clone(deep = TRUE), level,
      n_folds = 2, n_rep = 2, dml_procedure = "dml2", normalize_ipw = FALSE,
      trimming_threshold = threshold, draw_sample_splitting = FALSE)
  })
  for (obj in c(list(irm), apos)) {
    obj$set_sample_splitting(smpls)
    obj$fit(store_predictions = TRUE)
  }
  list(irm = irm, apo0 = apos[[1]], apo1 = apos[[2]], smpls = smpls)
}
# nolint end: object_usage_linter.
