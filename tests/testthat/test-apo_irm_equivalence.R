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

# Wrong residual weights, selected class, trimming, centering, repetition
# aggregation, or omitted cross-covariance must break this equivalence.
for (threshold in c(0, .05)) {
  test_that(paste("binary APO contrast equals actual IRM ATE at threshold", threshold), {
    fixture = apo_irm_equivalence_fixture(threshold)
    irm = fixture$irm
    apo0 = fixture$apo0
    apo1 = fixture$apo1
    tolerance = .Machine$double.eps^0.5
    for (obj in list(irm, apo0, apo1)) expect_identical(obj$smpls, fixture$smpls)

    # Establish nuisance identity before comparing any scores or estimates.
    expect_identical(apo0$predictions$ml_g, irm$predictions$ml_g0)
    expect_identical(apo1$predictions$ml_g, irm$predictions$ml_g1)
    expect_identical(apo1$predictions$ml_m, irm$predictions$ml_m)
    expect_identical(apo0$predictions$ml_m, 1 - irm$predictions$ml_m)
    expect_false(identical(irm$predictions$ml_g0[, 1, 1], irm$predictions$ml_g0[, 2, 1]))
    expect_false(identical(irm$predictions$ml_m[, 1, 1], irm$predictions$ml_m[, 2, 1]))
    expect_equal(as.vector(irm$predictions$ml_m[1:2, , 1]), rep(c(.02, .98), 2),
      tolerance = tolerance)
    for (obj in list(apo0, apo1)) {
      for (i_rep in 1:2) {
        diagnostic = obj$propensity_diagnostics[[i_rep]]
        expect_equal(diagnostic$fraction_trimmed, if (threshold == 0) 0 else 2 / 48,
          tolerance = tolerance)
        expect_equal(sort(diagnostic$effective_propensity[1:2]),
          if (threshold == 0) c(.02, .98) else c(.05, .95), tolerance = tolerance)
      }
    }

    expect_equal(as.vector(apo1$all_coef - apo0$all_coef), as.vector(irm$all_coef),
      tolerance = tolerance)
    expect_equal(unname(apo1$coef - apo0$coef), unname(irm$coef), tolerance = tolerance)
    expect_equal(as.vector(apo1$psi_b - apo0$psi_b), as.vector(irm$psi_b),
      tolerance = tolerance)
    influence = apo1$psi[, , 1] - apo0$psi[, , 1]
    expect_equal(as.vector(influence), as.vector(irm$psi), tolerance = tolerance)

    # Joint inference must retain the covariance between the fitted APOs.
    psi = array(0, c(48L, 2L, 2L))
    psi[, 1, ] = apo0$psi[, , 1]
    psi[, 2, ] = apo1$psi[, , 1]
    joint = DoubleMLFramework$new(
      all_coef = unname(rbind(apo0$all_coef, apo1$all_coef)),
      all_se = unname(rbind(apo0$all_se, apo1$all_se)), psi = psi,
      coef_names = c("APO0", "APO1"), smpls = fixture$smpls)
    contrast = joint$linear_transform(matrix(c(-1, 1), 1,
      dimnames = list("ATE", c("APO0", "APO1"))))
    expect_equal(as.vector(contrast$all_coef), as.vector(irm$all_coef), tolerance = tolerance)
    expect_equal(unname(contrast$coef), unname(irm$coef), tolerance = tolerance)
    expect_equal(as.vector(contrast$psi), as.vector(irm$psi), tolerance = tolerance)
    for (i_rep in 1:2) {
      covariance = crossprod(psi[, , i_rep]) / 48^2
      contrast_variance = covariance[1, 1] + covariance[2, 2] - 2 * covariance[1, 2]
      expect_equal(unname(joint$all_cov[, , i_rep]), covariance, tolerance = tolerance)
      expect_gt(abs(covariance[1, 2]), tolerance)
      expect_equal(unname(contrast$all_cov[1, 1, i_rep]), contrast_variance,
        tolerance = tolerance)
      expect_equal(sqrt(contrast_variance), unname(irm$all_se[1, i_rep]),
        tolerance = tolerance)
      expect_equal(sqrt(mean(influence[, i_rep]^2) / 48), unname(irm$all_se[1, i_rep]),
        tolerance = tolerance)
    }
    expect_equal(as.vector(contrast$all_se), as.vector(irm$all_se), tolerance = tolerance)
    expect_equal(unname(contrast$se), unname(irm$se), tolerance = tolerance)
    expected_se = sqrt(median(colMeans(influence^2) +
      (as.vector(apo1$all_coef - apo0$all_coef) - unname(apo1$coef - apo0$coef))^2) / 48)
    expect_equal(expected_se, unname(irm$se), tolerance = tolerance)
  })
}
