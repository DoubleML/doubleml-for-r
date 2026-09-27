# Package internals and deterministic fixtures are loaded by the testthat harness.
# nolint start: object_usage_linter.
gate_inference_fixture = function() {
  labels = c("a", "b", "c")
  fixture = make_apo_data(n_obs = 72L, treatment_levels = labels)
  model = DoubleMLAPOS$new(
    fixture$dml_data, "regr.rpart", "classif.rpart",
    treatment_levels = labels, n_folds = 2, n_rep = 2,
    draw_sample_splitting = FALSE
  )
  model$set_sample_splitting(rep(fixture$smpls, 2L))
  predictions = setNames(lapply(seq_along(labels), function(j) {
    g = matrix(fixture$conditional_means[, j], 72L, 2L)
    g[, 2L] = g[, 2L] + j * fixture$df$x / 12
    list(
      ml_g = g,
      ml_m = matrix(fixture$propensity[, j], 72L, 2L)
    )
  }), labels)
  model$fit(external_predictions = predictions)
  list(model = model, groups = rep(c("first", "second", "third"), c(18L, 24L, 30L)))
}

test_that("GATE stacking equals direct contrasted group signals", {
  setup = gate_inference_fixture()
  result = setup$model$gate(setup$groups, reference = "a")
  group_labels = c("first", "second", "third")
  pairs = list(c("b", "a"), c("c", "a"))
  expected_coef = matrix(0, 6L, 2L)
  expected_psi = array(0, c(72L, 6L, 2L))

  for (i_rep in 1:2) {
    column = 0L
    for (group in group_labels) {
      in_group = setup$groups == group
      prevalence = mean(in_group)
      for (pair in pairs) {
        column = column + 1L
        signal = setup$model$apo_models[[pair[1L]]]$psi_b[, i_rep, 1L] -
          setup$model$apo_models[[pair[2L]]]$psi_b[, i_rep, 1L]
        expected_coef[column, i_rep] = mean(signal[in_group])
        expected_psi[, column, i_rep] = as.integer(in_group) / prevalence *
          (signal - expected_coef[column, i_rep])
      }
    }
    expect_equal(unname(result$all_coef[, i_rep]), expected_coef[, i_rep])
    expect_equal(unname(result$psi[, , i_rep]), expected_psi[, , i_rep])
    expect_equal(unname(result$all_cov[, , i_rep]),
      crossprod(expected_psi[, , i_rep]) / 72^2)
    expect_equal(unname(result$all_cov[1:2, 3:6, i_rep]), matrix(0, 2L, 4L))
    expect_equal(unname(result$all_cov[3:4, 5:6, i_rep]), matrix(0, 2L, 2L))
  }

  across_groups = matrix(0, 1L, 6L,
    dimnames = list("second minus first for b-a", names(result$coef)))
  across_groups[1L, c(1L, 3L)] = c(-1, 1)
  heterogeneity = result$linear_transform(across_groups)
  expect_equal(unname(heterogeneity$all_coef),
    unname(result$all_coef[3L, , drop = FALSE] -
      result$all_coef[1L, , drop = FALSE]))
  expect_equal(unname(heterogeneity$psi),
    unname(result$psi[, 3L, , drop = FALSE] - result$psi[, 1L, , drop = FALSE]))

  redundant = rbind("b minus a" = c(-1, 1, 0), "twice b minus a" = c(-2, 2, 0))
  colnames(redundant) = c("a", "b", "c")
  singular = setup$model$gate(setup$groups, contrast = redundant)
  for (i_rep in 1:2) {
    for (rows in list(1:2, 3:4, 5:6)) {
      expect_equal(unname(singular$all_coef[rows[2L], i_rep]),
        unname(2 * singular$all_coef[rows[1L], i_rep]))
      expect_equal(singular$all_cov[rows[2L], rows[2L], i_rep],
        4 * singular$all_cov[rows[1L], rows[1L], i_rep])
      expect_equal(singular$all_cov[rows[1L], rows[2L], i_rep],
        2 * singular$all_cov[rows[1L], rows[1L], i_rep])
    }
  }
})

test_that("GATE repetitions and bootstrap follow framework joint inference", {
  setup = gate_inference_fixture()
  result = setup$model$gate(setup$groups, reference = "a")
  n_obs = 72L
  expected_coef = apply(result$all_coef, 1L, median)
  expected_se = vapply(seq_along(expected_coef), function(j) {
    sqrt(median(
      n_obs * result$all_se[j, ]^2 +
        (result$all_coef[j, ] - expected_coef[j])^2
    ) / n_obs)
  }, numeric(1L))
  expect_equal(unname(result$coef), unname(expected_coef))
  expect_equal(unname(result$se), expected_se)

  n_boot = 200L
  expected_boot = matrix(0, 6L, n_boot * 2L)
  set.seed(711)
  for (i_rep in 1:2) {
    weights = matrix(rnorm(n_boot * n_obs), n_boot, byrow = TRUE)
    columns = (i_rep - 1L) * n_boot + seq_len(n_boot)
    expected_boot[, columns] = t(weights %*% result$psi[, , i_rep]) / n_obs
  }
  set.seed(711)
  result$bootstrap("normal", n_boot)
  expect_equal(unname(result$boot_coef), expected_boot)
  expect_equal(dim(result$boot_coef), c(6L, 400L))
  expect_equal(unname(result$boot_t_stat),
    unname(expected_boot / result$all_se[, rep(1:2, each = n_boot)]))

  pointwise = result$confint()
  joint = result$confint(joint = TRUE)
  expect_true(all(joint[, 1L] <= pointwise[, 1L]))
  expect_true(all(joint[, 2L] >= pointwise[, 2L]))
  ordered = order(abs(result$t_stat), decreasing = TRUE)
  raw = vapply(seq_along(ordered), function(j) {
    remaining = ordered[j:length(ordered)]
    maxima = apply(abs(result$boot_t_stat[remaining, , drop = FALSE]), 2L, max)
    mean(maxima > abs(result$t_stat[ordered[j]]))
  }, numeric(1L))
  expect_equal(unname(result$p_adjust(return_matrix = FALSE)),
    cummax(raw)[order(ordered)])
})

for (threshold in c(0, .05)) {
  test_that(paste("binary APOS GATE equals grouped IRM at threshold", threshold), {
    fixture = apo_irm_equivalence_fixture(threshold)
    apo0 = fixture$apo0
    apo1 = fixture$apo1
    irm = fixture$irm
    apos = DoubleMLAPOS$new(
      apo0$data,
      apo0$learner$ml_g$clone(deep = TRUE),
      apo0$learner$ml_m$clone(deep = TRUE),
      treatment_levels = 0:1, n_folds = 2, n_rep = 2,
      trimming_threshold = threshold, draw_sample_splitting = FALSE
    )
    apos$set_sample_splitting(fixture$smpls)
    apos$fit(store_predictions = TRUE)
    groups = rep(c("low", "middle", "high"), each = 16L)
    if (threshold == 0) {
      result = expect_no_warning(apos$gate(groups, reference = 0))
    } else {
      result = expect_warning(apos$gate(groups, reference = 0), "trimming")
    }
    tolerance = sqrt(.Machine$double.eps)
    expected_coef = matrix(0, 3L, 2L)
    expected_psi = array(0, c(48L, 3L, 2L))
    independent_variance = matrix(0, 3L, 2L)

    expect_equal(as.vector(
      apos$apo_models[["1"]]$psi_b - apos$apo_models[["0"]]$psi_b
    ), as.vector(irm$psi_b), tolerance = tolerance)
    for (i_rep in 1:2) {
      for (i_group in 1:3) {
        in_group = groups == c("low", "middle", "high")[i_group]
        prevalence = mean(in_group)
        signal = irm$psi_b[, i_rep, 1L]
        expected_coef[i_group, i_rep] = mean(signal[in_group])
        expected_psi[, i_group, i_rep] = as.integer(in_group) / prevalence *
          (signal - expected_coef[i_group, i_rep])
        apo0_signal = apo0$psi_b[, i_rep, 1L]
        apo1_signal = apo1$psi_b[, i_rep, 1L]
        phi0 = as.integer(in_group) / prevalence *
          (apo0_signal - mean(apo0_signal[in_group]))
        phi1 = as.integer(in_group) / prevalence *
          (apo1_signal - mean(apo1_signal[in_group]))
        independent_variance[i_group, i_rep] =
          sum(phi0^2 + phi1^2) / 48^2
      }
      expect_equal(unname(result$all_coef[, i_rep]), expected_coef[, i_rep],
        tolerance = tolerance)
      expect_equal(unname(result$psi[, , i_rep]), expected_psi[, , i_rep],
        tolerance = tolerance)
      expected_cov = crossprod(expected_psi[, , i_rep]) / 48^2
      expect_equal(unname(result$all_cov[, , i_rep]), expected_cov,
        tolerance = tolerance)
      expect_equal(unname(result$all_se[, i_rep]), sqrt(diag(expected_cov)),
        tolerance = tolerance)
    }
    expect_equal(unname(result$coef), apply(expected_coef, 1L, median),
      tolerance = tolerance)
    expect_equal(unname(result$se), vapply(1:3, function(j) {
      sqrt(median(48 * result$all_se[j, ]^2 +
        (result$all_coef[j, ] - result$coef[j])^2) / 48)
    }, numeric(1L)), tolerance = tolerance)
    expect_true(any(abs(independent_variance - result$all_se^2) > tolerance))
  })
}
# nolint end: object_usage_linter.
