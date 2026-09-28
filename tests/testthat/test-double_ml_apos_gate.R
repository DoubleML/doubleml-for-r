# Package internals are loaded by the testthat harness.
# nolint start: object_usage_linter.
fitted_gate_apos = function(labels = c("control", "chlorine", "filter", "boiling"),
                             n_rep = 2L) {
  fixture = make_apo_data(n_obs = 72L, treatment_levels = labels)
  model = DoubleMLAPOS$new(
    fixture$dml_data, "regr.rpart", "classif.rpart",
    treatment_levels = labels, n_folds = 2, n_rep = n_rep,
    draw_sample_splitting = FALSE
  )
  model$set_sample_splitting(rep(fixture$smpls, n_rep))
  predictions = setNames(lapply(seq_along(labels), function(j) {
    g = matrix(fixture$conditional_means[, j], 72L, n_rep)
    observed_level = match(as.character(fixture$df$d), as.character(labels))
    g_complement = fixture$conditional_means[cbind(seq_len(72L), observed_level)]
    for (i_rep in seq_len(n_rep)) {
      g[, i_rep] = g[, i_rep] + (i_rep - 1L) * j * fixture$df$x / 10
    }
    list(
      ml_g = g,
      ml_m = matrix(fixture$propensity[, j], 72L, n_rep),
      ml_g_complement = matrix(g_complement, 72L, n_rep)
    )
  }), as.character(labels))
  model$fit(
    store_predictions = TRUE, store_models = TRUE,
    external_predictions = predictions[names(model$apo_models)]
  )
  list(fixture = fixture, model = model, predictions = predictions)
}

test_that("reference GATEs are group-major joint signal contrasts", {
  setup = fitted_gate_apos()
  groups = c(rep("g;one", 18L), rep("g%two", 24L), rep("grüp 3", 30L))
  before = list(
    framework = setup$model$framework,
    components = setup$model$apo_models,
    predictions = setup$model$predictions,
    models = setup$model$models,
    smpls = setup$model$smpls
  )

  result = setup$model$gate(groups, reference = "control")
  contrast_labels = c(
    "chlorine - control", "filter - control", "boiling - control"
  )
  expected_names = unlist(lapply(
    encode_apo_group_label(c("g;one", "g%two", "grüp 3")),
    function(group) {
      paste0("GATE[contrast=", encode_apo_group_label(contrast_labels),
        ";group=", group, "]")
    }
  ), use.names = FALSE)
  contrast = cbind(control = -1, diag(3L))
  colnames(contrast) = names(setup$model$apo_models)
  block = kronecker(diag(3L), contrast)
  group_framework = make_apo_group_framework(setup$model$apo_models, groups)

  expect_s3_class(result, "DoubleMLFramework")
  expect_identical(rownames(result$all_coef), expected_names)
  expect_equal(unname(result$all_coef),
    unname(block %*% group_framework$all_coef))
  expected_psi = array(0, c(72L, 9L, 2L))
  for (i_rep in 1:2) {
    expected_psi[, , i_rep] = group_framework$psi[, , i_rep] %*% t(block)
  }
  expect_equal(unname(result$psi), expected_psi)
  for (i_rep in 1:2) {
    expected_cov = block %*% group_framework$all_cov[, , i_rep] %*% t(block)
    expect_equal(unname(result$all_cov[, , i_rep]), unname(expected_cov))
    expect_equal(unname(result$all_se[, i_rep]), sqrt(diag(expected_cov)))
    for (i_group in 1:3) {
      in_group = groups == c("g;one", "g%two", "grüp 3")[i_group]
      direct = vapply(2:4, function(j) {
        mean(setup$model$apo_models[[j]]$psi_b[in_group, i_rep, 1L] -
          setup$model$apo_models[[1L]]$psi_b[in_group, i_rep, 1L])
      }, numeric(1L))
      rows = (i_group - 1L) * 3L + 1:3
      expect_equal(unname(result$all_coef[rows, i_rep]), direct)
    }
  }

  after = list(
    framework = setup$model$framework,
    components = setup$model$apo_models,
    predictions = setup$model$predictions,
    models = setup$model$models,
    smpls = setup$model$smpls
  )
  expect_identical(after, before)

  reason = paste(
    "Sensitivity analysis for GAPO/GATE is not implemented; conditional group",
    "prevalence requires a separate sensitivity influence derivation."
  )
  group_inference = list(
    coef = result$coef, all_coef = result$all_coef, se = result$se,
    all_se = result$all_se, psi = result$psi, all_cov = result$all_cov
  )
  expect_error(result$sensitivity_analysis(), paste0("^", reason, "$"))
  expect_identical(list(
    coef = result$coef, all_coef = result$all_coef, se = result$se,
    all_se = result$all_se, psi = result$psi, all_cov = result$all_cov
  ), group_inference)
})

test_that("GATE reference matching remains nominal and type safe", {
  numeric = fitted_gate_apos(labels = c(10, 2, 7, 1), n_rep = 1L)
  result = numeric$model$gate(rep(c("first", "second"), each = 36L), reference = 7L)
  expect_true(all(grepl(
    "GATE\\[contrast=(10|2|1)%20-%207;group=(first|second)\\]",
    names(result$coef)
  )))
  expect_error(numeric$model$gate(rep(c("first", "second"), each = 36L),
    reference = "7"), "type")

  character = fitted_gate_apos(labels = c("10", "02", "2", "7"), n_rep = 1L)
  result = character$model$gate(rep(c("first", "second"), each = 36L),
    reference = factor("02"))
  expect_true(all(grepl("%20-%2002;group=", names(result$coef))))
  expect_error(character$model$gate(rep(c("first", "second"), each = 36L),
    reference = 2), "type")
})

test_that("arbitrary GATE contrasts reorder columns and enforce zero sums", {
  setup = fitted_gate_apos()
  groups = rep(c("first", "second", "third"), c(18L, 24L, 30L))
  weights = rbind(
    "filter;minus%chlorine" = c(0, -1, 1, 0),
    "pooled minus control" = c(-1, .2, .3, .5)
  )
  colnames(weights) = names(setup$model$apo_models)
  supplied = weights[, c(4, 2, 1, 3)]
  before_weights = supplied
  before = list(
    framework = setup$model$framework,
    components = setup$model$apo_models,
    predictions = setup$model$predictions,
    models = setup$model$models
  )

  result = setup$model$gate(groups, contrast = supplied)
  block = kronecker(diag(3L), weights)
  group_framework = make_apo_group_framework(setup$model$apo_models, groups)
  expect_equal(unname(result$all_coef),
    unname(block %*% group_framework$all_coef))
  expect_identical(result$all_coef,
    setup$model$gate(groups, contrast = weights)$all_coef)
  expect_true(all(grepl("^GATE\\[contrast=", names(result$coef))))
  expect_true(any(grepl("filter%3Bminus%25chlorine", names(result$coef))))
  expect_identical(supplied, before_weights)
  expect_identical(list(
    framework = setup$model$framework,
    components = setup$model$apo_models,
    predictions = setup$model$predictions,
    models = setup$model$models
  ), before)

  tolerance = sqrt(.Machine$double.eps)
  near = matrix(c(-1, 1 + tolerance / 2, 0, 0), 1L,
    dimnames = list("near zero", names(setup$model$apo_models)))
  expect_s3_class(setup$model$gate(groups, contrast = near), "DoubleMLFramework")
  outside = near
  outside[1, 2] = 1 + 2 * tolerance
  expect_error(setup$model$gate(groups, contrast = outside), "sum to zero|zero-sum")
})

test_that("gate rejects ambiguous, malformed and unsupported requests", {
  setup = fitted_gate_apos()
  groups = rep(c("first", "second", "third"), c(18L, 24L, 30L))
  weights = matrix(c(-1, 1, 0, 0), 1L,
    dimnames = list("chlorine - control", names(setup$model$apo_models)))
  expect_error(setup$model$gate(groups), "exactly one")
  expect_error(setup$model$gate(groups, "control", weights), "exactly one")
  for (reference in list("absent", character(), c("control", "filter"),
    NA_character_, Inf, TRUE, list("control"), matrix("control"))) {
    expect_error(setup$model$gate(groups, reference = reference))
  }
  for (invalid in list(
    c(-1, 1, 0, 0), matrix(0, 1, 3), matrix(0, 1, 5), matrix(0, 0, 4),
    matrix("0", 1, 4), matrix(c(Inf, 1, 0, 0), 1),
    matrix(c(NA, 1, 0, 0), 1), matrix(0, 1, 4)
  )) {
    expect_error(setup$model$gate(groups, contrast = invalid),
      "contrast|matrix|finite|nonzero")
  }
  for (labels in list(
    c("control", "chlorine", "filter", "extra"),
    c("control", "chlorine", "filter", "filter"),
    c("control", "chlorine", "filter", NA_character_)
  )) {
    colnames(weights) = labels
    expect_error(setup$model$gate(groups, contrast = weights), "column")
  }
  colnames(weights) = names(setup$model$apo_models)
  for (labels in list(c("same", "same"), c("valid", " "),
    c("valid", NA_character_))) {
    invalid = rbind(weights, weights)
    rownames(invalid) = labels
    expect_error(setup$model$gate(groups, contrast = invalid), "name")
  }

  zero_se = setup$model$clone(deep = TRUE)
  zero_components = zero_se$.__enclos_env__$private$apo_models_
  zero_components[[2L]]$.__enclos_env__$private$psi_b_ =
    zero_components[[1L]]$psi_b
  expect_error(zero_se$gate(groups, reference = "control"),
    "positive standard error.*repetition 1")

  fixture = make_apo_data(n_obs = 72L, treatment_levels = c("control", "filter"))
  unfitted = DoubleMLAPOS$new(
    fixture$dml_data, "regr.rpart", "classif.rpart",
    draw_sample_splitting = FALSE
  )
  expect_error(unfitted$gate(rep(c("first", "second"), each = 36L),
    reference = "control"), "fit")
})
# nolint end: object_usage_linter.
