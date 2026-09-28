# Fixed contributions expose omitted correlation and wrong repetition scaling.
framework_fixture = function() {
  list(
    all_coef = matrix(c(1, 2, 3, 6), 2),
    all_se = matrix(c(sqrt(1.25), sqrt(.5), 1, sqrt(2)), 2),
    psi = array(c(-3, -1, 1, 3, -2, 0, 0, 2, -2, -2, 2, 2, -4, 0, 0, 4), c(4, 2, 2)),
    coef_names = c("a", "b")
  )
}

# Seeded independent multipliers catch drawing separate weights per estimand.
test_that("bootstrap shares multipliers within each repetition for every method", {
  obj = do.call(DoubleMLFramework$new, framework_fixture())
  for (method in c("normal", "Bayes", "wild")) {
    set.seed(17)
    expected = matrix(0, 2, 14)
    for (i_rep in 1:2) {
      draws = switch(method,
        normal = rnorm(28),
        Bayes = rexp(28) - 1,
        wild = rnorm(28) / sqrt(2) + (rnorm(28)^2 - 1) / 2
      )
      weights = matrix(draws, 7, byrow = TRUE)
      expected[, (i_rep - 1) * 7 + 1:7] = t(weights %*% obj$psi[, , i_rep]) / 4
    }
    set.seed(17)
    expect_identical(obj$bootstrap(method, 7), obj)
    expect_equal(dim(obj$boot_coef), c(2L, 14L))
    expect_equal(unname(obj$boot_coef), expected)
    expected_t = expected / obj$all_se[, rep(1:2, each = 7)]
    expect_equal(unname(obj$boot_t_stat), unname(expected_t))
    expect_identical(rownames(obj$boot_t_stat), c("a", "b"))
  }
})

test_that("bootstrap uses only each repetition's evaluation observations", {
  args = framework_fixture()
  args$apply_cross_fitting = FALSE
  args$smpls = list(
    list(train_ids = list(1:2), test_ids = list(3:4)),
    list(train_ids = list(1), test_ids = list(2:4))
  )
  obj = do.call(DoubleMLFramework$new, args)
  set.seed(23)
  w1 = matrix(rnorm(10), 5, byrow = TRUE)
  w2 = matrix(rnorm(15), 5, byrow = TRUE)
  expected = cbind(
    t(w1 %*% args$psi[3:4, , 1]) / 2,
    t(w2 %*% args$psi[2:4, , 2]) / 3
  )
  set.seed(23)
  obj$bootstrap(n_rep_boot = 5)
  expect_equal(unname(obj$boot_coef), expected)
  expect_equal(unname(obj$boot_t_stat), unname(expected / obj$all_se[, rep(1:2, each = 5)]))
})

test_that("confidence intervals use normal or simultaneous bootstrap critical values", {
  obj = do.call(DoubleMLFramework$new, framework_fixture())
  marginal = obj$confint(level = .9)
  expected = cbind(obj$coef - qnorm(.95) * obj$se, obj$coef + qnorm(.95) * obj$se)
  expect_equal(unname(marginal), unname(expected))
  expect_identical(colnames(marginal), c("5 %", "95 %"))
  expect_equal(obj$confint("b", level = .9), marginal["b", , drop = FALSE])
  expect_equal(obj$confint(2:1, level = .9), marginal[2:1, , drop = FALSE])
  set.seed(31)
  obj$bootstrap(n_rep_boot = 20)
  critical = unname(quantile(apply(abs(obj$boot_t_stat), 2, max), .9))
  expected = cbind(obj$coef - critical * obj$se, obj$coef + critical * obj$se)
  joint = obj$confint(joint = TRUE, level = .9)
  expect_equal(unname(joint), unname(expected))
  expect_equal(obj$confint("b", joint = TRUE, level = .9), joint["b", , drop = FALSE])
})

# Hand-checked seed 11 yields ordered raw p-values .08, .22, .18.
# Removing cummax, keeping rejected hypotheses, or losing original order fails.
test_that("Romano-Wolf drops rejected hypotheses and enforces stepdown monotonicity", {
  psi = array(c(-3, -1, 1, 3, -2, 0, 0, 2, 1, -1, -1, 1), c(4, 3, 1))
  se = matrix(c(sqrt(1.25), sqrt(.5), .5), 3)
  obj = DoubleMLFramework$new(se * c(1, 2, 1.5), se, psi, c("a", "b", "c"))
  set.seed(11)
  obj$bootstrap(n_rep_boot = 50)
  adjusted = obj$p_adjust(return_matrix = FALSE)
  expect_equal(unname(adjusted), c(.22, .08, .22))
  expect_true(all(diff(adjusted[order(abs(obj$t_stat), decreasing = TRUE)]) >= 0))
  expect_equal(obj$p_adjust("RW", FALSE), adjusted)
  table = obj$p_adjust()
  expect_identical(colnames(table), c("Estimate.", "pval"))
  expect_identical(rownames(table), c("a", "b", "c"))
  expect_equal(unname(table[, 2]), unname(adjusted))
  expect_equal(table[, 1], obj$coef)
})

test_that("standard p-value adjustments do not require bootstrap", {
  obj = do.call(DoubleMLFramework$new, framework_fixture())
  for (method in p.adjust.methods) {
    expect_equal(unname(obj$p_adjust(method, FALSE)), unname(p.adjust(obj$pval, method)))
  }
})

test_that("single-estimand bootstrap and inference preserve matrix dimensions", {
  obj = do.call(DoubleMLFramework$new, framework_fixture())
  obj = obj$linear_transform(matrix(c(1, 0), 1), "a")
  set.seed(3)
  obj$bootstrap(n_rep_boot = 1)
  expect_equal(dim(obj$boot_coef), c(1L, 2L))
  expect_equal(dim(obj$confint(joint = TRUE)), c(1L, 2L))
  expect_equal(
    unname(obj$p_adjust(return_matrix = FALSE)),
    mean(abs(obj$boot_t_stat) > abs(obj$t_stat))
  )
})

test_that("inference validates arguments and bootstrap prerequisites", {
  obj = do.call(DoubleMLFramework$new, framework_fixture())
  expect_error(obj$confint(joint = TRUE), "bootstrap")
  expect_error(obj$p_adjust(), "bootstrap")
  expect_error(obj$bootstrap("invalid"), "method")
  for (count in list(0, -1, 1.5, NA_real_, Inf, c(1, 2))) {
    expect_error(obj$bootstrap(n_rep_boot = count), "n_rep_boot")
  }
  for (level in list(0, 1, -1, NA_real_, Inf, c(.9, .95))) {
    expect_error(obj$confint(level = level), "level")
  }
  for (parm in list("missing", c("a", "missing"), 0, 3, 1.5, NA_real_, TRUE)) {
    expect_error(obj$confint(parm), "parm|parameter")
  }
  expect_error(obj$confint(joint = NA), "joint")
  expect_error(obj$p_adjust("invalid"), "method")
  expect_error(obj$p_adjust(return_matrix = NA), "return_matrix")
  set.seed(1)
  obj$bootstrap(n_rep_boot = 3)
  before = obj$boot_t_stat
  expect_error(obj$bootstrap("invalid"), "method")
  expect_identical(obj$boot_t_stat, before)
  expect_null(obj$linear_transform(diag(2))$boot_t_stat)
})

test_that("framework aggregates repeated estimates and joint covariance", {
  obj = do.call(DoubleMLFramework$new, framework_fixture())
  expect_equal(unname(obj$coef), c(2, 4))
  expect_equal(unname(obj$se), sqrt(c(1.375, 2.25)))
  expect_equal(unname(obj$all_cov[, , 1]), matrix(c(1.25, .75, .75, .5), 2))
  expect_equal(unname(obj$all_cov[, , 2]), matrix(c(1, 1, 1, 2), 2))
  expect_named(obj$coef, c("a", "b"))
  expect_equal(obj$t_stat, obj$coef / obj$se)
  expect_equal(obj$pval, 2 * pnorm(-abs(obj$t_stat)))
  expect_null(obj$boot_coef)
  expect_null(obj$boot_t_stat)
  expect_output(
    {
      table = obj$summary()
    },
    "Std. Error"
  )
  expect_equal(unname(table[, 1:2]), cbind(c(2, 4), sqrt(c(1.375, 2.25))))
  expect_output(obj$print(), "DoubleMLFramework")
  expect_error(
    {
      obj$coef = 1
    },
    "can't set"
  )
})

test_that("framework rejects malformed dimensions, values and names", {
  args = framework_fixture()
  bad = args
  bad$all_coef = bad$all_coef[, 1, drop = FALSE]
  expect_error(do.call(DoubleMLFramework$new, bad), "dimension")
  bad = args
  bad$psi = bad$psi[, , 1]
  expect_error(do.call(DoubleMLFramework$new, bad), "psi")
  bad = args
  bad$psi = array(0, c(4, 3, 2))
  expect_error(do.call(DoubleMLFramework$new, bad), "dimension")
  for (field in c("all_coef", "all_se", "psi")) {
    bad = args
    bad[[field]][1] = Inf
    expect_error(do.call(DoubleMLFramework$new, bad), "finite")
    bad[[field]][1] = NA_real_
    expect_error(do.call(DoubleMLFramework$new, bad), "finite")
  }
  bad = args
  bad$all_se[1] = -1
  expect_error(do.call(DoubleMLFramework$new, bad), "nonnegative")
  for (labels in list(c("a", "a"), c("a", ""), c("a", NA_character_), "a")) {
    bad = args
    bad$coef_names = labels
    expect_error(do.call(DoubleMLFramework$new, bad), "coef_names")
  }
  bad = args
  rownames(bad$all_coef) = c("b", "a")
  expect_error(do.call(DoubleMLFramework$new, bad), "names")
  bad = args
  bad$apply_cross_fitting = NA
  expect_error(do.call(DoubleMLFramework$new, bad), "apply_cross_fitting")
})

test_that("framework validates synchronized sample splits", {
  args = framework_fixture()
  split = list(train_ids = list(1:2, 3:4), test_ids = list(3:4, 1:2))
  args$smpls = list(split, split)
  expect_silent(do.call(DoubleMLFramework$new, args))
  args$smpls = list(split)
  expect_error(do.call(DoubleMLFramework$new, args), "repetition")
  bad = split
  bad$test_ids[[2]] = c(1, 3)
  args$smpls = list(split, bad)
  expect_error(do.call(DoubleMLFramework$new, args), "partition|overlap")
  bad = split
  bad$test_ids[[2]] = c(1, 5)
  args$smpls = list(split, bad)
  expect_error(do.call(DoubleMLFramework$new, args), "indices")
  bad = split
  bad$train_ids[[1]] = c(1, 3)
  args$smpls = list(split, bad)
  expect_error(do.call(DoubleMLFramework$new, args), "overlap")
})

test_that("no-cross-fitting covariance uses each evaluation set and its size", {
  args = framework_fixture()
  args$apply_cross_fitting = FALSE
  expect_error(do.call(DoubleMLFramework$new, args), "smpls")
  args$smpls = list(
    list(train_ids = list(1:2), test_ids = list(3:4)),
    list(train_ids = list(1), test_ids = list(2:4))
  )
  obj = do.call(DoubleMLFramework$new, args)
  expect_equal(unname(obj$all_cov[, , 1]), matrix(c(2.5, 1.5, 1.5, 1), 2))
  expect_equal(unname(obj$all_cov[, , 2]), matrix(c(12, 8, 8, 16), 2) / 9)
  expect_equal(unname(obj$se), sqrt(c(1.375, 2.25)))
  res = obj$linear_transform(matrix(c(-1, 1), 1))
  expect_equal(unname(res$all_se), matrix(sqrt(c(.5, 12 / 9)), 1))
  args$smpls[[2]] = list(train_ids = list(1:2, 3:4), test_ids = list(3:4, 1:2))
  expect_error(do.call(DoubleMLFramework$new, args), "single|one|fold")
})

test_that("linear transformations preserve correlated uncertainty and column identity", {
  obj = do.call(DoubleMLFramework$new, framework_fixture())
  contrast = matrix(c(1, -1), 1, dimnames = list("b-a", c("b", "a")))
  res = obj$linear_transform(contrast)
  expect_equal(unname(res$all_coef), matrix(c(1, 3), 1))
  expect_equal(unname(res$all_se), matrix(sqrt(c(.25, 1)), 1))
  expect_equal(unname(res$coef), 2)
  expect_equal(unname(res$se), sqrt(.875))
  expect_named(res$coef, "b-a")
  expect_equal(unname(res$psi[, 1, 1]), c(1, 1, -1, -1))
  expect_equal(obj$linear_transform(diag(2), c("x", "y"))$coef, c(x = 2, y = 4))
  expect_error(obj$linear_transform(matrix(1, 1, 3)), "contrast")
  expect_error(obj$linear_transform(matrix(NA_real_, 1, 2)), "finite")
  colnames(contrast) = c("b", "b")
  expect_error(obj$linear_transform(contrast), "names")
})

test_that("concatenation retains cross-object covariance and enforces compatibility", {
  args = framework_fixture()
  left = DoubleMLFramework$new(
    args$all_coef[1, , drop = FALSE],
    args$all_se[1, , drop = FALSE], args$psi[, 1, , drop = FALSE], "a"
  )
  right = DoubleMLFramework$new(
    args$all_coef[2, , drop = FALSE],
    args$all_se[2, , drop = FALSE], args$psi[, 2, , drop = FALSE], "b"
  )
  expect_equal(left$concat(right)$all_cov, do.call(DoubleMLFramework$new, args)$all_cov)
  expect_error(left$concat(left), "names")
  expect_error(left$concat(1), "DoubleMLFramework")
  bad = DoubleMLFramework$new(matrix(1, 1, 1), matrix(1, 1, 1), array(0, c(4, 1, 1)), "c")
  expect_error(left$concat(bad), "repetition")
  bad = DoubleMLFramework$new(matrix(1, 1, 2), matrix(1, 1, 2), array(0, c(3, 1, 2)), "c")
  expect_error(left$concat(bad), "observation")
  split = list(train_ids = list(1:2, 3:4), test_ids = list(3:4, 1:2))
  args$smpls = list(split, split)
  with_splits = do.call(DoubleMLFramework$new, args)$linear_transform(matrix(c(1, 0), 1), "c")
  expect_error(left$concat(with_splits), "splits")
})

test_that("identity transforms retain observation and repetition identity", {
  args = framework_fixture()
  colnames(args$all_coef) = colnames(args$all_se) = c("r1", "r2")
  dimnames(args$psi) = list(paste0("obs", 1:4), args$coef_names, c("r1", "r2"))
  obj = do.call(DoubleMLFramework$new, args)
  res = obj$linear_transform(diag(2), args$coef_names)
  expect_identical(dimnames(res$psi), dimnames(obj$psi))
  expect_identical(dimnames(res$all_se), dimnames(obj$all_se))
  expect_identical(res$all_cov, obj$all_cov)
  one = obj$linear_transform(matrix(c(1, 0), 1), "single")
  expect_equal(dim(one$linear_transform(matrix(2, 1, 1))$psi), c(4L, 1L, 2L))
  args$all_se = args$all_se[, 2:1]
  expect_error(do.call(DoubleMLFramework$new, args), "Repetition names")
})

test_that("empty, nonnumeric and zero-row inputs fail before inference", {
  args = framework_fixture()
  for (field in c("all_coef", "all_se")) {
    bad = args
    bad[[field]] = matrix(numeric(), 0, 2)
    expect_error(do.call(DoubleMLFramework$new, bad), "nonempty")
    bad[[field]] = matrix("x", 2, 2)
    expect_error(do.call(DoubleMLFramework$new, bad), "numeric")
  }
  bad = args
  bad$psi = array(numeric(), c(0, 2, 2))
  expect_error(do.call(DoubleMLFramework$new, bad), "nonempty")
  obj = do.call(DoubleMLFramework$new, args)
  for (field in c(
    "se", "t_stat", "pval", "all_coef", "all_se", "psi",
    "all_cov", "boot_coef", "boot_t_stat", "sensitivity_elements",
    "sensitivity_params", "sensitivity_summary"
  )) {
    expect_error(
      {
        obj[[field]] = NULL
      },
      "can't set"
    )
  }
})

test_that("concatenation rejects different evaluation sets", {
  args = framework_fixture()
  args$smpls = rep(list(list(train_ids = list(1:2), test_ids = list(3:4))), 2)
  args$apply_cross_fitting = FALSE
  left = do.call(DoubleMLFramework$new, args)$linear_transform(matrix(c(1, 0), 1), "left")
  args$smpls[[2]] = list(train_ids = list(3:4), test_ids = list(1:2))
  right = do.call(DoubleMLFramework$new, args)$linear_transform(matrix(c(0, 1), 1), "right")
  expect_error(left$concat(right), "splits")
})
