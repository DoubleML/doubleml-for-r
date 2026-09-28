# Regenerate from the repository root (development-only; never run on CRAN):
#   Rscript dev/validation/apo_apos_python_parity.R /tmp/apo-apos-parity
# Use a fresh output directory; omit it to create a persistent temporary directory.
# Requires R package dependencies plus pkgload/jsonlite, and Python doubleml,
# numpy, pandas, scikit-learn. Select the interpreter with DOUBLEML_PYTHON.
# No Python results are checked in or fabricated. common.json contains seed,
# data, zero-based train/test indices, settings, and ALL fixed nuisance values.
# Both languages reload that file; only R generates randomness. ml_g_d_lvl0 is
# serialized for Python's sensitivity machinery, but does not enter the APO score.
# Scope: iid observations, dml2, one repetition, numeric nominal labels, both IPW
# settings. Repeated splitting and character/factor labels are covered in R tests.
# R ml_g maps to Python ml_g_d_lvl1 (see DoubleML's irm/apo.py and irm/apos.py):
# https://github.com/DoubleML/doubleml-for-py/tree/main/doubleml/irm
# Python has no public covariance array; its reference covariance is the Gram
# matrix of the framework's scaled_psi divided by n^2, including off-diagonals.
# Python scaled_psi = raw psi / mean(psi_deriv) = -raw psi for APO, so negate
# that field before comparing to R's centered psi; raw APO psi needs no mapping.
# Exit codes: 0 = numerical parity; 2 = missing dependency; 1 = other failure.
# Re-run the Python consumer independently:
#   python3 dev/validation/apo_apos_python_reference.py /tmp/apo-apos-parity

required = c("pkgload", "jsonlite")
for (package in required) {
  if (!requireNamespace(package, quietly = TRUE)) {
    message("DEPENDENCY UNAVAILABLE: missing R package ", package)
    quit(status = 2L)
  }
}
pkgload::load_all(".", quiet = TRUE)
lgr::get_logger("mlr3")$set_threshold("warn")
cat("R: ", R.version.string, "\nR DoubleML: ",
  as.character(utils::packageVersion("DoubleML")), "\n", sep = "")
args = commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) stop("Supply at most one output directory.")
output = if (length(args)) args[1] else tempfile("apo-apos-parity-", tmpdir = dirname(tempdir()))
if (dir.exists(output) && length(list.files(output, all.files = TRUE, no.. = TRUE))) {
  stop("Use a fresh output directory to prevent stale reference results.")
}
dir.create(output, recursive = TRUE, showWarnings = FALSE)
output = normalizePath(output, mustWork = TRUE)
cat("Artifacts: ", output, "\n", sep = "")

seed = 9272026L
set.seed(seed)
n_obs = 120L
labels = c(10, 2, 7)
x = rnorm(n_obs)
d = rep(labels, length.out = n_obs)
means = sweep(outer(x, c(-.7, .5, 1.3)), 2, c(-1, 1, 3), "+")
y = means[cbind(seq_len(n_obs), match(d, labels))] + rnorm(n_obs)
p = exp(outer(x, c(-.2, 0, .2)))
p = p / rowSums(p)
# Include one clipped value per level, below the five-percent warning threshold.
p[1, ] = 1e-8
fold = ((seq_len(n_obs) - 1L) %/% length(labels)) %% 2L + 1L
folds = lapply(1:2, function(k) {
  list(train_ids = which(fold != k) - 1L, test_ids = which(fold == k) - 1L)
})
predictions = setNames(lapply(seq_along(labels), function(j) {
  list(
    ml_g = means[, j] + .1 * x^2,
    ml_m = p[, j],
    ml_g_d_lvl0 = rowMeans(means[, -j, drop = FALSE])
  )
}), as.character(labels))
common = list(
  seed = seed, n_rep = 1L, n_folds = 2L, levels = labels, reference = 10,
  trimming_threshold = .01, atol = 1e-10, rtol = 1e-10,
  r_version = R.version.string,
  r_doubleml_version = as.character(utils::packageVersion("DoubleML")),
  data = list(y = y, d = d, x = x), folds = folds, predictions = predictions
)
jsonlite::write_json(common, file.path(output, "common.json"),
  auto_unbox = TRUE, digits = NA, pretty = TRUE)

# Reconstruct even the R inputs from the serialized source of truth.
common = jsonlite::read_json(file.path(output, "common.json"), simplifyVector = FALSE)
df = as.data.frame(lapply(common$data, unlist))
labels = unlist(common$levels)
data = DoubleMLData$new(df, y_col = "y", d_cols = "d", x_cols = "x")
split = list(
  train_ids = lapply(common$folds, function(f) as.integer(unlist(f$train_ids)) + 1L),
  test_ids = lapply(common$folds, function(f) as.integer(unlist(f$test_ids)) + 1L)
)
predictions = lapply(common$predictions, function(p) {
  list(ml_g = matrix(unlist(p$ml_g)), ml_m = matrix(unlist(p$ml_m)))
})
pack = function(x) {
  shape = dim(x)
  if (is.null(shape)) shape = length(x)
  list(shape = I(as.integer(shape)), values = I(as.numeric(x)))
}
capture_framework = function(framework) {
  lapply(list(coef = framework$coef, se = framework$se,
    all_coef = framework$all_coef, all_se = framework$all_se,
    psi = framework$psi, covariance = framework$all_cov), pack)
}
results = list()
for (normalize in c(FALSE, TRUE)) {
  scenario = if (normalize) "normalized" else "unnormalized"
  obj = DoubleMLAPOS$new(data, "regr.rpart", "classif.rpart",
    treatment_levels = labels, normalize_ipw = normalize,
    trimming_threshold = common$trimming_threshold, draw_sample_splitting = FALSE)
  obj$set_sample_splitting(list(split))
  obj$fit(external_predictions = predictions)
  apo = DoubleMLAPO$new(data, "regr.rpart", "classif.rpart",
    treatment_level = common$reference, normalize_ipw = normalize,
    trimming_threshold = common$trimming_threshold, draw_sample_splitting = FALSE)
  apo$set_sample_splitting(list(split))
  apo$fit(external_predictions = predictions[[as.character(common$reference)]])
  results[[scenario]] = list(
    apo = lapply(list(coef = apo$coef, se = apo$se, psi = apo$psi), pack),
    apos = capture_framework(obj$framework),
    contrast = capture_framework(obj$causal_contrast(reference = common$reference))
  )
}
jsonlite::write_json(results, file.path(output, "r_results.json"),
  auto_unbox = TRUE, digits = NA, pretty = TRUE)
cat("Serialized R results for both IPW settings.\n")

python = Sys.getenv("DOUBLEML_PYTHON", unset = Sys.which("python3"))
if (!nzchar(python)) {
  message("DEPENDENCY UNAVAILABLE: python3 interpreter not found.")
  quit(status = 2L)
}
cat("Python interpreter: ", python, "\n", sep = "")
status = system2(python, c(
  shQuote("dev/validation/apo_apos_python_reference.py"), shQuote(output)))
if (status == 2L) {
  message("DEPENDENCY UNAVAILABLE: see the exact Python import failure above; ",
    "shared inputs and R results remain in ", output)
  quit(status = 2L)
}
if (status != 0L) stop("Python reference failed; this is not a dependency skip.")

# Python also compares independently on standalone invocation. R verifies the
# returned artifact so a successful child exit alone can never imply parity.
reference = jsonlite::read_json(file.path(output, "python_results.json"),
  simplifyVector = FALSE)
for (scenario in names(results)) {
  for (model in names(results[[scenario]])) {
    for (metric in names(results[[scenario]][[model]])) {
      expected = results[[scenario]][[model]][[metric]]
      actual = reference[[scenario]][[model]][[metric]]
      key = paste(scenario, model, metric, sep = "/")
      if (is.null(actual) || !identical(as.integer(unlist(actual$shape)),
        as.integer(expected$shape))) stop("Shape mismatch: ", key)
      value = unlist(actual$values)
      target = as.numeric(expected$values)
      if (length(value) != length(target) || any(!is.finite(value)) ||
        any(abs(value - target) > common$atol + common$rtol * abs(target))) {
        stop("Numerical parity mismatch: ", key)
      }
      cat(sprintf("PASS %s: max absolute error %.3g\n", key, max(abs(value - target))))
    }
  }
}
cat("PASS: APO/APOS Python parity, including full covariance and contrasts.\n")
