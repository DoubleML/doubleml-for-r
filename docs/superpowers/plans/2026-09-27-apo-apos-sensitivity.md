# APO/APOS Sensitivity Analysis Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add theoretically justified omitted-confounding sensitivity analysis for fitted APOs, APOS families, and arbitrary nonzero APO linear contrasts.

**Architecture:** `DoubleMLFramework` owns validation, bound calculation, repeated-split aggregation, robustness values, and presentation for canonical sensitivity arrays. `DoubleMLAPO` learns the additional complement outcome nuisance and constructs arm-specific Riesz elements; `DoubleMLAPOS` concatenates those elements and explicitly rebuilds them for contrasts instead of algebraically transforming marginal bounds. Unsupported estimands keep fitting normally and fail only when sensitivity is requested, with a precise reason.

**Tech Stack:** R, R6, mlr3, checkmate, testthat, roxygen2, base `stats::uniroot()`, DoubleML's existing DML2/cross-fitting infrastructure.

**Spec:** `docs/apo-apos-sensitivity-memo.md`

## Global Constraints

- Preserve all existing APO/APOS coefficients, standard errors, covariance, bootstrap, contrast, tuning, prediction-storage, and model-storage behavior.
- Support sensitivity only for fitted, fully cross-fitted DML2 APO/APOS models with independent observations and the built-in APO score.
- Treat factor, character, and numeric treatment labels as nominal; the complement outcome learner must receive the observed treatment as a categorical feature.
- Existing external prediction lists with only `ml_g` and `ml_m` remain valid for estimation; sensitivity is unavailable unless optional `ml_g_complement` predictions are supplied.
- Implement marginal APO/APOS and arbitrary nonzero linear-contrast bounds; do not implement simultaneous sensitivity regions.
- Do not implement GAPO/GATE sensitivity, DML1 sensitivity, no-cross-fitting sensitivity, clustered sensitivity, plotting, or benchmarking.
- Add no runtime dependency and do not modify existing IRM production code merely to create a parity target.
- Match DoubleML Python 0.11.4 at commit `96e32c9517812a8b8875dd74956704d7998f6510` where the approved memo says the theory coincides.
- Every production change starts with a failing focused test, ends with focused passing tests, and is committed before the next task.
- Execute natively in this session; do not dispatch subagents.

## Review Focus

- Trimmed and normalized propensities: sensitivity must use the exact effective APO weight, including normalization, and tests in Task 3 cover thresholds `0` and `0.05` plus `normalize_ipw = TRUE`.
- Legacy external predictions: two-key inputs must still estimate unchanged while returning a clear sensitivity-unavailable error; three-key inputs must validate and enable sensitivity in Task 3.
- Repeated splits: point bounds and standard errors use framework aggregation, while confidence bounds are medians of repetition-specific one-sided limits; deterministic Task 2 tests distinguish these formulas.
- Degenerate elements: a zero residual variance or zero contrast must not produce `NaN` silently; Task 2 tests the former error path and Task 4 rejects the latter.
- Nominal complement regression: factor/character/numeric labels must be encoded categorically and predictions must remain out of fold; leakage and label-invariance tests live in Task 3.

---

### Task 1: Canonical sensitivity elements and deterministic helpers

**Files:**
- Create: `R/helper_sensitivity.R`
- Create: `tests/testthat/test-sensitivity-helpers.R`

**Interfaces:**
- Consumes: the array convention `observation x estimand x repetition` used by `DoubleMLFramework$psi`.
- Produces: `compute_sensitivity_bias(sigma2, nu2, psi_sigma2, psi_nu2)` and `validate_sensitivity_elements(elements, n_obs, n_coef, n_rep, coef_names, rep_names)`; scalar elements are `estimand x repetition` matrices and influence elements are `observation x estimand x repetition` arrays.

- [ ] **Step 1: Write failing shape and formula tests**

  Add hand-calculated tests for `max_bias = sqrt(sigma2 * nu2)` and `psi_max_bias = (sigma2 * psi_nu2 + nu2 * psi_sigma2) / (2 * max_bias)`. Assert canonical dimnames and exact rejection of missing keys, non-finite entries, incompatible dimensions, nonpositive `sigma2`/`nu2`, and non-centered score arrays beyond numerical tolerance.

- [ ] **Step 2: Run the helper tests and verify red**

  Run: `Rscript -e 'testthat::test_file("tests/testthat/test-sensitivity-helpers.R", reporter = "summary")'`

  Expected: FAIL because the helpers do not exist.

- [ ] **Step 3: Implement the helpers**

  `compute_sensitivity_bias()` accepts conformable matrices/arrays and returns a list containing `max_bias` and `psi_max_bias`. `validate_sensitivity_elements()` requires the seven keys `sigma2`, `nu2`, `psi_sigma2`, `psi_nu2`, `riesz_rep`, `max_bias`, and `psi_max_bias`, validates dimensions/values/names without mutating the input, and returns a canonical deep copy.

- [ ] **Step 4: Run the helper tests and verify green**

  Run the command from Step 2.

  Expected: PASS.

- [ ] **Step 5: Commit**

  ```bash
  git add R/helper_sensitivity.R tests/testthat/test-sensitivity-helpers.R
  git commit -m "feat: add sensitivity element helpers"
  ```

### Task 2: Framework sensitivity engine

**Files:**
- Modify: `R/double_ml_framework.R`
- Create: `tests/testthat/test-double_ml_framework_sensitivity.R`
- Modify: `tests/testthat/test-double_ml_framework.R`

**Interfaces:**
- Consumes: canonical elements from Task 1.
- Produces: constructor arguments `sensitivity_elements = NULL` and `sensitivity_unavailable_reason = NULL`; read-only active fields `sensitivity_elements`, `sensitivity_params`, and `sensitivity_summary`; method `sensitivity_analysis(cf_y = 0.03, cf_d = 0.03, rho = 1, level = 0.95, null_hypothesis = 0)`.

- [ ] **Step 1: Write failing deterministic bound tests**

  Build a two-estimand, two-repetition fixture with hand-computed `max_bias`, `psi_max_bias`, lower/upper influence arrays, repetition-specific standard errors, median point bounds, between-split standard errors, and median repetition-specific confidence limits. Assert the returned object is invisible `self` and `sensitivity_params` has exactly `theta`, `se`, `ci`, `rv`, `rva`, and `input`, with named estimand vectors.

- [ ] **Step 2: Write failing validation and robustness-value tests**

  Assert `cf_y` and `cf_d` are finite scalars in `[0, 1)`, `rho` is in `[-1, 1]`, `level` is in `(0, 1)`, and `null_hypothesis` is a finite scalar or a vector of length `n_coef`. Pin `RV`/`RVa` at zero when the adverse bound already reaches the null, use the smallest root in `(0, 1)`, and return `1` when no strength below one reaches the null (including non-moving `rho = 0` cases).

- [ ] **Step 3: Run the framework sensitivity tests and verify red**

  Run: `Rscript -e 'testthat::test_file("tests/testthat/test-double_ml_framework_sensitivity.R", reporter = "summary")'`

  Expected: FAIL because the constructor fields and method do not exist.

- [ ] **Step 4: Implement storage, bounds, and robustness roots**

  Validate and copy optional elements at construction. Calculate `kappa = abs(rho) * sqrt(cf_y * cf_d / (1 - cf_d))`, repetition-specific bounds and influences, standard errors with the framework evaluation rows, point/SE aggregation with the existing framework rule, and confidence bounds as medians of repetition-specific one-sided limits. Use a monotone boundary check plus `stats::uniroot()` for the smallest attainable `RV` and `RVa`; never optimize a squared objective to a non-root.

- [ ] **Step 5: Implement summaries and immutability**

  `sensitivity_summary` returns a printable character value before and after analysis, showing the scenario, CI/point bounds, null, and RV percentages. Active fields reject assignment. A failed analysis leaves the prior successful `sensitivity_params` unchanged.

- [ ] **Step 6: Define transform and concatenation behavior**

  Extend `linear_transform(contrast, coef_names = NULL, sensitivity_elements = NULL, sensitivity_unavailable_reason = NULL)`: default calls do not propagate sensitivity and return the explicit generic-transform reason; independently derived elements may be supplied by APOS. `concat()` stacks canonical marginal elements only when both sides contain them, otherwise returns no elements and preserves the first concrete unavailable reason. Both operations return fresh objects with `sensitivity_params = NULL`.

- [ ] **Step 7: Run framework tests and verify green**

  Run: `Rscript -e 'testthat::test_local(filter = "framework|sensitivity.helpers", reporter = "summary")'`

  Expected: PASS with existing framework behavior unchanged.

- [ ] **Step 8: Commit**

  ```bash
  git add R/double_ml_framework.R tests/testthat/test-double_ml_framework.R tests/testthat/test-double_ml_framework_sensitivity.R
  git commit -m "feat: add framework sensitivity analysis"
  ```

### Task 3: APO complement nuisance and arm-specific sensitivity

**Files:**
- Modify: `R/helper_apo.R`
- Modify: `R/double_ml_apo.R`
- Modify: `tests/testthat/helper-18-dml_apo.R`
- Create: `tests/testthat/test-double_ml_apo_sensitivity.R`
- Modify: `tests/testthat/test-double_ml_apo.R`
- Modify: `tests/testthat/test-double_ml_apo_tuning.R`

**Interfaces:**
- Consumes: Task 1 canonical elements and Task 2 framework engine.
- Produces: optional external key `ml_g_complement`; read-only APO fields `framework`, `sensitivity_elements`, `sensitivity_params`, and `sensitivity_summary`; APO method `sensitivity_analysis()` with the Task 2 signature.

- [ ] **Step 1: Extend the independent oracle and write failing element tests**

  Add `apo_sensitivity_from_predictions(y, indicator, g_selected, g_complement, p, threshold, normalize_ipw)` to the test helper. For `n_rep = 2`, assert the fitted APO's `sigma2`, `nu2`, both centered scores, Riesz representation, maximum bias, and bias influence equal direct calculations at thresholds `0` and `0.05`, with and without normalization.

- [ ] **Step 2: Write failing learned-complement tests**

  Use leakage-detecting learners to assert complement models train only on `D != d`, predict only held-out rows, and receive the original treatment as a factor feature whose levels remain the full observed level set in every fold. Repeat with factor, character, and numerically spaced labels whose magnitudes must not affect predictions.

- [ ] **Step 3: Write failing external-prediction compatibility tests**

  Assert legacy `{ml_g, ml_m}` matrices still produce byte-for-byte equal APO coefficient/score results but `sensitivity_analysis()` reports the missing complement prediction. Assert `{ml_g, ml_m, ml_g_complement}` enables sensitivity, accepts keys in any order, and rejects wrong names, dimensions, types, or non-finite complement values atomically.

- [ ] **Step 4: Run APO sensitivity tests and verify red**

  Run: `Rscript -e 'testthat::test_file("tests/testthat/test-double_ml_apo_sensitivity.R", reporter = "summary")'`

  Expected: FAIL because complement fitting and APO sensitivity fields do not exist.

- [ ] **Step 5: Implement complement prediction without altering public nuisance storage**

  In fully cross-fitted DML2 learned fits, clone `ml_g`, train it on each fold's complement rows, retain the original treatment as a factor predictor alongside `x_cols`, and predict out of fold. Reuse the selected `ml_g` hyperparameters, including the corresponding fold-specific parameters when enabled; do not add a public tuning parameter, stored prediction, or stored-model key. Skip this nuisance for unsupported DML1/no-cross-fitting fits.

- [ ] **Step 6: Build APO elements during fit**

  Combine selected/complement outcome predictions into `g(D, X)`. Obtain the Riesz representation from the exact trimmed and normalized effective weight used in `score_elements()`, construct all seven elements per repetition regardless of `store_predictions`, and build a one-estimand `DoubleMLFramework` after successful fit. If residual variance is nonpositive, preserve the APO fit but attach a precise sensitivity-unavailable reason.

- [ ] **Step 7: Add APO delegation and unsupported-mode errors**

  Delegate the method and fields to the fitted framework. Before fit, error with `Fit the DoubleMLAPO object before calling sensitivity_analysis().`; for DML1 or no cross-fitting, state that the initial API requires fully cross-fitted DML2; for legacy external predictions, name `ml_g_complement` explicitly.

- [ ] **Step 8: Verify tuning/storage/non-mutation regressions**

  Assert existing `learner_names()`, `params_names()`, tuning results, `predictions`, and `models` retain their current keys; sensitivity calls do not change APO coefficients, scores, standard errors, diagnostics, bootstrap arrays, or stored nuisances.

- [ ] **Step 9: Run APO and focused regression tests**

  Run: `Rscript -e 'testthat::test_local(filter = "double.ml.apo|apo.crossfit|apo.parameter|apo.tuning|apo.simulation|sensitivity", reporter = "summary")'`

  Expected: PASS.

- [ ] **Step 10: Commit**

  ```bash
  git add R/helper_apo.R R/double_ml_apo.R tests/testthat/helper-18-dml_apo.R tests/testthat/test-double_ml_apo.R tests/testthat/test-double_ml_apo_tuning.R tests/testthat/test-double_ml_apo_sensitivity.R
  git commit -m "feat: add APO sensitivity elements"
  ```

### Task 4: APOS families and exact linear-contrast sensitivity

**Files:**
- Modify: `R/double_ml_apos.R`
- Create: `tests/testthat/test-double_ml_apos_sensitivity.R`
- Modify: `tests/testthat/test-double_ml_apos_contrasts.R`
- Modify: `tests/testthat/test-double_ml_apos.R`

**Interfaces:**
- Consumes: arm-specific framework elements from Task 3 and `linear_transform(..., sensitivity_elements = ...)` from Task 2.
- Produces: APOS read-only fields `sensitivity_elements`, `sensitivity_params`, and `sensitivity_summary`; APOS method `sensitivity_analysis()`; sensitivity-enabled frameworks from `causal_contrast()`.

- [ ] **Step 1: Write failing APOS family tests**

  Assert fitted APOS elements are arm-ordered concatenations of component elements for three and four nominal levels, repeated splits, reordered requested levels, and factor/character/numeric labels. Verify sensitivity delegation returns the coordinator invisibly and never mutates components.

- [ ] **Step 2: Write failing arbitrary-contrast tests**

  For named reordered matrices with multiple rows, independently calculate `alpha_c = sum(c_j * alpha_j)`, `nu_c^2 = sum(c_j^2 * nu_j^2)`, `psi_nu_c^2`, and the common `sigma_c^2`/score with weights `c_j^2 / sum(c^2)`. Assert exact element, point-bound, influence, SE, CI, RV, and RVa equality. Test signed, scaled, zero-sum, and nonzero-sum rows; reject every all-zero row.

- [ ] **Step 3: Run APOS sensitivity tests and verify red**

  Run: `Rscript -e 'testthat::test_file("tests/testthat/test-double_ml_apos_sensitivity.R", reporter = "summary")'`

  Expected: FAIL because APOS does not assemble or rebuild sensitivity elements.

- [ ] **Step 4: Assemble APOS sensitivity atomically**

  Build component frameworks from `DoubleMLAPO$framework`, concatenate their marginal elements, and publish the APOS framework only after every component succeeds. If any component lacks sensitivity, keep all existing APOS inference available and attach its concrete reason.

- [ ] **Step 5: Rebuild contrast sensitivity**

  Extend `causal_contrast()` to reorder the validated contrast once, reject zero rows, derive the four primitive contrast elements and combined Riesz array, call `compute_sensitivity_bias()`, and pass the new canonical list into `linear_transform()`. Never add/subtract marginal bounds and never carry previous `sensitivity_params` into the returned framework.

- [ ] **Step 6: Add delegation, immutability, and generic-transform tests**

  Confirm `APOS$sensitivity_analysis()` delegates to its framework; generic `framework$linear_transform()` drops sensitivity with an explanatory error; the APOS method preserves coefficients, covariance, bootstrap, component objects, and input contrast matrices.

- [ ] **Step 7: Run APOS and contrast regressions**

  Run: `Rscript -e 'testthat::test_local(filter = "double.ml.apos|apos.contrast|framework|sensitivity", reporter = "summary")'`

  Expected: PASS.

- [ ] **Step 8: Commit**

  ```bash
  git add R/double_ml_apos.R tests/testthat/test-double_ml_apos.R tests/testthat/test-double_ml_apos_contrasts.R tests/testthat/test-double_ml_apos_sensitivity.R
  git commit -m "feat: add APOS contrast sensitivity"
  ```

### Task 5: Binary IRM reduction and pinned Python parity

**Files:**
- Create: `tests/testthat/test-apo_sensitivity_irm_equivalence.R`
- Create: `tests/testthat/test-apo_sensitivity_python_parity.R`
- Create: `tests/testthat/fixtures/apo-sensitivity-python-0.11.4.csv`
- Modify: `tests/testthat/helper-18-dml_apo.R`

**Interfaces:**
- Consumes: APO/APOS sensitivity APIs from Tasks 3–4 and the existing `apo_irm_equivalence_fixture()`.
- Produces: immutable numerical regression fixtures tied to official Python commit `96e32c9517812a8b8875dd74956704d7998f6510`.

- [ ] **Step 1: Write the binary reduction tests**

  At trimming thresholds `0` and `0.05` with two repetitions, compare APOS contrast `c = (-1, 1)` against an independent binary IRM oracle using the IRM's stored `g0`, `g1`, and propensity predictions. Require machine-precision equality for Riesz arrays, `sigma2`, `nu2`, both scores, maximum bias, bias influence, scenario bounds, SEs, and CIs. Compare `RV` and `RVa` within root-finding tolerance and also require their evaluated adverse bounds to reach the null within that tolerance.

- [ ] **Step 2: Generate and record the pinned Python fixture**

  Run a temporary isolated Python 0.11.4 validation script on deterministic arrays, record only inputs and expected numerical outputs in the CSV, and include the version, commit, formulas, and generation command in comments in the R test. Do not add Python as an R test dependency.

- [ ] **Step 3: Write the R parity test and verify it detects perturbations**

  Load the committed fixture with base R, reproduce the same framework/APO calculations, and compare algebraic quantities at `tolerance = 1e-12`. Compare Python's bounded-minimization `RV`/`RVa` with R's smallest-root implementation at `tolerance = 1e-4` and verify the corresponding adverse bounds reach the null. Temporarily perturb one expected value to confirm the test fails, then restore it.

- [ ] **Step 4: Run parity tests**

  Run: `Rscript -e 'testthat::test_local(filter = "sensitivity.irm.equivalence|sensitivity.python.parity", reporter = "summary")'`

  Expected: PASS.

- [ ] **Step 5: Commit**

  ```bash
  git add tests/testthat/helper-18-dml_apo.R tests/testthat/test-apo_sensitivity_irm_equivalence.R tests/testthat/test-apo_sensitivity_python_parity.R tests/testthat/fixtures/apo-sensitivity-python-0.11.4.csv
  git commit -m "test: verify APO sensitivity reductions"
  ```

### Task 6: Explicit unsupported group sensitivity

**Files:**
- Modify: `R/helper_apo_group.R`
- Modify: `R/double_ml_apo.R`
- Modify: `R/double_ml_apos.R`
- Modify: `tests/testthat/test-double_ml_apo_gapo.R`
- Modify: `tests/testthat/test-double_ml_apos_gate.R`

**Interfaces:**
- Consumes: `sensitivity_unavailable_reason` from Task 2.
- Produces: the exact public GAPO/GATE error required by the memo.

- [ ] **Step 1: Write failing GAPO/GATE error tests**

  Fit sensitivity-capable APO/APOS objects, create GAPO/GATE frameworks, and assert `sensitivity_analysis()` errors exactly with: `Sensitivity analysis for GAPO/GATE is not implemented; conditional group prevalence requires a separate sensitivity influence derivation.` Existing group coefficients and inference must remain unchanged.

- [ ] **Step 2: Run group tests and verify red**

  Run: `Rscript -e 'testthat::test_local(filter = "apo.gapo|apos.gate", reporter = "summary")'`

  Expected: FAIL because group frameworks currently have only a generic missing-elements error.

- [ ] **Step 3: Attach the group-specific reason**

  Pass the exact reason when constructing group frameworks and ensure subsequent group linear transforms preserve it. Do not attach any global APO sensitivity arrays to conditional estimands.

- [ ] **Step 4: Run group and sensitivity tests**

  Run: `Rscript -e 'testthat::test_local(filter = "gapo|gate|sensitivity", reporter = "summary")'`

  Expected: PASS.

- [ ] **Step 5: Commit**

  ```bash
  git add R/helper_apo_group.R R/double_ml_apo.R R/double_ml_apos.R tests/testthat/test-double_ml_apo_gapo.R tests/testthat/test-double_ml_apos_gate.R
  git commit -m "feat: delimit group sensitivity support"
  ```

### Task 7: Public documentation and nominal-treatment example

**Files:**
- Modify: `R/double_ml_framework.R`
- Modify: `R/double_ml_apo.R`
- Modify: `R/double_ml_apos.R`
- Modify: `vignettes/apo-apos.Rmd`
- Modify: `docs/apo-apos-sensitivity-memo.md` only if implementation findings require a factual correction
- Generated: `man/DoubleMLFramework.Rd`
- Generated: `man/DoubleMLAPO.Rd`
- Generated: `man/DoubleMLAPOS.Rd`

**Interfaces:**
- Consumes: the final APIs and limitations from Tasks 1–6.
- Produces: complete R documentation and a runnable `control / chlorine / filter / boiling` sensitivity example.

- [ ] **Step 1: Add roxygen equations and API documentation**

  Document `cf_y`, `cf_d`, `rho`, maximum bias, bound influences, repeated-split aggregation, robustness values, all active fields, the optional external complement key, the DML2/cross-fitting restriction, and the exact unsupported cases. State that nonzero-sum contrast rows are linear potential-outcome functionals rather than treatment effects.

- [ ] **Step 2: Add the vignette example**

  Extend the nominal-treatment vignette with APO and APOS calls to `sensitivity_analysis()`, printed summaries, a named arbitrary contrast, and the GAPO/GATE limitation. Use a small deterministic learner setup and no plot/benchmark API.

- [ ] **Step 3: Regenerate documentation and inspect scope**

  Run: `Rscript -e 'devtools::document()'`

  Expected: only the three intended Rd files plus `NAMESPACE` if roxygen requires it. Revert unrelated roxygen-version churn.

- [ ] **Step 4: Validate examples and render the vignette**

  Run: `Rscript -e 'tools::Rd2ex("man/DoubleMLFramework.Rd", out = "/tmp/DoubleMLFramework.R"); source("/tmp/DoubleMLFramework.R", echo = FALSE)'`

  Run analogous commands for APO/APOS, then: `Rscript -e 'rmarkdown::render("vignettes/apo-apos.Rmd", output_dir = "/tmp/doubleml-vignette")'`

  Expected: all commands exit zero.

- [ ] **Step 5: Commit**

  ```bash
  git add R/double_ml_framework.R R/double_ml_apo.R R/double_ml_apos.R vignettes/apo-apos.Rmd man/DoubleMLFramework.Rd man/DoubleMLAPO.Rd man/DoubleMLAPOS.Rd NAMESPACE
  git commit -m "docs: document APO sensitivity analysis"
  ```

### Task 8: Whole-branch verification, local installation, push, and PR

**Files:**
- Verify all branch changes; no planned production edit.

**Interfaces:**
- Consumes: the completed APO/APOS/GATE/sensitivity branch.
- Produces: a verified source tarball installed in the user's active R library, a pushed GitHub branch, and an upstream pull request.

- [ ] **Step 1: Run static and focused checks**

  Run `git diff --check`, parse every `R/*.R`, run loaded-namespace lint, validate changed Rd files with `tools::checkRd()`, and execute the focused `apo|apos|framework|sensitivity|gate` suite. If `pre-commit` is installed, run `pre-commit run --all-files`; otherwise record its exact absence without claiming success.

- [ ] **Step 2: Run the complete test suite**

  Run: `Rscript -e 'testthat::test_local(reporter = "summary")'`

  Expected: zero failures; record documented skips and warning counts.

- [ ] **Step 3: Build and check the exact source artifact**

  Run `R CMD build .`, compute its SHA-256, and run `_R_CHECK_CRAN_INCOMING_REMOTE_=false R CMD check --as-cran <exact-tarball>`. Expected: zero errors and zero warnings; document all notes rather than suppressing them.

- [ ] **Step 4: Install and verify the same tarball locally**

  Run `R CMD INSTALL <exact-tarball>` into the first writable path from `.libPaths()`, then start a clean R process and assert `packageVersion("DoubleML")`, `DoubleMLAPO`, `DoubleMLAPOS`, sensitivity methods, and the installed package path come from that installation. Do not rebuild between check and install.

- [ ] **Step 5: Review branch scope and commit any verification-only correction**

  Inspect `git status`, `git diff main...HEAD --stat`, commit history, generated files, and absence of build/check artifacts in Git. If a correction is needed, return to the owning TDD task and rerun Steps 1–4.

- [ ] **Step 6: Push the verified branch**

  Run: `git push -u origin feature/apo-apos`

  Expected: the remote fork branch points to the exact verified HEAD.

- [ ] **Step 7: Create the upstream pull request**

  Run `gh pr create --repo DoubleML/doubleml-for-r --base main --head jadrk040507:feature/apo-apos` with a title and body summarizing APO/APOS, joint contrasts, GATE, sensitivity scope, binary/Python validation, checks, tarball checksum, unsupported cases, and the local-install verification.

  Expected: return the PR URL and confirm the PR head SHA equals the verified local HEAD.
