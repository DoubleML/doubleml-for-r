# APO/APOS Group Effects Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add group-specific average potential outcomes (GAPOs) and jointly inferred within-group treatment contrasts (GATEs) to the reviewed APO/APOS implementation.

**Architecture:** A focused internal helper validates a fixed categorical partition and converts fitted APO orthogonal signals into one `DoubleMLFramework`, ordered group-major and treatment-minor. `DoubleMLAPO$gapo()` exposes one-treatment group outcome levels; `DoubleMLAPOS$gate()` builds covariance-aware treatment contrasts by applying a block linear transform to the jointly stacked GAPO framework. The first implementation is deliberately limited to fitted, fully cross-fitted DML2 models; DML1 and no-cross-fitting remain unsupported until their fold-weighting target is derived separately.

**Tech Stack:** R, R6, data.table, testthat, mlr3, roxygen2, existing `DoubleMLAPO`, `DoubleMLAPOS`, and `DoubleMLFramework`.

**Spec:** `docs/apo-apos-gate-memo.md`

## Global Constraints

- Treatments and groups are nominal labels; numeric values never imply order, distance, or cut points.
- A group-specific APO is a GAPO; only a within-group treatment contrast whose treatment weights sum to zero is called a GATE.
- Consume fitted APO `psi_b` orthogonal signals; do not refit nuisances or reapply trimming/normalization within groups.
- Preserve observation order, synchronized repetitions and sample splits; never sort or join group membership by display label.
- The initial implementation accepts fully cross-fitted DML2 only. Reject DML1, all no-cross-fitting modes, clusters, instruments, custom scores, weights, soft/overlapping membership, and outcome-adaptive groups.
- Group inference uses prevalence-scaled centered contributions and full cross-treatment covariance; never use an independence approximation.
- Repeated-split aggregation, linear transformations, multiplier bootstrap, intervals, and adjusted p-values must use existing R `DoubleMLFramework` conventions.
- Use the same observation multiplier across every group, treatment, and contrast within a repetition.
- Preserve existing APO, APOS, IRM, and framework behavior and use equals-sign assignment style and roxygen documentation.
- Use TDD and a small commit per task. Generated documentation is committed only after inspecting the diff.
- Sensitivity analysis remains a separate memo gate and implementation plan.
- Do not install this interim GATE-only state. Update the local R installation from the exact final tarball after the approved sensitivity work and parent final-integration checks are complete.

## Review Focus

- A named or unnamed group vector that is positionally permuted must never be silently sorted or joined; Task 1 tests that supplied order is used verbatim.
- Delimiter-like, percent, Unicode, and numeric-looking labels must produce stable unique coefficient names without merging type-distinct estimands; Task 1 owns these tests.
- Rare groups can be algebraically valid but inferentially unusable; Task 1 tests singleton errors, counts two through five warnings, and zero-SE errors.
- A contrast row that is numerically close to, but not actually, zero-sum must not be mislabeled a GATE; Task 3 tests the exact tolerance and rejects zero rows.
- Repeated splits can hide covariance or aggregation mistakes; Task 4 hand-calculates every repetition before checking aggregation and the common-multiplier bootstrap.

---

### Task 1: Categorical-group validation and joint GAPO framework helper

**Files:**
- Create: `R/helper_apo_group.R`
- Create: `tests/testthat/test-apo_group_helpers.R`
- Modify: `DESCRIPTION` (`Collate` section)

**Interfaces:**
- Consumes: a fitted named list of synchronized `DoubleMLAPO` objects and an atomic group vector aligned position-for-position with their data.
- Produces: `prepare_apo_groups(groups, n_obs)` returning `list(levels, labels, membership)` and `make_apo_group_framework(apo_models, groups)` returning a group-major, treatment-minor `DoubleMLFramework`.

- [ ] **Step 1: Write failing group-input tests**

  In `test-apo_group_helpers.R`, test `prepare_apo_groups()` with factor, character, logical, and finite numeric labels. Assert observed factor-level order after dropping unused levels, first-observed order for all other types, unchanged row order in the membership matrix, and unique display labels. Add errors for wrong length, zero length, matrices/data frames/lists, NA, NaN, Inf, blank labels, and display-label collisions. Include delimiter-like, percent, Unicode, and numeric-looking labels and require deterministic collision-free encoded names.

- [ ] **Step 2: Run the helper test and verify RED**

  Run `Rscript -e 'pkgload::load_all(".", quiet = TRUE); testthat::test_file("tests/testthat/test-apo_group_helpers.R")'`.

  Expected: FAIL because `prepare_apo_groups()` and `make_apo_group_framework()` do not exist.

- [ ] **Step 3: Implement group preparation and name encoding**

  Add `prepare_apo_groups(groups, n_obs)` and an internal `encode_apo_group_label(x)` in `R/helper_apo_group.R`. Return an `n_obs` by `H` integer membership matrix with one one per row. Encode public name components with `utils::URLencode(as.character(x), reserved = TRUE)` and use `GAPO[treatment=<encoded>;group=<encoded>]`; reject any remaining duplicate full names.

- [ ] **Step 4: Add failing deterministic framework tests**

  Fit two APO components with fixed external predictions, two repetitions, and unequal groups. For each repetition assert
  `theta[d,g] = mean(psi_b[group == g, r, d])`,
  `phi[i,d,g] = 1{G_i=g} / q[g] * (psi_b[i,r,d] - theta[d,g])`,
  `all_cov[,,r] = crossprod(phi[,,r]) / n^2`, and
  `all_se[,r] = sqrt(diag(all_cov[,,r]))`.
  Assert group-major/treatment-minor order, exact zero cross-group blocks, generally nonzero within-group cross-treatment covariance, and no mutation of components or source data.

- [ ] **Step 5: Implement `make_apo_group_framework()`**

  Validate a nonempty uniquely named APO list, successful built-in APO fits, identical data/order/splits/repetitions, `dml_procedure == "dml2"`, and `apply_cross_fitting == TRUE`. Build the arrays from each model's stored `psi_b`; use `q = n_group / n`, center within group, and pass `all_coef`, diagonal-derived `all_se`, `psi`, coefficient names, shared splits, and `apply_cross_fitting = TRUE` to `DoubleMLFramework$new()`.

  Require at least two rows per group in every repetition, warn once per group/repetition for counts two through five, warn for an observed group-treatment cell with no treated rows, warn when more than five percent of that group's raw propensities were trimmed, and reject nonfinite signals or a nonpositive marginal SE with the treatment, group, and repetition in the message.

- [ ] **Step 6: Run focused and regression tests**

  Run the helper test plus `test-double_ml_apo.R`, `test-double_ml_apos.R`, and `test-double_ml_framework.R` through `pkgload::load_all()`.

  Expected: PASS with only explicitly asserted rare-support/trimming warnings.

- [ ] **Step 7: Commit**

  ```bash
  git add DESCRIPTION R/helper_apo_group.R tests/testthat/test-apo_group_helpers.R
  git commit -m "feat: build joint group APO framework"
  ```

### Task 2: Public `DoubleMLAPO$gapo()` method

**Files:**
- Modify: `R/double_ml_apo.R`
- Create: `tests/testthat/test-double_ml_apo_gapo.R`

**Interfaces:**
- Consumes: `make_apo_group_framework(list(<treatment label> = self), groups)` from Task 1.
- Produces: `DoubleMLAPO$gapo(groups)` returning a fitted `DoubleMLFramework` with one GAPO per observed group.

- [ ] **Step 1: Write failing GAPO API tests**

  Fit a deterministic APO with two repetitions and fixed external predictions. Assert `gapo(groups)` returns `DoubleMLFramework`; its coefficients, per-repetition coefficients, influence contributions, covariance, and SE equal hand calculations; group order follows Task 1; `summary()`, pointwise `confint()`, `bootstrap()`, joint `confint()`, and `p_adjust()` operate on the returned framework. Assert the APO's coefficients, predictions, models, diagnostics, and splits are unchanged.

  Add clear errors for calling before `fit()`, DML1, cross-fitting disabled, clusters/instruments/custom-score states where constructible, nonfinite fitted signals, invalid groups, singleton groups, and zero marginal SE.

- [ ] **Step 2: Run the GAPO test and verify RED**

  Run `Rscript -e 'pkgload::load_all(".", quiet = TRUE); testthat::test_file("tests/testthat/test-double_ml_apo_gapo.R")'`.

  Expected: FAIL because `gapo()` is absent.

- [ ] **Step 3: Implement `DoubleMLAPO$gapo(groups)`**

  Add a documented public method with signature `gapo = function(groups)`. Require a completed fit, wrap `self` in a treatment-named list, and delegate all construction and validation to `make_apo_group_framework()`. Do not refit, mutate, bootstrap automatically, or add a new result class.

- [ ] **Step 4: Run GAPO and APO regression tests**

  Run `test-double_ml_apo_gapo.R`, all `test-double_ml_apo*.R` files, `test-apo_irm_equivalence.R`, and `test-double_ml_irm*.R`.

  Expected: PASS; existing APO and IRM arrays remain unchanged.

- [ ] **Step 5: Commit**

  ```bash
  git add R/double_ml_apo.R tests/testthat/test-double_ml_apo_gapo.R
  git commit -m "feat: add group-specific APO estimates"
  ```

### Task 3: Public `DoubleMLAPOS$gate()` treatment contrasts

**Files:**
- Modify: `R/double_ml_apos.R`
- Create: `tests/testthat/test-double_ml_apos_gate.R`

**Interfaces:**
- Consumes: Task 1's stacked GAPO framework, APOS treatment order, and existing `DoubleMLFramework$linear_transform()`.
- Produces: `DoubleMLAPOS$gate(groups, reference = NULL, contrast = NULL)` returning one jointly inferred `DoubleMLFramework` of genuine within-group treatment contrasts.

- [ ] **Step 1: Write failing reference-GATE tests**

  With control/chlorine/filter/boiling and three unequal groups, assert `reference = "control"` returns group-major rows `chlorine - control`, `filter - control`, and `boiling - control` for each group. Check coefficients against direct within-group means of contrasted APO signals and check every variance/covariance against the block transform `L = I_H %x% C`. Assert type-safe factor/character/numeric reference matching and stable encoded names.

- [ ] **Step 2: Write failing arbitrary-contrast and validation tests**

  For a named `K` by `J` matrix, assert columns reorder to APOS treatment order, every row must be finite, nonzero, uniquely named, and sum to zero within `sqrt(.Machine$double.eps)`. Add errors for both/neither arguments, invalid reference, wrong width, missing/extra/duplicate columns, duplicate/blank row names, a zero row, and a nonzero-sum row just outside tolerance. Confirm input matrices and APOS state are not mutated.

- [ ] **Step 3: Run the GATE test and verify RED**

  Run `Rscript -e 'pkgload::load_all(".", quiet = TRUE); testthat::test_file("tests/testthat/test-double_ml_apos_gate.R")'`.

  Expected: FAIL because `gate()` is absent.

- [ ] **Step 4: Implement `DoubleMLAPOS$gate()`**

  Add a documented public method with signature `gate = function(groups, reference = NULL, contrast = NULL)`. Require a complete APOS fit; build the untransformed group-major GAPO framework from `self$apo_models`; reuse the type-safe reference and named-matrix rules of `causal_contrast()`; enforce the GATE zero-sum rule; construct `kronecker(diag(H), C)` with explicit group/contrast row names; and return `group_framework$linear_transform(L)`. Do not publish this temporary framework on APOS or refit components.

- [ ] **Step 5: Run APOS, contrast, and framework tests**

  Run `test-double_ml_apos_gate.R`, `test-double_ml_apos.R`, `test-double_ml_apos_contrasts.R`, and `test-double_ml_framework.R`.

  Expected: PASS with covariance-aware SEs and no APOS state changes.

- [ ] **Step 6: Commit**

  ```bash
  git add R/double_ml_apos.R tests/testthat/test-double_ml_apos_gate.R
  git commit -m "feat: add jointly inferred GATE contrasts"
  ```

### Task 4: Algebraic reductions, repetition aggregation, and inference stress tests

**Files:**
- Create: `tests/testthat/test-apo_gate_inference.R`
- Modify only if a demonstrated mismatch requires it: `R/helper_apo_group.R`, `R/double_ml_apo.R`, or `R/double_ml_apos.R`

**Interfaces:**
- Consumes: reviewed `gapo()`, `gate()`, `linear_transform()`, `bootstrap()`, `confint()`, and `p_adjust()` interfaces.
- Produces: regression evidence for the memo's direct-signal equivalence, binary IRM reduction, repeated aggregation, and common-multiplier joint inference.

- [ ] **Step 1: Write deterministic direct-versus-stacked tests**

  For multiple treatments, groups, and two repetitions, compare every `gate()` per-repetition coefficient and influence column to direct within-group means and centered contributions of `psi_b[d] - psi_b[k]`. Check cross-group heterogeneity transforms, singular redundant contrasts, and exact zero off-group covariance blocks.

- [ ] **Step 2: Write repeated-split and bootstrap tests**

  Hand-calculate the coordinate-wise median coefficients and
  `median(n * all_se^2 + (all_coef - coef)^2) / n`. Under a fixed seed, assert one multiplier matrix per repetition is shared across all groups and contrasts, bootstrap dimensions are correct, joint intervals are no narrower than pointwise intervals, and Romano-Wolf uses the transformed correlated statistics.

- [ ] **Step 3: Write the binary IRM group-reduction test**

  Reuse the deterministic matched-nuisance fixture from `test-apo_irm_equivalence.R`. For thresholds zero and 0.05, assert APOS GATE signals, group means, centered group contributions, covariance, SEs, and aggregate estimates equal group means of IRM ATE `psi_b` within `.Machine$double.eps^0.5`. Include a negative control showing independent-treatment covariance would fail.

- [ ] **Step 4: Run the inference test and diagnose any mismatch before editing**

  Run `Rscript -e 'pkgload::load_all(".", quiet = TRUE); testthat::test_file("tests/testthat/test-apo_gate_inference.R")'`.

  Expected: PASS. If it fails, identify whether the defect is prevalence centering, axis/order, covariance scaling, transformation, or repetition aggregation before changing source.

- [ ] **Step 5: Run the complete focused regression set**

  Run `Rscript -e 'pkgload::load_all(".", quiet = TRUE); testthat::test_dir("tests/testthat", filter = "apo|framework|double_ml_irm", reporter = "summary")'`.

  Expected: PASS with only previously documented trimming warnings/skips.

- [ ] **Step 6: Commit**

  ```bash
  git add R/helper_apo_group.R R/double_ml_apo.R R/double_ml_apos.R tests/testthat/test-apo_gate_inference.R
  git commit -m "test: verify APO group-effect inference"
  ```

### Task 5: GATE documentation and package verification

**Files:**
- Modify: `R/double_ml_apo.R`
- Modify: `R/double_ml_apos.R`
- Modify: `DESCRIPTION`
- Generate: `man/DoubleMLAPO.Rd`
- Generate: `man/DoubleMLAPOS.Rd`
- Modify: `vignettes/apo-apos.Rmd`
- Modify: `docs/apo-apos-mathematics.md`
- Modify: `docs/apo-apos-gate-memo.md`

**Interfaces:**
- Consumes: the approved group APIs and all verification evidence from Tasks 1–4.
- Produces: documented GAPO/GATE methods, executable examples, updated mathematical derivation, and a verified source package ready for the separate sensitivity gate.

- [ ] **Step 1: Add source documentation and executable examples**

  Document positional group alignment, accepted label types and ordering, DML2/full-cross-fitting restriction, fixed baseline-group assumption, GAPO versus GATE naming, group-major ordering, support warnings/errors, return-object inference methods, and unsupported inputs. Add fast examples for `apo$gapo(groups)` and `apos$gate(groups, reference = "control")`.

- [ ] **Step 2: Update the vignette and mathematics note**

  Extend the four-label vignette with categorical groups, GAPO output, GATE contrasts, bootstrap, and joint intervals. Replace the pending GATE paragraph in `apo-apos-mathematics.md` with the approved prevalence-scaled influence, covariance, and block-transform formulas. Mark the GATE memo implemented with commit/test references while leaving sensitivity explicitly pending.

- [ ] **Step 3: Regenerate and inspect documentation**

  Run `Rscript -e 'roxygen2::roxygenise()'`.

  Expected: no roxygen warnings; the two Rd pages contain the new methods/examples; NAMESPACE has no unintended change; unrelated roxygen-version churn is reverted after confirming it is mechanical.

- [ ] **Step 4: Run examples, focused tests, full tests, and build**

  Run the two Rd examples, render `vignettes/apo-apos.Rmd`, run the focused group/APO/APOS/framework/IRM tests, then `Rscript -e 'testthat::test_local(reporter = "summary")'` and `R CMD build .`.

  Expected: all tests/examples/vignette pass and the build prints the exact tarball name `DoubleML_1.0.1.9000.tar.gz`.

- [ ] **Step 5: Check the exact source tarball and repository hooks**

  Run `R CMD check --as-cran DoubleML_1.0.1.9000.tar.gz` and `pre-commit run --all-files`.

  Expected: zero check errors/warnings. Report check notes verbatim. If pre-commit remains unavailable, record the exact executable/module error and run the same native lint, style, whitespace, Rd, example, and build checks used for core documentation; do not claim the hook passed.

- [ ] **Step 6: Commit**

  ```bash
  git add DESCRIPTION R/helper_apo_group.R R/double_ml_apo.R R/double_ml_apos.R man/DoubleMLAPO.Rd man/DoubleMLAPOS.Rd vignettes/apo-apos.Rmd tests/testthat
  git add -f docs/apo-apos-mathematics.md docs/apo-apos-gate-memo.md
  git commit -m "docs: document APO group effects"
  ```

  Stop after this commit and report the GATE verification evidence. Resume the parent core plan at its mandatory sensitivity-theory memo; do not implement sensitivity formulas before that memo is reviewed.
