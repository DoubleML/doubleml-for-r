# APO/APOS Core Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (- [ ]) syntax for tracking.

**Goal:** Add nominal multi-valued treatment support through DoubleMLAPO and DoubleMLAPOS, including covariance-aware contrasts and joint inference, then produce the required GATE and sensitivity theory memos before those gated subsystems are planned.

**Architecture:** DoubleMLAPO subclasses the existing scalar DoubleML base and estimates one treatment level with a one-vs-rest propensity task. DoubleMLAPOS coordinates synchronized APO fits and combines their observation-level influence contributions in a reusable DoubleMLFramework; all contrasts and simultaneous inference are transformations of that joint framework. GATE and sensitivity are separate follow-up plans because their formulas must pass the specification's explicit memo gates first.

**Tech Stack:** R, R6, mlr3, data.table, checkmate, testthat, roxygen2, knitr/rmarkdown.

**Spec:** docs/superpowers/specs/2026-09-27-apo-apos-design.md

## Global Constraints

- Treatments are mutually exclusive nominal labels; numeric values never imply order or distance.
- The built-in APO score is Gamma_d = g_d(X) + 1{D=d}(Y-g_d(X))/p_d(X), with psi_d = Gamma_d - theta_d.
- Every nuisance prediction used by a score is out of fold unless explicitly supplied through the validated external-prediction API.
- The initial treatment model is one-vs-rest binary classification; no probability-sum-to-one claim is made.
- Repeated-split aggregation, trimming, normalization, bootstrap methods, confidence levels, and p-value adjustment follow existing DoubleML R conventions.
- APO contrasts use the complete cross-APO covariance; independent-standard-error approximations are forbidden.
- Existing DoubleMLIRM behavior and public APIs must remain unchanged.
- DoubleMLClusterData is rejected with an informative message until joint cluster scaling is implemented.
- GATE code starts only after the GATE memo is reviewed.
- Sensitivity code starts only after the sensitivity memo is reviewed.
- Use the repository's equals-sign assignment style and regenerate NAMESPACE/man files with roxygen2.
- Each implementation task follows red-green-refactor and ends with focused tests plus a small commit.

## Review Focus

- A factor containing unused levels: inferred APOS levels exclude unused categories, while an explicitly requested absent level errors; Task 1 pins both cases.
- NA, NaN, and empty treatment labels: constructors reject them before sample splitting or learner work; Task 1 pins these inputs.
- A training fold with only one one-vs-rest class: APO errors with treatment label, repetition, and fold rather than surfacing an opaque mlr3 failure; Task 5 pins it.
- External predictions with transposed dimensions, missing learner keys, or nonfinite probabilities: APO/APOS reject them before writing fit results; Tasks 4 and 8 pin these cases.
- Failure of one APOS component after earlier components fit: APOS remains observably unfitted and exposes no partial joint framework; Task 8 pins transactional fit behavior.

---

## Execution preflight

These are environment steps, not repository changes. Run them before Task 1 and stop if the untouched baseline still fails.

- [ ] **Step 1: Repair the local R development environment**

Run with network approval:

~~~bash
Rscript -e 'install.packages(c("pkgload", "remotes"), repos = "https://cloud.r-project.org"); remotes::install_deps(dependencies = TRUE, upgrade = "never")'
~~~

Expected: pkgload no longer calls the defunct rlang namespace API, mlr3 and all package Imports are installed, and the command exits 0.

- [ ] **Step 2: Run the untouched baseline suite**

Run:

~~~bash
Rscript -e 'testthat::test_local(reporter = "summary")'
~~~

Expected: exit 0. If it fails, capture the full traceback, identify whether the cause is repository or environment, report it, and do not begin feature code without resolving or explicitly agreeing on the baseline.

### Task 1: Nominal treatment helpers and deterministic fixtures

**Files:**
- Create: R/helper_apo.R
- Create: tests/testthat/helper-18-dml_apo.R
- Create: tests/testthat/test-apo_helpers.R
- Modify: DESCRIPTION (Collate section)

**Interfaces:**
- Consumes: DoubleMLData treatment vectors and existing sample-split lists.
- Produces: get_apo_levels(d, treatment_levels = NULL), make_apo_indicator(d, treatment_level), validate_apo_sample_splitting(smpls, indicator, treatment_level), validate_apo_external_predictions(external_predictions, n_obs, n_rep), and make_apo_data() test fixtures.

- [ ] **Step 1: Write failing label-order and validation tests**

Add tests named test_that("APO labels preserve categorical identity", ...), test_that("APOS inferred levels have stable order", ...), and test_that("invalid APO labels fail early", ...). Assert:

- factor input c("filter", "control") with levels c("control", "filter", "unused") returns c("control", "filter");
- character and numeric input return first-observed unique values;
- explicit levels retain caller order;
- numeric labels c(10, 2, 7) are unchanged labels;
- absent explicit levels, duplicated requested levels, NA, NaN, and empty character labels error with the offending value or condition.

- [ ] **Step 2: Run the helper tests to verify failure**

Run:

~~~bash
Rscript -e 'testthat::test_file("tests/testthat/test-apo_helpers.R")'
~~~

Expected: FAIL because the helper functions do not exist.

- [ ] **Step 3: Implement the label and indicator interfaces**

In R/helper_apo.R implement the five named helpers. make_apo_indicator() returns an integer vector of zeroes and ones using type-preserving equality. validate_apo_sample_splitting() checks every repetition/fold and reports a missing indicator class or missing outcome-training rows as "treatment <label>, repetition <r>, fold <k>".

In tests/testthat/helper-18-dml_apo.R add make_apo_data(n_obs, treatment_levels, seed) returning both a data.frame and DoubleMLData, analytically known conditional means, deterministic splits, and helper functions that calculate APO scores from fixed predictions.

- [ ] **Step 4: Run the helper tests**

Run the Task 1 test command again.

Expected: PASS with no warnings.

- [ ] **Step 5: Commit**

~~~bash
git add DESCRIPTION R/helper_apo.R tests/testthat/helper-18-dml_apo.R tests/testthat/test-apo_helpers.R
git commit -m "feat: add nominal treatment helpers"
~~~

### Task 2: Joint inference framework core

**Files:**
- Create: R/double_ml_framework.R
- Create: tests/testthat/test-double_ml_framework.R
- Modify: DESCRIPTION (Collate section)

**Interfaces:**
- Consumes: all_coef matrix (estimand by repetition), all_se matrix, psi array (observation by estimand by repetition), coefficient names, synchronized sample splits, and apply_cross_fitting.
- Produces: exported DoubleMLFramework R6 class with active bindings coef, se, t_stat, pval, all_coef, all_se, psi, all_cov, boot_coef, and boot_t_stat; methods summary(), print(), linear_transform(contrast, coef_names = NULL), and concat(other).

- [ ] **Step 1: Write failing deterministic aggregation tests**

Create fixed two-estimand, two-repetition arrays. Assert per-repetition covariance equals crossprod(psi[, , r]) / n^2, coef is the coordinate-wise median, and se uses median(n * all_se^2 + (all_coef - coef)^2) / n exactly.

Add tests that constructor dimension mismatches, nonfinite values, duplicated names, and incompatible sample splits error.

- [ ] **Step 2: Run the framework test to verify failure**

Run:

~~~bash
Rscript -e 'testthat::test_file("tests/testthat/test-double_ml_framework.R")'
~~~

Expected: FAIL because DoubleMLFramework is not defined.

- [ ] **Step 3: Implement DoubleMLFramework initialization and aggregation**

Define:

~~~r
DoubleMLFramework$new(
  all_coef,
  all_se,
  psi,
  coef_names,
  smpls = NULL,
  apply_cross_fitting = TRUE
)
~~~

psi is the centered influence contribution, not an unscaled arbitrary score. For no-cross-fitting repetitions, covariance uses the single evaluation test set and its size. linear_transform() accepts a numeric K by J matrix, reorders named columns to coef_names, transforms all_coef and psi, recomputes all_se from transformed per-repetition covariance, and returns a new framework. concat() validates repetition count, observation count, sample splits, and coefficient-name uniqueness before column-binding arrays.

- [ ] **Step 4: Run the framework tests**

Run the Task 2 test command.

Expected: PASS, including hand-calculated off-diagonal covariance and transformed standard errors.

- [ ] **Step 5: Commit**

~~~bash
git add DESCRIPTION R/double_ml_framework.R tests/testthat/test-double_ml_framework.R
git commit -m "feat: add joint inference framework"
~~~

### Task 3: Framework bootstrap and simultaneous inference

**Files:**
- Modify: R/double_ml_framework.R
- Modify: tests/testthat/test-double_ml_framework.R

**Interfaces:**
- Consumes: fitted DoubleMLFramework from Task 2 and existing draw_weights(method, n_rep_boot, n_obs).
- Produces: bootstrap(method = "normal", n_rep_boot = 500), confint(parm, joint = FALSE, level = 0.95), and p_adjust(method = "romano-wolf", return_matrix = TRUE).

- [ ] **Step 1: Write failing common-multiplier tests**

With a fixed seed and correlated two-column psi, assert bootstrap dimensions are J by n_rep_boot * n_rep, both columns use the same multiplier matrix within each repetition, joint intervals use the maximum absolute bootstrap t statistic, and Romano-Wolf stepdown is monotone in ordered absolute t statistics.

Add errors for joint confint before bootstrap, invalid method, invalid level, and unknown parm.

- [ ] **Step 2: Run tests to verify the new methods fail**

Run the Task 2 test file.

Expected: FAIL on missing bootstrap/inference methods.

- [ ] **Step 3: Implement bootstrap and inference methods**

Reuse draw_weights() and match DoubleML method names, method choices, confidence interval labels, and returned matrix columns. Draw weights once per repetition and reuse them for every estimand. Do not delegate to scalar fits or draw per-column multipliers.

- [ ] **Step 4: Run framework tests**

Expected: PASS with deterministic seeded bootstrap assertions.

- [ ] **Step 5: Commit**

~~~bash
git add R/double_ml_framework.R tests/testthat/test-double_ml_framework.R
git commit -m "feat: add joint framework inference"
~~~

### Task 4: Minimal DoubleMLAPO with fixed nuisance predictions

**Files:**
- Create: R/double_ml_apo.R
- Create: tests/testthat/test-double_ml_apo.R
- Modify: DESCRIPTION (Collate section)

**Interfaces:**
- Consumes: DoubleMLData with one treatment column, ml_g, ml_m, one treatment_level, and optional external_predictions = list(ml_g = n by n_rep matrix, ml_m = n by n_rep matrix).
- Produces: exported DoubleMLAPO subclass; constructor from the spec; fit(store_predictions = FALSE, store_models = FALSE, external_predictions = NULL); active bindings treatment_level, trimming_rule, trimming_threshold, normalize_ipw, and propensity_diagnostics.

- [ ] **Step 1: Write failing hand-calculated APO tests**

Use n = 6, fixed y, nominal d, fixed g_hat, and fixed p_hat. Assert:

- psi_b equals g_hat + indicator / p_hat * (y - g_hat);
- psi_a is -1;
- coef equals mean(psi_b);
- psi equals psi_b - coef;
- se equals sqrt(mean(psi^2) / n);
- coef, se, t_stat, and pval are named with the treatment-level display label;
- stored predictions have dimensions n by 1 by 1;
- character, factor, and numeric treatment_level select the same observations as direct equality.

Add external-prediction failures for missing ml_g/ml_m keys, wrong orientation, wrong repetition count, NA/Inf, and probabilities outside [0, 1].

- [ ] **Step 2: Run the APO test to verify failure**

Run:

~~~bash
Rscript -e 'testthat::test_file("tests/testthat/test-double_ml_apo.R")'
~~~

Expected: FAIL because DoubleMLAPO is not defined.

- [ ] **Step 3: Implement constructor, external fit path, and score**

Call super$initialize_double_ml(), require exactly one treatment column, reject instruments and DoubleMLClusterData, validate score == "APO", require regression ml_g and classification ml_m, and initialize parameter keys ml_g and ml_m.

Override fit() only to validate/store external predictions around super$fit(); do not duplicate the base fitting loop. nuisance_est() reads one repetition of validated external predictions when supplied and otherwise leaves the learner path for Task 5. score_elements(y, indicator, g_hat, m_hat) implements the exact APO score.

- [ ] **Step 4: Run APO and regression tests**

Run:

~~~bash
Rscript -e 'testthat::test_file("tests/testthat/test-double_ml_apo.R"); testthat::test_file("tests/testthat/test-double_ml_irm.R")'
~~~

Expected: both files PASS, proving the new class did not change IRM.

- [ ] **Step 5: Commit**

~~~bash
git add DESCRIPTION R/double_ml_apo.R tests/testthat/test-double_ml_apo.R
git commit -m "feat: add fixed-prediction APO estimator"
~~~

### Task 5: Genuine APO cross-fitting and fold diagnostics

**Files:**
- Modify: R/double_ml_apo.R
- Modify: R/helper_apo.R
- Modify: tests/testthat/test-double_ml_apo.R
- Create: tests/testthat/test-double_ml_apo_crossfit.R

**Interfaces:**
- Consumes: APO sample splits and mlr3 learners.
- Produces: nuisance_est(smpls, ...) that trains ml_g only on D = treatment_level rows and ml_m on the full one-vs-rest task, then predicts both only on held-out rows.

- [ ] **Step 1: Write failing cross-fitting and leakage tests**

Add a small custom test learner in helper-18-dml_apo.R that records training row IDs and errors when asked to predict a training row. Assert DoubleMLAPO$fit() succeeds with this learner under two-fold cross-fitting and every stored test prediction is filled exactly once.

Add custom splits where one training fold lacks the requested treatment and where it lacks the complement. Assert the error includes treatment label, repetition, and fold. Add a four-level nominal treatment case where unrequested levels remain in the complement class.

- [ ] **Step 2: Run the cross-fitting test to verify failure**

Run:

~~~bash
Rscript -e 'testthat::test_file("tests/testthat/test-double_ml_apo_crossfit.R")'
~~~

Expected: FAIL because the learner nuisance path is not implemented.

- [ ] **Step 3: Implement the nuisance learner path**

Create the binary indicator in an APO-local data.table copy. Call validate_apo_sample_splitting() before mlr3. Use existing dml_cv_predict() for ml_m with the binary target and for ml_g with treatment-level conditional training indices and original held-out test indices. Keep positive class "1" so extract_prediction() selects prob.1 by name.

Return preds and models under exact keys ml_g and ml_m. Do not mutate self$data$data or self$data$data_model.

- [ ] **Step 4: Run APO cross-fitting and helper tests**

Expected: PASS, with no in-sample predictions and informative fold failures.

- [ ] **Step 5: Commit**

~~~bash
git add R/double_ml_apo.R R/helper_apo.R tests/testthat/helper-18-dml_apo.R tests/testthat/test-double_ml_apo.R tests/testthat/test-double_ml_apo_crossfit.R
git commit -m "feat: cross-fit APO nuisance models"
~~~

### Task 6: Trimming, normalization, storage, parameters, and tuning

**Files:**
- Modify: R/double_ml_apo.R
- Modify: tests/testthat/test-double_ml_apo.R
- Create: tests/testthat/test-double_ml_apo_parameter_passing.R
- Create: tests/testthat/test-double_ml_apo_tuning.R

**Interfaces:**
- Consumes: existing DoubleML prediction/model storage and tuning hooks.
- Produces: exact truncate behavior, normalize_ipw behavior, treatment-specific diagnostics, inherited set_ml_nuisance_params(), and inherited tune() backed by APO nuisance_tuning().

- [ ] **Step 1: Write failing score-convention tests**

With external predictions p = c(0, 0.001, 0.5, 0.999, 1, 0.2), assert truncate clamps to epsilon and 1 - epsilon exactly. For normalize_ipw = TRUE assert w_i = (T_i / p_i) / mean(T / p) and psi_b = g + w * (y - g). Assert diagnostics retain raw minimum, quantiles, fraction trimmed, effective propensity/weight, and fold class counts.


Assert a warning names the treatment level when more than 5 percent of predictions are changed by trimming.
- [ ] **Step 2: Write failing storage and tuning tests**

Follow test-double_ml_irm_parameter_passing.R conventions. Assert global and fold-specific ml_g/ml_m parameters reach cloned learners; store_predictions and store_models use keys ml_g/ml_m; tune() returns both nuisance parameter sets; a reordered binary factor still maps positive-class probability by name rather than column position.

- [ ] **Step 3: Run the three APO test files to verify failure**

Run the Task 4 file plus the two new files.

Expected: FAIL on normalize_ipw, diagnostics, and nuisance_tuning.

- [ ] **Step 4: Implement conventions and tuning**

Validate trimming_rule == "truncate" and threshold in [0, 0.5). Preserve raw m_hat before truncation. Implement nuisance_tuning() using the same full/conditional data split as nuisance_est(). Keep all parameter-list names consistent with ml_g and ml_m.

- [ ] **Step 5: Run focused APO tests**

Expected: all APO helper, estimator, cross-fitting, parameter, and tuning files PASS.

- [ ] **Step 6: Commit**

~~~bash
git add R/double_ml_apo.R tests/testthat/test-double_ml_apo.R tests/testthat/test-double_ml_apo_parameter_passing.R tests/testthat/test-double_ml_apo_tuning.R
git commit -m "feat: complete APO learner integration"
~~~

### Task 7: Mandatory binary IRM equivalence

**Files:**
- Create: tests/testthat/test-apo_irm_equivalence.R
- Modify only if the test exposes a demonstrated convention mismatch: R/double_ml_apo.R or R/helper_apo.R

**Interfaces:**
- Consumes: fixed common sample splits and identical g0, g1, and p predictions.
- Produces: regression evidence that APO(1) - APO(0) equals DoubleMLIRM ATE under matched conventions.

- [ ] **Step 1: Write the deterministic equivalence test**

Construct binary data and two repetitions. Fit DoubleMLIRM and both APO models with identical externally assigned sample splits and cloned deterministic mlr3 learners: the conditional outcome learner must return identical g0/g1 predictions in IRM and the matching APO, and the propensity learner must return p for IRM/APO(1) and 1 - p for APO(0). Store predictions and assert these nuisance arrays are identical before comparing estimands.

Assert APO(1) - APO(0) and the actual DoubleMLIRM ATE agree for per-repetition coefficients, aggregate coefficient, observation-level influence difference, standard error, and covariance-derived contrast within .Machine$double.eps^0.5. Repeat the score comparison with threshold = 0.05.

- [ ] **Step 2: Run the equivalence test**

Run:

~~~bash
Rscript -e 'testthat::test_file("tests/testthat/test-apo_irm_equivalence.R")'
~~~

Expected before any correction: either PASS or a numerical failure that names the exact convention mismatch. Do not change code until the mismatch is traced to score, trimming, repetition aggregation, or scaling.

- [ ] **Step 3: Make the smallest convention correction if required**

Only modify the proven source of mismatch. Do not change DoubleMLIRM.

- [ ] **Step 4: Run equivalence plus all IRM tests**

Run:

~~~bash
Rscript -e 'testthat::test_file("tests/testthat/test-apo_irm_equivalence.R"); testthat::test_dir("tests/testthat", filter = "double_ml_irm", reporter = "summary")'
~~~

Expected: machine-precision equivalence and all existing IRM tests PASS.

- [ ] **Step 5: Commit**

~~~bash
git add R/double_ml_apo.R R/helper_apo.R tests/testthat/test-apo_irm_equivalence.R
git commit -m "test: verify APO reduction to binary IRM"
~~~

### Task 8: DoubleMLAPOS coordinator and joint covariance

**Files:**
- Create: R/double_ml_apos.R
- Create: tests/testthat/test-double_ml_apos.R
- Modify: DESCRIPTION (Collate section)

**Interfaces:**
- Consumes: DoubleMLAPO, DoubleMLFramework, shared smpls, optional nested external_predictions keyed by display label.
- Produces: exported DoubleMLAPOS R6 class; active bindings treatment_levels, apo_models, framework, coef, se, t_stat, pval, predictions, models, smpls; methods set_sample_splitting(), fit(), tune(), summary(), and print().

- [ ] **Step 1: Write failing construction and synchronization tests**

Assert NULL levels infer factor/character/numeric order from Task 1, explicit subsets fit only requested levels, and all APO components receive identical splits. With fixed external predictions for three labels, assert APOS coef equals the three separate APO coefficients and framework all_cov equals the hand-calculated cross-product of the stacked psi matrix divided by n^2.

Assert invalid levels, duplicate display labels, cluster data, and calling summary before fit error. Simulate a failure in the second component and assert framework remains NULL, coef remains unavailable, and no partial APOS fit is published.

- [ ] **Step 2: Run APOS tests to verify failure**

Run:

~~~bash
Rscript -e 'testthat::test_file("tests/testthat/test-double_ml_apos.R")'
~~~

Expected: FAIL because DoubleMLAPOS is not defined.

- [ ] **Step 3: Implement synchronized transactional fitting**

Constructor clones learners for each treatment level and either draws one APO's splits for propagation or waits for set_sample_splitting(). fit() validates all nested external predictions first, fits component APOs into a temporary list, verifies observation/repetition/split identity, builds the joint framework, and publishes state only after every step succeeds.

Delegated inference bindings read from framework. predictions and models are nested first by treatment display label, then ml_g/ml_m. tune() delegates level by level with synchronized folds and returns a named list.

- [ ] **Step 4: Run APO, APOS, and framework tests**

Expected: PASS, including exact off-diagonal covariance and transactional failure behavior.

- [ ] **Step 5: Commit**

~~~bash
git add DESCRIPTION R/double_ml_apos.R tests/testthat/test-double_ml_apos.R
git commit -m "feat: add jointly inferred APOS estimator"
~~~

### Task 9: Reference and arbitrary causal contrasts

**Files:**
- Modify: R/double_ml_apos.R
- Create: tests/testthat/test-double_ml_apos_contrasts.R

**Interfaces:**
- Consumes: fitted APOS framework and either reference or contrast.
- Produces: causal_contrast(reference = NULL, contrast = NULL) returning a transformed DoubleMLFramework; delegated bootstrap(), confint(), and p_adjust() on APOS itself.

- [ ] **Step 1: Write failing reference-contrast tests**

For levels control/chlorine/filter/boiling, assert reference = "control" creates rows chlorine - control, filter - control, boiling - control in APOS order. Assert each coefficient is the direct difference and variance is c' V c including the covariance term.

Add invalid reference, both-arguments-provided, neither-argument-provided, wrong matrix width, duplicate row names, missing named columns, and extra named columns.

- [ ] **Step 2: Write failing joint-inference tests**

With a fixed seed, assert APOS and a contrast framework use common observation-level multipliers, joint intervals contain pointwise intervals or are equal, and Romano-Wolf results use the transformed correlated t statistics.

- [ ] **Step 3: Run contrast tests to verify failure**

Run:

~~~bash
Rscript -e 'testthat::test_file("tests/testthat/test-double_ml_apos_contrasts.R")'
~~~

Expected: FAIL because causal_contrast() is absent.

- [ ] **Step 4: Implement contrasts through linear_transform()**

Build a reference matrix from type-safe label matching. For explicit named matrices, reorder columns to APOS level order; for unnamed matrices, require exact width. Return the framework produced by linear_transform() without re-estimating APOs.

- [ ] **Step 5: Run APOS and framework tests**

Expected: PASS with covariance-aware standard errors and simultaneous inference.

- [ ] **Step 6: Commit**

~~~bash
git add R/double_ml_apos.R tests/testthat/test-double_ml_apos_contrasts.R
git commit -m "feat: add APO causal contrasts"
~~~

### Task 10: Simulation, edge cases, and Python parity artifact

**Files:**
- Create: tests/testthat/test-double_ml_apo_simulation.R
- Create: tests/testthat/test-double_ml_apos_edge_cases.R
- Create: dev/validation/apo_apos_python_parity.R
- Create: dev/validation/apo_apos_python_reference.py
- Modify: .Rbuildignore

**Interfaces:**
- Consumes: completed core public APIs.
- Produces: fast analytical DGP tests, rare-support tests, and a reproducible non-CRAN parity script recording versions, seeds, folds, and fixed predictions.

- [ ] **Step 1: Add fast analytical simulation tests**

Use a three-level multinomial assignment and outcome model Y(d) = alpha_d + beta_d X + epsilon with E[X] = 0, so true APO is alpha_d. With fixed seed and modest n, assert each estimate and contrast is within a precomputed multiple of its reported Monte Carlo standard error rather than a brittle raw tolerance.

- [ ] **Step 2: Add remaining edge-case tests**

Cover four-level character labels, reordered factors, nonordinal numeric labels, rare but valid category, near-zero propensity, requested subsets, repeated splitting, externally supplied splits, deterministic learners, invalid probabilities, and positive-probability column ordering. Keep fold-missing-class errors in the focused cross-fit file.

- [ ] **Step 3: Add the parity script**

The R driver and Python reference script must print their DoubleML versions, use serialized common data/splits/predictions, and compare coefficients, standard errors, psi, covariance, and contrasts. They must fail nonzero on tolerance violations and explain how reference data are regenerated. Add ^dev$ to .Rbuildignore in sorted order.

- [ ] **Step 4: Run fast statistical tests**

Run:

~~~bash
Rscript -e 'testthat::test_file("tests/testthat/test-double_ml_apo_simulation.R"); testthat::test_file("tests/testthat/test-double_ml_apos_edge_cases.R")'
~~~

Expected: PASS reproducibly under repeated seeds used in the test files.

- [ ] **Step 5: Run the parity script when Python DoubleML is available**

Run:

~~~bash
Rscript dev/validation/apo_apos_python_parity.R
~~~

Expected: PASS; if Python dependencies are unavailable, record the exact missing dependency and keep the script reproducible rather than hard-coding undocumented output.

- [ ] **Step 6: Commit**

~~~bash
git add .Rbuildignore tests/testthat/test-double_ml_apo_simulation.R tests/testthat/test-double_ml_apos_edge_cases.R dev/validation/apo_apos_python_parity.R dev/validation/apo_apos_python_reference.py
git commit -m "test: validate APO and APOS statistically"
~~~

### Task 11: Core documentation and package integration

**Files:**
- Modify: R/double_ml_apo.R
- Modify: R/double_ml_apos.R
- Modify: R/double_ml_framework.R
- Modify: DESCRIPTION
- Generate: NAMESPACE
- Generate: man/DoubleMLAPO.Rd
- Generate: man/DoubleMLAPOS.Rd
- Generate: man/DoubleMLFramework.Rd
- Create: vignettes/apo-apos.Rmd
- Create: docs/apo-apos-mathematics.md

**Interfaces:**
- Consumes: final core APIs and verified formulas.
- Produces: roxygen API documentation, nominal-treatment vignette, mathematical derivation through contrasts, and package exports.

- [ ] **Step 1: Add executable roxygen examples and vignette**

Document identification, assumptions, nuisance functions, score, positivity, trimming, one-vs-rest behavior, label ordering, binary IRM relation, covariance, contrasts, bootstrap, clustering limitation, and the pending GATE/sensitivity memo gates.

The vignette uses control/chlorine/filter/boiling labels and demonstrates fit(), summary(), causal_contrast(reference = "control"), bootstrap(), and joint confint(). Examples use fast learners and donttest{} where runtime requires it.

- [ ] **Step 2: Write the developer mathematical note**

Derive APO identification, Neyman orthogonality, per-repetition covariance scaling, linear contrast covariance, repeated-split aggregation, and binary reduction. State that the sensitivity section is completed only by Task 13 after review; do not insert an unproved formula.

- [ ] **Step 3: Regenerate documentation**

Run:

~~~bash
Rscript -e 'roxygen2::roxygenise()'
~~~

Expected: NAMESPACE exports DoubleMLAPO, DoubleMLAPOS, and DoubleMLFramework; three Rd files are generated with no roxygen warnings.

- [ ] **Step 4: Run focused and full core verification**

Run:

~~~bash
Rscript -e 'testthat::test_dir("tests/testthat", filter = "apo|framework", reporter = "summary")'
Rscript -e 'testthat::test_local(reporter = "summary")'
R CMD build .
~~~

Expected: all focused and full tests PASS and a source tarball is built.

- [ ] **Step 5: Run repository quality hooks**

Run:

~~~bash
pre-commit run --all-files
~~~

Expected: all hooks PASS. Apply only mechanical style/documentation changes, rerun affected tests, and inspect generated diffs.

- [ ] **Step 6: Commit**

~~~bash
git add DESCRIPTION NAMESPACE R/double_ml_apo.R R/double_ml_apos.R R/double_ml_framework.R man/DoubleMLAPO.Rd man/DoubleMLAPOS.Rd man/DoubleMLFramework.Rd vignettes/apo-apos.Rmd
git add -f docs/apo-apos-mathematics.md
git commit -m "docs: document APO and APOS inference"
~~~

### Task 12: Required GATE theory memo and follow-up plan gate

**Files:**
- Create: docs/apo-apos-gate-memo.md
- Create after review: docs/superpowers/plans/YYYY-MM-DD-apo-apos-gate.md

**Interfaces:**
- Consumes: fitted APO/APOS influence arrays and any existing R group-effect infrastructure found at execution time.
- Produces: the requested four-part memo and, only after user review, a separate TDD implementation plan for group-specific APOs and true GATE contrasts.

- [ ] **Step 1: Audit current R and current official Python group-effect code**

Record whether either implementation operates on orthogonal signals, the exact regression/averaging convention, covariance scaling, and bootstrap behavior. Use official source only for Python behavior.

- [ ] **Step 2: Derive the proposed group influence representation**

For each categorical group g, derive theta_d(g), its prevalence scaling, and tau_dk(g). Show whether simple group means of Gamma_hat are equivalent to the package convention and how common-multiplier covariance is formed across groups and treatments.

- [ ] **Step 3: Write and verify the memo**

The memo answers every item in specification section 26, distinguishes GAPO from GATE, selects the public return object, and lists unsupported group inputs. Check it contains no unresolved markers and that every formula defines its normalization.

- [ ] **Step 4: Commit and stop for review**

~~~bash
git add -f docs/apo-apos-gate-memo.md
git commit -m "docs: derive APO group effects"
~~~

Do not write GATE implementation code. Present the memo to the user. After approval, use superpowers:writing-plans to create the separate GATE plan and obtain execution approval.

### Task 13: Required sensitivity theory memo and follow-up plan gate

**Files:**
- Create: docs/apo-apos-sensitivity-memo.md
- Modify after approved implementation: docs/apo-apos-mathematics.md
- Create after review: docs/superpowers/plans/YYYY-MM-DD-apo-apos-sensitivity.md

**Interfaces:**
- Consumes: current DoubleML sensitivity theory/source, APO residual and Riesz-representer components, and the APOS joint influence structure.
- Produces: the requested seven-part sensitivity memo and, only after user review, a separate TDD implementation plan for the theoretically justified subset.

- [ ] **Step 1: Reconstruct the current IRM sensitivity formulas**

Cite the exact official theory/source and define residual outcome variation, treatment-assignment/Riesz variation, confounding strengths, rho, bounds, and robustness values with their sample analogues and scaling.

- [ ] **Step 2: Derive the single-APO mapping**

For T_d = 1{D=d}, prove or reject each IRM-to-APO transfer. State precisely whether the target is E[Y(d)], which observed regression and Riesz representer are used, and what positivity is required against the all-other-level complement.

- [ ] **Step 3: Derive or delimit contrast sensitivity**

For theta_j - theta_k, identify the cross-counterfactual confounding quantities needed for exact bounds. If the current model does not encode them, specify the marginal analyses that remain valid and the exact error for unsupported joint contrast bounds.

- [ ] **Step 4: Validate binary reductions on paper and with deterministic quantities**

Show which APO/APOS sensitivity elements reduce to IRM quantities for D in {0, 1}; list the numerical tests a follow-up implementation must contain.

- [ ] **Step 5: Write and verify the memo**

Answer all seven questions in specification section 25, list assumptions beyond standard APO identification, separate implemented recommendations from intentional non-support, and scan for undefined symbols or unresolved markers.

- [ ] **Step 6: Commit and stop for review**

~~~bash
git add -f docs/apo-apos-sensitivity-memo.md
git commit -m "docs: derive APO sensitivity analysis"
~~~

Do not write sensitivity implementation code. Present the memo to the user. After approval, use superpowers:writing-plans to create the separate sensitivity plan and obtain execution approval.

### Task 14: Final integration after the gated subplans

**Files:**
- Modify: vignettes/apo-apos.Rmd
- Modify: docs/apo-apos-mathematics.md
- Modify generated documentation from the approved GATE and sensitivity plans
- Modify: NEWS.md if present at execution time

**Interfaces:**
- Consumes: completed core, GATE, and sensitivity branches/tasks.
- Produces: final deliverables, package-wide verification evidence, limitations list, and upstream-ready history.

- [ ] **Step 1: Complete examples and mathematical documentation**

Add the approved GATE and sensitivity APIs to the nominal-treatment vignette and replace the memo-gate statements in the mathematical note with the exact implemented formulas and explicit unsupported cases.

- [ ] **Step 2: Regenerate documentation and run the full suite**

Run:

~~~bash
Rscript -e 'roxygen2::roxygenise()'
Rscript -e 'testthat::test_local(reporter = "summary")'
R CMD build .
R CMD check --as-cran DoubleML_*.tar.gz
pre-commit run --all-files
~~~

Expected: zero test failures; R CMD check has zero errors and zero warnings; every pre-commit hook passes. Notes must be reported verbatim and assessed before completion.

- [ ] **Step 3: Audit the final diff against the specification**

Verify every requested edge case, binary equivalence, joint covariance, contrast, GATE, sensitivity reduction, example, export, and limitation has a named test or documentation location. Confirm no unrelated IRM diff and no generated file is stale.

- [ ] **Step 4: Commit final integration**

~~~bash
git add DESCRIPTION NAMESPACE R man tests vignettes
git add -f docs/apo-apos-mathematics.md docs/apo-apos-gate-memo.md docs/apo-apos-sensitivity-memo.md
git commit -m "feat: complete APO and APOS support"
~~~

- [ ] **Step 5: Prepare the final handoff**

Report files added/modified, architecture, mathematics, API examples, focused/full test counts, R CMD check result, known limitations, Python differences, and suggested follow-up PRs. Do not push or open a PR unless separately authorized.
