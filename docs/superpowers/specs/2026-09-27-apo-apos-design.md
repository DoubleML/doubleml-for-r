# DoubleML APO/APOS design specification

Date: 2026-09-27
Branch: feature/apo-apos
Status: approved architecture, implementation pending

## Objective

Add production-quality estimation of average potential outcomes (APOs) for nominal, mutually exclusive, discrete treatments to DoubleML for R. The implementation will provide a single-level DoubleMLAPO estimator, a jointly inferred DoubleMLAPOS collection, covariance-aware linear contrasts, group effects, and only sensitivity functionality that is justified by an explicit theoretical derivation.

The design preserves existing DoubleML conventions for R6 models, cross-fitting, repeated sample splitting, learner cloning, trimming, inference, bootstrap, documentation, and testing. Numeric treatment values are labels; no ordering, spacing, or dose-response interpretation is inferred.

## Repository findings that shape the design

The existing DoubleML base class already supplies repeated sample splitting, fold iteration, score aggregation, scalar variance estimation, multiplier bootstrap, confidence intervals, and adjusted p-values. DoubleMLIRM supplies the closest nuisance-estimation pattern and the current trimming and inverse-probability-weight normalization behavior.

DoubleMLData counts treatment columns, not levels. A single nominal treatment column therefore still has n_treat equal to one, which lets DoubleMLAPO reuse the scalar base-model lifecycle while constructing a treatment-level indicator internally.

The current learner helpers are designed around binary classification probabilities and select a probability for class 1. There is no stable multiclass probability-column mapping abstraction in the R package. The first upstream-quality implementation will therefore use one-vs-rest propensity learners, with explicit binary factor levels and validation. A future shared multiclass nuisance adapter can be added without changing the APO/APOS public estimands.

The R package currently has no equivalent of Python DoubleMLFramework, no generic GATE/CATE result layer, and no reusable sensitivity implementation that can simply be attached to a new model. Those gaps require narrowly scoped new abstractions rather than unrelated changes to IRM.

## Statistical contract

For treatment labels d in a finite observed set, define

theta_d = E[Y(d)],
g_d(x) = E[Y | D = d, X],
p_d(x) = P(D = d | X).

Under consistency, conditional unconfoundedness, and positivity, the orthogonal signal is

Gamma_d(W; eta) =
  g_d(X) + 1{D = d} / p_d(X) * (Y - g_d(X)),

and the score and influence contribution are

psi_d(W; theta_d, eta) = Gamma_d(W; eta) - theta_d.

For one sample-splitting repetition r,

theta_hat_d,r = n^(-1) sum_i Gamma_hat_i,d,r

and the estimated covariance of the APO vector is

V_hat_r = n^(-2) sum_i psi_hat_i,r psi_hat_i,r'.

This scaling is the covariance of the estimator, not the covariance of an individual influence contribution. The implementation will keep raw observation-level score arrays so scalar, joint, and contrast inference share the same source.

For a K by J contrast matrix C,

tau_hat_r = C theta_hat_r,
psi_hat_tau,r = psi_hat_r C',
V_hat_tau,r = C V_hat_r C'.

No contrast standard error will be formed by assuming APO independence.

Repeated-splitting point estimates and uncertainty will follow the existing DoubleML median aggregation convention. The joint framework will apply that convention coordinate-wise while preserving each repetition's full covariance and common bootstrap draw. This yields scalar results identical to the base convention and covariance-aware results for every linear transformation.

## Public model design

### DoubleMLAPO

DoubleMLAPO will be an R6 subclass of DoubleML. Its constructor will follow existing model signatures and add:

~~~r
DoubleMLAPO$new(
  data,
  ml_g,
  ml_m,
  treatment_level,
  n_folds = 5,
  n_rep = 1,
  score = "APO",
  dml_procedure = "dml2",
  trimming_rule = "truncate",
  trimming_threshold = 0.01,
  normalize_ipw = FALSE,
  draw_sample_splitting = TRUE
)
~~~

The initial score set contains only the built-in "APO" score. A callable custom score will not be exposed until the package has a vector-score contract that can validate it safely.

The model accepts a DoubleMLData object with exactly one treatment column. It retains the original treatment labels and creates T_d = 1{D = d} only within nuisance training, scoring, and validation. It does not mutate the user's DoubleMLData object.

The outcome learner ml_g is trained on training observations with D = d and predicts g_d for every held-out observation. The propensity learner ml_m is trained on all training observations with the binary response T_d and predicts p_d out of fold. A fold that lacks either indicator class, or lacks treated observations needed for ml_g, fails with an error that names the repetition, fold, and treatment label.

The class will reuse the base class for sample splitting, repeated fitting, coefficient aggregation, scalar inference, bootstrap, confidence intervals, and p-value adjustment. It will expose treatment_level, score elements, nuisance predictions, fitted learners when requested, and treatment-specific propensity diagnostics.

The fit method will retain the existing arguments and accept an APO-scoped external_predictions argument. External predictions must have the package's observation-by-repetition shape, be finite on every test observation, and use explicit ml_g and ml_m keys. Propensity predictions must lie in [0, 1] before trimming. This narrow feature supports deterministic validation and binary IRM equivalence without changing unrelated model APIs.

Trimming and normalize_ipw will call shared helpers already used by DoubleML estimators. The score will use the resulting weights exactly once. Diagnostics will retain both untrimmed propensity predictions and the effective score propensity or weight so support problems remain visible.

### DoubleMLAPOS

DoubleMLAPOS will be a standalone R6 coordinator rather than a subclass pretending that treatment levels are treatment columns. It will own one DoubleMLAPO instance per requested label and a joint DoubleMLFramework object.

Its constructor will mirror APO nuisance and resampling arguments and add treatment_levels. If treatment_levels is NULL, labels are selected as follows:

- factors use their observed factor-level order, after dropping unused levels;
- character, logical, and numeric vectors use first-observed order;
- supplied treatment_levels preserve the caller's exact order.

Every requested label must occur in the data and labels must be unique after type-preserving matching. Numeric values remain categorical labels.

APOS draws or accepts one sample-splitting object, assigns the same splits to every component APO, and then fits all components. Each level currently receives a cloned one-vs-rest propensity learner. The resulting probabilities are individually valid Bernoulli probabilities but are not forced to sum to one. This is the deliberate compatibility choice for the present mlr3 adapter.

The object will expose package-consistent coef, se, summary(), confint(), bootstrap(), p_adjust(), predictions, learners, sample splits, and score arrays. Output names are stable display labels while an internal index preserves type-safe label identity.

APOS must not behave as a loose list of fits. fit() is successful only when all component APOs have synchronized repetitions and observation order and the joint framework has been constructed. Partial fits are not published as a valid APOS result.

## Joint inference abstraction

A lightweight internal/public DoubleMLFramework R6 class will hold inference results independently of nuisance fitting:

- treatment or estimand names;
- all_thetas with dimensions estimand by repetition;
- all_ses with dimensions estimand by repetition;
- psi with dimensions observation by estimand by repetition;
- per-repetition covariance matrices;
- aggregated coefficients and standard errors;
- bootstrap draws and critical values;
- optional sensitivity elements.

The framework will validate dimensions and finiteness on creation. It will provide:

- concatenate compatible scalar frameworks along the estimand dimension;
- transform by an arbitrary numeric contrast matrix;
- pointwise confidence intervals and p-values;
- common-multiplier joint bootstrap;
- simultaneous confidence intervals and multiplicity adjustment.

All estimands in a concatenated or transformed framework must share observation order, repetitions, and sample splitting. Clustered frameworks are not concatenated in the initial implementation because the current R architecture does not provide the cluster metadata and scaling contract needed to prove correctness.

Bootstrap multipliers are drawn once per observation and repetition and applied to all APO score columns. Reusing these multipliers is essential for joint inference.

## Causal contrasts

DoubleMLAPOS$causal_contrast() will support a named reference label and an explicit contrast matrix.

A reference contrast creates rows e_j - e_reference in APOS display order and excludes the zero reference row. An explicit contrast matrix must have one column per APO; named columns are reordered and validated against treatment labels, while unnamed columns must already match APOS order.

The method returns a framework-backed contrast result with coefficients, standard errors, t statistics, p-values, confidence intervals, bootstrap, and simultaneous inference. Pairwise and general linear contrasts use transformed observation-level scores and therefore include all cross-APO covariance.

For binary treatment with matched nuisance predictions, splits, trimming, and normalization,

APO(1) - APO(0) = ATE

must agree with DoubleMLIRM at machine precision. A deterministic external-prediction test is the primary regression test for this identity.

## Propensity and label handling

One-vs-rest is selected for the first implementation because the current binary prediction helper has an explicit and testable class-1 contract, whereas a multiclass matrix adapter would require broader learner-interface changes.

A dedicated label helper will:

- compare original atomic values without numeric ordering semantics;
- construct a binary factor with levels c("0", "1");
- retain the original label for messages and output;
- reject missing treatment values;
- detect labels absent globally or within training folds;
- avoid relying on factor alphabetic order.

A dedicated probability extractor will validate the positive-class column by learner prediction metadata rather than positional assumptions. If a learner omits a class probability or returns only hard responses, fitting fails before scoring.

The architecture will leave a clear internal interface for a future multiclass propensity provider returning an n by J matrix plus an explicit label-to-column map. APOS will consume that interface only after tests cover reordered factor levels, missing fold classes, and arbitrary probability-column ordering.

## Sample splitting and leakage prevention

All nuisance values entering Gamma are predictions for held-out indices from the corresponding fold. The implementation will reuse dml_cv_predict where possible and add APO-specific training-row selection for g_d.

Tests will use memorizing or index-aware learners to fail if any held-out observation is used for its own nuisance fit. External sample splits will be validated for complete, non-overlapping test coverage per repetition and then propagated unchanged to every APO component.

## Positivity, trimming, and diagnostics

The supported trimming_rule initially matches the existing DoubleML truncation semantics. Raw p_d predictions are checked for finiteness and [0, 1] bounds, then the existing trimming helper is applied with trimming_threshold.

Treatment-specific diagnostics will report at least the raw minimum, selected quantiles, the fraction affected by trimming, and fold-level class counts. Severe support problems are not silently reclassified as successful: degenerate folds are errors, while heavy but nondegenerate trimming produces an explicit warning and inspectable diagnostics.

normalize_ipw follows the package's current convention for normalizing inverse-probability weights within the relevant repetition. Tests will compare the exact weights and score against hand calculations.

## Group effects checkpoint and design

Stage 5 is gated by the requested GATE memo. Before code is added, the memo will verify the available R infrastructure, the pseudo-outcome mapping, and covariance scaling.

The intended estimands are group-specific APOs

theta_d(g) = E[Y(d) | G = g]

and genuine GATE contrasts

tau_dk(g) = theta_d(g) - theta_k(g).

A group-specific APO will be named as such and never presented as a treatment effect. The implementation will start from cross-fitted Gamma_hat_i,d. For mutually exclusive categorical groups, group-specific APOs are group means of the orthogonal signal, with influence contributions scaled by group prevalence. Contrasts are formed by applying linear transformations to the stacked group-by-treatment influence representation.

The public surface will separate DoubleMLAPO$gapo(groups) from DoubleMLAPOS$gate(groups, reference or contrast). Both return framework-backed inference results. Groups must align row-for-row with the estimation sample, contain no missing values, and have adequate observations in every requested level. Standard errors, confidence intervals, and joint bootstrap use the group-specific influence matrix and common multipliers.

If the required memo shows that an existing package abstraction should own this result instead, the implementation may reuse that abstraction while preserving these names, estimands, and inference guarantees.

## Sensitivity checkpoint and design boundary

Stage 6 is gated by the requested theoretical sensitivity memo and a user-visible review point. No binary IRM formula will be copied merely because T_d is binary.

Before implementation, the memo will derive the current IRM sensitivity quantities, identify which outcome-residual and Riesz-representer terms map to a single APO, state whether one-vs-rest assignment is sufficient for the target theta_d, and distinguish bounds for one APO from bounds for a correlated APO contrast.

DoubleMLAPO will store the nuisance residuals and score components needed for any justified APO sensitivity calculation from the beginning, but no public sensitivity method will be enabled until the derivation is established.

For APOS contrasts, exact joint sensitivity bounds will be implemented only if the omitted-confounding model specifies the necessary cross-APO dependence. Otherwise the package will expose supported marginal APO analyses and clearly reject unsupported exact contrast bounds. It will not combine marginal worst cases and label them exact.

Binary reductions will be tested against IRM sensitivity quantities only where the memo proves that the estimands and conventions coincide.

## Clustering

The present DoubleML R base can compute model-specific scalar inference, but it does not expose a generic, concatenable cluster influence contract for an APOS collection. Initial APO/APOS support will therefore reject clustered DoubleMLData with an informative limitation instead of silently applying iid covariance formulas. Cluster support is a separate extension once the framework carries cluster identifiers, effective sample sizes, and cluster bootstrap semantics.

## Error handling

Errors will be early and specific for:

- more than one treatment column;
- missing treatment values or outcomes;
- unknown, duplicated, or empty treatment_levels;
- invalid reference labels or contrast dimensions;
- absent treatment classes in a training fold;
- no outcome-training rows for a requested level;
- learners with incompatible task or prediction types;
- missing, nonfinite, or out-of-range probabilities;
- external prediction shape or naming mismatches;
- incompatible sample splits across APOs;
- calls to inference, contrasts, group effects, or sensitivity before fit;
- unsupported clustered data or unsupported sensitivity targets.

Warnings will identify treatment labels and repetition/fold indices when trimming or support is concerning.

## Files and integration points

Expected new files:

- R/double_ml_framework.R
- R/double_ml_apo.R
- R/double_ml_apos.R
- tests/testthat/test_double_ml_framework.R
- tests/testthat/test_double_ml_apo.R
- tests/testthat/test_double_ml_apos.R
- tests/testthat/test_apo_apos_binary_equivalence.R
- tests/testthat/test_apo_apos_group_effects.R
- tests/testthat/test_apo_apos_sensitivity.R
- vignettes/apo-apos.Rmd
- developer-facing APO/APOS mathematical note in the repository documentation area

Expected targeted modifications:

- DESCRIPTION Collate order and any vignette dependency declarations;
- NAMESPACE through roxygen generation;
- R/helper.R for reusable type-safe label and probability validation helpers;
- narrowly scoped base-class hooks only when required for framework export or external predictions;
- package-level documentation and navigation;
- test helpers for deterministic nominal-treatment data.

No IRM behavior will change. Shared helper extraction must retain existing IRM tests unchanged.

## Test strategy

Fast deterministic unit tests will cover:

- hand-calculated APO scores, coefficients, standard errors, and normalized weights;
- framework covariance and arbitrary linear transformations;
- common-multiplier bootstrap shape and covariance preservation;
- binary APO contrast equivalence to IRM with identical external predictions;
- character, factor, reordered factor, and nonordinal numeric labels;
- inferred and explicitly ordered treatment levels;
- three- and four-level nominal treatments;
- repeated and externally supplied sample splits;
- leakage-detecting learners;
- rare categories and a treatment class missing from a training fold;
- near-zero propensities and exact trimming behavior;
- invalid probability predictions and reordered probability columns;
- omitted unrequested categories;
- invalid references and contrast matrices;
- pre-fit method errors;
- group-specific APO versus GATE naming and hand calculations;
- all sensitivity reductions justified by the memo.

Simulation tests will use analytically known APOs under multinomial assignment and heterogeneous outcome regressions. The default suite will keep sample size and repetitions small enough for CI; larger coverage experiments and Python parity checks will live in a reproducible development validation script or a repository-appropriate slow-test context.

Python parity validation will record the Python DoubleML version, random seeds, sample splits, nuisance predictions, and generated reference artifacts. It will compare coefficients, score components, standard errors, covariance matrices, and contrasts without treating undocumented hard-coded numbers as authoritative.

Package validation at each stage includes focused tests, the full testthat suite, R CMD check, documentation regeneration checks, and repository lint/style hooks where available.

## Implementation stages and gates

1. Implement the framework primitives and minimal DoubleMLAPO with deterministic score tests.
2. Complete APO inference, repeated splitting, external predictions, and binary IRM equivalence.
3. Implement DoubleMLAPOS synchronization, joint covariance, and nominal-label tests.
4. Implement linear/reference contrasts and common-multiplier simultaneous inference.
5. Stop for the required GATE memo, then implement group-specific APOs and GATEs if the derivation confirms the design.
6. Stop for the required sensitivity memo and user review, then implement only the justified subset.
7. Complete roxygen documentation, mathematical note, nominal-treatment vignette, examples, and Python parity artifact.
8. Run full checks, resolve all introduced failures, and document environmental or upstream limitations separately.

Each stage ends with its focused tests passing before the next stage starts. Failures in the untouched baseline are documented independently and are not hidden by feature changes.

## Baseline environment note

At worktree creation, the repository was clean on branch feature/apo-apos. The local baseline test command fails before loading package tests because installed pkgload 1.5.0 calls rlang::ns_registry_env(), which is defunct in installed rlang 1.3.0. The required mlr3 package is also not installed. These are environment/dependency issues, not repository failures. Dependency repair will be handled explicitly before feature tests are used as acceptance evidence.

## Accepted tradeoffs

The selected architecture favors correctness and compatibility over a prematurely broad learner adapter:

- one-vs-rest propensity models now, with a tested multiclass extension point;
- a small reusable inference framework instead of duplicating covariance logic in APOS;
- exact covariance-aware contrasts rather than independent-fit approximations;
- explicit rejection of clustering until its joint scaling is supported;
- GATE and sensitivity checkpoints before implementation;
- no claim of exact contrast sensitivity bounds without a joint confounding model.

This keeps the contribution reviewable, leaves IRM behavior unchanged, and supports future multi-estimand DoubleML models beyond APO/APOS without forcing them into the meaning of treatment columns.
