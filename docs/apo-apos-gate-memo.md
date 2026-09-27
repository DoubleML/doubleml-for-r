# APO group outcomes and GATE: theory and API review memo

Date: 2026-09-27. Status: proposed group-effect contract for user review;
no group-effect implementation or follow-up implementation plan is included.
R audit base: `bc2cf570fb4231168bb67e9e8321b1451602e9a1`.

This four-part memo answers original specification section 26 and the
categorical-group requirements in section 10. Its notation extends
[the committed core mathematical note](apo-apos-mathematics.md).
The recommendation is to calculate group means of fitted APO signals, stack
their prevalence-scaled centered contributions, and return the existing
`DoubleMLFramework`. A group average potential outcome (GAPO) is an outcome
level. Only a treatment contrast within a group is a GATE.

## 1. Does existing GATE machinery operate on orthogonal signals?

### R checkout audit

At the audit base, searches of `R/` find no `gate()`, `gapo()`, `cate()` or
`DoubleMLBLP` implementation. In particular, `DoubleMLIRM` has no post-fit
group-effect method. The older design's statement that R has no framework
described the pre-feature baseline: this checkout now has the reviewed
`DoubleMLFramework` and it is the reusable inference infrastructure.

The audited local sources are:

- [`R/double_ml_irm.R`](../R/double_ml_irm.R), `score_elements()`: for ATE,
  `psi_b` is the augmented treatment-effect signal and `psi_a` is minus one.
  ATTE uses a different derivative and target and is outside this mapping.
- [`R/double_ml_apo.R`](../R/double_ml_apo.R), `score_elements()`: `psi_b`
  retains the fitted APO augmented outcome, including its existing truncation
  and optional IPW normalization; `psi_a` is minus one.
- [`R/double_ml.R`](../R/double_ml.R): public `psi_b`, `psi`, `all_coef`,
  `all_se`, `smpls`, `apply_cross_fitting`, and `dml_procedure`; coefficient
  estimation, score calculation, variance and repetition aggregation.
- [`R/double_ml_apos.R`](../R/double_ml_apos.R): synchronized component
  `apo_models`, transactional `fit()`, `framework`, `causal_contrast()` and
  delegated inference. APO score axes are observation, repetition, treatment;
  APOS permutes them to observation, estimand, repetition.
- [`R/double_ml_framework.R`](../R/double_ml_framework.R): constructor
  `new(all_coef, all_se, psi, coef_names, smpls, apply_cross_fitting)`,
  `concat()`, `linear_transform()`, covariance, bootstrap, intervals and
  adjusted p-values. Its field is `all_coef`, not `all_thetas`.
- [`R/helper.R`](../R/helper.R), `draw_weights()`: normal, centered exponential
  (`Bayes`), and wild multipliers, each with mean zero and variance one.

These relative links refer to the specified R audit commit, not a claim about
future revisions. Group inference can reuse the framework, but cannot call
a nonexistent R BLP or group method.

### Current official Python audit

Official upstream refs were resolved on 2026-09-27. The latest release API
reports [0.11.4](https://github.com/DoubleML/doubleml-for-py/releases/tag/0.11.4),
published 2026-08-10, at commit
`96e32c9517812a8b8875dd74956704d7998f6510`. Current `main` resolves to
`6884b5f9816f180324d854a4d81b22e9d334f5e0` (2026-09-23). The audited behavior
below is present in both; release links are immutable source references.

- [IRM source](https://github.com/DoubleML/doubleml-for-py/blob/96e32c9517812a8b8875dd74956704d7998f6510/doubleml/irm/irm.py):
  `gate()` passes a dummy group basis to `cate(..., is_gate=True)`;
  `cate()` requires score ATE and passes `psi_elements["psi_b"]`, with the
  singleton treatment axis removed, to `DoubleMLBLP`. It uses the orthogonal
  signal, not the globally centered score. For ordinary unweighted ATE with
  matched propensity processing, this signal is the difference of the two
  APO signals.
- [APO source](https://github.com/DoubleML/doubleml-for-py/blob/96e32c9517812a8b8875dd74956704d7998f6510/doubleml/irm/apo.py):
  `gapo()` calls `capo(..., is_gate=True)`, which likewise passes APO `psi_b`
  to BLP. Thus upstream explicitly distinguishes GAPO from GATE.
- [PLR source](https://github.com/DoubleML/doubleml-for-py/blob/96e32c9517812a8b8875dd74956704d7998f6510/doubleml/plm/plr.py):
  its group route instead regresses a partialled-out outcome on group dummies
  multiplied by residualized treatment in each repetition. Its coefficient
  for a disjoint group is the sum of residualized-treatment times outcome
  divided by the sum of squared residualized treatment in that group.
  This is a different orthogonal estimating equation, not the group mean of
  an IRM/APO augmented signal. The direct plug-in claim concerns IRM/APO;
  substituting APO signals into PLR's residual-weighted basis is unjustified.
- [BLP source](https://github.com/DoubleML/doubleml-for-py/blob/96e32c9517812a8b8875dd74956704d7998f6510/doubleml/utils/blp.py):
  `fit(cov_type="HC0")` runs `statsmodels.OLS(signal, basis).fit()` per
  repetition, without inserting an intercept; it stores `cov_params()` as
  `blp_omega`. Signals may have one or multiple repetitions. The basis may
  be shared or supplied per repetition. Alternative covariance types are
  forwarded; the equivalence derived below concerns default HC0 only.
- [APOS source](https://github.com/DoubleML/doubleml-for-py/blob/96e32c9517812a8b8875dd74956704d7998f6510/doubleml/irm/apos.py):
  there is `causal_contrast()` but no public APOS `gate()` or `gapo()`.
  Separate APO BLP objects do not retain their joint cross-treatment score
  covariance as one result.
- [Repetition helper](https://github.com/DoubleML/doubleml-for-py/blob/96e32c9517812a8b8875dd74956704d7998f6510/doubleml/utils/_estimation.py#L343-L354):
  Python aggregates coefficients by medians, and computes the reported SE as
  `[median_r(coef_r + 1.96 se_r) - median_r(coef_r)] / 1.96`.
  This differs from the reviewed R rule derived in part 4.

The corresponding current-development files were also inspected:
[IRM](https://github.com/DoubleML/doubleml-for-py/blob/6884b5f9816f180324d854a4d81b22e9d334f5e0/doubleml/irm/irm.py),
[APO](https://github.com/DoubleML/doubleml-for-py/blob/6884b5f9816f180324d854a4d81b22e9d334f5e0/doubleml/irm/apo.py),
[APOS](https://github.com/DoubleML/doubleml-for-py/blob/6884b5f9816f180324d854a4d81b22e9d334f5e0/doubleml/irm/apos.py),
[BLP](https://github.com/DoubleML/doubleml-for-py/blob/6884b5f9816f180324d854a4d81b22e9d334f5e0/doubleml/utils/blp.py), and
[aggregation](https://github.com/DoubleML/doubleml-for-py/blob/6884b5f9816f180324d854a4d81b22e9d334f5e0/doubleml/utils/_estimation.py#L344-L355).

Python's group wrappers require a pandas DataFrame, accepting boolean/integer
dummy columns or converting a single other-type column with `get_dummies()`.
They warn for a group count at most five. The wrappers do not themselves
validate mutual exclusivity, exhaustive coverage or all integer values being
zero/one. Passing overlapping indicators to OLS generally estimates projection
coefficients, not marginal group means. Its documentation asks for mutually
exclusive groups; permissive implementation is not a basis for broader R
causal claims. The official
[heterogeneity guide](https://docs.doubleml.org/stable/guide/heterogeneity.html#group-average-treatment-effects-gates)
describes this dummy-basis BLP approach and joint Gaussian inference.

Python BLP has no separate observation-multiplier `bootstrap()` method. For
`confint(joint=True)`, it simulates coefficient perturbations with covariance
`blp_omega` using its matrix square root and standard normal draws. It forms
the maximum absolute standardized prediction across requested basis rows,
takes a quantile per repetition, then medians of lower/upper interval
endpoints across repetitions. For groups the default prediction basis is
the identity matrix. There is no further sample-size divisor because
`blp_omega` already estimates coefficient covariance. Release 0.11.4 corrects
the maximum/quantile axes. This is distributionally equivalent to a normal
observation-multiplier bootstrap with the same HC0 covariance for one
repetition and the same estimand family, not necessarily identical random
draws or repeated-split intervals.

## 2. Can APO pseudo-outcomes plug into that machinery?

### Identification and orthogonality within groups

Use independent observations $W_i=(Y_i,D_i,X_i)$, $i=1,\ldots,N$, requested
treatment labels $d=1,\ldots,J$ understood as indices of nominal labels, and
a fixed finite categorical partition $G=h(X)$ with groups $g=1,\ldots,H$.
The labels themselves carry no numeric order or spacing. Groups must be
defined by baseline information included in $X$ (or measurable from $X$).
An additional baseline variable can be included in $X$ before fitting;
global identification given an $X$ that omits it does not automatically
justify subgroup orthogonality. Groups selected using estimation outcomes,
treatment realization or in-sample fitted effects require a separate theory
and are unsupported. Externally learned group rules may be used when fixed
independently of the estimation sample and measurable from its baseline $X$.

Let $S_{ig}=1\{G_i=g\}$, $q_g=E[S_g]>0$,
$T_{id}=1\{D_i=d\}$, and retain $g_d(X)$ and $p_d(X)$ from the core note.
Consistency, conditional exchangeability and positivity within the requested
groups identify

$$
\theta_d(g)=E[Y(d)\mid G=g]
=\frac{E[S_g g_d(X)]}{q_g}
=\frac{E[S_g\Gamma_d(W;\eta_d)]}{q_g},\qquad
\Gamma_d=g_d(X)+\frac{T_d}{p_d(X)}[Y-g_d(X)].
$$

The residual term has conditional mean zero given $X$. For nuisance
directions $h_g(X)$ and $h_p(X)$ the derivative of its group moment is

$$
E\left[S_g\left\{\left(1-\frac{T_d}{p_d}\right)h_g
-\frac{T_d(Y-g_d)}{p_d^2}h_p\right\}\right]=0.
$$

It vanishes by conditioning on $X$, because $S_g$ is measurable there.
Thus full-sample cross-fitted nuisances can be reused without groupwise
refitting. Standard root-$N$ inference additionally requires suitable moments,
overlap and nuisance product-rate control on each group, with $H,J$ fixed
and group prevalences bounded away from zero. A tiny group is not made
asymptotically reliable merely by passing an input check.

### Prevalence estimation and centered influence

Write $a_{dg}=E[S_g\Gamma_d]=q_g\theta_d(g)$. The ratio map $a/q$ has
derivative $(1/q,-a/q^2)$. Combining the influence of the numerator and
the estimated prevalence gives

$$
\phi_{dg}(W)=\frac{S_g\Gamma_d-a_{dg}}{q_g}
-\frac{\theta_d(g)}{q_g}(S_g-q_g)
=\frac{S_g}{q_g}[\Gamma_d-\theta_d(g)].
$$

Equivalently, the unnormalized score is
$\psi_{dg}=S_g(\Gamma_d-\theta_d(g))$, with derivative $J_{dg}=-q_g$;
the framework needs $-J_{dg}^{-1}\psi_{dg}=\phi_{dg}$.
This accounts for prevalence estimation. Using $S_g\Gamma_d/q_g-\theta_d(g)$
would instead treat the denominator as known and generally give the wrong
covariance. Multiplying the globally centered APO score by $S_g/q_g$ also
fails: it centers at $\theta_d$, not $\theta_d(g)$.

For repetition $r$, let $I_r$ be the actual evaluation rows and
$m_r=|I_r|$. With cross-fitting, $I_r=\{1,\ldots,N\}$ and $m_r=N$.
Let $A_{idr}=\widehat\Gamma_{idr}$ denote exactly the stored APO `psi_b`
from the core note, $n_{gr}=\sum_{i\in I_r}S_{ig}$, and
$\hat q_{gr}=n_{gr}/m_r$. Then the proposed estimates and contributions are

$$
\hat\theta_{dr}(g)=\frac{1}{n_{gr}}\sum_{i\in I_r}S_{ig}A_{idr},\qquad
\hat\phi_{i,dg,r}=\frac{S_{ig}}{\hat q_{gr}}
[A_{idr}-\hat\theta_{dr}(g)],\quad i\in I_r.
$$

They obey $\sum_{i\in I_r}\hat\phi_{i,dg,r}=0$ exactly. Outside $I_r$
store finite zeros, preserving the original $N$-row layout; do not count
those rows in $n_{gr}$, $m_r$ or covariance. These formulas describe an
honest single-repetition held-out sample with representative evaluation
sampling, but the initial public group API does not expose that case: the
base class routes no-cross-fitting to DML1, whose group fold weighting needs
a separate derivation. Arbitrary selective holdout sampling can also change
the target population.

The canonical signal source is `apo$psi_b[, r, 1]`, available even when
optional prediction storage is disabled. On evaluation rows it can also be
recovered as `apo$psi[, r, 1] + apo$all_coef[1, r]`, or from the corresponding
APOS framework column plus its per-repetition coefficient. Never add the
aggregated coefficient and never reconstruct from zero-filled nonevaluation
rows. Group centering is new and cannot be achieved by a constant linear
transform of the globally centered framework alone.

### Exact dummy-regression equivalence and its boundaries

For one repetition define the $m_r\times H$ matrix $B$ by $B_{ig}=S_{ig}$.
Exhaustive disjoint groups give
$B^{\mathsf T}B=\operatorname{diag}(n_{1r},\ldots,n_{Hr})$.
Let $A_d$ collect the evaluation signals for treatment $d$ in this repetition.
Regressing it on all group dummies with no additional intercept yields

$$
\hat\beta_d=(B^{\mathsf T}B)^{-1}B^{\mathsf T}A_d,
\qquad \hat\beta_{dg}=\hat\theta_{dr}(g).
$$

The default HC0 sandwich, with residuals
$e_{idr}=A_{idr}-\hat\theta_{dr}(G_i)$, has entries

$$
\widehat V^{\mathrm{HC0}}_{dg,dh,r}
=\frac{\sum_{i\in I_r} S_{ig}S_{ih}e_{idr}^{2}}{n_{gr}n_{hr}}.
$$

Its diagonal is $\sum_{i\in I_r:G_i=g}e_{idr}^2/n_{gr}^2$, exactly the influence
covariance below. There is no Bessel correction; `var(A[group])/n_group`
uses denominator $n_{gr}(n_{gr}-1)$ and is not HC0. Adding an intercept
alongside every dummy creates collinearity; using an intercept plus omitted
category changes coefficient interpretation. Default homoskedastic OLS SEs,
HC1/HC2/HC3 and overlapping bases are not the claimed equivalence.

Answer: APO signals plug directly into Python's existing GAPO BLP machinery
for this basis. In R, the same means and HC0 influence arrays plug into the
reviewed framework after prevalence scaling. This reuses its inference
without adding a general BLP dependency or pretending R has Python's API.

The initial proposed group API accepts only fitted DML2 models. DML1 averages
fold estimates equally; even balanced overall folds need not have balanced
group counts. An all-row group mean generally differs from an average of
fold-specific group means. Rejecting DML1 avoids silently changing that
convention. Extending it needs a separate fold-weighting derivation.

Truncation and `normalize_ipw` remain exactly those of the fitted signal;
neither is reapplied within groups. For normalized weights let
$\hat\kappa_{dr}=m_r^{-1}\sum_{i\in I_r}T_{id}/\bar p_{idr}$, where
$\bar p$ is the existing truncated propensity. The signal is then
$\hat g+T_d(Y-\hat g)/(\bar p\hat\kappa)$.
At the true nuisance and nonbinding truncation, $\kappa=1$, and the derivative
of the group signal mean with respect to $\kappa$ is
$-E[S_g T_d(Y-g_d)/p_d]/q_g=0$; normalization adds no first-order term there.
Fixed truncation binding in the limit can invalidate the target and
orthogonality, and the core influence approximation is not a guarantee for
arbitrary misspecified nuisances. The finite-sample mean/HC0 equivalence
nevertheless holds for any fixed finite input signal. Group normalization
would define a different finite-sample estimator and is not proposed.

## 3. Construct GATE from group APOs or directly contrasted signals?

Both constructions are algebraically identical per repetition when they
share rows, group membership, nuisance predictions and normalization.
For treatments $d,k$ define the genuine group treatment effect

$$
\tau_{dk}(g)=\theta_d(g)-\theta_k(g)
=E[Y(d)-Y(k)\mid G=g].
$$

Its fitted value and contribution are

$$
\hat\tau_{dk,r}(g)=\frac{1}{n_{gr}}
\sum_{i\in I_r}S_{ig}(A_{idr}-A_{ikr}),\qquad
\hat\phi^{\tau}_{i,dk,g,r}
=\frac{S_{ig}}{\hat q_{gr}}
\{A_{idr}-A_{ikr}-\hat\tau_{dk,r}(g)\}
=\hat\phi_{i,dg,r}-\hat\phi_{i,kg,r}.
$$

Choose the stacked group-APO construction as the canonical representation:
it serves both GAPO and GATE, preserves every covariance, and allows the
existing `linear_transform()` to produce treatment and cross-group contrasts.
Use the direct contrasted-signal construction as an independent algebraic
verification. Do not subtract independently aggregated GAPO summaries.

Order the $HJ$ estimates by group, and within group by APOS treatment order:
$\hat\vartheta_r=(\hat\theta_{1r}(1),\ldots,\hat\theta_{Jr}(1),\ldots,
\hat\theta_{1r}(H),\ldots,\hat\theta_{Jr}(H))^{\mathsf T}$.
Let $\vartheta$ be the population targets in that same order.
For a $K\times J$ treatment contrast matrix $C$, use
$L=I_H\otimes C$, of dimension $HK\times HJ$. A reference row is
$e_d^{\mathsf T}-e_k^{\mathsf T}$, where $e_d$ selects treatment $d$.
For the name GATE, each nonzero row must sum to zero; an arbitrary linear
combination not summing to zero remains an APO functional. Preserve general
linear transforms on the returned framework, but do not label every such
transform a treatment effect.

A cross-group difference $\theta_d(g)-\theta_d(h)$ compares potential-outcome
levels across populations; it is not a causal effect of group membership.
The heterogeneity contrast $\tau_{dk}(g)-\tau_{dk}(h)$ has a row with
$+1,-1,-1,+1$ on $(g,d),(g,k),(h,d),(h,k)$ respectively. Both are valid fixed
linear transforms of the stacked representation and carry its covariance.

For binary treatment, matched outcome predictions, complementary processed
propensities, common splits and unnormalized weights,
$A_1-A_0$ equals R IRM's ATE `psi_b`. Its group mean and centered group
contribution therefore equal the dummy-basis IRM GATE construction. This is
a mathematical reduction, not a claim that R already exposes `IRM$gate()`.
Independently fitted noncomplementary propensities or unmatched normalization
do not establish this identity.

## 4. Standard errors, joint covariance, return object and scope

### Covariance and contrasts

Let $\hat\phi_{ir}$ be the $HJ$-vector of group APO contributions and
$\Phi_r$ the $m_r\times HJ$ matrix of those row vectors. The asymptotic
representation and estimated covariance of the estimates are

$$
\sqrt{m_r}(\hat\vartheta_r-\vartheta)
=m_r^{-1/2}\sum_{i\in I_r}\phi_i+o_p(1),\qquad
\widehat V_r=m_r^{-2}\Phi_r^{\mathsf T}\Phi_r.
$$

The first expression applies under the preceding sampling and nuisance
conditions (for full cross-fitting, $m_r=N$). Entries explicitly reduce to

$$
\widehat V_{dg,kh,r}
=\frac{1}{n_{gr}n_{hr}}\sum_{i\in I_r}S_{ig}S_{ih}
[A_{idr}-\hat\theta_{dr}(g)]
[A_{ikr}-\hat\theta_{kr}(h)].
$$

The factors $m_r^{-2}$ cancel the two prevalence numerators $m_r^2$.
For disjoint $g\ne h$, entries are zero exactly in this first-order estimate;
shared nuisance fitting can still induce higher-order finite-sample
dependence. Within a group, cross-treatment entries generally are nonzero.
Thus

$$
\hat s_{dk,g,r}^{2}=\widehat V_{dg,dg,r}+\widehat V_{kg,kg,r}-2\widehat V_{dg,kg,r},\qquad
\widehat V^L_r=L\widehat V_rL^{\mathsf T},\quad
\hat\vartheta^L_r=L\hat\vartheta_r,\quad
\Phi^L_r=\Phi_rL^{\mathsf T}.
$$

An observation's influence has outcome units since prevalence is
dimensionless. Its squared sum divided by $m_r^2$ has units of variance of
an outcome mean, scaling as $1/n_{gr}$ for a fixed group distribution.
Using $1/m_r$, dividing again by $n_{gr}$, or computing independent-treatment
SEs would mis-scale uncertainty. The framework receives $N\times HJ\times R$
`psi`, $HJ\times R$ `all_coef` and `all_se`, and stores $HJ\times HJ\times R$
`all_cov`. Supply `all_se` as square roots of the covariance diagonal: its
constructor does not enforce this equality for the caller.

### Repetition aggregation and confidence intervals

For every final estimand $a$ (including a contrast), first construct its
per-repetition $\hat\delta_{ar}$ and $s_{ar}$. The existing R rule is

$$
\tilde\delta_a=\operatorname{median}_r\hat\delta_{ar},\qquad
\tilde s_a^2=\frac{1}{N}\operatorname{median}_r
\{N s_{ar}^{2}+(\hat\delta_{ar}-\tilde\delta_a)^2\}.
$$

Here $N$ is the full observation axis, not $n_{gr}$ or $m_r$; $s_{ar}$
already contains group prevalence and evaluation scaling. The single
no-cross-fitting repetition has no repetition-dispersion term. This follows
the committed R convention, including its division of the dispersion term
by $N$, and deliberately does not substitute current Python aggregation.
There is no aggregated joint covariance object. Medians do not commute
with linear transformations: transform each repetition and then aggregate.

Pointwise normal intervals are
$\tilde\delta_a\pm z_{1-\alpha/2}\tilde s_a$, with two-sided p-values
$2\Phi(-|\tilde\delta_a/\tilde s_a|)$; $z$ and $\Phi$ are the standard
normal quantile and distribution functions. These and multiplicity
adjustments reuse framework methods.

### Common-multiplier inference

For bootstrap draw $b$ and repetition $r$, use the same independent
mean-zero, variance-one multiplier $\xi_{ibr}$ for observation $i$ across
every group, treatment and contrast in the result family. Then

$$
\Delta_{abr}=\frac{1}{m_r}\sum_{i\in I_r}\xi_{ibr}\hat\phi^L_{iar},\qquad
t^*_{abr}=\Delta_{abr}/s_{ar},\qquad
\operatorname{Cov}_*(\Delta_{\cdot br})=\widehat V^L_r.
$$

For an untransformed GAPO stack use $L=I_{HJ}$. The covariance identity
follows because $E_*[\xi_i\xi_j]=1\{i=j\}$ conditional on the fitted
signals, producing precisely $m_r^{-2}\sum_i\phi_i^L(\phi_i^L)^{\mathsf T}$.
For a single group the same perturbation is
$n_{gr}^{-1}\sum_{i\in I_r:G_i=g}\xi_{ibr}(A_{idr}-\hat\theta_{dr}(g))$.
Independent bootstraps for treatments discard their covariance; resetting
seeds on differently subsetted groups is not a substitute for aligned rows.

For confidence level $1-\alpha$, the R framework pools repetition blocks
of bootstrap columns and takes the empirical $(1-\alpha)$ quantile $c$ of
$\max_a|t^*_{abr}|$. Its simultaneous intervals are
$\tilde\delta_a\pm c\tilde s_a$. The maximum includes the full returned
family even if `parm` selects fewer displayed rows. Romano-Wolf stepdown
uses maxima over remaining hypotheses and monotone adjusted tail proportions.
This inherits the reviewed R repeated-split procedure, not Python's median
of separately bootstrapped interval endpoints. New transformed or concatenated
frameworks start without bootstrap draws and must be bootstrapped for their
own family. Singular covariance from redundant contrasts is permissible
because observation multipliers need no matrix inverse; zero marginal SEs
are not suitable for studentization and the group methods should reject
such inference results with an estimand-specific error.

### Proposed public contract and unsupported inputs

Propose `DoubleMLAPO$gapo(groups)` returning a `DoubleMLFramework` with one
GAPO per observed group, and
`DoubleMLAPOS$gate(groups, reference = NULL, contrast = NULL)` returning a
`DoubleMLFramework` for within-group treatment contrasts. Exactly one of
`reference` and `contrast` is required; reference excludes its zero self-row.
Use the existing APOS contrast label/order validation, plus the zero-row-sum
requirement for the name GATE. A matrix contrast can contain multiple rows;
all groups and rows belong to one joint-inference family. Group-specific
APOs for multiple treatments would be available from component APO `gapo()`
results and compatible `concat()`; use shared original observation and
repetition order and disjoint coefficient names. The same stack is the
internal input to APOS GATE. No new public result class or Python BLP port
is necessary. Existing `coef`, `se`, `all_coef`, `all_se`, `psi`, `all_cov`,
`summary()`, `confint()`, `bootstrap()`, `p_adjust()`, and
`linear_transform()` supply the requested inference surface.

The proposed first implementation has these explicit boundaries:

- `groups` is one atomic categorical vector of length $N$: factor, character,
  logical or finite numeric labels. Numeric values are labels, not cutpoints.
  Factor order uses observed levels; other types use first-observed order.
  Drop unused factor levels. Reject missing/nonfinite/blank labels, list
  columns, matrices, data frames, arbitrary bases and overlapping indicators.
  A caller with a one-hot partition must supply its categorical vector.
  Do not infer bins for a continuous variable.
- Membership must align position-for-position with the fitted data. Preserve
  original row identities through score construction and concatenation;
  a length check alone cannot detect a caller's permutation. Do not silently
  sort groups or join by display labels. Group definitions are fixed across
  repetitions. Post-treatment groups and data-adaptive selection on this
  estimation sample are outside the causal contract.
- Every observed group must have at least two evaluation observations in
  each repetition; reject an empty evaluation group or singleton and warn
  for counts two through five. The count floor avoids automatic zero HC0
  variance for singletons; it is not a coverage guarantee. Report warnings
  for absent requested treatment observations within a group and for severe
  group-specific propensity trimming. A zero observed treatment cell can
  still produce finite augmented signals from cross-group nuisance fits,
  so it is a support warning, not proof of population nonidentification.
  Actual population overlap and positive limiting group prevalence remain
  required. Core nuisance-training support errors still apply.
- Require successful fitted built-in APO models, synchronized rows/splits,
  fully cross-fitted DML2, finite evaluation signals and positive
  per-repetition marginal SEs for returned estimands. Reject DML1 and every
  no-cross-fitting mode,
  clustered data, instruments, survey weights, in-sample learned or soft membership
  and a custom-score extension. No group-specific nuisance refitting or
  alteration of fitted trimming/normalization is performed.
- Construct stable unique names containing both original treatment and
  group labels (and contrast labels for GATE); escape delimiters or reject
  collisions rather than silently merging estimands. Preserve type-safe
  label identity separately while assembling matrices. Group-major ordering
  is the fixed convention above. Cross-group contrasts can then select
  named columns with `linear_transform()`.

### Review conclusions and verification obligations

All four section 26 questions are resolved: (1) Python IRM GATE uses
orthogonal `psi_b` signals; R has no preexisting group API. (2) APO signals
map exactly to dummy-basis group means, with prevalence-scaled centering
required before R framework inference. (3) Group APO contrasts and direct
contrasted signals coincide; the stacked representation is the chosen
implementation boundary. (4) Estimator covariance is the scaled joint
cross-product, propagated before repetition aggregation and bootstrapped
with common observation multipliers. Existing R infrastructure is reused
where these identities establish compatibility.

The derivation requires verification of group means against no-intercept
OLS/HC0, prevalence centering, full cross-treatment covariance, zero
off-group blocks, direct versus stacked treatment and cross-group contrasts,
binary IRM signal reduction, unequal group sizes, repetitions and held-out
evaluation scaling. Common-multiplier perturbations must reproduce the
matrix formula under a fixed seed; invalid group inputs and every unsupported
fit mode must fail informatively. These are acceptance obligations for a
later plan, not implementation steps or claims of implemented group methods.

This memo is the required review checkpoint. Stop here for user review;
only after approval should a separate GATE TDD implementation plan be
written and execution approval obtained. Sensitivity remains a separate
theory gate.
