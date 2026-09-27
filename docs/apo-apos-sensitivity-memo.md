# APO/APOS sensitivity analysis: theory and implementation memo

Date: 2026-09-27. Status: proposed for review; no sensitivity code is
authorized by this memo until the user approves it.

This memo answers the seven questions in section 25 of the original
specification. It applies the omitted-variable-bias framework of
[Chernozhukov, Cinelli, Newey, Sharma and Syrgkanis](https://arxiv.org/abs/2112.13398)
to the APO/APOS estimands already implemented on `feature/apo-apos`.
The official behavioral reference is DoubleML Python 0.11.4 at immutable
commit `96e32c9517812a8b8875dd74956704d7998f6510`, especially its
[IRM](https://github.com/DoubleML/doubleml-for-py/blob/96e32c9517812a8b8875dd74956704d7998f6510/doubleml/irm/irm.py),
[APO](https://github.com/DoubleML/doubleml-for-py/blob/96e32c9517812a8b8875dd74956704d7998f6510/doubleml/irm/apo.py),
[APOS](https://github.com/DoubleML/doubleml-for-py/blob/96e32c9517812a8b8875dd74956704d7998f6510/doubleml/irm/apos.py),
[framework](https://github.com/DoubleML/doubleml-for-py/blob/96e32c9517812a8b8875dd74956704d7998f6510/doubleml/double_ml_framework.py),
and [bias helper](https://github.com/DoubleML/doubleml-for-py/blob/96e32c9517812a8b8875dd74956704d7998f6510/doubleml/utils/_sensitivity.py).
The official [DoubleML sensitivity guide](https://docs.doubleml.org/stable/guide/sensitivity.html)
provides the same general formulas and interpretation.

The current R checkout has no reusable sensitivity implementation in
`DoubleML`, `DoubleMLIRM` or `DoubleMLFramework`. Consequently, “current IRM
formulas” below means the official DoubleML theory and Python implementation,
not a hidden R API that can be reused.

## Notation and the current DoubleML construction

Let the short observed information be $W=(Y,D,X)$ and the long information be
$\widetilde W=(Y,D,X,A)$, where $A$ is an omitted pre-treatment confounder.
For a continuous linear functional

$$
\theta = E[m(W,g)], \qquad g(D,X)=E[Y\mid D,X],
$$

let $\alpha(W)$ be its short Riesz representer, so that

$$
E[m(W,g)] = E[g(W)\alpha(W)].
$$

The orthogonal signal is

$$
\Gamma(W)=m(W,g)+(Y-g(W))\alpha(W), \qquad \phi=\Gamma-\theta.
$$

DoubleML defines

$$
\sigma^2=E[(Y-g(W))^2], \qquad
\nu^2=E[\alpha(W)^2]
=E[2m(W,\alpha)-\alpha(W)^2],
$$

with orthogonal sensitivity scores

$$
\psi_{\sigma^2}=(Y-g(W))^2-\sigma^2,
$$

$$
\psi_{\nu^2}=2m(W,\alpha)-\alpha(W)^2-\nu^2.
$$

Write `cf_y` as $C_Y^2$, the share of short residual outcome variation
explained by $A$, and `cf_d` as

$$
\operatorname{cf}_d
=1-\frac{E[\alpha(W)^2]}{E[\widetilde\alpha(\widetilde W)^2]}.
$$

Then $C_D^2=\operatorname{cf}_d/(1-\operatorname{cf}_d)$. The parameter
$\rho\in[-1,1]$ is the correlation between the short-to-long changes in the
outcome regression and Riesz representer. For

$$
\kappa
=|\rho|\sqrt{\operatorname{cf}_y
  \frac{\operatorname{cf}_d}{1-\operatorname{cf}_d}},
\qquad b=\sqrt{\sigma^2\nu^2},
$$

the worst-case bounds are

$$
\theta_- = \theta-\kappa b, \qquad
\theta_+ = \theta+\kappa b.
$$

The influence contribution of $b$ is

$$
\psi_b
=\frac{\sigma^2\psi_{\nu^2}+\nu^2\psi_{\sigma^2}}
       {2\sqrt{\sigma^2\nu^2}},
$$

and the bound influences are $\phi_-=\phi-\kappa\psi_b$ and
$\phi_+=\phi+\kappa\psi_b$. Per repetition, the R package must use its
existing convention

$$
\widehat{\operatorname{Var}}(\widehat\theta_\pm)
=n^{-2}\sum_{i=1}^n\widehat\phi_{\pm,i}^2.
$$

Repeated-split point bounds use the existing coordinate-wise median
aggregation, $\widehat\theta_\pm=\operatorname{median}_r
(\widehat\theta_{\pm,r})$. Their standard errors use the framework's between-
split correction,

$$
\widehat{se}_\pm
=\sqrt{\frac{1}{n}\operatorname{median}_r\left(
n\widehat{se}_{\pm,r}^2+
(\widehat\theta_{\pm,r}-\widehat\theta_\pm)^2\right)}.
$$

The confidence bounds are one-sided:
$\widehat\theta_- - z_{\text{level}}\widehat{se}_-$ and
$\widehat\theta_+ + z_{\text{level}}\widehat{se}_+$. The robustness value
`RV` is the smallest $r\in[0,1)$ with `cf_y = cf_d = r` for which a point
bound reaches the requested null; `RVa` is defined analogously using the
confidence bound. Both keep the requested $\rho$ fixed.

### Binary IRM specialization

For binary ATE, with $T=1\{D=1\}$, propensity $p(X)$, and
$g_t(X)=E[Y\mid D=t,X]$,

$$
m_{ATE}(W,g)=g_1(X)-g_0(X), \qquad
\alpha_{ATE}(W)=\frac{T}{p(X)}-\frac{1-T}{1-p(X)}.
$$

Thus

$$
\sigma^2
=E[(Y-Tg_1(X)-(1-T)g_0(X))^2],
$$

$$
\nu_{ATE}^2
=E\left[\frac{1}{p(X)}+\frac{1}{1-p(X)}\right].
$$

The official implementation uses cross-fitted plug-in versions of these
quantities and the two orthogonal scores above. It computes
`max_bias = sqrt(sigma2 * nu2)`, applies $\kappa$ to the point estimate and
influence, and obtains `RV`/`RVa` by scalar root search.

## 1. Which parts transfer unchanged to one APO?

For one nominal treatment level $d$, define

$$
T_d=1\{D=d\}, \qquad p_d(X)=P(D=d\mid X),
$$

and the linear functional

$$
m_d(W,g)=g(d,X), \qquad \theta_d=E[g(d,X)].
$$

Its short Riesz representer is

$$
\alpha_d(W)=\frac{T_d}{p_d(X)},
$$

because $E[T_df(D,X)/p_d(X)]=E[f(d,X)]$. Therefore the existing APO
orthogonal signal is exactly the general debiased linear-functional signal.
The general definitions of `cf_y`, `cf_d`, $\rho$, $\kappa$, point bounds,
bound influences, one-sided intervals and robustness values transfer
unchanged.

For the APO Riesz norm,

$$
\nu_d^2=E[\alpha_d^2]=E[1/p_d(X)],
$$

and

$$
\psi_{\nu_d^2}
=\frac{2}{p_d(X)}-\frac{T_d}{p_d(X)^2}-\nu_d^2.
$$

For repetition $r$, let $\bar p_{d,r,-i}$ be the trimmed propensity. If IPW
normalization is active, set

$$
s_{d,r}=\frac{1}{n}\sum_{i=1}^n
\frac{T_{d,i}}{\bar p_{d,r,-i}(X_i)}, \qquad
p^*_{d,r,-i}(X_i)=s_{d,r}\bar p_{d,r,-i}(X_i);
$$

otherwise set $p^*_{d,r,-i}=\bar p_{d,r,-i}$. Thus
$T_{d,i}/p^*_{d,r,-i}$ is exactly the effective inverse-propensity weight
used by the fitted APO score. The cross-fitted sample analogues are

$$
\widehat\sigma_r^2
=\frac{1}{n}\sum_{i=1}^n
\left(Y_i-\widehat g_{r,-i}(D_i,X_i)\right)^2,
$$

$$
\widehat\nu_{d,r}^2
=\frac{1}{n}\sum_{i=1}^n\left(
\frac{2}{\widehat p^*_{d,r,-i}(X_i)}
-\frac{T_{d,i}}{\widehat p^*_{d,r,-i}(X_i)^2}\right).
$$

The empirical scores subtract these sample means from the corresponding
summands, so each score is centered within repetition. The formulas use
out-of-fold predictions throughout.

The sample implementation must use this same effective propensity. Trimming
and normalization change the reported finite-sample sensitivity quantities
just as they change the fitted score; they must not be silently recomputed
from raw propensities.

## 2. What must change for multi-valued treatments?

The APO score only needs $g_d(X)$, but sensitivity also needs

$$
\sigma^2=E[(Y-g(D,X))^2]
$$

over every observed treatment level. Using $(Y-g_d(X))^2$ on all rows is
wrong: on $D\ne d$ those residuals are not residuals from the observed
conditional outcome regression.

Following official Python APO, a learned R fit must therefore obtain a second,
cross-fitted outcome prediction for complement rows. It should train a clone of
`ml_g` on $D\ne d$, include the original treatment as a categorical feature,
and combine predictions as

$$
\widehat g(D_i,X_i)
=T_{d,i}\widehat g_d(X_i)
+(1-T_{d,i})\widehat g_{-d}(D_i,X_i).
$$

Numeric treatment values must be converted to nominal labels in this nuisance
task; their magnitude cannot enter as a dose. External-prediction fits may
optionally supply this complement prediction. An existing two-key external fit
must remain valid for APO estimation, but sensitivity must fail clearly if no
cross-fitted complement prediction is available; it must not substitute the
selected-level regression.

When the complement predictions are available, the fitted sensitivity arrays
are per observation, repetition and APO:
`sigma2`, `nu2`, `psi_sigma2`, `psi_nu2`, `riesz_rep`, `max_bias` and
`psi_max_bias`. They must be computed during `fit()` without requiring
`store_predictions = TRUE` and without changing APO coefficients, scores,
diagnostics or stored-model behavior.

## 3. Are binary one-vs-rest indicators theoretically sufficient?

Yes for an individual APO, with an important qualification. The indicator
$T_d$ and its one-vs-rest propensity are sufficient to construct the APO Riesz
representer and its assignment-side sensitivity. The complement is not a
second intervention and no ordering among its labels is used.

The indicator alone is not sufficient to estimate outcome residual variation.
That part requires a treatment-aware estimate of $g(D,X)$ for the actually
observed nominal level. Also, applying the complete binary IRM ATE formula to
$T_d$ would target the difference between $d$ and its heterogeneous complement,
not $E[Y(d)]$. The correct APO Riesz is $T_d/p_d$, not the IRM ATE Riesz
$T_d/p_d-(1-T_d)/(1-p_d)$.

## 4. How does APO sensitivity differ from contrast sensitivity?

For a fixed linear contrast $c\in\mathbb R^J$, define

$$
\theta_c=\sum_{j=1}^J c_j\theta_j, \qquad
m_c(W,g)=\sum_{j=1}^J c_jg_j(X).
$$

The correct Riesz representer is

$$
\alpha_c(W)=\sum_{j=1}^J c_j\frac{T_j}{p_j(X)}.
$$

Because nominal treatments are mutually exclusive, $T_jT_k=0$ for $j\ne k$.
Hence no unencoded cross-counterfactual covariance is needed:

$$
\nu_c^2=E[\alpha_c^2]=\sum_{j=1}^J c_j^2\nu_j^2,
$$

$$
\psi_{\nu_c^2}=\sum_{j=1}^J c_j^2\psi_{\nu_j^2}.
$$

The outcome residual target $\sigma^2$ is common to all APO functionals. When
component APOs provide different cross-fitted estimators of that same quantity,
the contrast may use a symmetric convex combination with weights
$c_j^2/\sum_k c_k^2$; its score uses the same weights. This is the general
version of Python APOS's equal average for a pairwise $(-1,1)$ contrast. It
remains an estimator of one common population $\sigma^2$, while $\nu_c^2$ uses
squared—not absolute or signed—contrast weights.

Therefore marginal sensitivity bounds for arbitrary nonzero APO linear
contrasts are theoretically justified. Zero-sum rows are causal treatment
contrasts; nonzero-sum rows remain linear combinations of potential-outcome
levels and must not be called treatment effects. Sensitivity elements cannot be
propagated by ordinary matrix multiplication, and marginal worst cases cannot
be subtracted or added. `DoubleMLAPOS$causal_contrast()` must rebuild the
contrast-specific Riesz norm and bias influence explicitly.

These are marginal bounds for each requested contrast. A simultaneous
confidence region over several sensitivity bounds would require a separately
specified joint procedure and is not part of the initial API.

Group-conditioned targets are also excluded initially. GAPO is a conditional
ratio estimand involving the estimated prevalence $q_g$; its sensitivity
influence must include the effect of estimating that denominator. Applying the
global APO bias influence after `gapo()` or `gate()` would omit this term.
Until that ratio derivation is reviewed, the exact public error should be:

> Sensitivity analysis for GAPO/GATE is not implemented; conditional group
> prevalence requires a separate sensitivity influence derivation.

## 5. Binary reductions and required validation

An individual APO sensitivity object is not supposed to equal IRM ATE
sensitivity. For $D\in\{0,1\}$, however, the APOS contrast $c=(-1,1)$ has

$$
\alpha_c
=\frac{D}{p(X)}-\frac{1-D}{1-p(X)}, \qquad
\nu_c^2=\nu_1^2+\nu_0^2,
$$

and therefore reduces exactly to binary IRM ATE sensitivity when sample splits,
outcome predictions, complementary propensities, trimming, normalization and
the common residual score agree.

The follow-up implementation must test, at thresholds zero and 0.05 and with
two repetitions:

1. APO arm-specific Riesz representations and $\nu_j^2$ against direct
   deterministic calculations.
2. The common $\sigma^2$ and both orthogonal sensitivity scores.
3. The APOS $(-1,1)$ Riesz, $\nu^2$, maximum bias and bias influence against
   the IRM formulas at machine precision.
4. Point bounds, bound standard errors, one-sided confidence bounds, `RV` and
   `RVa` against an independent deterministic oracle matching official Python.
5. Three- and four-level nominal treatments, arbitrary named contrast matrices,
   reordered columns, repeated splits, factor/character/numeric labels and
   external complement predictions.
6. Errors before fit, for missing complement predictions, invalid strengths or
   null vectors, generic framework transforms without rebuilt sensitivity,
   DML1/no-cross-fitting in the initial release, clusters, GAPO and GATE.
7. No mutation of fitted APO/APOS objects and no change to existing
   coefficient, covariance, bootstrap or contrast results.

Since this R repository has no IRM sensitivity API, the test oracle must encode
the formulas above independently and a development validation script should
compare the same deterministic inputs to official Python 0.11.4. No production
R IRM source needs to change merely to manufacture a parity target.

## 6. Recommended implementation

Implement the following reviewed subset after approval:

1. Generalize `DoubleMLFramework` with optional sensitivity elements and
   `sensitivity_analysis()`, `sensitivity_params` and `sensitivity_summary`.
   Preserve all behavior when elements are absent. Implement marginal point
   bounds, bound influences, repeated-split aggregation, one-sided intervals,
   `RV` and `RVa`; defer plotting and benchmarking.
2. Extend `DoubleMLAPO` fit internals with the cross-fitted complement outcome
   nuisance and construct the APO sensitivity arrays from the fitted effective
   propensity. Expose the framework-backed sensitivity methods and fields.
3. Concatenate arm-specific elements in `DoubleMLAPOS`. For
   `causal_contrast()`, explicitly rebuild $\sigma_c^2$, $\nu_c^2$, their
   scores, maximum bias and bias influence for every contrast row.
4. Limit the first sensitivity API to fitted, fully cross-fitted DML2 built-in
   APO/APOS models with independent observations. Preserve existing external
   prediction calls; sensitivity is unavailable unless the complement outcome
   prediction was learned or supplied.
5. Reject generic sensitivity propagation through `linear_transform()` and
   reject sensitivity on GAPO/GATE results with the explicit errors above.
   Clustered data, instruments, custom scores and survey weights remain outside
   APO/APOS generally.

This design reuses one framework engine for bounds and aggregation, but keeps
model-specific Riesz construction in APO/APOS. It follows official Python where
the theory matches, generalizes its pairwise contrast calculation to arbitrary
linear contrasts, and is deliberately narrower where this R architecture has
not yet derived DML1, no-cross-fitting or conditional-group sensitivity.

## 7. Assumptions beyond standard APO identification

The sensitivity bounds require more than consistency, observed conditional
exchangeability and positivity:

- $A$ is a pre-treatment omitted variable and the short/long regressions are
  nested square-integrable projections under the same population law.
- The APO or contrast is a continuous linear functional with square-integrable
  short and long Riesz representers.
- Overlap is strong enough for finite Riesz second moments; for APO this
  includes $E[1/p_d(X)]<\infty$, and analogously for the long propensity.
- Outcome residuals and bound influences have the moments needed for root-$n$
  inference; cross-fitted nuisance errors satisfy the usual product-rate
  conditions for the target and sensitivity elements.
- `cf_y` and `cf_d` are user-supplied restrictions on short-to-long variation,
  not quantities identified from the observed data. The chosen $\rho$
  describes their alignment; $|\rho|=1$ is adversarial.
- For a contrast, one common omitted-confounding model applies to the combined
  functional. Coefficients are fixed before inspecting outcomes, and treatments
  are mutually exclusive so the Riesz cross-products vanish.
- Trimming and normalization define the fitted finite-sample score convention.
  Sensitivity cannot repair practical positivity violations or reinterpret a
  heavily trimmed estimand as the untrimmed population APO.

The result is a worst-case robustness analysis, not an identified bias
correction. It does not prove that an omitted confounder of the specified
strength exists, nor determine the sign of confounding without the user's
$\rho$ assumption.

## Review decision requested

Approval of this memo authorizes a separate TDD implementation plan for:
framework-backed marginal sensitivity, single APO sensitivity, APOS marginal
families, and arbitrary APO linear-contrast sensitivity under fully
cross-fitted DML2. It does not authorize GAPO/GATE sensitivity, DML1,
no-cross-fitting, cluster sensitivity, contour plotting or benchmarking.
