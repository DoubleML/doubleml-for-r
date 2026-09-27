# APO/APOS core mathematics and implementation contract

This note covers reviewed core behavior in `DoubleMLAPO`, `DoubleMLAPOS` and
`DoubleMLFramework`. It is a derivation and implementation reference, not a
claim that arbitrary nuisance learners satisfy asymptotic assumptions.

## Target and identification

Let independent observations be $W_i=(Y_i,D_i,X_i)$, $i=1,\ldots,N$, with
nominal treatment labels in a finite set $\mathcal D$. Numeric labels do not
encode a dose. For a requested label $d$, write $T_d=1\{D=d\}$ and

$$
\theta_d=E[Y(d)],\qquad g_d(x)=E[Y\mid D=d,X=x],\qquad
p_d(x)=P(D=d\mid X=x).
$$

Assume consistency $Y=Y(D)$, conditional exchangeability
$Y(d)\perp D\mid X$, and positivity $p_d(X)>0$ almost surely for the requested
labels. Then iterated expectation gives

$$
E[Y(d)]=E[E[Y(d)\mid X]]=E[E[Y\mid D=d,X]]=E[g_d(X)].
$$

Population positivity is an identification condition. Root-$N$ inference
requires stronger regularity, such as probabilities bounded away from zero,
finite suitable moments and nuisance convergence/product-rate conditions.
Restricting the requested labels does not condition the target population on
membership in those labels. A propensity learner uses a binary $d$-versus-rest
target, with every other observed label in the complement.

## Orthogonal score

For candidate nuisances $(g,p)$ define

$$
\psi_d(W;\theta,g,p)=g(X)+\frac{T_d}{p(X)}[Y-g(X)]-\theta.
$$

At the truth, $E[T_d(Y-g_d(X))\mid X]=0$, so $E[\psi_d]=0$ and
$J_d=\partial_\theta E[\psi_d]=-1$. Consider paths
$g_t=g_d+t h_g$, $p_t=p_d+t h_p$ with denominators bounded away from zero
and sufficient integrability to differentiate under expectation. The derivative is

$$
\left.\frac{d}{dt}E[\psi_d(W;\theta_d,g_t,p_t)]\right|_{t=0}
=E\left[\left(1-\frac{T_d}{p_d(X)}\right)h_g(X)
-\frac{T_d(Y-g_d(X))}{p_d(X)^2}h_p(X)\right]=0.
$$

The two terms vanish by conditioning on $X$. More explicitly, for fixed candidate
nuisances the population bias of the augmented outcome is

$$
E\left[g(X)+\frac{T_d}{p(X)}(Y-g(X))\right]-\theta_d
=E\left[\frac{p(X)-p_d(X)}{p(X)}(g(X)-g_d(X))\right].
$$

Thus the leading nuisance bias is a product. Cross-fitting estimates nuisances
on training rows and evaluates scores on disjoint rows. Typical sufficient
conditions include $L_2$ consistency and
$\|\hat g_d-g_d\|_2\|\hat p_d-p_d\|_2=o_p(N^{-1/2})$, together with
overlap and moment assumptions controlling the empirical remainder.

## Fitted scores, truncation and normalization

For repetition $r$ and evaluation set $I_r$ of size $m_r$, define
$\bar p_{idr}=\min(\max(\hat p_{idr},\tau),1-\tau)$ for $\tau>0$;
with $\tau=0$ use the raw probability, which must be strictly positive.
Default residual weights and augmented outcomes are

$$
w_{idr}=T_{id}/\bar p_{idr},\qquad
A_{idr}=\hat g_{idr}+w_{idr}(Y_i-\hat g_{idr}).
$$

`normalize_ipw = TRUE` replaces $w_{idr}$ by
$w_{idr}/[m_r^{-1}\sum_{i\in I_r}w_{idr}]$, requiring a positive finite
denominator. This is a finite-sample alternative; the preceding exact bias and
orthogonality identities describe the population score at the true propensity.
Fixed truncation may bias the target score if it binds in the limit. Truncation
does not prove overlap and does not discard observations. Diagnostics retain
raw and effective propensities, effective weights and fold support counts.

For DML2, $\hat\theta_{dr}=m_r^{-1}\sum_{i\in I_r}A_{idr}$.
For DML1, the implementation averages fold means with equal fold weights.
Equal-sized folds give the same estimate; unequal fold sizes can differ.
The stored contribution is always

$$
\hat\phi_{idr}=-J_d^{-1}\psi_d(W_i;\hat\theta_{dr},\hat\eta_{dr})
=A_{idr}-\hat\theta_{dr}.
$$

For unequal-fold DML1 this need not have zero unweighted sample mean; the
framework preserves the base model's score and variance convention without
additional recentering or unequal-fold reweighting. The usual root-$N$
interpretation uses balanced folds; do not interpret this convention as a
new derivation for arbitrary unequal fold designs.

## Joint covariance and linear contrasts

Stack the $J$ requested contributions as a column vector $\hat\phi_{ir}$.
For DML2 with cross-fitting, the asymptotic linear representation is
$\sqrt N(\hat\theta_r-\theta)=N^{-1/2}\sum_i\phi_i+o_p(1)$ under the
conditions above. Consequently the estimated covariance of the estimates is

$$
\widehat V_r=\frac{1}{m_r^2}\sum_{i\in I_r}
\hat\phi_{ir}\hat\phi_{ir}^{\mathsf T}.
$$

The two powers of $m_r$ matter: one averages score products, the other scales
to the variance of a mean. There is no Bessel correction or extra recentering.
Cross-fitting uses all $N$ observations, hence $m_r=N$. Without cross-fitting,
only the actual held-out evaluation rows enter covariance and bootstrap.
`all_cov` has dimensions estimand by estimand by repetition, while `psi` has
dimensions observation by estimand by repetition. The component APO's `psi`
instead has observation by repetition by one treatment dimensions.

For a finite $K\times J$ contrast matrix $C$,

$$
\hat\delta_r=C\hat\theta_r,\qquad
\hat\phi^\delta_{ir}=C\hat\phi_{ir},\qquad
\widehat V_r^\delta=C\widehat V_r C^{\mathsf T}.
$$

`linear_transform()` implements these operations before aggregation and
recomputes each standard error from $\operatorname{diag}(\widehat V_r^\delta)$.
A reference contrast has $+1$ on the other label and $-1$ on the reference.
In particular, $\widehat{\operatorname{Var}}(\hat\theta_d-\hat\theta_c)
=V_{dd}+V_{cc}-2V_{dc}$. Concatenation retains the observation alignment so
that cross-products include cross-covariance between all component APOs.

## Repeated-split aggregation

For $R$ repetitions, the implementation uses coordinate-wise medians:

$$
\widetilde\theta_j=\operatorname{median}_r\hat\theta_{jr},\qquad
\widetilde s_j^2=\frac{1}{N}\operatorname{median}_r
\left\{N\hat s_{jr}^2+(\hat\theta_{jr}-\widetilde\theta_j)^2\right\}.
$$

This is the repository's existing convention: the dispersion term is divided
by $N$ after the median. The framework uses the full observation dimension $N$
for aggregation, even though its covariance uses $m_r$. APO/APOS support only
one repetition without cross-fitting, so the dispersion term is then zero.
Learner fitting requires a disjoint held-out set. Full-sample one-fold splits
are supported only with externally supplied predictions; the caller must
establish their out-of-sample provenance. Repeated no-cross-fitting inference
is not an APO/APOS feature.

For contrasts, replace $\hat\theta_{jr}$ and $\hat s_{jr}$ by the transformed
values in this same formula. Generally $C\operatorname{median}_r\hat\theta_r
\ne\operatorname{median}_r(C\hat\theta_r)$. Thus subtracting aggregated APO
coefficients is not a replacement for transforming each repetition. There is
no aggregated joint covariance field.

## Common-multiplier bootstrap

For draw $b$ within repetition $r$, draw a multiplier $\xi_{ibr}$ for every
evaluation observation, shared across all estimands, and compute

$$
\Delta_{jbr}=\frac{1}{m_r}\sum_{i\in I_r}\xi_{ibr}\hat\phi_{ijr},\qquad
t^*_{jbr}=\Delta_{jbr}/\hat s_{jr}.
$$

The supported multiplier methods are `normal`, `Bayes` and `wild`, following
the repository's `draw_weights()` helper. Bootstrap columns are stored in
repetition blocks. Joint intervals use the empirical level-$1-\alpha$
quantile $c$ of $\max_j|t^*_{jbr}|$, pooling columns over repetitions, and
report $\widetilde\theta_j\pm c\widetilde s_j$. Selecting displayed parameters
does not change the family used for the maximum. Romano-Wolf stepdown uses
the maxima over remaining hypotheses and a cumulative maximum of the ordered
tail proportions. A transformed or concatenated framework starts without
bootstrap draws and must be bootstrapped for its own family.

## Binary reduction to IRM ATE

Let $D\in\{0,1\}$, $p_1=m$, $p_0=1-m$, and use the same folds, outcome
predictions and symmetric truncation rule. Truncation obeys
$\operatorname{clip}(1-m)=1-\operatorname{clip}(m)$. With unnormalized weights,

$$
A_{i1r}-A_{i0r}=\hat g_{1ir}-\hat g_{0ir}
+\frac{D_i(Y_i-\hat g_{1ir})}{\bar m_{ir}}
-\frac{(1-D_i)(Y_i-\hat g_{0ir})}{1-\bar m_{ir}}.
$$

This is the IRM ATE augmented score. The APO contrast and IRM therefore have
identical per-repetition estimates under the same DML procedure, and identical
centered contributions $\hat\phi_{i1r}-\hat\phi_{i0r}$. Their covariance-aware
variances, standard errors and repeated-split aggregation agree. Independently
trained one-vs-rest classifiers need not produce complementary probabilities;
learner names alone do not establish this equivalence. Separate normalization
also changes the finite-sample score and is excluded from this identity.
Bootstrap draws need not match bit-for-bit across class implementations; this
identity concerns the score, estimates and covariance-aware standard errors.

## Group-specific APOs and GATEs

Let $G=h(X)$ be a fixed finite categorical partition defined from baseline
information, with $S_{ig}=1\{G_i=g\}$ and $q_g=E[S_g]>0$. Group labels are
nominal. For treatment label $d$, the group average potential outcome is

$$
\theta_d(g)=E[Y(d)\mid G=g]
=\frac{E[S_g A_d]}{q_g},
$$

where $A_d$ is the APO augmented signal defined above. Because $S_g$ is
measurable with respect to baseline $X$, the APO orthogonality argument remains
valid after multiplication by $S_g$. Estimating the prevalence is part of the
ratio target. Its centered influence contribution is

$$
\phi_{dg}(W)=\frac{S_g}{q_g}\{A_d-\theta_d(g)\},
$$

not $S_g A_d/q_g-\theta_d(g)$ and not the globally centered APO contribution
multiplied by $S_g/q_g$.

The implemented group API is restricted to fully cross-fitted DML2, so every
repetition evaluates all $N$ observations. With
$n_g=\sum_i S_{ig}$ and $\hat q_g=n_g/N$, it uses the stored APO `psi_b`
signal $A_{idr}$ directly:

$$
\hat\theta_{dr}(g)=\frac{1}{n_g}\sum_{i:G_i=g}A_{idr},\qquad
\hat\phi_{i,dg,r}=\frac{S_{ig}}{\hat q_g}
\{A_{idr}-\hat\theta_{dr}(g)\}.
$$

Stack columns in group-major, treatment-minor order. For repetition $r$ the
joint covariance of all GAPOs is

$$
\widehat V_r^{\mathrm{GAPO}}
=\frac{1}{N^2}\sum_{i=1}^N
\hat\phi_{ir}^{\mathrm{GAPO}}
(\hat\phi_{ir}^{\mathrm{GAPO}})^{\mathsf T}.
$$

Contributions from two disjoint groups have zero cross-product, while
cross-treatment covariance within a group is retained. A marginal GAPO is an
outcome level, not a treatment effect.

Let $C$ be a $K\times J$ treatment-contrast matrix whose nonzero rows sum to
zero. These rows define within-group treatment effects and therefore qualify as
GATEs. With $H$ groups, the block transform is

$$
L=I_H\otimes C,\qquad
\hat\delta_r=L\hat\theta_r^{\mathrm{GAPO}},\qquad
\hat\phi_{ir}^{\delta}=L\hat\phi_{ir}^{\mathrm{GAPO}},\qquad
\widehat V_r^{\delta}=L\widehat V_r^{\mathrm{GAPO}}L^{\mathsf T}.
$$

`DoubleMLAPO$gapo()` returns the GAPO framework. `DoubleMLAPOS$gate()` applies
this block transform and returns one framework ordered group-major and
contrast-minor. Both inherit the repeated-split aggregation, common-multiplier
bootstrap, joint intervals and Romano-Wolf procedure described above. The
binary contrast $A_1-A_0$ also implies that grouped APOS GATEs reduce to group
means of the matched IRM ATE signal under the same conditions as the global
binary reduction.

Every observed group must contain at least two evaluation rows; counts two
through five warn. Missing observed group-treatment cells and more than five
percent group-specific trimming also warn. Nonfinite signals and nonpositive
marginal standard errors fail. These finite-sample checks do not replace the
population requirements of positive limiting group prevalence, within-group
overlap, moments and nuisance rates.

## Boundaries and pending sensitivity gate

APOS publishes results only after all cloned component fits and joint covariance
construction succeed. Failed refits retain the previous complete result; new
splits and successful tuning invalidate it. Clustered data and instruments are
rejected: the formulas above are for independent observations.

Group membership must be a fixed, exhaustive categorical baseline partition
aligned position-for-position with the fitted rows. DML1, no-cross-fitting,
post-treatment or outcome-adaptive groups, soft or overlapping membership,
clusters, instruments, survey weights and custom scores are outside the group
API. Sensitivity theory and its section remain a separate mandatory memo gate;
no sensitivity bound, Riesz formula or other unreviewed extension is specified
here.
