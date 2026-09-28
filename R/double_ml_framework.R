#' @title Joint inference for DoubleML estimates
#'
#' @description
#' Stores synchronized estimates and centered influence contributions for joint
#' inference, linear contrasts and concatenation across estimands.
#' Covariance is computed separately for each repetition. Repeated estimates
#' use coordinate-wise medians and the same aggregation as DoubleML models.
#'
#' @format [R6::R6Class] object.
#' @seealso [DoubleMLAPO], [DoubleMLAPOS]
#' @details
#' This class does not fit nuisance models or establish causal identification.
#' Inputs must refer to the same observations, estimands and sample splits.
#' Influence contributions must include the negative inverse score derivative;
#' for APO they are the augmented outcome minus its estimate.
#' For evaluation rows \eqn{I_r} in repetition \eqn{r}, covariance is
#' \deqn{\widehat V_r = |I_r|^{-2}\sum_{i \in I_r}\phi_{ir}\phi_{ir}^T.}
#' This is the covariance of estimates, not the unscaled covariance of scores.
#' Supplied standard errors are checked for shape and nonnegativity; callers
#' must ensure they agree with the influence contributions. Linear transforms
#' recompute standard errors from the transformed covariance.
#'
#' Let \eqn{N} be the observation dimension of `psi`. Each coefficient is the
#' median across repetitions, with squared standard error
#' \eqn{\operatorname{median}_r\{N s_{jr}^2 +
#' (\widehat\theta_{jr}-\widetilde\theta_j)^2\}/N}.
#' Without cross-fitting, covariance uses the actual evaluation size but
#' aggregation still uses \eqn{N}. APOS supports only one such repetition.
#' There is no aggregated covariance: `all_cov` retains each repetition.
#'
#' `linear_transform()` applies a matrix to each repetition before aggregation
#' and retains off-diagonal covariance. `concat()` requires aligned observations,
#' repetitions and splits; callers must establish observation identity when row
#' names are absent. Both return fresh, unbootstrapped objects. Bootstrap
#' multipliers are common to all estimands within each repetition. Joint
#' intervals use the quantile of the maximum absolute bootstrap statistic over
#' the full estimand family, pooling columns across repetitions.
#'
#' With canonical sensitivity elements, `sensitivity_analysis()` evaluates
#' omitted-confounding bounds. For residual outcome variation \eqn{\sigma^2},
#' Riesz variation \eqn{\nu^2}, and maximum bias
#' \eqn{B = \sqrt{\sigma^2\nu^2}}, the confounding strength is
#' \eqn{\kappa=|\rho|\sqrt{C_Y C_D/(1-C_D)}} and the repetition-specific point
#' bounds are \eqn{\widehat\theta_r \mathbin{\pm} \kappa\widehat B_r}.
#' The bound influence contributions are `psi` plus or minus \eqn{\kappa} times
#' `psi_max_bias`. Point bounds and their standard errors use the framework's
#' repeated-split aggregation; confidence bounds are medians of the
#' repetition-specific one-sided limits. `rv` is the smallest common value
#' \eqn{C_Y=C_D} whose adverse point bound reaches the null, and `rva` is its
#' confidence-bound analogue. Values of one mean that the null is not reached
#' below one. Bounds are marginal, not simultaneous sensitivity regions.
#' Generic linear transforms discard sensitivity unless independently derived
#' elements are supplied. Cluster-robust and group-conditional sensitivity are
#' not implemented.
#' @usage NULL
#' @examples
#' set.seed(42)
#' outcomes = cbind(control = rnorm(40), filter = rnorm(40, mean = 1))
#' means = colMeans(outcomes)
#' influence = sweep(outcomes, 2, means)
#' framework = DoubleMLFramework$new(
#'   all_coef = matrix(means, ncol = 1),
#'   all_se = matrix(sqrt(colSums(influence^2)) / 40, ncol = 1),
#'   psi = array(influence, c(40, 2, 1)), coef_names = colnames(outcomes)
#' )
#' contrast = matrix(c(-1, 1),
#'   nrow = 1,
#'   dimnames = list("filter - control", colnames(outcomes))
#' )
#' effects = framework$linear_transform(contrast)
#' effects$summary()
#' effects$bootstrap(n_rep_boot = 199)
#' effects$confint(joint = TRUE)
#' @export
DoubleMLFramework = R6Class("DoubleMLFramework",
  active = list(
    #' @field coef (`numeric()`) Aggregated, named estimates.
    coef = function(value) {
      if (missing(value)) {
        return(private$coef_)
      }
      stop("can't set field coef")
    },
    #' @field se (`numeric()`) Aggregated standard errors.
    se = function(value) {
      if (missing(value)) {
        return(private$se_)
      }
      stop("can't set field se")
    },
    #' @field t_stat (`numeric()`) Estimates divided by standard errors.
    t_stat = function(value) {
      if (missing(value)) {
        return(self$coef / self$se)
      }
      stop("can't set field t_stat")
    },
    #' @field pval (`numeric()`) Two-sided normal p-values.
    pval = function(value) {
      if (missing(value)) {
        return(2 * pnorm(-abs(self$t_stat)))
      }
      stop("can't set field pval")
    },
    #' @field all_coef (`matrix()`) Estimand-by-repetition estimates.
    all_coef = function(value) {
      if (missing(value)) {
        return(private$all_coef_)
      }
      stop("can't set field all_coef")
    },
    #' @field all_se (`matrix()`) Estimand-by-repetition standard errors.
    all_se = function(value) {
      if (missing(value)) {
        return(private$all_se_)
      }
      stop("can't set field all_se")
    },
    #' @field psi (`array()`) Observation-by-estimand-by-repetition centered
    #' influence contributions, already scaled by the score derivative.
    psi = function(value) {
      if (missing(value)) {
        return(private$psi_)
      }
      stop("can't set field psi")
    },
    #' @field all_cov (`array()`) Estimand-by-estimand-by-repetition covariance.
    all_cov = function(value) {
      if (missing(value)) {
        return(private$all_cov_)
      }
      stop("can't set field all_cov")
    },
    #' @field boot_coef (`matrix()` or `NULL`) Bootstrap perturbations, initially unset.
    boot_coef = function(value) {
      if (missing(value)) {
        return(private$boot_coef_)
      }
      stop("can't set field boot_coef")
    },
    #' @field boot_t_stat (`matrix()` or `NULL`) Bootstrap t-statistics, initially unset.
    boot_t_stat = function(value) {
      if (missing(value)) {
        return(private$boot_t_stat_)
      }
      stop("can't set field boot_t_stat")
    },
    #' @field sensitivity_elements (`list()` or `NULL`) Canonical sensitivity
    #' components, when available.
    sensitivity_elements = function(value) {
      if (missing(value)) return(private$sensitivity_elements_)
      stop("can't set field sensitivity_elements")
    },
    #' @field sensitivity_params (`list()` or `NULL`) Most recently evaluated
    #' sensitivity scenario.
    sensitivity_params = function(value) {
      if (missing(value)) return(private$sensitivity_params_)
      stop("can't set field sensitivity_params")
    },
    #' @field sensitivity_summary (`character(1)`) Formatted sensitivity results.
    sensitivity_summary = function(value) {
      if (missing(value)) return(private$format_sensitivity_summary())
      stop("can't set field sensitivity_summary")
    }
  ),
  public = list(
    #' @description Create a joint inference object.
    #' @param all_coef (`matrix()`) Finite estimates, estimand by repetition.
    #' @param all_se (`matrix()`) Finite, nonnegative standard errors with the
    #' same dimensions as `all_coef`.
    #' @param psi (`array()`) Finite centered influence contributions, observation
    #' by estimand by repetition. For no cross-fitting, only evaluation rows are
    #' used for covariance; other rows must still be finite.
    #' @param coef_names (`character()`) Unique, nonempty estimand names.
    #' @param smpls (`list()`) Optional synchronized splits, one entry per
    #' repetition, each containing lists `train_ids` and `test_ids`.
    #' @param apply_cross_fitting (`logical(1)`) Whether all observations are
    #' evaluation observations. Without cross-fitting, splits are required and
    #' each repetition must have exactly one evaluation set.
    #' @param sensitivity_elements (`list()`, `NULL`) Optional canonical list
    #' containing `sigma2`, `nu2`, `psi_sigma2`, `psi_nu2`, `riesz_rep`,
    #' `max_bias`, and `psi_max_bias` with dimensions matching the framework.
    #' @param sensitivity_unavailable_reason (`character(1)`, `NULL`) Optional
    #' concrete error message used when sensitivity elements are unavailable.
    #' @return A new `DoubleMLFramework` object.
    initialize = function(all_coef, all_se, psi, coef_names, smpls = NULL,
                          apply_cross_fitting = TRUE, sensitivity_elements = NULL,
                          sensitivity_unavailable_reason = NULL) {
      private$check_matrix(all_coef, "all_coef")
      private$check_matrix(all_se, "all_se")
      if (!identical(dim(all_coef), dim(all_se))) {
        stop("all_coef and all_se dimensions must match.")
      }
      if (any(all_se < 0)) stop("all_se must be nonnegative.")
      if (!is.numeric(psi) || length(dim(psi)) != 3L || any(dim(psi) < 1L)) {
        stop("psi must be a nonempty numeric three-dimensional array.")
      }
      if (any(!is.finite(psi))) stop("psi must contain only finite values.")
      if (!identical(dim(psi)[2:3], dim(all_coef))) {
        stop("psi dimensions must match the estimands and repetitions in all_coef.")
      }
      private$check_names(coef_names, nrow(all_coef))
      for (labels in list(rownames(all_coef), rownames(all_se), dimnames(psi)[[2]])) {
        if (!is.null(labels) && !identical(labels, coef_names)) {
          stop("Estimand names must agree with coef_names in order.")
        }
      }
      rep_names = list(colnames(all_coef), colnames(all_se), dimnames(psi)[[3]])
      rep_names = Filter(Negate(is.null), rep_names)
      if (length(rep_names) > 1L &&
        !all(vapply(rep_names, identical, logical(1), rep_names[[1]]))) {
        stop("Repetition names must agree in order.")
      }
      assert_flag(apply_cross_fitting)
      private$apply_cross_fitting_ = apply_cross_fitting
      private$smpls_ = private$check_splits(smpls, dim(psi)[1], ncol(all_coef))
      rownames(all_coef) = rownames(all_se) = coef_names
      dimnames(psi)[[2]] = coef_names
      if (length(rep_names)) {
        colnames(all_coef) = colnames(all_se) = rep_names[[1]]
        dimnames(psi)[[3]] = rep_names[[1]]
      }
      if (!is.null(sensitivity_unavailable_reason)) {
        if (!is.character(sensitivity_unavailable_reason) ||
          length(sensitivity_unavailable_reason) != 1L ||
          is.na(sensitivity_unavailable_reason) ||
          !nzchar(trimws(sensitivity_unavailable_reason))) {
          stop("sensitivity_unavailable_reason must be one nonempty string or NULL.")
        }
      }
      if (!is.null(sensitivity_elements) && !is.null(sensitivity_unavailable_reason)) {
        stop("Supply sensitivity_elements or sensitivity_unavailable_reason, not both.")
      }
      private$all_coef_ = all_coef
      private$all_se_ = all_se
      private$psi_ = psi
      private$all_cov_ = private$covariance(psi)
      private$coef_ = apply(all_coef, 1, median)
      # Use the common observation dimension for repeated-estimate aggregation;
      # covariance below uses each repetition's actual evaluation sample size.
      n_obs = dim(psi)[1]
      private$se_ = sqrt(apply(
        n_obs * all_se^2 + (all_coef - self$coef)^2,
        1, median
      ) / n_obs)
      private$sensitivity_elements_ = if (is.null(sensitivity_elements)) {
        NULL
      } else {
        validate_sensitivity_elements(
          sensitivity_elements, n_obs, nrow(all_coef), ncol(all_coef),
          coef_names, if (length(rep_names)) rep_names[[1]] else NULL
        )
      }
      private$sensitivity_unavailable_reason_ = sensitivity_unavailable_reason
      private$sensitivity_params_ = NULL
      invisible(self)
    },
    #' @description Print an estimates and significance table.
    #' @param digits (`integer(1)`) Significant digits used for printing.
    #' @return `summary()` invisibly returns the printed matrix. `print()`
    #' invisibly returns the framework.
    summary = function(digits = max(3L, getOption("digits") - 3L)) {
      table = cbind(
        "Estimate." = self$coef, "Std. Error" = self$se,
        "t value" = self$t_stat, "Pr(>|t|)" = self$pval
      )
      printCoefmat(table, digits = digits, P.values = TRUE, has.Pvalue = TRUE)
      invisible(table)
    },
    #' @description Print the framework and its summary.
    #' @return Invisibly returns the framework.
    print = function() {
      cat("================== DoubleMLFramework ==================\n")
      self$summary()
      invisible(self)
    },
    #' @description Multiplier bootstrap with common weights across estimands.
    #' @param method (`character(1)`) One of `"normal"`, `"Bayes"`, or `"wild"`.
    #' @param n_rep_boot (`integer(1)`) Positive number of bootstrap replications.
    #' @return Invisibly returns the framework. Bootstrap matrices have one row
    #' per estimand and `n_rep_boot` columns per repetition, in repetition order.
    bootstrap = function(method = "normal", n_rep_boot = 500) {
      assert_choice(method, c("normal", "Bayes", "wild"))
      assert_count(n_rep_boot, positive = TRUE)
      n_rep = ncol(self$all_coef)
      boot_coef = boot_t_stat = matrix(NA_real_, length(self$coef), n_rep_boot * n_rep,
        dimnames = list(names(self$coef), NULL)
      )
      for (i_rep in seq_len(n_rep)) {
        ids = if (private$apply_cross_fitting_) {
          seq_len(dim(self$psi)[1])
        } else {
          private$smpls_[[i_rep]]$test_ids[[1]]
        }
        weights = draw_weights(method, n_rep_boot, length(ids))
        contributions = matrix(self$psi[ids, , i_rep, drop = FALSE], nrow = length(ids))
        columns = (i_rep - 1) * n_rep_boot + seq_len(n_rep_boot)
        perturbations = t(weights %*% contributions) / length(ids)
        boot_coef[, columns] = perturbations
        boot_t_stat[, columns] = perturbations / self$all_se[, i_rep]
      }
      private$boot_coef_ = boot_coef
      private$boot_t_stat_ = boot_t_stat
      invisible(self)
    },
    #' @description Compute marginal normal or joint bootstrap confidence intervals.
    #' @param parm (`numeric()` or `character()`) Parameter positions or names.
    #' Defaults to all parameters.
    #' @param joint (`logical(1)`) Use a simultaneous critical value over all
    #' framework estimands. Requires `bootstrap()` even when selecting parameters.
    #' @param level (`numeric(1)`) Confidence level strictly between zero and one.
    #' @return A matrix with named rows and confidence-bound percentage columns.
    confint = function(parm, joint = FALSE, level = 0.95) {
      assert_flag(joint)
      assert_number(level, finite = TRUE)
      if (level <= 0 || level >= 1) stop("'level' must be > 0 and < 1.")
      if (missing(parm)) {
        parm = names(self$coef)
      } else if (is.numeric(parm) && is.null(dim(parm))) {
        if (anyNA(parm) || any(!is.finite(parm)) || any(parm != floor(parm)) ||
          any(parm < 1 | parm > length(self$coef)) || length(parm) > length(self$coef)) {
          stop("parm must contain valid parameter positions.")
        }
        parm = names(self$coef)[parm]
      } else if (!is.character(parm) || !is.null(dim(parm)) || anyNA(parm) ||
        any(!parm %in% names(self$coef)) || length(parm) > length(self$coef)) {
        stop("parm must contain valid parameter names or positions.")
      }
      probabilities = c((1 - level) / 2, (1 + level) / 2)
      critical = if (joint) {
        if (is.null(self$boot_t_stat)) stop("Apply bootstrap() before joint confint().")
        unname(quantile(apply(abs(self$boot_t_stat), 2, max), probs = level))
      } else {
        qnorm(probabilities[2])
      }
      ci = cbind(
        self$coef[parm] - critical * self$se[parm],
        self$coef[parm] + critical * self$se[parm]
      )
      dimnames(ci) = list(parm, format_perc(probabilities, 3))
      ci
    },
    #' @description Adjust p-values with Romano-Wolf stepdown or `stats::p.adjust()`.
    #' @param method (`character(1)`) `"romano-wolf"` (or `"rw"`), requiring a
    #' bootstrap, or any method supported by `stats::p.adjust()`.
    #' @param return_matrix (`logical(1)`) Return estimates and adjusted p-values
    #' as a matrix rather than a numeric vector.
    #' @return Adjusted p-values in original estimand order, optionally in a
    #' matrix with columns `"Estimate."` and `"pval"`.
    p_adjust = function(method = "romano-wolf", return_matrix = TRUE) {
      assert_string(method)
      assert_flag(return_matrix)
      if (tolower(method) %in% c("rw", "romano-wolf")) {
        if (is.null(self$boot_t_stat)) stop("Apply bootstrap() before Romano-Wolf p_adjust().")
        ordered = order(abs(self$t_stat), decreasing = TRUE)
        raw = numeric(length(ordered))
        for (i_coef in seq_along(ordered)) {
          remaining = ordered[i_coef:length(ordered)]
          maxima = apply(abs(self$boot_t_stat[remaining, , drop = FALSE]), 2, max)
          raw[i_coef] = mean(maxima > abs(self$t_stat[ordered[i_coef]]))
        }
        p_val = cummax(raw)[order(ordered)]
      } else {
        assert_choice(method, p.adjust.methods)
        p_val = p.adjust(self$pval, method = method, n = length(self$coef))
      }
      if (return_matrix) {
        result = cbind(self$coef, p_val)
        colnames(result) = c("Estimate.", "pval")
        result
      } else {
        p_val
      }
    },
    #' @description Evaluate omitted-confounding sensitivity bounds.
    #' @param cf_y (`numeric(1)`) Share of residual outcome variation explained
    #' by omitted confounding, in `[0, 1)`.
    #' @param cf_d (`numeric(1)`) Proportional gain in Riesz-representer
    #' variation due to omitted confounding, in `[0, 1)`.
    #' @param rho (`numeric(1)`) Alignment between outcome and Riesz nuisance
    #' errors, in `[-1, 1]`; only its absolute value changes two-sided bounds.
    #' @param level (`numeric(1)`) One-sided confidence level in `(0, 1)`.
    #' @param null_hypothesis (`numeric()`) One null per estimand, or one shared null.
    #' @return Invisibly returns the framework and stores results in
    #' `sensitivity_params`.
    sensitivity_analysis = function(cf_y = 0.03, cf_d = 0.03, rho = 1,
                                    level = 0.95, null_hypothesis = 0) {
      if (is.null(self$sensitivity_elements)) {
        reason = private$sensitivity_unavailable_reason_
        if (is.null(reason)) reason = "Sensitivity analysis is not available for this framework."
        stop(reason)
      }
      private$check_sensitivity_scalar(cf_y, "cf_y", 0, 1, include_lower = TRUE)
      private$check_sensitivity_scalar(cf_d, "cf_d", 0, 1, include_lower = TRUE)
      private$check_sensitivity_scalar(
        rho, "rho", -1, 1, include_lower = TRUE, include_upper = TRUE
      )
      private$check_sensitivity_scalar(level, "level", 0, 1)
      null_hypothesis = private$check_null_hypothesis(null_hypothesis)

      result = private$calculate_sensitivity(cf_y, cf_d, rho, level)
      rv = rva = numeric(length(self$coef))
      for (i_coef in seq_along(self$coef)) {
        rv[i_coef] = private$robustness_value(
          null_hypothesis[i_coef], level, rho, i_coef, "theta"
        )
        rva[i_coef] = private$robustness_value(
          null_hypothesis[i_coef], level, rho, i_coef, "ci"
        )
      }
      names(rv) = names(rva) = names(self$coef)
      result$rv = rv
      result$rva = rva
      result$input = list(
        cf_y = cf_y, cf_d = cf_d, rho = rho, level = level,
        null_hypothesis = null_hypothesis
      )
      private$sensitivity_params_ = result
      invisible(self)
    },
    #' @description Apply linear contrasts jointly to each repetition.
    #' @param contrast (`matrix()`) Finite K-by-J numeric matrix. Named columns
    #' are reordered to match the framework's estimand names.
    #' @param coef_names (`character()`) Optional output names. Defaults to
    #' contrast row names, or `contrast1`, ..., `contrastK`.
    #' @param sensitivity_elements (`list()`, `NULL`) Optional independently
    #' derived canonical sensitivity elements for the transformed estimands.
    #' @param sensitivity_unavailable_reason (`character(1)`, `NULL`) Optional
    #' concrete reason that sensitivity is unavailable after the transform.
    #' @return A new `DoubleMLFramework` with transformed influence contributions
    #' and standard errors recomputed from their joint covariance.
    linear_transform = function(contrast, coef_names = NULL,
                                sensitivity_elements = NULL,
                                sensitivity_unavailable_reason = NULL) {
      private$check_matrix(contrast, "contrast")
      if (ncol(contrast) != length(self$coef)) stop("contrast has incompatible dimensions.")
      if (!is.null(colnames(contrast))) {
        labels = colnames(contrast)
        if (anyDuplicated(labels) || !setequal(labels, names(self$coef))) {
          stop("contrast column names must match the estimand names uniquely.")
        }
        contrast = contrast[, names(self$coef), drop = FALSE]
      }
      if (is.null(coef_names)) coef_names = rownames(contrast)
      if (is.null(coef_names)) coef_names = paste0("contrast", seq_len(nrow(contrast)))
      private$check_names(coef_names, nrow(contrast))
      transformed_coef = contrast %*% self$all_coef
      rownames(transformed_coef) = coef_names
      psi = array(0, c(dim(self$psi)[1], nrow(contrast), ncol(self$all_coef)),
        dimnames = list(dimnames(self$psi)[[1]], coef_names, colnames(self$all_coef))
      )
      for (i_rep in seq_len(ncol(self$all_coef))) {
        psi[, , i_rep] = matrix(self$psi[, , i_rep], nrow = dim(self$psi)[1]) %*% t(contrast)
      }
      cov = private$covariance(psi)
      se = matrix(0, nrow(contrast), ncol(self$all_coef),
        dimnames = list(coef_names, colnames(self$all_coef))
      )
      for (i_rep in seq_len(ncol(se))) {
        se[, i_rep] = sqrt(diag(matrix(cov[, , i_rep], nrow = nrow(contrast))))
      }
      if (is.null(sensitivity_elements) && is.null(sensitivity_unavailable_reason)) {
        sensitivity_unavailable_reason = private$sensitivity_unavailable_reason_
        if (is.null(sensitivity_unavailable_reason)) {
          sensitivity_unavailable_reason = paste(
            "Sensitivity elements are not propagated through generic linear_transform();",
            "supply independently derived elements."
          )
        }
      }
      DoubleMLFramework$new(transformed_coef, se, psi, coef_names,
        smpls = private$smpls_, apply_cross_fitting = private$apply_cross_fitting_,
        sensitivity_elements = sensitivity_elements,
        sensitivity_unavailable_reason = sensitivity_unavailable_reason
      )
    },
    #' @description Concatenate compatible frameworks, retaining cross-covariance.
    #' @param other (`DoubleMLFramework`) Framework with synchronized observations,
    #' repetitions and splits, and disjoint estimand names.
    #' @return A new joint `DoubleMLFramework`.
    concat = function(other) {
      if (!inherits(other, "DoubleMLFramework")) stop("other must be a DoubleMLFramework.")
      if (ncol(self$all_coef) != ncol(other$all_coef)) stop("Incompatible repetition counts.")
      if (dim(self$psi)[1] != dim(other$psi)[1]) stop("Incompatible observation counts.")
      other_private = other$.__enclos_env__$private
      if (!identical(private$smpls_, other_private$smpls_) ||
        !identical(private$apply_cross_fitting_, other_private$apply_cross_fitting_)) {
        stop("Incompatible sample splits or cross-fitting settings.")
      }
      if (anyDuplicated(c(names(self$coef), names(other$coef)))) {
        stop("Concatenated coefficient names must be unique.")
      }
      obs_names = list(dimnames(self$psi)[[1]], dimnames(other$psi)[[1]])
      if (!identical(obs_names[[1]], obs_names[[2]])) stop("Incompatible observation names.")
      rep_names = list(colnames(self$all_coef), colnames(other$all_coef))
      if (!identical(rep_names[[1]], rep_names[[2]])) stop("Incompatible repetition names.")
      psi = array(0, c(
        dim(self$psi)[1], length(self$coef) + length(other$coef),
        ncol(self$all_coef)
      ))
      left = seq_along(self$coef)
      right = length(left) + seq_along(other$coef)
      psi[, left, ] = self$psi
      psi[, right, ] = other$psi
      dimnames(psi) = list(obs_names[[1]], NULL, rep_names[[1]])
      sensitivity_elements = NULL
      sensitivity_unavailable_reason = NULL
      if (!is.null(private$sensitivity_elements_) &&
        !is.null(other_private$sensitivity_elements_)) {
        sensitivity_elements = private$concat_sensitivity_elements(
          private$sensitivity_elements_, other_private$sensitivity_elements_
        )
      } else {
        reasons = Filter(Negate(is.null), list(
          private$sensitivity_unavailable_reason_,
          other_private$sensitivity_unavailable_reason_
        ))
        sensitivity_unavailable_reason = if (length(reasons)) {
          reasons[[1]]
        } else {
          "Sensitivity analysis is not available for all concatenated estimands."
        }
      }
      DoubleMLFramework$new(rbind(self$all_coef, other$all_coef),
        rbind(self$all_se, other$all_se), psi, c(names(self$coef), names(other$coef)),
        smpls = private$smpls_, apply_cross_fitting = private$apply_cross_fitting_,
        sensitivity_elements = sensitivity_elements,
        sensitivity_unavailable_reason = sensitivity_unavailable_reason
      )
    }
  ),
  private = list(
    coef_ = NULL,
    se_ = NULL,
    all_coef_ = NULL,
    all_se_ = NULL,
    psi_ = NULL,
    all_cov_ = NULL,
    boot_coef_ = NULL,
    boot_t_stat_ = NULL,
    smpls_ = NULL,
    apply_cross_fitting_ = NULL,
    sensitivity_elements_ = NULL,
    sensitivity_params_ = NULL,
    sensitivity_unavailable_reason_ = NULL,
    check_matrix = function(x, label) {
      if (!is.matrix(x) || !is.numeric(x) || any(dim(x) < 1L)) {
        stop(paste0(label, " must be a nonempty numeric matrix."))
      }
      if (any(!is.finite(x))) stop(paste0(label, " must contain only finite values."))
    },
    check_names = function(labels, n_coef) {
      if (!is.character(labels) || length(labels) != n_coef || anyNA(labels) ||
        any(!nzchar(trimws(labels))) || anyDuplicated(labels)) {
        stop("coef_names must contain one unique, nonempty name per estimand.")
      }
    },
    check_splits = function(smpls, n_obs, n_rep) {
      if (is.null(smpls)) {
        if (!private$apply_cross_fitting_) stop("smpls are required without cross-fitting.")
        return(NULL)
      }
      if (!is.list(smpls) || length(smpls) != n_rep) {
        stop("smpls must contain one split per repetition.")
      }
      n_folds = NULL
      for (i_rep in seq_len(n_rep)) {
        split = smpls[[i_rep]]
        if (!is.list(split) || !setequal(names(split), c("train_ids", "test_ids")) ||
          length(split) != 2L || !is.list(split$train_ids) || !is.list(split$test_ids) ||
          length(split$test_ids) < 1L || length(split$train_ids) != length(split$test_ids)) {
          stop("Each sample split must contain matching train_ids and test_ids lists.")
        }
        if (is.null(n_folds)) n_folds = length(split$test_ids)
        if (n_folds != length(split$test_ids)) stop("Sample splits must have matching fold counts.")
        if (!private$apply_cross_fitting_ && n_folds != 1L) {
          stop("Without cross-fitting each repetition must have a single evaluation set.")
        }
        for (i_fold in seq_len(n_folds)) {
          for (field in c("train_ids", "test_ids")) {
            ids = split[[field]][[i_fold]]
            if (!is.numeric(ids) || !is.null(dim(ids)) || !length(ids) || anyNA(ids) ||
              any(!is.finite(ids)) || any(ids != floor(ids)) ||
              any(ids < 1 | ids > n_obs) || anyDuplicated(ids)) {
              stop(paste(
                "Sample split indices must be nonempty, unique integers",
                "in the observation range."
              ))
            }
            split[[field]][[i_fold]] = sort(as.integer(ids))
          }
          train = split$train_ids[[i_fold]]
          test = split$test_ids[[i_fold]]
          full_sample = identical(train, seq_len(n_obs)) && identical(test, seq_len(n_obs))
          if (length(intersect(train, test)) && !full_sample) {
            stop("Train and test indices overlap.")
          }
        }
        if (private$apply_cross_fitting_ &&
          !identical(sort(unlist(split$test_ids, use.names = FALSE)), seq_len(n_obs))) {
          stop("Cross-fitting test sets must partition the observations.")
        }
        smpls[[i_rep]] = list(
          train_ids = unname(split$train_ids),
          test_ids = unname(split$test_ids)
        )
      }
      unname(smpls)
    },
    covariance = function(psi) {
      n_obs = dim(psi)[1]
      n_coef = dim(psi)[2]
      cov = array(0, c(n_coef, n_coef, dim(psi)[3]),
        dimnames = list(dimnames(psi)[[2]], dimnames(psi)[[2]], dimnames(psi)[[3]])
      )
      for (i_rep in seq_len(dim(psi)[3])) {
        ids = if (private$apply_cross_fitting_) {
          seq_len(n_obs)
        } else {
          private$smpls_[[i_rep]]$test_ids[[1]]
        }
        contributions = matrix(psi[ids, , i_rep, drop = FALSE], nrow = length(ids))
        cov[, , i_rep] = crossprod(contributions) / length(ids)^2
      }
      cov
    },
    check_sensitivity_scalar = function(value, label, lower, upper,
                                        include_lower = FALSE,
                                        include_upper = FALSE) {
      if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value)) {
        stop(label, " must be one finite numeric value.")
      }
      lower_invalid = if (include_lower) value < lower else value <= lower
      upper_invalid = if (include_upper) value > upper else value >= upper
      if (lower_invalid || upper_invalid) {
        left = if (include_lower) "[" else "("
        right = if (include_upper) "]" else ")"
        stop(label, " must lie in ", left, lower, ", ", upper, right, ".")
      }
      invisible(NULL)
    },
    check_null_hypothesis = function(value) {
      if (!is.numeric(value) || !is.null(dim(value)) || !length(value) ||
        anyNA(value) || any(!is.finite(value)) ||
        !(length(value) %in% c(1L, length(self$coef)))) {
        stop(
          "null_hypothesis must be a finite numeric scalar or contain one value per estimand."
        )
      }
      if (length(value) == 1L) value = rep(value, length(self$coef))
      if (!is.null(names(value))) {
        if (anyDuplicated(names(value)) || !setequal(names(value), names(self$coef))) {
          stop("Named null_hypothesis values must match the estimand names uniquely.")
        }
        value = value[names(self$coef)]
      }
      names(value) = names(self$coef)
      value
    },
    calculate_sensitivity = function(cf_y, cf_d, rho, level) {
      strength = abs(rho) * sqrt(cf_y * cf_d / (1 - cf_d))
      max_bias = self$sensitivity_elements$max_bias
      all_theta_lower = self$all_coef - strength * max_bias
      all_theta_upper = self$all_coef + strength * max_bias
      psi_lower = self$psi - strength * self$sensitivity_elements$psi_max_bias
      psi_upper = self$psi + strength * self$sensitivity_elements$psi_max_bias
      all_se_lower = private$influence_se(psi_lower)
      all_se_upper = private$influence_se(psi_upper)
      lower = private$aggregate_bounds(all_theta_lower, all_se_lower)
      upper = private$aggregate_bounds(all_theta_upper, all_se_upper)
      quantile = qnorm(level)
      ci_lower = apply(all_theta_lower - quantile * all_se_lower, 1, median)
      ci_upper = apply(all_theta_upper + quantile * all_se_upper, 1, median)
      names(ci_lower) = names(ci_upper) = names(self$coef)
      list(
        theta = list(lower = lower$theta, upper = upper$theta),
        se = list(lower = lower$se, upper = upper$se),
        ci = list(lower = ci_lower, upper = ci_upper)
      )
    },
    influence_se = function(psi) {
      result = matrix(0, dim(psi)[2], dim(psi)[3],
        dimnames = list(dimnames(psi)[[2]], dimnames(psi)[[3]])
      )
      for (i_rep in seq_len(dim(psi)[3])) {
        ids = if (private$apply_cross_fitting_) {
          seq_len(dim(psi)[1])
        } else {
          private$smpls_[[i_rep]]$test_ids[[1]]
        }
        values = matrix(psi[ids, , i_rep, drop = FALSE], nrow = length(ids))
        result[, i_rep] = sqrt(colSums(values^2)) / length(ids)
      }
      result
    },
    aggregate_bounds = function(all_theta, all_se) {
      theta = apply(all_theta, 1, median)
      n_obs = dim(self$psi)[1]
      se = sqrt(apply(
        n_obs * all_se^2 + (all_theta - theta)^2,
        1, median
      ) / n_obs)
      names(theta) = names(se) = names(self$coef)
      list(theta = theta, se = se)
    },
    robustness_value = function(null_hypothesis, level, rho, i_coef, quantity) {
      bound = if (null_hypothesis > self$coef[i_coef]) "upper" else "lower"
      value_at = function(strength) {
        private$calculate_sensitivity(strength, strength, rho, level)[[quantity]][[bound]][i_coef]
      }
      reached = function(value) {
        if (bound == "upper") value >= null_hypothesis else value <= null_hypothesis
      }
      if (reached(value_at(0))) return(0)
      upper = 1 - 1e-12
      if (!reached(value_at(upper))) return(1)
      root = uniroot(
        function(strength) value_at(strength) - null_hypothesis,
        lower = 0, upper = upper, tol = 1e-10
      )$root
      min(1, max(0, root))
    },
    format_sensitivity_summary = function() {
      header = "================== Sensitivity Analysis =================="
      if (is.null(private$sensitivity_params_)) {
        return(paste(header, "Apply sensitivity_analysis() to generate sensitivity_summary.",
          sep = "\n"
        ))
      }
      params = private$sensitivity_params_
      bounds = data.frame(
        `CI lower` = params$ci$lower,
        `theta lower` = params$theta$lower,
        theta = self$coef,
        `theta upper` = params$theta$upper,
        `CI upper` = params$ci$upper,
        check.names = FALSE
      )
      robustness = data.frame(
        H_0 = params$input$null_hypothesis,
        `RV (%)` = 100 * params$rv,
        `RVa (%)` = 100 * params$rva,
        check.names = FALSE
      )
      paste(c(
        header,
        "", "------------------ Scenario          ------------------",
        paste0("Significance Level: level=", params$input$level),
        paste0(
          "Sensitivity parameters: cf_y=", params$input$cf_y,
          "; cf_d=", params$input$cf_d, "; rho=", params$input$rho
        ),
        "", "------------------ Bounds with CI    ------------------",
        capture.output(print(bounds)),
        "", "------------------ Robustness Values ------------------",
        capture.output(print(robustness))
      ), collapse = "\n")
    },
    concat_sensitivity_elements = function(left, right) {
      scalar_keys = c("sigma2", "nu2", "max_bias")
      array_keys = c("psi_sigma2", "psi_nu2", "riesz_rep", "psi_max_bias")
      result = vector("list", 7L)
      names(result) = c(
        "sigma2", "nu2", "psi_sigma2", "psi_nu2", "riesz_rep",
        "max_bias", "psi_max_bias"
      )
      for (key in scalar_keys) result[[key]] = rbind(left[[key]], right[[key]])
      for (key in array_keys) {
        value = array(0, c(
          dim(left[[key]])[1],
          dim(left[[key]])[2] + dim(right[[key]])[2],
          dim(left[[key]])[3]
        ))
        left_ids = seq_len(dim(left[[key]])[2])
        right_ids = length(left_ids) + seq_len(dim(right[[key]])[2])
        value[, left_ids, ] = left[[key]]
        value[, right_ids, ] = right[[key]]
        result[[key]] = value
      }
      result
    }
  )
)
