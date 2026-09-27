#' @title Joint inference for DoubleML estimates
#'
#' @description
#' Stores synchronized estimates and centered influence contributions for joint
#' inference, linear contrasts and concatenation across estimands.
#' Covariance is computed separately for each repetition. Repeated estimates
#' use coordinate-wise medians and the same aggregation as DoubleML models.
#'
#' @format [R6::R6Class] object.
#' @family DoubleML
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
    initialize = function(all_coef, all_se, psi, coef_names, smpls = NULL,
                          apply_cross_fitting = TRUE) {
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
    #' @description Apply linear contrasts jointly to each repetition.
    #' @param contrast (`matrix()`) Finite K-by-J numeric matrix. Named columns
    #' are reordered to match the framework's estimand names.
    #' @param coef_names (`character()`) Optional output names. Defaults to
    #' contrast row names, or `contrast1`, ..., `contrastK`.
    #' @return A new `DoubleMLFramework` with transformed influence contributions
    #' and standard errors recomputed from their joint covariance.
    linear_transform = function(contrast, coef_names = NULL) {
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
      DoubleMLFramework$new(transformed_coef, se, psi, coef_names,
        smpls = private$smpls_, apply_cross_fitting = private$apply_cross_fitting_
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
      DoubleMLFramework$new(rbind(self$all_coef, other$all_coef),
        rbind(self$all_se, other$all_se), psi, c(names(self$coef), names(other$coef)),
        smpls = private$smpls_, apply_cross_fitting = private$apply_cross_fitting_
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
    }
  )
)
