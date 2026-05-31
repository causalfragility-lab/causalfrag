# =============================================================================
# CFI Extensions:
#   1. compute_cfi_weighted() -- discipline-free weight presets
#   2. compute_cfi_benchmarked() -- benchmark-adjusted CFI
#   3. compare_cfi() -- multi-treatment comparison table
# =============================================================================


#' Compute CFI with user-defined or preset weights
#'
#' @description
#' Extends \code{compute_cfi()} with named weight presets and user-defined
#' weights. Different sensitivity frameworks carry different evidential weight
#' depending on the study design and analyst priorities.
#'
#' @section Weight presets:
#' \describe{
#'   \item{balanced}{Equal weight to all available components (default).}
#'   \item{rv_priority}{Double weight on both Robustness Value components.
#'     Use when the partial R-squared framework is most relevant.}
#'   \item{evalue_priority}{Double weight on E-value. Use when risk-ratio
#'     framing is most natural (e.g. binary outcomes, relative risks).}
#'   \item{itcv_priority}{Double weight on percent bias (ITCV/konfound).
#'     Use when case-replacement interpretability is prioritised.}
#' }
#'
#' @note Presets are transparent defaults, not discipline-specific claims.
#'   Present chosen weights explicitly when reporting results.
#'
#' @param x A \code{sens_results} object from \code{run_sensitivity()}.
#' @param weights Character preset or named numeric vector.
#'   Character: one of \code{"balanced"}, \code{"rv_priority"},
#'   \code{"evalue_priority"}, \code{"itcv_priority"}.
#'   Numeric: named vector with any subset of
#'   \code{c("rv", "rv_alpha", "evalue", "pct_bias")}.
#'
#' @return The input \code{sens_results} object with \code{$cfi} updated.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("sensemakr", quietly = TRUE)) {
#'   data("darfur", package = "sensemakr")
#'   fit <- lm(peacefactor ~ directlyharmed + age + female, data = darfur)
#'   res <- run_sensitivity(fit, treatment = "directlyharmed", data = darfur)
#'   res <- compute_cfi_weighted(res, weights = "rv_priority")
#'   print_cfi(res)
#' }
#' }
#'
#' @export
compute_cfi_weighted <- function(x, weights = "balanced") {

  if (!is_sens_results(x)) {
    rlang::abort("`x` must be a `sens_results` object.")
  }

  w <- .resolve_weights(weights)
  x <- compute_cfi(x, weights = w)

  # record preset name in cfi object
  x$cfi$weight_preset <- if (is.character(weights)) weights else "custom"
  x
}


#' Compute benchmark-adjusted CFI
#'
#' @description
#' Extends the CFI with benchmark-relative interpretation. Rather than
#' assessing fragility against absolute thresholds, the benchmark-adjusted
#' CFI compares the required confounding strength to the observed association
#' of a named covariate with treatment and outcome.
#'
#' This answers: "Would an omitted confounder need to be stronger than
#' \code{benchmark_covariate} to overturn the conclusion?"
#'
#' @param x A \code{sens_results} object with sensemakr results.
#' @param benchmark_covariate Character. Name of the covariate to benchmark
#'   against. Must be a covariate in the original model.
#' @param kd Numeric. Multiplier for treatment partial R-squared in benchmark.
#'   Default 1 (benchmark at 1x covariate strength).
#'
#' @return The input \code{sens_results} object with \code{$cfi_benchmarked}
#'   populated as a named list with elements:
#'   \describe{
#'     \item{rv_q_benchmark}{Ratio: RV / benchmark partial R-squared (treat).}
#'     \item{rv_qa_benchmark}{Same for significance threshold.}
#'     \item{benchmark_label}{Plain-language benchmark statement.}
#'     \item{exceeds_benchmark}{Logical. Does required confounding exceed
#'       the benchmark covariate's strength?}
#'   }
#'
#' @examples
#' \donttest{
#' if (requireNamespace("sensemakr", quietly = TRUE)) {
#'   data("darfur", package = "sensemakr")
#'   fit <- lm(peacefactor ~ directlyharmed + age + female + village,
#'             data = darfur)
#'   res <- run_sensitivity(fit, treatment = "directlyharmed", data = darfur,
#'                          benchmark_covariates = "female")
#'   res <- compute_cfi_benchmarked(res, benchmark_covariate = "female")
#'   cat(res$cfi_benchmarked$benchmark_label)
#' }
#' }
#'
#' @export
compute_cfi_benchmarked <- function(x,
                                    benchmark_covariate,
                                    kd = 1) {

  if (!is_sens_results(x)) {
    rlang::abort("`x` must be a `sens_results` object.")
  }
  if (!"sensemakr" %in% x$frameworks) {
    rlang::abort(
      "Benchmark-adjusted CFI requires sensemakr results. ",
      "Run `run_sensitivity()` with frameworks including 'sensemakr'."
    )
  }

  sm_raw <- x$results$sensemakr$raw_object
  if (is.null(sm_raw)) {
    rlang::abort("No sensemakr raw object found. Re-run `run_sensitivity()`.")
  }

  # extract benchmark bounds from sensemakr object
  bounds <- tryCatch(
    sm_raw$bounds,
    error = function(e) NULL
  )

  if (is.null(bounds) || nrow(bounds) == 0L) {
    rlang::abort(
      glue::glue(
        "No benchmark bounds found. Re-run `run_sensitivity()` with ",
        "`benchmark_covariates = '{benchmark_covariate}'`."
      )
    )
  }

  # find the matching benchmark row
  bm_row <- bounds[grepl(benchmark_covariate, bounds[["bound_label"]],
                          ignore.case = TRUE), ]

  if (nrow(bm_row) == 0L) {
    rlang::abort(glue::glue(
      "Benchmark covariate '{benchmark_covariate}' not found in bounds table. ",
      "Available: {paste(bounds$bound_label, collapse = ', ')}"
    ))
  }

  # partial R2 of benchmark covariate with treatment
  # guard against data.frame column returning numeric(0)
  .safe_r2 <- function(col) {
    v <- tryCatch(as.numeric(col[[1L]]), error = function(e) NA_real_)
    if (length(v) == 0L || is.na(v)) NA_real_ else v
  }
  # sensemakr uses dots in column names: r2dz.x and r2yz.dx
  r2dz_bench <- .safe_r2(bm_row[["r2dz.x"]])
  r2yz_bench <- .safe_r2(bm_row[["r2yz.dx"]])

  if (is.na(r2dz_bench)) {
    rlang::abort(glue::glue(
      "Could not extract partial R2 for benchmark '{benchmark_covariate}'. ",
      "Check that `benchmark_covariates` was set in `run_sensitivity()`."
    ))
  }

  rv_q  <- x$results$sensemakr$rv_q
  rv_qa <- x$results$sensemakr$rv_qa

  # benchmark ratio: how many times stronger than benchmark is required?
  ratio_point <- if (!is.null(rv_q)  && !is.na(rv_q)  && r2dz_bench > 0)
    round(rv_q  / r2dz_bench, 2) else NA_real_
  ratio_alpha <- if (!is.null(rv_qa) && !is.na(rv_qa) && r2dz_bench > 0)
    round(rv_qa / r2dz_bench, 2) else NA_real_

  exceeds <- !is.na(ratio_point) && ratio_point > kd

  # plain-language label
  bench_label <- if (!is.na(ratio_point)) {
    comparison <- if (ratio_point > 1.5) {
      glue::glue("stronger than {round(ratio_point, 1)}x the strength of `{benchmark_covariate}`")
    } else if (ratio_point > 0.8) {
      glue::glue("approximately as strong as `{benchmark_covariate}`")
    } else {
      glue::glue("weaker than `{benchmark_covariate}` ({round(ratio_point, 2)}x its strength)")
    }

    sig_str <- if (!is.na(ratio_alpha)) {
      glue::glue(" For statistical significance, confounding {round(ratio_alpha, 2)}x the strength of `{benchmark_covariate}` would suffice.")
    } else ""

    glue::glue(
      "To reduce the estimated effect to zero, an omitted confounder would need ",
      "to be {comparison}.{sig_str}"
    )
  } else {
    "Benchmark comparison not available."
  }

  x$cfi_benchmarked <- list(
    benchmark_covariate = benchmark_covariate,
    r2dz_bench          = r2dz_bench,
    r2yz_bench          = r2yz_bench,
    rv_q_ratio          = ratio_point,
    rv_qa_ratio         = ratio_alpha,
    exceeds_benchmark   = exceeds,
    benchmark_label     = as.character(bench_label)
  )

  if (is.null(x$cfi)) x <- compute_cfi(x)

  # update CFI interpretation to include benchmark context
  x$cfi$interpretation <- paste0(
    x$cfi$interpretation, " ",
    bench_label
  )

  x
}


#' Compare CFI across multiple treatment effects
#'
#' @description
#' Produces a side-by-side comparison table of CFI scores and fragility
#' classifications across two or more \code{sens_results} objects. Useful
#' for comparing policy interventions or treatment arms.
#'
#' @param ... Two or more named \code{sens_results} objects.
#'   Names become the row labels in the output table.
#'
#' @return A data frame with one row per treatment and columns:
#'   \code{treatment}, \code{outcome}, \code{cfi}, \code{label},
#'   \code{point_fragility}, \code{significance_fragility},
#'   \code{rv_q}, \code{evalue}, \code{pct_bias}.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("sensemakr", quietly = TRUE)) {
#'   data("darfur", package = "sensemakr")
#'
#'   fit_a <- lm(peacefactor ~ directlyharmed + age + female, data = darfur)
#'   fit_b <- lm(peacefactor ~ herder_dar    + age + female, data = darfur)
#'
#'   res_a <- run_sensitivity(fit_a, "directlyharmed", darfur, verbose = FALSE)
#'   res_b <- run_sensitivity(fit_b, "herder_dar",     darfur, verbose = FALSE)
#'
#'   res_a <- flag_fragility(res_a)
#'   res_b <- flag_fragility(res_b)
#'
#'   compare_cfi(
#'     "Directly harmed" = res_a,
#'     "Herder"          = res_b
#'   )
#' }
#' }
#'
#' @export
compare_cfi <- function(...) {

  args <- list(...)

  if (length(args) < 2L) {
    rlang::abort("`compare_cfi()` requires at least two `sens_results` objects.")
  }

  # use names if provided, else generate labels
  if (is.null(names(args))) {
    names(args) <- paste0("Treatment_", seq_along(args))
  }

  rows <- lapply(names(args), function(nm) {
    x <- args[[nm]]
    if (!is_sens_results(x)) {
      rlang::warn(glue::glue("'{nm}' is not a sens_results object. Skipping."))
      return(NULL)
    }
    if (is.null(x$cfi)) x <- compute_cfi(x)

    data.frame(
      treatment            = nm,
      outcome              = x$outcome,
      cfi                  = round(x$cfi$score, 1),
      label                = x$cfi$label,
      point_fragility      = x$fragility$point  %||% NA_character_,
      signif_fragility     = x$fragility$alpha  %||% NA_character_,
      rv_q                 = round(x$results$sensemakr$rv_q  %||% NA_real_, 3),
      evalue               = round(x$results$evalue$evalue   %||% NA_real_, 3),
      pct_bias             = round(x$results$itcv$pct_bias   %||% NA_real_, 1),
      stringsAsFactors     = FALSE
    )
  })

  out <- do.call(rbind, Filter(Negate(is.null), rows))
  rownames(out) <- NULL

  # print formatted table
  cat("\n=== CFI Comparison Table ===\n\n")
  cat(sprintf("%-22s  %5s  %-20s  %-20s  %-20s\n",
              "Treatment", "CFI", "Label", "Point fragility", "Signif fragility"))
  cat(paste(rep("-", 92), collapse = ""), "\n")
  for (i in seq_len(nrow(out))) {
    cat(sprintf("%-22s  %5.1f  %-20s  %-20s  %-20s\n",
                out$treatment[i], out$cfi[i], out$label[i],
                out$point_fragility[i], out$signif_fragility[i]))
  }
  cat("\n")

  invisible(out)
}


# ==============================================================================
# Internal: weight preset resolver
# ==============================================================================

.resolve_weights <- function(weights) {
  if (is.numeric(weights)) return(weights)

  presets <- list(
    balanced      = c(rv = 1.0, rv_alpha = 1.0, evalue = 1.0, pct_bias = 1.0),
    rv_priority   = c(rv = 2.0, rv_alpha = 2.0, evalue = 1.0, pct_bias = 1.0),
    evalue_priority = c(rv = 1.0, rv_alpha = 1.0, evalue = 2.0, pct_bias = 1.0),
    itcv_priority = c(rv = 1.0, rv_alpha = 1.0, evalue = 1.0, pct_bias = 2.0)
  )

  if (!weights %in% names(presets)) {
    rlang::abort(glue::glue(
      "`weights` must be one of: {paste(names(presets), collapse = ', ')}. ",
      "Got: '{weights}'"
    ))
  }

  presets[[weights]]
}
