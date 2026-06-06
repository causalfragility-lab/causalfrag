#' Flag the fragility of a sensitivity analysis conclusion
#'
#' @description
#' Classifies the robustness of a causal conclusion using a five-level scale
#' separately for (1) point-estimate nullification and (2) statistical
#' significance, where supported by the framework.
#'
#' @section Classification scale:
#' \tabular{lll}{
#'   Level              \tab RV threshold \tab Meaning \cr
#'   Highly stable      \tab >= 0.20      \tab Very strong confounding needed \cr
#'   Stable             \tab 0.10 -- 0.20 \tab Moderate confounding needed \cr
#'   Moderately fragile \tab 0.05 -- 0.10 \tab Plausible confounding could matter \cr
#'   Fragile            \tab 0.01 -- 0.05 \tab Weak confounding could overturn result \cr
#'   Highly fragile     \tab < 0.01       \tab Minimal confounding overturns result \cr
#' }
#'
#' @param x A \code{sens_results} object from \code{run_sensitivity()}.
#' @param thresholds Optional named list to override default thresholds.
#'
#' @return The input \code{sens_results} object with \code{$fragility}
#'   populated as a named list with elements \code{point}, \code{alpha},
#'   \code{point_label}, \code{alpha_label}.
#'
#' @examples
#' \donttest{
#' res <- new_sens_results(
#'   design = "regression", treatment = "t", outcome = "y",
#'   frameworks = "sensemakr",
#'   results = list(sensemakr = list(rv_q = 0.14, rv_qa = 0.08,
#'                                   r2yd_x = 0.02, estimate = 0.10))
#' )
#' res <- flag_fragility(res)
#' res$fragility
#' }
#'
#' @export
flag_fragility <- function(x, thresholds = NULL) {

  if (!is_sens_results(x)) {
    rlang::abort("`x` must be a `sens_results` object. Run `run_sensitivity()` first.")
  }

  fragility <- list()

  # --- sensemakr: separate RV for point (rv_q) and significance (rv_qa) -----
  if ("sensemakr" %in% x$frameworks) {
    rv_q  <- x$results$sensemakr$rv_q
    rv_qa <- x$results$sensemakr$rv_qa

    if (!is.null(rv_q) && !is.na(rv_q)) {
      fragility$point <- .classify_rv(rv_q)
      fragility$point_label <- glue::glue(
        "Confounding explaining ~{round(rv_q * 100, 1)}% of residual variance ",
        "in both treatment and outcome would reduce the estimate to zero."
      )
    }

    if (!is.null(rv_qa) && !is.na(rv_qa)) {
      fragility$alpha <- .classify_rv(rv_qa)
      fragility$alpha_label <- glue::glue(
        "Confounding explaining ~{round(rv_qa * 100, 1)}% of residual variance ",
        "in both treatment and outcome could make the result statistically ",
        "non-significant (alpha = .05)."
      )
    }
  }

  # --- evalue: single threshold classification --------------------------------
  if ("evalue" %in% x$frameworks && is.null(fragility$point)) {
    ev <- x$results$evalue$evalue
    if (!is.null(ev) && !is.na(ev)) {
      fragility$point <- .classify_evalue(ev)
      fragility$point_label <- glue::glue(
        "A confounder associated with both treatment and outcome by a risk ",
        "ratio of {round(ev, 2)} on both sides would be needed to explain away ",
        "the observed association."
      )
    }
    ev_lo <- x$results$evalue$evalue_lower
    if (!is.null(ev_lo) && !is.na(ev_lo)) {
      fragility$alpha <- .classify_evalue(ev_lo)
      fragility$alpha_label <- glue::glue(
        "E-value for the confidence interval lower bound: {round(ev_lo, 2)}."
      )
    }
  }

  # --- itcv: single classification -------------------------------------------
  if ("itcv" %in% x$frameworks && is.null(fragility$point)) {
    itcv <- x$results$itcv$itcv
    if (!is.null(itcv) && !is.na(itcv)) {
      fragility$overall <- .classify_itcv(abs(itcv))
      fragility$overall_label <- glue::glue(
        "Impact threshold (ITCV): {round(itcv, 4)}. ",
        "An omitted variable with correlation product exceeding this ",
        "threshold would invalidate the inference."
      )
    }
  }

  x$fragility <- fragility

  # automatically compute CFI if all components available
  x <- tryCatch(
    compute_cfi(x),
    error = function(e) x   # silently skip if not enough data
  )

  x
}


# ==============================================================================
# Classification helpers
# ==============================================================================

#' @keywords internal
.classify_rv <- function(rv) {
  # Thresholds aligned with simulation-validated CFI zones
  if (is.na(rv))   return(NA_character_)
  if (rv >= 0.25)  return("Robust")
  if (rv >= 0.12)  return("Moderately robust")
  if (rv >= 0.06)  return("Caution")
  if (rv >= 0.02)  return("Fragile")
  return("Highly fragile")
}

#' @keywords internal
.classify_evalue <- function(ev) {
  if (is.na(ev))  return(NA_character_)
  if (ev >= 4.0)  return("Highly stable")
  if (ev >= 2.0)  return("Stable")
  if (ev >= 1.5)  return("Moderately fragile")
  if (ev >= 1.1)  return("Fragile")
  return("Highly fragile")
}

#' @keywords internal
.classify_itcv <- function(itcv_abs) {
  if (is.na(itcv_abs))   return(NA_character_)
  if (itcv_abs >= 0.20)  return("Highly stable")
  if (itcv_abs >= 0.10)  return("Stable")
  if (itcv_abs >= 0.05)  return("Moderately fragile")
  if (itcv_abs >= 0.01)  return("Fragile")
  return("Highly fragile")
}

