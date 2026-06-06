#' Compute the Causal Fragility Index (CFI)
#'
#' @description
#' The Causal Fragility Index (CFI) is a single 0--100 composite score that
#' integrates sensitivity evidence across multiple causal frameworks into one
#' interpretable measure of robustness.
#'
#' Each framework contributes one or more component scores, which are averaged
#' into the final CFI. Higher scores indicate greater robustness to unmeasured
#' confounding.
#'
#' @section Scoring rules:
#' \tabular{lll}{
#'   Component         \tab Rule                        \tab Full score at \cr
#'   RV (point)        \tab rv / 0.20 * 100             \tab RV >= 0.20    \cr
#'   RV (significance) \tab rv_alpha / 0.10 * 100       \tab RV >= 0.10    \cr
#'   E-value           \tab (evalue - 1) / 2.0 * 100    \tab E-value >= 3  \cr
#'   Percent bias      \tab pct_bias (direct)           \tab pct >= 100    \cr
#' }
#'
#' @section CFI classification:
#' \tabular{ll}{
#'   CFI range \tab Label              \cr
#'   0 -- 24   \tab Fragile            \cr
#'   25 -- 49  \tab Moderately fragile \cr
#'   50 -- 74  \tab Moderately robust  \cr
#'   75 -- 100 \tab Robust             \cr
#' }
#'
#' @param x A \code{sens_results} object from \code{run_sensitivity()}.
#' @param weights Optional named numeric vector to weight components.
#'   Names must match: \code{"rv"}, \code{"rv_alpha"}, \code{"evalue"},
#'   \code{"pct_bias"}. Default: equal weights.
#'
#' @return The input \code{sens_results} object with \code{$cfi} populated
#'   as a named list with elements:
#'   \describe{
#'     \item{score}{Numeric 0--100. The overall CFI.}
#'     \item{label}{Character. CFI classification label.}
#'     \item{components}{Named numeric vector of component scores.}
#'     \item{interpretation}{Character. One-sentence plain-language summary.}
#'   }
#'
#' @examples
#' \donttest{
#' res <- new_sens_results(
#'   design = "regression", treatment = "directlyharmed",
#'   outcome = "peacefactor", frameworks = c("sensemakr", "evalue", "itcv"),
#'   results = list(
#'     sensemakr = list(rv_q = 0.139, rv_qa = 0.076,
#'                      r2yd_x = 0.022, estimate = 0.097),
#'     evalue    = list(evalue = 1.90, evalue_lower = 1.55,
#'                      estimate = 0.097),
#'     itcv      = list(itcv = 0.065, rir = 678, pct_bias = 53.1)
#'   )
#' )
#' res <- compute_cfi(res)
#' print_cfi(res)
#' }
#'
#' @export
compute_cfi <- function(x, weights = NULL) {

  if (!is_sens_results(x)) {
    rlang::abort("`x` must be a `sens_results` object.")
  }

  components <- .compute_cfi_components(x)

  if (length(components) == 0L) {
    rlang::abort(
      "No scoreable components found. Run `run_sensitivity()` with at least one framework."
    )
  }

  # apply weights
  if (!is.null(weights)) {
    common <- intersect(names(weights), names(components))
    if (length(common) == 0L) {
      rlang::warn("No weight names matched component names. Using equal weights.")
    } else {
      w <- rep(1, length(components))
      names(w) <- names(components)
      w[common] <- weights[common]
      components <- components * w / mean(w)
    }
  }

  score <- round(mean(components, na.rm = TRUE), 1)
  label <- .classify_cfi(score)

  interpretation <- .cfi_interpretation(score, label, x)

  x$cfi <- list(
    score          = score,
    label          = label,
    components     = components,
    interpretation = interpretation
  )

  x
}


#' Print the Causal Fragility Index
#'
#' @description
#' Displays a formatted summary of the CFI score, component breakdown,
#' and interpretation. Call after \code{compute_cfi()}.
#'
#' @param x A \code{sens_results} object with \code{$cfi} populated.
#' @param ... Further arguments (unused).
#'
#' @return Invisibly returns \code{x}.
#' @export
print_cfi <- function(x, ...) {

  if (is.null(x$cfi)) {
    rlang::abort("CFI not computed. Run `compute_cfi()` first.")
  }

  cfi <- x$cfi

  cli::cli_h1("Causal Fragility Index (CFI)")

  # --- score bar ---
  bar_width  <- 40L
  filled     <- as.integer(round(cfi$score / 100 * bar_width))
  empty      <- bar_width - filled
  bar        <- paste0(
    "[", paste(rep("=", filled), collapse = ""),
    paste(rep(" ", empty), collapse = ""), "]"
  )

  cat(sprintf("\n  CFI Score:  %5.1f / 100\n", cfi$score))
  cat(sprintf("  %s\n", bar))
  cat(sprintf("  Label:      %s\n\n", cfi$label))

  # --- component breakdown ---
  cli::cli_h2("Component scores")
  comp <- cfi$components
  for (nm in names(comp)) {
    v      <- comp[[nm]]
    filled_c <- as.integer(round(v / 100 * 20))
    empty_c  <- 20L - filled_c
    mini_bar <- paste0(
      paste(rep("|", filled_c), collapse = ""),
      paste(rep(".", empty_c),  collapse = "")
    )
    cat(sprintf("  %-18s %5.1f  [%s]\n", .cfi_component_label(nm), v, mini_bar))
  }

  # --- interpretation ---
  cli::cli_h2("Interpretation")
  cat(" ", cfi$interpretation, "\n")

  invisible(x)
}


# ==============================================================================
# Internal: component scoring
# ==============================================================================

.compute_cfi_components <- function(x) {

  comp <- numeric(0)

  # --- sensemakr ---
  if ("sensemakr" %in% x$frameworks) {
    rv_q  <- x$results$sensemakr$rv_q
    rv_qa <- x$results$sensemakr$rv_qa

    # --- Conservative scaling ---
  # Full score (100) requires strong robustness evidence.
  # Thresholds raised from v1 based on adjustment-based simulation diagnostics:
  #   RV (point):  full score at RV >= 0.30 (was 0.20)
  #   RV (signif): full score at RV_alpha >= 0.15 (was 0.10)
  #   E-value:     full score at E-val >= 4.0 (was 3.0), linear from 1
  #   Pct bias:    full score at pct >= 80% (was 100%)
  # This prevents moderate-robustness cases from saturating to 100.

    if (!is.null(rv_q)  && !is.na(rv_q)  && is.numeric(rv_q))
      comp["rv"]       <- min(100, rv_q  / 0.30 * 100)

    if (!is.null(rv_qa) && !is.na(rv_qa) && is.numeric(rv_qa))
      comp["rv_alpha"] <- min(100, rv_qa / 0.15 * 100)
  }

  # --- evalue ---
  if ("evalue" %in% x$frameworks) {
    ev <- x$results$evalue$evalue
    if (!is.null(ev) && !is.na(ev) && is.numeric(ev))
      comp["evalue"] <- min(100, max(0, (ev - 1) / 3.0 * 100))
  }

  # --- itcv ---
  if ("itcv" %in% x$frameworks) {
    pct <- x$results$itcv$pct_bias
    if (!is.null(pct) && !is.na(pct) && is.numeric(pct))
      comp["pct_bias"] <- min(100, max(0, pct / 0.80 * 100))
  }

  comp
}


.classify_cfi <- function(score) {
  # Thresholds derived from adjustment-based simulation validation
  # (Hait, 2026). CFI < 50: fragile warning zone (specificity 95.3%).
  # CFI 50-70: caution zone. CFI 70-85: moderately robust. CFI > 85: robust.
  # Best balanced accuracy at threshold 70 (72.8% in simulation, n=693).
  if (score < 50)  return("Fragile")
  if (score < 70)  return("Caution")
  if (score < 85)  return("Moderately robust")
  return("Robust")
}


.cfi_interpretation <- function(score, label, x) {
  fra_point <- x$fragility$point %||% "unassessed"
  fra_alpha <- x$fragility$alpha %||% "unassessed"

  glue::glue(
    "Overall CFI = {score}/100, indicating {tolower(label)} robustness to ",
    "unmeasured confounding. The point estimate is {tolower(fra_point)}, ",
    "but statistical significance is {tolower(fra_alpha)}. ",
    "Triangulated across {length(x$frameworks)} sensitivity framework(s): ",
    "{paste(toupper(x$frameworks), collapse = ', ')}."
  )
}


.cfi_component_label <- function(nm) {
  switch(nm,
    rv       = "RV (point est.)",
    rv_alpha = "RV (significance)",
    evalue   = "E-value",
    pct_bias = "Pct bias (ITCV)",
    nm
  )
}
