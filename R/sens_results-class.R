#' Create a sens_results object
#'
#' @description
#' Constructor for the unified \code{sens_results} S3 class. This is the
#' central data structure that all \code{causalfrag} functions produce
#' and consume.
#'
#' @param design Character. Detected study design. One of \code{"regression"},
#'   \code{"matched"}, \code{"iv"}, \code{"survival"}.
#' @param treatment Character. Name of the treatment variable.
#' @param outcome Character. Name of the outcome variable.
#' @param frameworks Character vector. Which sensitivity frameworks were run.
#' @param results Named list. Raw numeric results from each framework.
#' @param fragility Named list with elements \code{point} and \code{alpha}.
#' @param narrative Character. Plain-language interpretation.
#' @param llm_used Logical. Whether an LLM was used for narrative generation.
#' @param call The matched call.
#'
#' @return An object of class \code{sens_results}.
#'
#' @examples
#' \donttest{
#' res <- new_sens_results(
#'   design    = "regression",
#'   treatment = "treat",
#'   outcome   = "outcome",
#'   frameworks = "sensemakr",
#'   results   = list(sensemakr = list(rv_q = 0.14, rv_qa = 0.08,
#'                                     r2yd_x = 0.02, estimate = 0.10))
#' )
#' print(res)
#' }
#'
#' @export
new_sens_results <- function(design,
                             treatment,
                             outcome,
                             frameworks,
                             results,
                             fragility = NULL,
                             narrative = NULL,
                             llm_used  = FALSE,
                             call      = NULL) {

  valid_designs <- c("regression", "matched", "iv", "survival", "unknown")
  if (!design %in% valid_designs) {
    rlang::abort(glue::glue(
      "`design` must be one of: {paste(valid_designs, collapse = ', ')}. ",
      "Got: '{design}'"
    ))
  }

  valid_frameworks <- c("itcv", "sensemakr", "evalue", "rosenbaum")
  bad <- setdiff(frameworks, valid_frameworks)
  if (length(bad) > 0L) {
    rlang::abort(glue::glue(
      "`frameworks` contains unrecognised values: {paste(bad, collapse = ', ')}"
    ))
  }

  structure(
    list(
      design     = design,
      treatment  = treatment,
      outcome    = outcome,
      frameworks = frameworks,
      results    = results,
      fragility  = fragility,
      narrative  = narrative,
      llm_used   = llm_used,
      call       = call,
      timestamp  = Sys.time()
    ),
    class = "sens_results"
  )
}


#' Print method for sens_results
#' @param x A \code{sens_results} object.
#' @param ... Further arguments (unused).
#' @export
print.sens_results <- function(x, ...) {

  cli::cli_h1("causalfrag Sensitivity Analysis")

  cli::cli_h2("Study")
  cli::cli_alert_info("Design:     {x$design}")
  cli::cli_alert_info("Treatment:  {x$treatment}")
  cli::cli_alert_info("Outcome:    {x$outcome}")
  cli::cli_alert_info("Frameworks: {paste(x$frameworks, collapse = ', ')}")

  cli::cli_h2("Numeric results")

  if ("sensemakr" %in% x$frameworks) {
    r <- x$results$sensemakr
    cat("\n  [SENSEMAKR -- Cinelli & Hazlett, 2020]\n")
    if (!is.null(r$estimate))  cat(sprintf("    Estimate:                     %8.4f\n", r$estimate))
    if (!is.null(r$r2yd_x))    cat(sprintf("    Partial R2 (treat-outcome):   %8.4f\n", r$r2yd_x))
    if (!is.null(r$rv_q))      cat(sprintf("    Robustness Value (q=1):       %8.4f\n", r$rv_q))
    if (!is.null(r$rv_qa))     cat(sprintf("    Robustness Value (q=1,a=.05): %8.4f\n", r$rv_qa))
  }

  if ("evalue" %in% x$frameworks) {
    r <- x$results$evalue
    cat("\n  [E-VALUE -- VanderWeele & Ding, 2017]\n")
    if (!is.null(r$estimate))     cat(sprintf("    Estimate:                     %8.4f\n", r$estimate))
    if (!is.null(r$evalue))       cat(sprintf("    E-value (point):              %8.4f\n", r$evalue))
    if (!is.null(r$evalue_lower)) cat(sprintf("    E-value (CI lower):           %8.4f\n", r$evalue_lower))
  }

  if ("itcv" %in% x$frameworks) {
    r <- x$results$itcv
    cat("\n  [ITCV -- Frank, 2000]\n")
    if (!is.null(r$itcv))     cat(sprintf("    ITCV index:                   %8.4f\n", r$itcv))
    if (!is.null(r$rir))      cat(sprintf("    Robustness of Inference:      %8.4f\n", r$rir))
    if (!is.null(r$pct_bias)) cat(sprintf("    Percent bias to invalidate:   %8.2f%%\n", r$pct_bias))
  }

  cat("\n")

  if (!is.null(x$fragility)) {
    cli::cli_h2("Fragility")
    if (!is.null(x$fragility$point)) {
      .print_fragility_line("Point estimate", x$fragility$point,
                            x$fragility$point_label)
    }
    if (!is.null(x$fragility$alpha)) {
      .print_fragility_line("Statistical significance (alpha=.05)",
                            x$fragility$alpha, x$fragility$alpha_label)
    }
    if (is.null(x$fragility$point) && !is.null(x$fragility$overall)) {
      .print_fragility_line("Overall", x$fragility$overall,
                            x$fragility$overall_label)
    }
  }

  # --- CFI score ---
  if (!is.null(x$cfi)) {
    cfi <- x$cfi
    bar_width <- 30L
    filled    <- as.integer(round(cfi$score / 100 * bar_width))
    empty     <- bar_width - filled
    bar       <- paste0("[", paste(rep("=", filled), collapse = ""),
                        paste(rep(" ", empty), collapse = ""), "]")
    cli::cli_h2("Causal Fragility Index (CFI)")
    cat(sprintf("  Score:  %5.1f / 100  %s  [%s]\n\n",
                cfi$score, bar, cfi$label))
    for (nm in names(cfi$components)) {
      cat(sprintf("    %-18s %5.1f\n",
                  switch(nm, rv="RV (point)", rv_alpha="RV (signif.)",
                         evalue="E-value", pct_bias="Pct bias", nm),
                  cfi$components[[nm]]))
    }
    cat("\n")
  }

  if (!is.null(x$narrative)) {
    cli::cli_h2("Narrative")
    cat(x$narrative, "\n")
    src <- if (x$llm_used) "LLM-assisted" else "template-based"
    cli::cli_alert_info("Narrative generated via {src} interpretation.")
  }

  invisible(x)
}


.print_fragility_line <- function(label, level, description) {
  icon <- switch(level,
    "Robust"             = "++",
    "Moderately robust"  = "+" ,
    "Caution"            = "~" ,
    "Fragile"            = "-" ,
    "Highly fragile"     = "--",
    "?"
  )
  cat(sprintf("  [%s]  %-38s  %s\n", icon, label, level))
  if (!is.null(description) && nchar(description) > 0L) {
    cat(sprintf("        %s\n", description))
  }
}


#' Summary method for sens_results
#' @param object A \code{sens_results} object.
#' @param ... Further arguments (unused).
#' @export
summary.sens_results <- function(object, ...) {
  cat("causalfrag sensitivity analysis summary\n")
  cat("------------------------------------------\n")
  cat("Design:     ", object$design,    "\n")
  cat("Treatment:  ", object$treatment, "\n")
  cat("Outcome:    ", object$outcome,   "\n")
  cat("Frameworks: ", paste(object$frameworks, collapse = ", "), "\n")
  frag <- if (is.null(object$fragility)) {
    "not assessed"
  } else {
    paste0("point=",        object$fragility$point %||% "NA",
           ", significance=", object$fragility$alpha %||% "NA")
  }
  cat("Fragility:  ", frag, "\n")
  cat("Narrative:  ",
      ifelse(is.null(object$narrative), "not generated", "available ($narrative)"),
      "\n")
  invisible(object)
}


#' Plot method for sens_results
#' @param x A \code{sens_results} object.
#' @param ... Passed to \code{visualize_sensitivity()}.
#' @export
plot.sens_results <- function(x, ...) {
  visualize_sensitivity(x, ...)
}


#' Check if object is a sens_results
#' @param x Any R object.
#' @return Logical.
#' @export
is_sens_results <- function(x) inherits(x, "sens_results")


# null-coalescing operator (defined here as primary location)
`%||%` <- function(x, y) if (is.null(x)) y else x
