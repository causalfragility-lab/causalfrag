#' Extract and standardise results from a sensemakr object
#'
#' @description
#' Inspects a fitted \code{sensemakr} object and returns a clean named list
#' with correctly mapped field names, ready to pass to \code{new_sens_results()}.
#'
#' This function exists because \code{sensemakr} internal field names
#' (\code{rv_q}, \code{rv_qa}) are not immediately obvious from the printed
#' output, and differ across versions. Always use this extractor rather than
#' accessing \code{sm$sensitivity_stats} directly.
#'
#' @param sm A \code{sensemakr} object returned by \code{sensemakr::sensemakr()}.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{estimate}{Point estimate of the treatment effect.}
#'   \item{se}{Standard error.}
#'   \item{t_stat}{t-statistic.}
#'   \item{r2yd_x}{Partial R-squared of treatment with outcome.}
#'   \item{r2dz_x}{Partial R-squared of treatment with treatment (benchmark).}
#'   \item{rv_q}{Robustness Value for q=1 (point-estimate nullification).}
#'   \item{rv_qa}{Robustness Value for q=1, alpha=.05 (significance).}
#'   \item{raw_object}{The original \code{sensemakr} object.}
#' }
#'
#' @examples
#' \donttest{
#' if (requireNamespace("sensemakr", quietly = TRUE)) {
#'   data("darfur", package = "sensemakr")
#'   fit <- lm(peacefactor ~ directlyharmed + age + female + village,
#'             data = darfur)
#'   sm  <- sensemakr::sensemakr(fit, treatment = "directlyharmed")
#'   extract_sensemakr(sm)
#' }
#' }
#'
#' @export
extract_sensemakr <- function(sm) {

  if (!inherits(sm, "sensemakr")) {
    rlang::abort("`sm` must be a `sensemakr` object.")
  }

  ss <- sm$sensitivity_stats

  # defensive field extraction with fallback to NA
  .get <- function(field) {
    val <- ss[[field]]
    if (is.null(val) || length(val) == 0L) NA_real_ else as.numeric(val)
  }

  # -- map sensemakr field names carefully --
  # rv_q  = Robustness Value, q = 1        (point-estimate threshold)
  # rv_qa = Robustness Value, q = 1, a=.05 (significance threshold)
  # These names changed slightly across sensemakr versions; try both
  rv_q  <- .get("rv_q")
  rv_qa <- if (!is.na(.get("rv_qa"))) .get("rv_qa") else .get("rv_q_a")

  # r2yd_x: try every known location in the sensemakr object
  # sensemakr stores sensitivity_stats as a data.frame row, not a plain list
  # so [[]] may return numeric(0) — always guard with length check
  .safe_field <- function(...) {
    for (expr in list(...)) {
      v <- tryCatch(expr, error = function(e) NULL)
      if (!is.null(v) && length(v) > 0L && !is.na(suppressWarnings(as.numeric(v[[1L]])))) {
        return(as.numeric(v[[1L]]))
      }
    }
    NA_real_
  }

  r2yd_x_val <- .safe_field(
    ss[["r2yd_x"]],                          # sensitivity_stats row
    sm$sensitivity_stats[["r2yd_x"]],        # direct access
    sm$bounds[["r2yd_x"]][[1L]],             # bounds table first row
    sm$sensitivity_stats$r2yd_x              # $ access (may differ)
  )

  list(
    estimate   = .get("estimate"),
    se         = .get("se"),
    t_stat     = .get("t_statistic"),
    r2yd_x     = r2yd_x_val,
    r2dz_x     = .get("r2dz_x"),
    rv_q       = rv_q,
    rv_qa      = rv_qa,
    raw_object = sm
  )
}


#' Plot sensitivity contours from a sens_results object
#'
#' @description
#' Produces sensitivity analysis figures using \pkg{sensemakr}'s built-in
#' plotting functions, or \pkg{confoundvis} if installed and \code{engine}
#' is set accordingly. Plots are generated for each framework in the
#' \code{sens_results} object that supports visualisation.
#'
#' @param x A \code{sens_results} object.
#' @param engine Character. One of \code{"sensemakr"}, \code{"confoundvis"}.
#'   Default \code{"sensemakr"} (uses the raw sensemakr object stored in
#'   \code{x$results$sensemakr$raw_object}).
#' @param type Character. Plot type for sensemakr engine. One of
#'   \code{"contour"}, \code{"extreme"}. Default \code{"contour"}.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @return Invisibly returns \code{x}. Called for side effects (plots).
#'
#' @examples
#' \donttest{
#' if (requireNamespace("sensemakr", quietly = TRUE)) {
#'   data("darfur", package = "sensemakr")
#'   fit <- lm(peacefactor ~ directlyharmed + age + female + village,
#'             data = darfur)
#'   res <- run_sensitivity(fit, treatment = "directlyharmed", data = darfur,
#'                          benchmark_covariates = "female")
#'   visualize_sensitivity(res)
#'   visualize_sensitivity(res, type = "extreme")
#' }
#' }
#'
#' @export
visualize_sensitivity <- function(x, engine = "sensemakr",
                                  type = "contour", ...) {
  if (!is_sens_results(x)) {
    rlang::abort("`x` must be a `sens_results` object.")
  }

  if (engine == "sensemakr") {
    sm_raw <- x$results$sensemakr$raw_object
    if (is.null(sm_raw)) {
      rlang::abort(
        "No sensemakr raw object found in results. ",
        "Run `run_sensitivity()` with framework = 'sensemakr' first."
      )
    }
    rlang::check_installed("sensemakr",
      reason = "to produce sensemakr sensitivity plots.")

    if (type == "contour") {
      plot(sm_raw, type = "contour", ...)
    } else if (type == "extreme") {
      plot(sm_raw, type = "extreme", ...)
    } else {
      rlang::abort("`type` must be 'contour' or 'extreme'.")
    }

  } else if (engine == "confoundvis") {
    rlang::check_installed("confoundvis",
      reason = "to use the confoundvis plotting engine.")
    rlang::abort(
      "confoundvis engine integration is coming in the next version. ",
      "Use engine = 'sensemakr' for now."
    )
  } else {
    rlang::abort("`engine` must be 'sensemakr' or 'confoundvis'.")
  }

  invisible(x)
}
