#' Run sensitivity analysis across frameworks
#'
#' @description
#' Dispatches to the appropriate sensitivity analysis packages based on the
#' detected or user-specified study design. Returns a unified `sens_results`
#' object containing numeric results from all frameworks run.
#'
#' Each framework is called only if the required package is installed.
#' Missing packages produce a warning, not an error, so the function
#' always returns whatever results it can compute.
#'
#' @param model A fitted model object (e.g. from \code{lm()}, \code{glm()}).
#' @param treatment Character. Name of the treatment variable.
#' @param data A data frame containing the model variables.
#' @param design Character. Study design. If \code{NULL}, calls
#'   \code{detect_design()} automatically. One of \code{"regression"},
#'   \code{"matched"}, \code{"iv"}, \code{"survival"}.
#' @param frameworks Character vector. Which frameworks to run. If \code{NULL},
#'   selects automatically based on design. Subset of
#'   \code{c("sensemakr", "evalue", "itcv", "rosenbaum")}.
#' @param benchmark_covariates Character vector. Covariate names to use as
#'   benchmarks in sensemakr. Default \code{NULL}.
#' @param verbose Logical. Print progress messages. Default \code{TRUE}.
#' @param ... Additional arguments (reserved for future use).
#'
#' @return A \code{sens_results} object with \code{$results} populated for
#'   each framework successfully run.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("sensemakr", quietly = TRUE)) {
#'   data("darfur", package = "sensemakr")
#'   fit <- lm(peacefactor ~ directlyharmed + age + female + village,
#'             data = darfur)
#'   res <- run_sensitivity(fit, treatment = "directlyharmed", data = darfur)
#'   print(res)
#' }
#' }
#'
#' @export
run_sensitivity <- function(model,
                            treatment,
                            data,
                            design     = NULL,
                            frameworks = NULL,
                            benchmark_covariates = NULL,
                            verbose    = TRUE,
                            ...) {

  # --- validate inputs -------------------------------------------------------
  if (!is.character(treatment) || length(treatment) != 1L) {
    rlang::abort("`treatment` must be a single character string.")
  }
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.")
  }

  # --- detect design ---------------------------------------------------------
  if (is.null(design)) {
    design <- detect_design(model, verbose = verbose)
  }

  # --- select frameworks -----------------------------------------------------
  if (is.null(frameworks)) {
    frameworks <- .recommend_frameworks(design)
  }

  if (verbose) {
    cli::cli_alert_info(
      "Running frameworks: {paste(toupper(frameworks), collapse = ', ')}"
    )
  }

  # --- run each framework ----------------------------------------------------
  results <- list()

  if ("sensemakr" %in% frameworks) {
    results$sensemakr <- .run_sensemakr(
      model, treatment, benchmark_covariates, verbose
    )
  }

  if ("evalue" %in% frameworks) {
    results$evalue <- .run_evalue(model, treatment, data, verbose)
  }

  if ("itcv" %in% frameworks) {
    results$itcv <- .run_itcv(model, treatment, data, verbose)
  }

  if ("rosenbaum" %in% frameworks) {
    results$rosenbaum <- .run_rosenbaum(model, treatment, data, verbose)
  }

  # remove frameworks that returned NULL (package not available)
  ran <- names(Filter(Negate(is.null), results))

  if (length(ran) == 0L) {
    rlang::abort(
      paste0(
        "No sensitivity frameworks could be run. ",
        "Install at least one of: sensemakr, EValue, konfound, rbounds.\n",
        "  install.packages(c('sensemakr', 'EValue', 'konfound'))"
      )
    )
  }

  # --- detect outcome variable name ------------------------------------------
  outcome <- .extract_outcome(model)

  # --- assemble sens_results -------------------------------------------------
  new_sens_results(
    design     = design,
    treatment  = treatment,
    outcome    = outcome,
    frameworks = ran,
    results    = results,
    call       = match.call()
  )
}


# ==============================================================================
# Internal dispatchers — one per framework
# ==============================================================================

# --- sensemakr ----------------------------------------------------------------
.run_sensemakr <- function(model, treatment, benchmark_covariates, verbose) {
  if (!requireNamespace("sensemakr", quietly = TRUE)) {
    if (verbose) cli::cli_alert_warning(
      "Package 'sensemakr' not installed -- skipping. ",
      "Install with: install.packages('sensemakr')"
    )
    return(NULL)
  }

  if (verbose) cli::cli_alert_info("Running sensemakr (Cinelli & Hazlett, 2020)...")

  sm <- tryCatch(
    sensemakr::sensemakr(
      model                = model,
      treatment            = treatment,
      benchmark_covariates = benchmark_covariates
    ),
    error = function(e) {
      cli::cli_alert_warning("sensemakr failed: {conditionMessage(e)}")
      NULL
    }
  )

  if (is.null(sm)) return(NULL)

  # use extract_sensemakr() to correctly map all field names
  extract_sensemakr(sm)
}


# --- E-value ------------------------------------------------------------------
.run_evalue <- function(model, treatment, data, verbose) {
  if (!requireNamespace("EValue", quietly = TRUE)) {
    if (verbose) cli::cli_alert_warning(
      "Package 'EValue' not installed -- skipping. ",
      "Install with: install.packages('EValue')"
    )
    return(NULL)
  }

  if (verbose) cli::cli_alert_info(
    "Running E-value (VanderWeele & Ding, 2017)..."
  )

  # extract point estimate and SE from model
  coefs <- tryCatch(summary(model)$coefficients, error = function(e) NULL)
  if (is.null(coefs) || !treatment %in% rownames(coefs)) {
    cli::cli_alert_warning(
      "Could not extract coefficient for '{treatment}' -- skipping E-value."
    )
    return(NULL)
  }

  est <- coefs[treatment, "Estimate"]
  se  <- coefs[treatment, "Std. Error"]
  n   <- nrow(data)

  # compute standardised mean difference approximation for continuous outcomes
  ev <- tryCatch(
    suppressMessages(suppressWarnings(
    EValue::evalues.OLS(
      est  = est,
      se   = se,
      sd   = stats::sd(stats::model.response(stats::model.frame(model))),
      delta = 1,
      true  = 0
    ))),
    error = function(e) {
      cli::cli_alert_warning("EValue::evalues.OLS failed: {conditionMessage(e)}")
      NULL
    }
  )

  if (is.null(ev)) return(NULL)

  list(
    estimate     = est,
    se           = se,
    evalue       = ev["E-values", "point"],
    evalue_lower = ev["E-values", "lower"],
    raw_object   = ev
  )
}


# --- ITCV via konfound ---------------------------------------------------------
.run_itcv <- function(model, treatment, data, verbose) {
  if (!requireNamespace("konfound", quietly = TRUE)) {
    if (verbose) cli::cli_alert_warning(
      "Package 'konfound' not installed -- skipping ITCV. ",
      "Install with: install.packages('konfound')"
    )
    return(NULL)
  }

  if (verbose) cli::cli_alert_info("Running ITCV / konfound (Frank, 2000)...")

  kf <- tryCatch(
    suppressMessages(
      konfound::pkonfound(
        est_eff   = summary(model)$coefficients[treatment, "Estimate"],
        std_err   = summary(model)$coefficients[treatment, "Std. Error"],
        n_obs     = nrow(data),
        n_covariates = length(attr(stats::terms(model), "term.labels")) - 1L,
        to_return = "raw_output"
      )
    ),
    error = function(e) {
      cli::cli_alert_warning("konfound failed: {conditionMessage(e)}")
      NULL
    }
  )

  if (is.null(kf)) return(NULL)

  # konfound field names confirmed from raw_output structure:
  # itcvGz, RIR_primary, perc_bias_to_change
  .safe_num <- function(x) {
    val <- x
    if (is.null(val) || length(val) == 0L) return(NA_real_)
    suppressWarnings(as.numeric(val[[1L]]))
  }

  list(
    itcv       = .safe_num(kf$itcvGz),
    rir        = .safe_num(kf$RIR_primary),
    pct_bias   = .safe_num(kf$perc_bias_to_change),
    raw_object = kf
  )
}


# --- Rosenbaum bounds ---------------------------------------------------------
.run_rosenbaum <- function(model, treatment, data, verbose) {
  if (!requireNamespace("rbounds", quietly = TRUE)) {
    if (verbose) cli::cli_alert_warning(
      "Package 'rbounds' not installed -- skipping Rosenbaum bounds. ",
      "Install with: install.packages('rbounds')"
    )
    return(NULL)
  }

  # Rosenbaum bounds require a matched design — skip gracefully for regression
  if (verbose) cli::cli_alert_warning(
    "Rosenbaum bounds require a matched study design. ",
    "Skipping for regression model. Use a Match or matchit object instead."
  )
  NULL
}


# ==============================================================================
# Helper: extract outcome variable name from model
# ==============================================================================

.extract_outcome <- function(model) {
  tryCatch({
    as.character(stats::formula(model)[[2L]])
  }, error = function(e) "unknown")
}
