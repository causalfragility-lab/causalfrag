#' Interpret sensitivity results using an LLM or concise templates
#'
#' @description
#' Generates a concise plain-language interpretation (2-3 sentences per
#' framework) of sensitivity analysis findings. Uses an LLM if configured
#' via \code{use_llm_provider()}; falls back to pre-written concise templates
#' when no LLM is available.
#'
#' The LLM assists only in phrasing. All statistical values come from the
#' \code{sens_results} object computed by transparent R functions.
#'
#' @param x A \code{sens_results} object with fragility assessed.
#' @param verbosity Character. Controls output length.
#'   \code{"brief"} = one sentence per framework.
#'   \code{"standard"} = 2-3 sentences per framework (default).
#'   \code{"reviewer-ready"} = full methods paragraph with citations.
#' @param cache Logical. Cache the LLM response to avoid repeat API calls.
#'   Default \code{TRUE}.
#' @param ... Reserved for future use.
#'
#' @return The input \code{sens_results} object with \code{$narrative}
#'   populated.
#'
#' @examples
#' \donttest{
#' res <- new_sens_results(
#'   design = "regression", treatment = "directlyharmed",
#'   outcome = "peacefactor", frameworks = "sensemakr",
#'   results = list(sensemakr = list(
#'     rv_q = 0.139, rv_qa = 0.076, r2yd_x = 0.022, estimate = 0.097
#'   )),
#'   fragility = list(point = "Stable", alpha = "Moderately fragile",
#'                    point_label = "", alpha_label = "")
#' )
#' res <- interpret_sensitivity(res)
#' cat(res$narrative)
#' }
#'
#' @export
interpret_sensitivity <- function(x, verbosity = "standard",
                                  cache = TRUE, ...) {

  if (!is_sens_results(x)) {
    rlang::abort("`x` must be a `sens_results` object.")
  }

  # --- check cache first -------------------------------------------------------
  cache_key <- paste0("narrative_", digest_results(x), "_", verbosity)
  if (cache && !is.null(.causalfrag_env$cache[[cache_key]])) {
    x$narrative <- .causalfrag_env$cache[[cache_key]]
    x$llm_used  <- .causalfrag_env$cache[[paste0(cache_key, "_llm")]]
    return(x)
  }

  # --- build narrative per framework -------------------------------------------
  parts <- character(0)

  if ("sensemakr" %in% x$frameworks) {
    parts <- c(parts, .narrate_sensemakr(x, verbosity))
  }
  if ("evalue" %in% x$frameworks) {
    parts <- c(parts, .narrate_evalue(x, verbosity))
  }
  if ("itcv" %in% x$frameworks) {
    parts <- c(parts, .narrate_itcv(x, verbosity))
  }

  narrative <- paste(parts, collapse = " ")
  llm_used  <- FALSE

  # --- try LLM if configured ---------------------------------------------------
  cfg <- .get_llm_config()
  if (cfg$provider != "none" && verbosity == "reviewer-ready") {
    llm_result <- tryCatch(
      .call_llm_narrative(narrative, cfg),
      error = function(e) {
        cli::cli_alert_warning("LLM call failed ({conditionMessage(e)}). Using template.")
        NULL
      }
    )
    if (!is.null(llm_result)) {
      narrative <- llm_result
      llm_used  <- TRUE
    }
  }

  # --- cache and return --------------------------------------------------------
  if (cache) {
    .causalfrag_env$cache[[cache_key]]              <- narrative
    .causalfrag_env$cache[[paste0(cache_key, "_llm")]] <- llm_used
  }

  x$narrative <- narrative
  x$llm_used  <- llm_used
  x
}


# ==============================================================================
# Per-framework template narrators — concise by design
# ==============================================================================

.narrate_sensemakr <- function(x, verbosity) {
  r   <- x$results$sensemakr
  fra <- x$fragility

  rv_q  <- round(r$rv_q  * 100, 1)
  rv_qa <- round(r$rv_qa * 100, 1)
  est   <- round(r$estimate, 3)

  if (verbosity == "brief") {
    return(glue::glue(
      "Sensitivity analysis (Cinelli & Hazlett, 2020) showed the estimate ",
      "(b = {est}) is {tolower(fra$point %||% 'unclassified')} to unmeasured confounding ",
      "(RV = {rv_q}%)."
    ))
  }

  # standard / reviewer-ready — 2-3 sentences
  s1 <- glue::glue(
    "To reduce the estimated effect of {x$treatment} on {x$outcome} ",
    "(b = {est}) to zero, an unmeasured confounder would need to explain ",
    "at least {rv_q}% of the residual variance in both the treatment and outcome ",
    "(Robustness Value, RV = {round(r$rv_q, 3)})."
  )

  s2 <- glue::glue(
    "To render the effect statistically non-significant (alpha = .05), ",
    "a confounder explaining {rv_qa}% of residual variance on both sides ",
    "would suffice (RV_alpha = {round(r$rv_qa, 3)})."
  )

  s3 <- if (!is.null(fra$point) && !is.null(fra$alpha) &&
             fra$point != fra$alpha) {
    glue::glue(
      "The conclusion is therefore {tolower(fra$point)} for the point estimate ",
      "but {tolower(fra$alpha)} for statistical significance."
    )
  } else {
    glue::glue(
      "Overall, the conclusion is classified as {tolower(fra$point %||% 'unclassified')}."
    )
  }

  paste(s1, s2, s3)
}


.narrate_evalue <- function(x, verbosity) {
  r  <- x$results$evalue
  ev <- round(r$evalue, 2)
  el <- round(r$evalue_lower, 2)

  if (verbosity == "brief") {
    return(glue::glue(
      "The E-value for the point estimate was {ev} ",
      "(VanderWeele & Ding, 2017)."
    ))
  }

  s1 <- glue::glue(
    "The E-value for the point estimate was {ev}, indicating that a confounder ",
    "associated with both treatment and outcome by a risk ratio of {ev} on ",
    "each side would be needed to fully explain away the observed association."
  )
  s2 <- glue::glue(
    "The E-value for the lower confidence limit was {el}."
  )
  paste(s1, s2)
}


.narrate_itcv <- function(x, verbosity) {
  r <- x$results$itcv

  # safe numeric extraction — never crash on NULL or NA
  .n <- function(val, digits = 4) {
    if (is.null(val) || length(val) == 0L) return(NA_real_)
    v <- suppressWarnings(as.numeric(val))
    if (is.na(v)) return(NA_real_)
    round(v, digits)
  }

  itcv <- .n(r$itcv, 4)
  rir  <- .n(r$rir,  0)
  pct  <- .n(r$pct_bias, 1)

  if (is.na(itcv)) return("")  # nothing to narrate

  if (verbosity == "brief") {
    pct_str <- if (!is.na(pct)) glue::glue(" ({pct}% bias needed)") else ""
    return(glue::glue(
      "The impact threshold (ITCV = {itcv}){pct_str} indicated the ",
      "strength of omitted confounding needed to invalidate the inference ",
      "(Frank, 2000)."
    ))
  }

  s1 <- glue::glue(
    "Using the Impact Threshold for a Confounding Variable (Frank, 2000), ",
    "the ITCV was {itcv}, meaning an omitted variable would need to be ",
    "correlated with both treatment and outcome beyond this threshold to ",
    "invalidate the inference."
  )

  s2 <- if (!is.na(rir) && !is.na(pct)) {
    glue::glue(
      "Equivalently, {rir} cases ({pct}% of the sample) would need to be ",
      "replaced with cases where the treatment effect was zero to overturn ",
      "the conclusion."
    )
  } else if (!is.na(pct)) {
    glue::glue(
      "Approximately {pct}% of the observed effect would need to be due to ",
      "bias to invalidate the inference."
    )
  } else {
    ""
  }

  paste(s1, if (nchar(s2) > 0) s2 else NULL)
}


# ==============================================================================
# LLM call — used only for reviewer-ready polish
# ==============================================================================

.call_llm_narrative <- function(template_narrative, cfg) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    rlang::abort("Package 'httr2' is required for LLM calls. Install with: install.packages('httr2')")
  }

  system_prompt <- paste0(
    "You are a statistical writing assistant. ",
    "You receive a draft sensitivity analysis narrative and return a polished ",
    "version suitable for an academic journal results section. ",
    "Rules: (1) Do not change any numeric values. ",
    "(2) Do not add causal claims beyond what is stated. ",
    "(3) Keep it under 120 words. ",
    "(4) Use past tense. ",
    "(5) Return only the narrative text, nothing else."
  )

  url <- switch(cfg$provider,
    anthropic = "https://api.anthropic.com/v1/messages",
    openai    = "https://api.openai.com/v1/chat/completions"
  )

  body <- switch(cfg$provider,
    anthropic = list(
      model      = cfg$model,
      max_tokens = cfg$max_tokens,
      system     = system_prompt,
      messages   = list(list(role = "user", content = template_narrative))
    ),
    openai = list(
      model      = cfg$model,
      max_tokens = cfg$max_tokens,
      messages   = list(
        list(role = "system", content = system_prompt),
        list(role = "user",   content = template_narrative)
      )
    )
  )

  req <- httr2::request(url) |>
    httr2::req_headers(
      "Content-Type"  = "application/json",
      "Authorization" = paste("Bearer", cfg$api_key)
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(30)

  resp <- httr2::req_perform(req)
  parsed <- httr2::resp_body_json(resp)

  # extract text from response
  if (cfg$provider == "anthropic") {
    parsed$content[[1]]$text
  } else {
    parsed$choices[[1]]$message$content
  }
}


# ==============================================================================
# Lightweight digest for cache key (no digest package dependency)
# ==============================================================================

digest_results <- function(x) {
  paste0(
    x$treatment, "_", x$outcome, "_", x$design, "_",
    paste(x$frameworks, collapse = "-")
  )
}
