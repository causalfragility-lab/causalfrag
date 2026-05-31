#' Configure the LLM provider for causalfrag
#'
#' @description
#' Sets up the LLM provider used by `interpret_sensitivity()` and
#' `generate_report()`. Configuration is stored in the R session environment.
#'
#' **The package works fully without calling this function.** When no LLM
#' is configured, narrative output falls back to pre-written templates
#' based on the numeric results.
#'
#' @param provider Character. One of `"openai"`, `"anthropic"`, `"none"`.
#'   Use `"none"` to explicitly disable LLM and use templates only.
#' @param api_key Character. Your API key. If `NULL`, the function looks for
#'   environment variables `OPENAI_API_KEY` or `ANTHROPIC_API_KEY`.
#' @param model Character. Model name to use.
#'   \itemize{
#'     \item OpenAI default: `"gpt-4o"`
#'     \item Anthropic default: `"claude-3-5-haiku-latest"`
#'   }
#' @param max_tokens Integer. Maximum tokens for LLM response. Default 512.
#'
#' @return Invisibly returns a list of the current LLM configuration.
#'
#' @examples
#' # Use Anthropic (key from environment variable)
#' use_llm_provider("anthropic")
#'
#' # Use OpenAI with explicit key
#' \dontrun{
#' use_llm_provider("openai", api_key = "sk-...")
#' }
#'
#' # Disable LLM explicitly (template mode)
#' use_llm_provider("none")
#'
#' @export
use_llm_provider <- function(
    provider  = c("openai", "anthropic", "none"),
    api_key   = NULL,
    model     = NULL,
    max_tokens = 512L
) {
  provider <- match.arg(provider)

  # resolve API key from environment if not supplied
  if (provider != "none" && is.null(api_key)) {
    env_var <- switch(provider,
      openai    = "OPENAI_API_KEY",
      anthropic = "ANTHROPIC_API_KEY"
    )
    api_key <- Sys.getenv(env_var, unset = NA)
    if (is.na(api_key) || nchar(api_key) == 0) {
      cli::cli_alert_warning(
        "No API key found in environment variable {.envvar {env_var}}. \\
         Falling back to template-based narrative."
      )
      provider <- "none"
      api_key  <- NULL
    }
  }

  # default model per provider
  if (is.null(model)) {
    model <- switch(provider,
      openai    = "gpt-4o",
      anthropic = "claude-3-5-haiku-latest",
      none      = NA_character_
    )
  }

  config <- list(
    provider   = provider,
    api_key    = api_key,
    model      = model,
    max_tokens = as.integer(max_tokens)
  )

  # store in package environment
  .causalfrag_env$llm_config <- config

  if (provider == "none") {
    cli::cli_alert_info("LLM disabled. Using template-based narrative generation.")
  } else {
    cli::cli_alert_success(
      "LLM configured: {provider} / {model}"
    )
  }

  invisible(config)
}


#' Retrieve current LLM configuration
#'
#' @return Named list with provider, model, max_tokens.
#'   Returns a `"none"` configuration if never set.
#' @keywords internal
.get_llm_config <- function() {
  cfg <- .causalfrag_env$llm_config
  if (is.null(cfg)) {
    # check environment variables automatically on first use
    if (nchar(Sys.getenv("ANTHROPIC_API_KEY")) > 0) {
      return(list(
        provider   = "anthropic",
        api_key    = Sys.getenv("ANTHROPIC_API_KEY"),
        model      = "claude-3-5-haiku-latest",
        max_tokens = 512L
      ))
    }
    if (nchar(Sys.getenv("OPENAI_API_KEY")) > 0) {
      return(list(
        provider   = "openai",
        api_key    = Sys.getenv("OPENAI_API_KEY"),
        model      = "gpt-4o",
        max_tokens = 512L
      ))
    }
    # no key found — template mode
    return(list(provider = "none", api_key = NULL, model = NA, max_tokens = 512L))
  }
  cfg
}


# ---- package environment for session state -----------------------------------

.causalfrag_env <- new.env(parent = emptyenv())


#' @keywords internal
.onLoad <- function(libname, pkgname) {
  .causalfrag_env$llm_config <- NULL
  .causalfrag_env$cache      <- list()
}
