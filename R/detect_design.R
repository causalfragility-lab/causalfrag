#' Detect the study design from a model object
#'
#' @description
#' Inspects a fitted model object and returns the most appropriate sensitivity
#' analysis design type. The detected design determines which sensitivity
#' frameworks are run by `run_sensitivity()`.
#'
#' Users can always override the detected design manually via the `design`
#' argument in `sens_report()` or `run_sensitivity()`.
#'
#' @param model A fitted model object. Supported classes:
#'   \itemize{
#'     \item `lm`, `glm` → `"regression"`
#'     \item `lmerMod`, `glmerMod` (lme4) → `"regression"`
#'     \item Objects with class `"Match"` (Matching package) → `"matched"`
#'     \item `coxph`, `survreg` (survival) → `"survival"`
#'     \item `ivreg` (ivreg/AER) → `"iv"`
#'   }
#' @param verbose Logical. If `TRUE`, prints the detected design. Default `TRUE`.
#'
#' @return A character string: one of `"regression"`, `"matched"`,
#'   `"survival"`, `"iv"`, or `"unknown"`.
#'
#' @examples
#' fit <- lm(mpg ~ am + wt + hp, data = mtcars)
#' detect_design(fit)
#'
#' @export
detect_design <- function(model, verbose = TRUE) {

  cls <- class(model)

  design <- .route_by_class(cls)

  if (verbose) {
    cli::cli_alert_info(
      "Detected design: {.strong {design}} (model class: {paste(cls, collapse = ', ')})"
    )
    if (design == "unknown") {
      cli::cli_alert_warning(
        "Design could not be detected automatically. \\
         Set `design` manually in `sens_report()` or `run_sensitivity()`."
      )
    }
  }

  design
}


# ---- internal routing table --------------------------------------------------

.route_by_class <- function(cls) {

  # regression: standard lm / glm
  if (any(cls %in% c("lm", "glm"))) return("regression")

  # mixed models via lme4
  if (any(cls %in% c("lmerMod", "glmerMod", "lmerModLmerTest"))) return("regression")

  # matched designs
  if (any(cls %in% c("Match", "matchit", "optmatch"))) return("matched")

  # survival models
  if (any(cls %in% c("coxph", "survreg", "survfit"))) return("survival")

  # instrumental variable
  if (any(cls %in% c("ivreg", "tsls"))) return("iv")

  # fixest (common in economics)
  if (any(cls %in% c("fixest"))) return("regression")

  "unknown"
}


# ---- framework recommendation ------------------------------------------------

#' Recommend sensitivity frameworks for a given design
#'
#' @description
#' Internal helper that maps a study design to the recommended set of
#' sensitivity frameworks. Called by `run_sensitivity()`.
#'
#' @param design Character. One of the design strings from `detect_design()`.
#' @return Character vector of framework names.
#' @keywords internal
.recommend_frameworks <- function(design) {
  switch(design,
    regression = c("sensemakr", "evalue", "itcv"),
    matched    = c("rosenbaum", "evalue"),
    survival   = c("evalue"),
    iv         = c("sensemakr", "evalue"),
    unknown    = c("evalue")   # safest single framework when design unclear
  )
}
