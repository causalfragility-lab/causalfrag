# ---- stubs ------------------------------------------------------------------
# These functions are exported and documented but not yet fully implemented.
# They return informative errors so users know they are coming in a future
# version. Replace each stub with the real implementation progressively.










#' Run the full sensitivity analysis pipeline
#'
#' @description
#' The main user-facing function. It runs the available sensitivity-analysis
#' frameworks, classifies fragility, creates a narrative interpretation, and
#' optionally draws a sensitivity plot when a supported plotting object is
#' available.
#'
#' @param model A fitted model object (for example, from `lm()` or `glm()`).
#' @param treatment Character. Name of the treatment variable in the model.
#' @param data A data frame containing the variables in `model`.
#' @param design Character. Optional study-design override. One of
#'   `"regression"`, `"matched"`, `"iv"`, or `"survival"`. The default,
#'   `NULL`, detects the design automatically.
#' @param narrative Character. Narrative detail: `"brief"`, `"standard"`, or
#'   `"reviewer-ready"`. The default is `"standard"`.
#' @param plot Logical. If `TRUE`, draw a sensitivity plot when the required
#'   plotting object and package are available. The default is `TRUE`.
#' @param ... Additional arguments passed to `run_sensitivity()`.
#'
#' @return A `sens_results` object containing the framework-specific numeric
#'   results, fragility classifications, composite index when computable, and
#'   narrative interpretation. When `plot = TRUE`, a plot may also be produced
#'   as a side effect.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("sensemakr", quietly = TRUE)) {
#'   data("darfur", package = "sensemakr")
#'   fit <- lm(peacefactor ~ directlyharmed + age + female + village,
#'             data = darfur)
#'   result <- sens_report(
#'     fit,
#'     treatment = "directlyharmed",
#'     data = darfur,
#'     frameworks = "sensemakr",
#'     plot = FALSE
#'   )
#'   print(result)
#' }
#' }
#'
#' @export
sens_report <- function(model, treatment, data, design = NULL,
                        narrative = "standard", plot = TRUE, ...) {
  narrative <- match.arg(
    narrative,
    choices = c("brief", "standard", "reviewer-ready")
  )

  result <- run_sensitivity(
    model = model,
    treatment = treatment,
    data = data,
    design = design,
    ...
  )

  result <- flag_fragility(result)
  result <- interpret_sensitivity(result, verbosity = narrative)

  if (isTRUE(plot) &&
      "sensemakr" %in% result$frameworks &&
      !is.null(result$results$sensemakr$raw_object)) {
    visualize_sensitivity(result, engine = "sensemakr")
  }

  result
}
