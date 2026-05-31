# ---- stubs ------------------------------------------------------------------
# These functions are exported and documented but not yet fully implemented.
# They return informative errors so users know they are coming in a future
# version. Replace each stub with the real implementation progressively.










#' Run the full sensitivity analysis pipeline
#'
#' @description
#' The main user-facing function. Runs the complete pipeline:
#' detect design → run sensitivity → flag fragility → interpret → visualize.
#'
#' @param model A fitted model object (e.g. from `lm()`, `glm()`).
#' @param treatment Character. Name of the treatment variable in the model.
#' @param data A data frame containing the variables in `model`.
#' @param design Character. Override automatic design detection. One of
#'   `"regression"`, `"matched"`, `"iv"`, `"survival"`. Default `NULL`
#'   (auto-detect).
#' @param narrative Character. Level of narrative detail. One of `"brief"`,
#'   `"standard"`, `"reviewer-ready"`. Default `"standard"`.
#' @param plot Logical. Produce sensitivity plots. Default `TRUE`.
#' @param ... Additional arguments passed to `run_sensitivity()`.
#'
#' @return A `sens_results` object with all fields populated.
#'
#' @examples
#' \dontrun{
#' fit <- lm(mpg ~ am + wt + hp, data = mtcars)
#' result <- sens_report(fit, treatment = "am", data = mtcars)
#' print(result)
#' cat(result$narrative)
#' }
#'
#' @export
sens_report <- function(model, treatment, data, design = NULL,
                        narrative = "standard", plot = TRUE, ...) {
  abort(
    paste0(
      "`sens_report()` is not yet implemented in this version.\n",
      "This is the main pipeline function that will orchestrate:\n",
      "  detect_design() -> run_sensitivity() -> flag_fragility() -> ",
      "interpret_sensitivity() -> visualize_sensitivity()\n\n",
      "The building blocks `detect_design()`, `flag_fragility()`, and ",
      "`new_sens_results()` are fully implemented and testable now."
    )
  )
}
