#' causalfrag: A Cross-Framework Causal Fragility Index for Sensitivity Analysis
#'
#' @description
#' \pkg{causalfrag} provides a unified workflow for running, classifying,
#' visualizing, and interpreting sensitivity analyses for unmeasured
#' confounding across multiple causal frameworks.
#'
#' The core contribution is the \strong{Causal Fragility Index (CFI)}: a
#' single 0--100 composite score that integrates sensitivity evidence across
#' frameworks into one interpretable measure of robustness.
#'
#' @section Main workflow:
#' ```r
#' fit    <- lm(outcome ~ treatment + covariates, data = mydata)
#' res    <- run_sensitivity(fit, treatment = "treatment", data = mydata)
#' res    <- flag_fragility(res)   # also computes CFI automatically
#' print(res)                      # shows CFI score + component breakdown
#' print_cfi(res)                  # dedicated CFI display
#' generate_report(res)            # full structured report
#' ```
#'
#' @section Causal Fragility Index (CFI):
#' The CFI combines evidence from up to four sensitivity components:
#' \itemize{
#'   \item \strong{RV} (point): Robustness Value for point-estimate nullification
#'   \item \strong{RV} (significance): Robustness Value for statistical significance
#'   \item \strong{E-value}: Risk-ratio style confounding strength
#'   \item \strong{Pct bias}: Percent bias needed to invalidate (ITCV)
#' }
#'
#' @section Supported sensitivity frameworks:
#' \itemize{
#'   \item \strong{sensemakr}: Cinelli & Hazlett (2020) partial R-squared approach
#'   \item \strong{EValue}: VanderWeele & Ding (2017) E-value metrics
#'   \item \strong{konfound}: Frank (2000) Impact Threshold (ITCV)
#'   \item \strong{rbounds}: Rosenbaum (2002) bounds for matched designs
#' }
#'
#' @section Relationship to confoundvis:
#' \pkg{confoundvis} (Hait, 2026) provides visualization tools for sensitivity
#' analysis. \pkg{causalfrag} provides the unified fragility scoring,
#' interpretation, and reporting layer that optionally uses \pkg{confoundvis}
#' graphics as part of the workflow.
#'
#' @references
#' Frank, K. A. (2000). Impact of a confounding variable on the inference of
#' a regression coefficient. \emph{Sociological Methods & Research}, 29(2),
#' 147--194. \doi{10.1177/0049124100029002001}
#'
#' Cinelli, C., & Hazlett, C. (2020). Making sense of sensitivity: Extending
#' omitted variable bias. \emph{Journal of the Royal Statistical Society:
#' Series B}, 82(1), 39--67. \doi{10.1111/rssb.12348}
#'
#' VanderWeele, T. J., & Ding, P. (2017). Sensitivity analysis in
#' observational research: Introducing the E-value. \emph{Annals of Internal
#' Medicine}, 167(4), 268--274. \doi{10.7326/M16-2607}
#'
#' @author Subir Hait \email{haitsubi@@msu.edu}
#'
#' @docType package
#' @name causalfrag-package
#' @aliases causalfrag
"_PACKAGE"

utils::globalVariables(c("."))

#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
#' @importFrom cli cli_alert_danger cli_h1 cli_h2
#' @importFrom rlang abort warn inform check_installed
#' @importFrom glue glue
#' @importFrom jsonlite toJSON fromJSON
NULL
