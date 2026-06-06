# causalfrag <img src="man/figures/logo.png" align="right" height="139" alt="" />

> A Cross-Framework Causal Fragility Index for Sensitivity Analysis in Observational Studies

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/causalfrag)](https://CRAN.R-project.org/package=causalfrag)
[![R-CMD-check](https://github.com/causalfragility-lab/causalfrag/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/causalfragility-lab/causalfrag/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

`causalfrag` provides a unified workflow for running, classifying, visualizing,
and interpreting sensitivity analyses for unmeasured confounding across multiple
causal frameworks.

The core contribution is the **Causal Fragility Index (CFI)**: a single 0--100
composite score that integrates evidence from multiple sensitivity frameworks
into one interpretable measure of robustness.

## Relationship to confoundvis

[`confoundvis`](https://CRAN.R-project.org/package=confoundvis) (Hait, 2026)
provides visualization tools for sensitivity analysis. `causalfrag` provides
the unified fragility scoring, interpretation, and reporting layer that
optionally uses `confoundvis` graphics.

## Installation

```r
# CRAN (once available)
install.packages("causalfrag")

# Development version
remotes::install_github("causalfragility-lab/causalfrag")
```

## Quick start

```r
library(causalfrag)

fit <- lm(peacefactor ~ directlyharmed + age + female, data = darfur)

res <- run_sensitivity(fit, treatment = "directlyharmed", data = darfur)
res <- flag_fragility(res)   # CFI computed automatically

print(res)       # full output with CFI score
print_cfi(res)   # dedicated CFI display
generate_report(res)  # full structured report
```

## Causal Fragility Index

| Component | Framework | Full score at |
|---|---|---|
| RV (point est.) | sensemakr | RV >= 0.20 |
| RV (significance) | sensemakr | RV >= 0.10 |
| E-value | EValue | E-value >= 3.0 |
| Pct bias | konfound | pct >= 100% |

| CFI range | Label |
|---|---|
| 0 -- 24 | Fragile |
| 25 -- 49 | Moderately fragile |
| 50 -- 74 | Moderately robust |
| 75 -- 100 | Robust |

## Author

**Subir Hait** -- Michigan State University  
[haitsubi@msu.edu](mailto:haitsubi@msu.edu)  
GitHub: [causalfragility-lab](https://github.com/causalfragility-lab)
