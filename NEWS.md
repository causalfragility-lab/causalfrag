# causalfrag 0.1.0

* Initial CRAN submission.
* Introduces the Causal Fragility Index (CFI): a 0-100 composite score
  integrating sensitivity evidence across frameworks.
* `run_sensitivity()` -- unified dispatch to sensemakr, EValue, konfound.
* `flag_fragility()` -- five-level fragility classification, auto-computes CFI.
* `compute_cfi()` / `print_cfi()` -- CFI scoring and display.
* `interpret_sensitivity()` -- concise template-based narrative interpretation.
* `generate_report()` -- structured markdown/text report with CFI section.
* `visualize_sensitivity()` -- sensitivity plot integration.
* `sens_results` S3 class for unified output across all functions.
