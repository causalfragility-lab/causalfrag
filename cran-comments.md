## R CMD check results

0 errors | 0 warnings | 1 note

* This is a resubmission of 0.1.0.

## Changes in response to CRAN reviewer comments (Benjamin Altmann):

* Shortened Title to < 65 characters
* Added \value tags to plot.sens_results.Rd, print.sens_results.Rd, summary.sens_results.Rd
* Replaced \dontrun{} with \donttest{} throughout
* Replaced bare print()/cat() in cfi_extensions.R with message()
* Changed inst/simulation/cfi_validation_plots.R to write to tempdir()
