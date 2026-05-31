test_that("new_sens_results creates object with correct class", {
  res <- new_sens_results(
    design     = "regression",
    treatment  = "treat",
    outcome    = "outcome",
    frameworks = c("sensemakr", "evalue"),
    results    = list(
      sensemakr = list(rv = 0.23, rv_qa = 0.18, r2yd_x = 0.15, estimate = 0.4),
      evalue    = list(evalue = 2.8, evalue_lower = 1.9)
    )
  )
  expect_s3_class(res, "sens_results")
  expect_equal(res$design, "regression")
  expect_equal(res$treatment, "treat")
  expect_equal(res$frameworks, c("sensemakr", "evalue"))
  expect_null(res$fragility)
  expect_null(res$narrative)
  expect_false(res$llm_used)
})


test_that("new_sens_results rejects invalid design", {
  expect_error(
    new_sens_results(
      design = "invalid_design",
      treatment = "x", outcome = "y",
      frameworks = "evalue", results = list()
    ),
    regexp = "design"
  )
})


test_that("new_sens_results rejects invalid framework", {
  expect_error(
    new_sens_results(
      design = "regression",
      treatment = "x", outcome = "y",
      frameworks = c("evalue", "made_up_framework"),
      results = list()
    ),
    regexp = "frameworks"
  )
})


test_that("is_sens_results returns TRUE for sens_results objects", {
  res <- new_sens_results(
    design = "regression", treatment = "t", outcome = "y",
    frameworks = "evalue", results = list()
  )
  expect_true(is_sens_results(res))
  expect_false(is_sens_results(list()))
  expect_false(is_sens_results("not a result"))
})


test_that("print.sens_results runs without error", {
  res <- new_sens_results(
    design = "regression", treatment = "treat", outcome = "y",
    frameworks = "evalue",
    results = list(evalue = list(evalue = 2.1, evalue_lower = 1.5)),
    fragility = "stable",
    narrative = "The analysis showed robust conclusions."
  )
  expect_output(print(res), regexp = "EVALUE|Sensitivity|causalfrag")
})
