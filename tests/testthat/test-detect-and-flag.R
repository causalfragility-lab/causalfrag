test_that("detect_design correctly identifies lm as regression", {
  fit <- lm(mpg ~ am + wt, data = mtcars)
  expect_equal(detect_design(fit, verbose = FALSE), "regression")
})


test_that("detect_design correctly identifies glm as regression", {
  fit <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  expect_equal(detect_design(fit, verbose = FALSE), "regression")
})


test_that("detect_design returns unknown for unrecognised class", {
  fake_model <- structure(list(), class = "totally_unknown_model")
  expect_equal(detect_design(fake_model, verbose = FALSE), "unknown")
})


test_that("flag_fragility classifies stable result correctly", {
  res <- new_sens_results(
    design = "regression", treatment = "t", outcome = "y",
    frameworks = c("sensemakr", "evalue"),
    results = list(
      sensemakr = list(rv = 0.35),   # > 0.20 threshold → stable
      evalue    = list(evalue = 3.2) # > 2.00 threshold → stable
    )
  )
  res <- flag_fragility(res)
  expect_equal(res$fragility, "stable")
})


test_that("flag_fragility classifies fragile result correctly", {
  res <- new_sens_results(
    design = "regression", treatment = "t", outcome = "y",
    frameworks = c("sensemakr", "evalue"),
    results = list(
      sensemakr = list(rv = 0.15),   # between 0.10 and 0.20 → fragile
      evalue    = list(evalue = 2.5) # > 2.00 → stable, but sensemakr is fragile
    )
  )
  res <- flag_fragility(res)
  expect_equal(res$fragility, "fragile")
})


test_that("flag_fragility classifies critical result correctly", {
  res <- new_sens_results(
    design = "regression", treatment = "t", outcome = "y",
    frameworks = "evalue",
    results = list(evalue = list(evalue = 1.2)) # < 1.50 → critical
  )
  res <- flag_fragility(res)
  expect_equal(res$fragility, "critical")
})


test_that("flag_fragility takes worst score across frameworks", {
  res <- new_sens_results(
    design = "regression", treatment = "t", outcome = "y",
    frameworks = c("sensemakr", "evalue"),
    results = list(
      sensemakr = list(rv = 0.40),    # stable
      evalue    = list(evalue = 1.1)  # critical — should dominate
    )
  )
  res <- flag_fragility(res)
  expect_equal(res$fragility, "critical")
})


test_that("flag_fragility errors on non-sens_results input", {
  expect_error(flag_fragility(list()), regexp = "sens_results")
})
