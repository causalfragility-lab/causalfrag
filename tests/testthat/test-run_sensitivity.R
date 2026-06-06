test_that("run_sensitivity returns sens_results for lm with sensemakr", {
  skip_if_not_installed("sensemakr")
  data("darfur", package = "sensemakr")
  fit <- lm(peacefactor ~ directlyharmed + age + female + village,
            data = darfur)
  res <- run_sensitivity(fit, treatment = "directlyharmed",
                         data = darfur, verbose = FALSE)
  expect_s3_class(res, "sens_results")
  expect_equal(res$design, "regression")
  expect_equal(res$treatment, "directlyharmed")
  expect_true("sensemakr" %in% res$frameworks)
  expect_false(is.null(res$results$sensemakr$rv))
  expect_true(is.numeric(res$results$sensemakr$rv))
})

test_that("run_sensitivity works with evalue framework", {
  skip_if_not_installed("EValue")
  fit <- lm(mpg ~ am + wt + hp, data = mtcars)
  res <- run_sensitivity(fit, treatment = "am", data = mtcars,
                         frameworks = "evalue", verbose = FALSE)
  expect_s3_class(res, "sens_results")
  expect_true("evalue" %in% res$frameworks)
  expect_true(is.numeric(res$results$evalue$evalue))
  expect_true(res$results$evalue$evalue >= 1)
})

test_that("run_sensitivity auto-detects design", {
  skip_if_not_installed("sensemakr")
  fit <- lm(mpg ~ am + wt, data = mtcars)
  res <- run_sensitivity(fit, treatment = "am", data = mtcars,
                         verbose = FALSE)
  expect_equal(res$design, "regression")
})

test_that("run_sensitivity errors on missing treatment name", {
  fit <- lm(mpg ~ am + wt, data = mtcars)
  expect_error(
    run_sensitivity(fit, treatment = c("am", "wt"), data = mtcars),
    regexp = "single character"
  )
})

test_that("flag_fragility works on run_sensitivity output", {
  skip_if_not_installed("sensemakr")
  data("darfur", package = "sensemakr")
  fit <- lm(peacefactor ~ directlyharmed + age + female + village,
            data = darfur)
  res <- run_sensitivity(fit, treatment = "directlyharmed",
                         data = darfur, verbose = FALSE)
  res <- flag_fragility(res)
  expect_true(res$fragility %in% c("stable", "fragile", "critical"))
})
