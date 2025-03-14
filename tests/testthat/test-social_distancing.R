# Tests for the internal function for social distancing coefficient
test_that("Social distancing: basic expectations", {
  lower_limit <- 0.15
  coeffs <- get_distancing_coefficient(0:1e4)
  expect_gte(min(coeffs), lower_limit)
  expect_identical(max(coeffs), 1.0)
})
