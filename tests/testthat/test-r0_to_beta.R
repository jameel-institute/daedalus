test_that("Calculating beta from R0", {
  x <- daedalus_infection("sars_cov_1")
  y <- daedalus_country("United Kingdom")

  expect_no_condition(get_beta(x, y))

  expect_snapshot(get_beta(x, y))
})
