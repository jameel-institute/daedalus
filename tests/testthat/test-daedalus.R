# Basic tests for DAEDALUS
test_that("daedalus: basic expectations", {
  # expect no conditions
  expect_no_condition({
    output <- daedalus("Canada")
  })

  # expect classed output, type double, and non-negative
  expect_s3_class(output, "deSolve")
  expect_type(output, "double")
  expect_true(all(output >= 0.0))

  # expect closed population with no change in total size
  # NOTE: disregard first column holding time
  # NOTE: set tolerance to a reasonable value
  expect_identical(
    sum(tail(output[, -1L], 1)),
    sum(head(output[, -1L], 1)),
    tolerance = 1e-12
  )
})

test_that("daedalus: statistical correctness", {
  output <- daedalus("Canada")

  # expectations when immunity wanes allowing R -> S
  # no elegant way of programmatically accessing idx
  deaths <- output[, (25L:28L) + 1L]
  expect_true(
    all(diff(deaths) >= 0.0)
  )
  susceptibles <- output[, (1L:4L) + 1L]
  expect_false(
    all(diff(susceptibles) <= 0.0)
  )

  recovered <- output[, (21L:24L) + 1L]
  expect_false(
    all(diff(recovered) >= 0.0)
  )
  # NOTE: added as a baseline to compare future model structure changes
  # checking with immunity waning
  expect_snapshot(
    head(output[, seq(N_AGE_GROUPS * N_EPI_COMPARTMENTS + 1L)], 10L)
  )

  # expectations when immunity does not wane
  # - monotonically decreasing susceptibles
  # - monotonically increasing recovered and deaths
  output <- daedalus("Canada", rho = 0.0)
  susceptibles <- output[, (1L:4L) + 1L]
  expect_true(
    all(diff(susceptibles) <= 0.0)
  )

  recovered <- output[, (21L:24L) + 1L]
  expect_true(
    all(diff(recovered) >= 0.0)
  )

  deaths <- output[, (25L:28L) + 1L]
  expect_true(
    all(diff(deaths) >= 0.0)
  )

  # checking without immunity waning allowed
  # checking only first economic stratum
  expect_snapshot(
    head(output[, seq(N_AGE_GROUPS * N_EPI_COMPARTMENTS + 1L)], 10L)
  )
})

test_that("daedalus: errors and warnings", {
  # expect errors on country
  expect_error(
    daedalus("U.K."),
    regexp = "Expected `country` to be.*from among `country_names`"
  )

  # expect errors on poorly specified time_end
  expect_error(
    daedalus("Canada", time_end = -1),
    regexp = "Expected `time_end` to be a single positive integer-like number."
  )
  expect_error(
    daedalus("Canada", time_end = 100.5),
    regexp = "Expected `time_end` to be a single positive integer-like number."
  )
  expect_error(
    daedalus("Canada", time_end = Inf),
    regexp = "Expected `time_end` to be a single positive integer-like number."
  )
})
