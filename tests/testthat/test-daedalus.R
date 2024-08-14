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

# test that daedalus runs for all epidemic infection parameter sets
test_that("daedalus: Runs for all epidemics", {
  epidemic_names <- names(infection_data)
  # expect no conditions
  expect_no_condition({
    output_list <- lapply(epidemic_names, daedalus, country = "Canada")
  })

  # expect classed output, type double, and non-negative
  checkmate::expect_list(output_list, "deSolve")
  expect_true(
    all(vapply(output_list, function(x) {
      all(x >= 0.0)
    }, FUN.VALUE = logical(1)))
  )
})

# test statistical correctness for only the covid wildtype infection param set
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

  # expect errors on epidemic
  expected_error <- "Expected `epidemic` to be a string from among `epidemic_names`"
  expect_error(
    daedalus(
      "Canada", "unfluenza_1920"
    ),
    regexp =
    )
  expect_error(
    daedalus(
      "Canada", NULL
    ),
    regexp = "Expected `epidemic` to be a string from among `epidemic_names`"
  )
  expect_error(
    daedalus(
      "Canada", NA_character_
    ),
    regexp = "Expected `epidemic` to be a string from among `epidemic_names`"
  )
  expect_error(
    daedalus(
      "Canada", 1L
    ),
    regexp = "Expected `epidemic` to be a string from among `epidemic_names`"
  )
})
