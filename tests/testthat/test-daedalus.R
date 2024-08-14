# Basic tests for DAEDALUS
test_that("daedalus: basic expectations", {
  # expect no conditions
  expect_no_condition({
    output <- daedalus("Canada", "influenza_1918")
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

# test that passing model parameters works
test_that("daedalus: Passing model parameters", {
  new_infection_params <- list(
    r0 = 1.3, eta = c(0.1, 0.2, 0.3, 0.4)
  )
  expect_no_condition(
    daedalus(
      "Canada", "influenza_1918",
      infect_params_manual = new_infection_params
    )
  )

  new_country_params <- list(
    contacts_workplace = rep(10, N_ECON_SECTORS)
  )
  expect_no_condition(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = new_country_params
    )
  )
})

# test statistical correctness for only the covid wildtype infection param set
test_that("daedalus: statistical correctness", {
  output <- daedalus("Canada", "influenza_1918")

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
  output <- daedalus(
    "Canada", "influenza_1918",
    infect_params_manual = list(rho = 0.0)
  )
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
    daedalus("U.K.", "influenza_1918"),
    regexp = "`country` must be one of"
  )

  # expect errors on poorly specified time_end
  expect_error(
    daedalus("Canada", "influenza_1918", time_end = -1),
    regexp = "Expected `time_end` to be a single positive integer-like number."
  )
  expect_error(
    daedalus("Canada", "influenza_1918", time_end = 100.5),
    regexp = "Expected `time_end` to be a single positive integer-like number."
  )
  expect_error(
    daedalus("Canada", "influenza_1918", time_end = Inf),
    regexp = "Expected `time_end` to be a single positive integer-like number."
  )

  # expect errors on epidemic
  expected_error <- "Expected `epidemic` to be a string from among `epidemic_names`"
  expect_error(
    daedalus("Canada", "unfluenza_1920"),
    regexp = "`epidemic` must be one of"
  )
  expect_error(
    daedalus("Canada", NULL),
    regexp = "`epidemic` must be a character vector, not `NULL`."
  )
  expect_error(
    daedalus("Canada", NA_character_),
    regexp = "`epidemic` must be a single string, not a character `NA`."
  )
  expect_error(
    daedalus(
      "Canada", 1L
    ),
    regexp = "`epidemic` must be a character vector, not the number 1"
  )
})
