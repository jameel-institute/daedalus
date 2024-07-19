# Basic tests for DAEDALUS
initial_state <- default_inputs()[["initial_state"]]

test_that("daedalus: basic expectations", {
  # expect no conditions
  expect_no_condition(
    do.call(daedalus, default_inputs())
  )

  output <- do.call(daedalus, default_inputs())

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
  output <- do.call(daedalus, default_inputs())

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
  no_reinfections <- default_inputs()
  no_reinfections[["parameters"]][["rho"]] <- 0.0

  output <- do.call(daedalus, no_reinfections)
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
  # expect errors on poorly specified initial state
  expect_error(
    daedalus(initial_state = as.data.frame(initial_state)),
    regexp = "(Expected `initial_state` to be a numeric)*(array)"
  )
  expect_error(
    daedalus(initial_state = matrix("1", N_AGE_GROUPS, N_EPI_COMPARTMENTS)),
    regexp = "(Expected `initial_state` to be a numeric)*(array)"
  )
  expect_error(
    daedalus(initial_state = array(1, dim = c(1L, 1L, 3L, 4L))),
    regexp = "(Expected `initial_state`)*(array)*(with three dimensions)"
  )

  expected_dims <- glue::glue_collapse(
    c(N_AGE_GROUPS, N_EPI_COMPARTMENTS, N_ECON_SECTORS),
    sep = ", ", last = ", and "
  )
  expect_error(
    daedalus(initial_state = initial_state[, -i_S, ]),
    regexp = glue::glue("to have dimensions {expected_dims},")
  )
  expect_error(
    daedalus(initial_state = initial_state[-1L, , ]),
    regexp = glue::glue("to have dimensions {expected_dims},")
  )

  initial_state_ <- initial_state
  initial_state_[1L, , ] <- NA_real_
  expect_error(
    daedalus(initial_state_),
    regexp = "(Expected `initial_state`)*(array)*(with no missing elements)"
  )

  # expect errors on poorly specified time_end
  expect_error(
    daedalus(initial_state, time_end = -1),
    regexp = "Expected `time_end` to be a single positive number."
  )
  expect_error(
    daedalus(initial_state, time_end = 100.5),
    regexp = "Expected `time_end` to be a single positive number."
  )
  expect_error(
    daedalus(initial_state, time_end = Inf),
    regexp = "Expected `time_end` to be a single positive number."
  )

  # expect errors on poorly specified parameter list
  expect_error(
    daedalus(initial_state, parameters = "parameters"),
    regexp = "Expected `parameters` to be a list with only numeric elements"
  )
  expect_error(
    daedalus(
      initial_state,
      parameters = lapply(default_parameters(), as.character)
    ),
    regexp = "Expected `parameters` to be a list with only numeric elements"
  )
})
