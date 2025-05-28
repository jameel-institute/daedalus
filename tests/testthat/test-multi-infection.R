test_that("daedalus can handle multiple infection objects", {
  # NOTE: only for testing, the intention is to accommodate uncertainty in
  # single infection's parameters, not to compare across infections
  infection <- lapply(
    daedalus.data::epidemic_names,
    daedalus_infection
  )

  expect_no_condition(
    daedalus_multi_infection(
      "GBR",
      infection,
      time_end = 100
    )
  )

  expect_no_condition(
    daedalus_multi_infection(
      "GBR",
      infection,
      rep(0.8, N_ECON_SECTORS),
      time_end = 100
    )
  )

  expect_no_condition(
    daedalus_multi_infection(
      "GBR",
      infection,
      "elimination",
      time_end = 100
    )
  )

  expect_no_condition(
    daedalus_multi_infection(
      "GBR",
      infection,
      "elimination",
      "high",
      time_end = 100
    )
  )

  output <- daedalus_multi_infection(
    "GBR",
    infection,
    "elimination",
    time_end = 100
  )
  checkmate::expect_list(output, "daedalus_output")

  expect_identical(
    vapply(output, function(x) x$infection_parameters$name, character(1)),
    daedalus.data::epidemic_names
  )

  # passing ode control args works
  expect_no_condition(
    daedalus_multi_infection(
      "GBR",
      infection,
      "elimination",
      time_end = 100,
      max_steps = 9e3
    )
  )
})

test_that("daedalus_multi_infection: errors", {
  infection <- lapply(
    daedalus.data::epidemic_names,
    daedalus_infection
  )
  expect_error(
    daedalus_multi_infection(
      "GBR",
      list(daedalus_infection("sars_cov_1"))
    ),
    "Must have length >= 2"
  )

  expect_error(
    daedalus_multi_infection(
      "GBR",
      infection,
      response_strategy = rep("dummy", N_ECON_SECTORS)
    ),
    "Got an unexpected value for `response_strategy`"
  )
})
