test_that("Calculating beta from R0", {
  x <- daedalus_infection("sars_cov_1")
  y <- daedalus_country("United Kingdom")

  expect_no_condition(get_beta(x, y))

  expect_snapshot(get_beta(x, y))
})

test_that("Calculating beta from R0", {
  # check R_eff return type
  x <- daedalus_infection("sars_cov_1")
  y <- daedalus_country("United Kingdom")

  parameters <- c(
    prepare_parameters.daedalus_infection(x),
    prepare_parameters.daedalus_country(y)
  )
  parameters$beta <- get_beta(x, y)

  state <- make_initial_state(y, list())

  expect_no_condition(r_eff(state, parameters))
  expect_snapshot(r_eff(state, parameters))
  z <- r_eff(state, parameters)

  checkmate::expect_number(z, lower = 0, finite = TRUE)

  # check that R_eff at initial conditions
  # is roughly equal to R0
  # NOTE: difference is due to a small number of infected
  r0 <- get_data(x, "r0")
  expect_identical(z, r0, tolerance = 1e-3)
})
