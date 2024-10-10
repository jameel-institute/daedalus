# Tests for the <daedalus_vaccination> class

test_that("class <daedalus_vaccination>: basic expectations", {
  expect_no_condition(
    lapply(daedalus::vaccination_scenario_names, daedalus_vaccination)
  )

  # rate can be set independently
  expect_no_condition(
    lapply(
      daedalus::vaccination_scenario_names, daedalus_vaccination,
      nu = 0.3
    )
  )

  new_nu <- 0.467
  expect_identical(
    get_data(
      daedalus_vaccination("low", nu = new_nu),
      "nu"
    ),
    new_nu
  )

  x <- daedalus_vaccination("medium")
  expect_s3_class(x, "daedalus_vaccination")
  expect_snapshot(x)
  checkmate::expect_list(
    x, c("character", "numeric"),
    any.missing = FALSE
  )

  # model function can handle either string or class input
  data_string <- daedalus(
    "Canada", "influenza_1918",
    vaccine_investment = "medium"
  )
  data_string <- get_new_vaccinations(data_string)

  data_s3 <- daedalus(
    "Canada", "influenza_1918",
    vaccine_investment = daedalus_vaccination("medium")
  )
  data_s3 <- get_new_vaccinations(data_s3)

  expect_identical(
    data_string,
    data_s3,
    tolerance = 1e-6
  )

  expect_snapshot(
    daedalus::vaccination_scenario_data
  )
})

test_that("class <daedalus_vaccination>: class validation", {
  x <- daedalus_vaccination("low")
  class(x) <- "dummy_class"

  expect_error(
    validate_daedalus_vaccination(x)
  )

  x <- daedalus_vaccination("medium")
  x$dummy_param <- NA_character_
  expect_error(
    validate_daedalus_vaccination(x)
  )

  x <- daedalus_vaccination("high")
  x$nu <- -99L
  expect_error(
    validate_daedalus_vaccination(x)
  )
})

test_that("class <daedalus_vaccination>: access and assignment", {
  expect_no_condition(
    lapply(daedalus::vaccination_parameter_names, function(x) {
      y <- daedalus_vaccination("medium")

      get_data(y, x)
    })
  )

  new_rate <- 0.345
  x <- daedalus_vaccination("medium")

  expect_no_condition({
    new_vax <- set_data(x, nu = new_rate)
  })
  expect_identical(
    get_data(new_vax, "nu"),
    new_rate
  )

  # access and setting errors
  expect_error(
    get_data(x, "vax_rate")
  )
  expect_error({
    set_data(x, vax_rate = 0.01)
  })
})

test_that("class <daedalus_vaccination>: errors", {
  expect_error(
    daedalus_vaccination("LOW")
  )
  expect_error(
    daedalus_vaccination("dummy")
  )
  expect_error(
    daedalus_vaccination("medium", nu = "0.01")
  )
  expect_error(
    daedalus_vaccination("medium", nu = c(0.01, 0.01))
  )
  expect_error(
    daedalus_vaccination("medium", nu = -0.01)
  )
  expect_error(
    daedalus_vaccination("medium", nu = NULL)
  )
  expect_error(
    daedalus_vaccination("medium", nu = NA_real_)
  )
  expect_error(
    daedalus_vaccination("medium", dummy_param = 0.01)
  )
})
