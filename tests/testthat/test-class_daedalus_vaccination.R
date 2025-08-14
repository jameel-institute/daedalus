# Tests for the <daedalus_vaccination> class

test_that("class <daedalus_vaccination>: basic expectations", {
  country <- daedalus_country("GBR")
  expect_no_condition(lapply(
    daedalus.data::vaccination_scenario_names,
    daedalus_vaccination,
    country = country
  ))

  # rate can be set independently
  expect_no_condition(lapply(
    daedalus.data::vaccination_scenario_names,
    daedalus_vaccination,
    rate = 0.3,
    country = country
  ))

  new_nu <- 0.467
  expect_identical(
    get_data(
      daedalus_vaccination("low", rate = new_nu, country = country),
      "rate"
    ),
    new_nu
  )

  x <- daedalus_vaccination("medium", country = country)
  expect_s3_class(x, c("daedalus_vaccination", "daedalus_response"))
  expect_snapshot(x)
  checkmate::expect_list(
    x,
    c("character", "numeric", "list", "NULL")
  )
  checkmate::expect_list(
    x$parameters,
    "numeric"
  )

  # model function can handle either string or class input
  data_string <- daedalus(
    "Canada",
    "influenza_1918",
    vaccine_investment = "medium"
  )
  data_string <- get_new_vaccinations(data_string)

  data_s3 <- daedalus(
    "Canada",
    "influenza_1918",
    vaccine_investment = daedalus_vaccination("medium", country = "Canada")
  )
  data_s3 <- get_new_vaccinations(data_s3)

  expect_identical(data_string, data_s3, tolerance = 1e-6)
})

test_that("class <daedalus_vaccination>: class validation", {
  country <- "GBR"
  x <- daedalus_vaccination("low", country = country)
  class(x) <- "dummy_class"

  expect_error(
    validate_daedalus_vaccination(x),
    "(daedalus\\_vaccination)*(check class assignment)"
  )

  x <- daedalus_vaccination("medium", country = country)
  x$parameters$dummy_param <- NA_character_
  expect_error(
    validate_daedalus_vaccination(x),
    "(is class)*(daedalus\\_vaccination)*(does not have the correct attributes)"
  )

  x <- daedalus_vaccination("high", country = country)
  expect_error(
    set_data(x, rate = -99),
    "(member)*(rate)*(must be a single finite positive number)"
  )
})

test_that("class <daedalus_vaccination>: access and assignment", {
  country <- "GBR"
  expect_no_condition(lapply(
    daedalus.data::vaccination_parameter_names,
    function(x) {
      y <- daedalus_vaccination("medium", country)

      get_data(y, x)
    }
  ))

  new_rate <- 0.345
  x <- daedalus_vaccination("medium", country)

  expect_no_condition({
    new_vax <- set_data(x, rate = new_rate)
  })
  expect_identical(get_data(new_vax, "rate"), new_rate)

  # access and setting errors
  expect_error(get_data(x, "vax_rate"))
  expect_error({
    set_data(x, vax_rate = 0.01)
  })
})

test_that("Vaccination events launch and end as expected", {
  # expect vaccination is launched if chosen and does not end
  cty <- "THA"
  vax_time <- 33
  v <- daedalus_vaccination("low", cty, start_time = vax_time)
  expected_vaccinations <- v$value_state_off

  output <- daedalus(
    cty,
    "sars_cov_1",
    vaccine_investment = v
  )

  expect_true(
    any(grepl("vaccination_time_on*", output$event_data$name))
  )
  expect_identical(
    output$event_data[
      grepl("vaccination_time_on", output$event_data$name),
      "time"
    ],
    vax_time
  )

  total_vaccinations <- sum(get_new_vaccinations(output)$new_vaccinations)
  expect_identical(
    total_vaccinations,
    expected_vaccinations,
    tolerance = 1e-6
  )
})

test_that("class <daedalus_vaccination>: errors", {
  expect_error(
    daedalus_vaccination("LOW", "GBR"),
    "`name` must be one of"
  )
  expect_error(
    daedalus_vaccination("dummy", "GBR"),
    "`name` must be one of"
  )
  expect_error(
    daedalus_vaccination("medium", rate = "0.01", "GBR"),
    "(rate)*(Must be of type 'number')"
  )
  expect_error(
    daedalus_vaccination("medium", "GBR", rate = c(0.01, 0.01)),
    "(rate)*(Must have length 1)"
  )
  expect_error(
    daedalus_vaccination("medium", "GBR", rate = -0.01),
    "(rate)*(Element 1 is not >= 0)"
  )
  expect_error(
    daedalus_vaccination("medium", "GBR", rate = NA_real_),
    "(rate)*(May not be NA)"
  )
})
