# Basic tests for DAEDALUS
# select a test country
country_canada <- "Canada"

test_that("daedalus: basic expectations", {
  time_end <- 700L
  # expect no conditions
  expect_no_condition({
    output <- daedalus2(country_canada, "influenza_1918", time_end = time_end)
  })

  # expect type double and non-negative
  expect_s3_class(output, "daedalus_output")
  data <- get_data(output)
  expect_length(data, N_OUTPUT_COLS)

  # as non-working groups do not have data per sector
  expected_rows <- (time_end + 1L) *
    N_MODEL_COMPARTMENTS *
    (N_AGE_GROUPS + N_ECON_SECTORS) *
    N_VACCINE_STRATA
  # add new vaccinations which are only logged at the group level
  expected_rows <- expected_rows +
    ((time_end + 1L) * (N_AGE_GROUPS + N_ECON_SECTORS))

  expect_identical(nrow(data), expected_rows)
  expect_named(
    data,
    c(
      "time",
      "age_group",
      "compartment",
      "econ_sector",
      "vaccine_group",
      "value"
    ),
    ignore.order = TRUE
  )
  checkmate::expect_numeric(data[["time"]], lower = 0, upper = time_end)
  expect_type(data[["age_group"]], "character")
  expect_type(data[["compartment"]], "character")
  expect_type(data[["econ_sector"]], "character")
  expect_type(data[["vaccine_group"]], "character")
  expect_type(data[["value"]], "double")

  # expect closed population with no change in total size
  # NOTE: disregard first column holding time
  # NOTE: set tolerance to a reasonable value
  expect_identical(
    sum(
      data[
        data$time == max(data$time) &
          data$compartment %in% COMPARTMENTS[i_EPI_COMPARTMENTS] &
          data$vaccine_group != "new_vaccinations",
      ]$value
    ),
    sum(
      data[
        data$time == min(data$time) &
          data$compartment %in% COMPARTMENTS[i_EPI_COMPARTMENTS] &
          data$vaccine_group != "new_vaccinations",
      ]$value
    ),
    tolerance = 1e-12
  )
})

test_that("Can run with ISO2 country parameter", {
  expect_no_condition({
    output <- daedalus2("CA", "influenza_1918")
  })
  data <- get_data(output)
  expect_length(data, N_OUTPUT_COLS)
})

test_that("Can run with ISO3 country parameter", {
  expect_no_condition({
    output <- daedalus2("CAN", "influenza_1918")
  })
  data <- get_data(output)
  expect_length(data, N_OUTPUT_COLS)
})

# test that daedalus runs for all epidemic infection parameter sets
test_that("daedalus: Runs for all country x infection x response", {
  country_infection_combos <- data.table::CJ(
    country = daedalus.data::country_names,
    infection = daedalus.data::epidemic_names
  )

  dummy_vax <- daedalus_vaccination("low", start_time = 5)
  time_end <- 10

  # expect no conditions
  invisible(Map(
    country_infection_combos$country,
    country_infection_combos$infection,
    f = function(x, y) {
      expect_no_condition(daedalus2(
        x,
        y,
        time_end = time_end,
        response_time = 2,
        vaccine_investment = dummy_vax
      ))
    }
  ))
})

# test that passing model parameters works
test_that("daedalus: Passing model parameters", {
  expect_no_condition(daedalus2(
    country_canada,
    daedalus_infection("influenza_1918", r0 = 1.3, eta = c(0.1, 0.2, 0.3, 0.4))
  ))
})

# test statistical correctness for only the covid wildtype infection param set
test_that("daedalus: statistical correctness", {
  output <- daedalus2("Canada", "influenza_1918")
  data <- get_data(output)
  # tests on single compartment without workers
  data <- data[data$age_group == "65+" & data$vaccine_group == "unvaccinated", ]

  # expectations when immunity wanes allowing R -> S
  # no elegant way of programmatically accessing idx
  deaths <- data[data$compartment == "dead", ]$value
  expect_true(all(diff(deaths) >= 0.0))
  susceptibles <- data[data$compartment == "susceptible", ]$value
  expect_lt(min(diff(susceptibles)), 0.0)

  # NOTE: expecting false due to R => S transitons
  recovered <- data[data$compartment == "recovered", ]$value
  expect_false(all(diff(recovered) >= 0.0))

  # expectations when immunity does not wane
  # - monotonically decreasing susceptibles
  # - monotonically increasing recovered and deaths
  output <- daedalus2("Canada", daedalus_infection("influenza_1918", rho = 0.0))
  data <- get_data(output)
  data <- data[data$age_group == "65+", ]

  susceptibles <- data[
    data$compartment == "susceptible" &
      data$vaccine_group != "new_vaccinations",
  ]
  susceptibles <- tapply(susceptibles$value, susceptibles$time, sum)
  expect_lte(
    max(diff(susceptibles)),
    1e-6 # allowing small positive diff
  )

  # NOTE: allow very small negative values
  recovered <- data[
    data$compartment == "recovered" & data$vaccine_group != "new_vaccinations",
  ]
  recovered <- tapply(recovered$value, recovered$time, sum)
  expect_gte(min(diff(recovered)), -1e-6)

  deaths <- data[
    data$compartment == "dead" & data$vaccine_group == "unvaccinated",
  ]$value
  expect_gte(min(diff(deaths)), 0.0)
})

test_that("daedalus: errors and warnings", {
  # expect errors on country
  expect_error(
    daedalus2("U.K.", "influenza_1918"),
    regexp = "`country` must be one of"
  )

  # expect errors on poorly specified time_end
  expect_error(
    daedalus2(country_canada, "influenza_1918", time_end = -1),
    regexp = "Expected `time_end` to be a single positive integer-like number."
  )
  expect_error(
    daedalus2(country_canada, "influenza_1918", time_end = 100.5),
    regexp = "Expected `time_end` to be a single positive integer-like number."
  )
  expect_error(
    daedalus2(country_canada, "influenza_1918", time_end = Inf),
    regexp = "Expected `time_end` to be a single positive integer-like number."
  )

  expect_error(daedalus2(country_canada, daedalus_infection("unfluenza_1920")))

  # expect errors on bad response time
  expect_error(
    daedalus2(
      country_canada,
      daedalus_infection("sars_cov_1"),
      response_strategy = "elimination",
      response_time = -1
    ),
    regexp = "Expected `response_time` to be between 1"
  )
  expect_error(
    daedalus2(
      country_canada,
      daedalus_infection("sars_cov_1"),
      response_strategy = "elimination",
      response_time = 30,
      time_end = 30
    ),
    regexp = "Expected `response_time` to be between 1"
  )
  expect_error(
    daedalus2(
      country_canada,
      daedalus_infection("sars_cov_1"),
      response_strategy = "elimination",
      response_time = Inf
    ),
    regexp = "Expected `response_time` to be between 1"
  )
  expect_error(
    daedalus2(
      country_canada,
      daedalus_infection("sars_cov_1"),
      response_strategy = "elimination",
      response_time = NA_real_
    ),
    regexp = "Expected `response_time` to be between 1"
  )
  expect_error(
    daedalus2(
      country_canada,
      daedalus_infection("sars_cov_1"),
      response_strategy = "elimination",
      response_time = 10.5
    ),
    regexp = "Expected `response_time` to be between 1"
  )
})
