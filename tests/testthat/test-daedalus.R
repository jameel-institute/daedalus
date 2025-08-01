# Tests for daedalus()
country_canada <- daedalus_country("Canada")

test_that("daedalus: basic expectations", {
  time_end <- 700L
  # expect no conditions
  expect_no_condition({
    daedalus(country_canada, "influenza_1918")
  })
  output <- daedalus(country_canada, "influenza_1918", time_end = time_end)

  # expect type double and non-negative
  expect_s3_class(output, "daedalus_output")
  data <- get_data(output)
  expect_length(data, N_OUTPUT_COLS)

  # as non-working groups do not have data per sector
  expected_rows <- (time_end + 1L) *
    N_MODEL_COMPARTMENTS *
    (N_AGE_GROUPS + N_ECON_SECTORS) *
    N_VACCINE_STRATA

  expected_rows <- expected_rows +
    ((N_AGE_GROUPS + N_ECON_SECTORS) * (time_end + 1L))

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

  # check for no change in total size; some rounding needed
  pop_sizes_time <- aggregate(
    data[
      data$compartment %in% COMPARTMENTS[i_EPI_COMPARTMENTS],
    ],
    value ~ time,
    sum
  )$value
  pop_size <- sum(get_data(country_canada, "demography"))
  expect_length(unique(round(pop_sizes_time)), 1L)
  expect_true(all(abs(pop_sizes_time - pop_size) <= 1e-6))

  # expect vaccination groups are zero
  checkmate::expect_numeric(
    data[data$vaccine_group == "vaccinated", "value"],
    lower = 0,
    upper = 0
  )
})

test_that("daedalus: Can run with ISO2 country parameter", {
  expect_no_condition({
    daedalus("CA", "influenza_1918")
  })
  expect_s3_class(
    daedalus("CA", "influenza_1918"),
    "daedalus_output"
  )

  data <- get_data(daedalus("CA", "influenza_1918"))
  expect_length(data, N_OUTPUT_COLS)
})

test_that("daedalus: Can run with ISO3 country parameter", {
  expect_no_condition({
    daedalus("GBR", "influenza_1918")
  })
  expect_s3_class(
    daedalus("GBR", "influenza_1918"),
    "daedalus_output"
  )

  data <- get_data(daedalus("THA", "influenza_1918"))
  expect_length(data, N_OUTPUT_COLS)
})

test_that("daedalus: Can run with ODE control arguments", {
  expect_no_condition(
    daedalus("GBR", "influenza_1918", atol = 1e-5)
  )
  expect_error(
    daedalus("GBR", "influenza_1918", dummy = 1e-5),
    "unused argument"
  )
})

# test that daedalus runs for all epidemic infection parameter sets
test_that("daedalus: Runs for all country x infection x response", {
  country_infection_combos <- data.table::CJ(
    country = daedalus.data::country_names,
    infection = daedalus.data::epidemic_names
  )

  time_end <- 10

  # expect no conditions
  invisible(Map(
    country_infection_combos$country,
    country_infection_combos$infection,
    f = function(x, y) {
      expect_no_condition(
        daedalus(x, y, time_end = time_end, response_time = time_end)
      )
    }
  ))
})

test_that("daedalus: Runs with custom openness values", {
  expect_no_condition(
    daedalus(
      "GBR",
      "sars_cov_1",
      response_strategy = rep(0.5, N_ECON_SECTORS),
      time_end = 100
    )
  )
  expect_error(
    daedalus(
      "GBR",
      "sars_cov_1",
      response_strategy = rep(0.5, N_ECON_SECTORS - 1)
    ),
    "Must have length 45"
  )
  expect_error(
    daedalus(
      "GBR",
      "sars_cov_1",
      NA_character_
    ),
    "Got an unexpected value for `response_strategy`"
  )
  expect_error(
    daedalus(
      "GBR",
      "sars_cov_1",
      "elimination",
      response_time = 0
    ),
    "Expected `response_time` to be between 1"
  )
  expect_error(
    daedalus(
      "GBR",
      "sars_cov_1",
      "elimination",
      response_duration = c(1, 1)
    ),
    "Expected `response_duration` to be a single positive integer-like"
  )
})

# test that passing model parameters works
test_that("daedalus: Passing model parameters", {
  expect_no_condition(daedalus(
    country_canada,
    daedalus_infection("influenza_1918", r0 = 1.3, eta = c(0.1, 0.2, 0.3, 0.4))
  ))
})

# test statistical correctness for only the covid wildtype infection param set
test_that("daedalus: statistical correctness", {
  output <- daedalus("Canada", "influenza_1918")
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
  output <- daedalus("Canada", daedalus_infection("influenza_1918", rho = 0.0))
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

# check for vaccination mechanism
test_that("daedalus: vaccination works", {
  # NOTE: test for truly no vaccination are in default daedalus test above

  # NOTE: event starting at t = 0 does not work
  vax <- daedalus_vaccination("low", "THA", 10, 0.1, 100)
  expect_no_condition(
    daedalus("THA", "sars_cov_1", vaccine_investment = vax)
  )
  data <- get_data(daedalus("THA", "sars_cov_1", vaccine_investment = vax))

  # expect vaccination group is non-zero
  data_vax_susc <- data[
    data$vaccine_group == "vaccinated" & data$compartment == "susceptible",
    "value"
  ]
  expect_true(any(data_vax_susc > 0))

  # test that vaccination infection pathways are active
  data_vax_expo <- data[
    data$vaccine_group == "vaccinated" & data$compartment == "exposed",
    "value"
  ]
  expect_true(any(data_vax_expo > 0))

  # expect that vaccination reduces final size
  output_novax <- get_data(daedalus("THA", "sars_cov_1"))
  fs_daedalus <- get_epidemic_summary(data, "infections")$value
  fs_daedalus_novax <- get_epidemic_summary(output_novax, "infections")$value

  expect_lt(fs_daedalus, fs_daedalus_novax)

  # see `../test-equivalence.R` for tests that no vaccination in
  # `daedalus()` is equivalent to no vaccination in `daedalus()`
})

test_that("daedalus: advanced vaccination features", {
  # use dummy scenarios to check that vaccination uptake limit is respected
  x <- daedalus_country("THA")
  disease_x <- daedalus_infection("sars_cov_1", r0 = 0)

  uptake_limit <- 40
  popsize <- sum(get_data(x, "demography"))
  vax <- daedalus_vaccination(
    "high",
    uptake_limit = uptake_limit,
    country = x,
    waning_period = 3000
  )
  # final size is zero
  data <- get_data(
    daedalus(
      "THA",
      disease_x,
      vaccine_investment = vax,
      time_end = 600
    )
  )

  n_vax <- aggregate(
    data[
      data$compartment %in%
        c("susceptible", "recovered") &
        data$vaccine_group == "vaccinated",
    ],
    value ~ time,
    sum
  )
  n_vax <- tail(n_vax, 1)$value

  # higher tolerance as vaccination is expected to be asymptotic
  expect_identical(
    n_vax,
    uptake_limit * popsize / 100,
    tolerance = 1
  )
})

test_that("daedalus: responses triggered by hospital capacity event", {
  # with absolutely no response
  expect_no_condition(
    daedalus("GBR", "sars_cov_1")
  )

  # with named responses (none = absolutely no resp)
  invisible(
    lapply(
      names(daedalus.data::closure_data),
      function(x) {
        expect_no_condition({
          daedalus("GBR", "sars_cov_1", x, time_end = 100)
        })
      }
    )
  )

  # expect lower final sizes for all interventions
  # very low hosp capacity trigger
  x <- daedalus_country("GBR")
  x$hospital_capacity <- 1e4

  output_list <- lapply(
    names(daedalus.data::closure_data),
    daedalus,
    country = x,
    infection = "sars_cov_1"
  )
  resp_scenario_names <- names(daedalus.data::closure_data)
  output_fs <- vapply(
    output_list,
    function(x) {
      get_epidemic_summary(x, "infections")$value
    },
    FUN.VALUE = numeric(1)
  )
  names(output_fs) <- resp_scenario_names

  invisible(
    lapply(output_fs[names(output_fs) != "none"], expect_lt, output_fs["none"])
  )
})

# NOTE: see PR #83 for a reprex
skip("Root jumping causes test to fail")
test_that("daedalus: responses ended by epidemic growth", {
  # start response early
  time_end <- 100
  x <- daedalus_country("GBR")
  x$hospital_capacity <- 1e3

  d <- daedalus_infection("influenza_2009")

  output <- daedalus(
    x,
    "influenza_2009",
    "elimination",
    time_end = time_end,
    response_time = 98
  )

  event_data <- output$event_data
  output <- output$data
  # check that epidemic stops growing by IPR method; IPR < gamma
  ipr <- colSums(output$new_inf) / colSums(output$Is + output$Ia)
  expect_lt(
    min(ipr - d$gamma_Is),
    0.0
  )

  # find end idx
  end_time <- which.min(abs(ipr - d$gamma_Ia)) + 1

  # check that response is switched off at expected time
  expect_identical(
    event_data[event_data$name == "npi_state_off", "time"],
    end_time
  )
})

test_that("daedalus: Errors and messages", {
  expect_error(
    daedalus(
      "GBR",
      "sars_cov_1",
      as.character(1:49)
    ),
    "Got an unexpected value for `response_strategy`."
  )
  expect_error(
    daedalus(
      "GBR",
      "sars_cov_1",
      1:50
    ),
    "Assertion on 'response_strategy' failed: Must have length"
  )
  expect_error(
    daedalus(
      "GBR",
      "sars_cov_1",
      "elimination",
      time_end = -1
    ),
    "Expected `time_end` to be a single positive integer-like number."
  )
  expect_error(
    daedalus(
      "GBR",
      "sars_cov_1",
      "elimination",
      time_end = 0
    ),
    "Expected `time_end` to be a single positive integer-like number."
  )
  expect_error(
    daedalus(
      "GBR",
      "sars_cov_1",
      "elimination",
      time_end = c(10, 10)
    ),
    "Expected `time_end` to be a single positive integer-like number."
  )
})
