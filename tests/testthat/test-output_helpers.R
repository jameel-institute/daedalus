# Tests for epidemic summary functions
# Test `get_new_infections()`
test_that("Daily incidence: basic expectations", {
  country <- daedalus_country("Canada")
  data <- daedalus(country, "sars_cov_1")

  expect_no_condition({
    incidence <- get_incidence(data)
  })
  expect_setequal(
    unique(incidence$measure),
    c("daily_infections", "daily_deaths", "daily_hospitalisations")
  )
  expect_gte(min(incidence$value), 0.0)
  expect_lt(max(incidence$value), sum(get_data(country, "demography")))

  expect_error(get_incidence(data, measures = "dummy"))
  expect_error(get_incidence(data, groups = "dummy"))
  expect_error(
    get_incidence("dummy_data"),
    "(Expected `data`)*(data.frame)*(daedalus_output)"
  )
})

test_that("Epidemic summary: basic expectations", {
  country <- daedalus_country("Canada")
  data <- daedalus(country, daedalus_infection("sars_cov_1", rho = 0))

  expect_no_condition({
    data_summary <- get_epidemic_summary(data)
  })
  checkmate::expect_data_frame(
    data_summary,
    any.missing = FALSE,
    nrows = 3L,
    ncols = 2L
  )
  expect_setequal(
    unique(data_summary$measure),
    c("epidemic_size", "total_deaths", "total_hospitalisations")
  )
  expect_gte(min(data_summary$value), 0.0)
  # NOTE: expectations on final size only valid when reinfection = 0
  expect_lt(max(data_summary$value), sum(get_data(country, "demography")))

  expect_error(get_epidemic_summary(data, measures = "dummy"))
  expect_error(get_epidemic_summary(data, groups = "dummy"))
  expect_error(
    get_epidemic_summary("dummy_data"),
    "(Expected `data` to be)*(data.frame)*(daedalus_output)"
  )
})

# Test get_new_vaccinations() - expect that there are no
# vaccinations before the scenario-specific start time
test_that("New vaccinations: basic expectations", {
  vaccine_level <- daedalus_vaccination("medium", "Canada")
  vax_time <- get_data(vaccine_level, "start_time")
  time_end <- 600
  data <- daedalus(
    "Canada",
    daedalus_infection("sars_cov_1", rho = 0.0),
    vaccine_investment = vaccine_level,
    time_end = time_end
  )

  expect_no_condition(get_new_vaccinations(data))
  expect_no_condition(get_new_vaccinations(
    data,
    groups = c("age_group", "econ_sector")
  ))
  expect_error(
    get_new_vaccinations("dummy_data"),
    "(Expected `data` to be)*(data.frame)*(daedalus_output)"
  )
  expect_error(get_new_vaccinations(
    data,
    groups = c("age_group", "vaccine_group")
  ))

  # check that the first vaccinations start at vax_time + 2
  new_vax <- get_new_vaccinations(data)$new_vaccinations

  # equivalency expectation due to integer/double comparison
  expect_equal(min(which(new_vax > 0)), vax_time + 2L, ignore_attr = TRUE)

  # check new vaccinations is always positive
  expect_gte(min(new_vax), 0.0)
  checkmate::expect_numeric(
    new_vax,
    finite = TRUE,
    any.missing = FALSE,
    len = time_end + 1
  )
})
