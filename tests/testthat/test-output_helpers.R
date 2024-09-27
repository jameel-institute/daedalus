# Tests for epidemic summary functions
# Test `get_new_infections()`
test_that("Daily incidence: basic expectations", {
  country = daedalus_country("Canada")
  data <- daedalus(country, "sars_cov_1")
  
  expect_no_condition(
    {incidence = get_incidence(data)}
  )
  expect_setequal(
    unique(incidence$measure),
    c("daily_infections", "daily_deaths", "daily_hospitalisations")
  )
  expect_gte(
    min(incidence$value), 0.0
  )
  expect_lt(
    max(incidence$value),
    sum(get_data(country, "demography"))
  )
  
  expect_error(
    get_incidence(data, measures = "dummy")
  )
  expect_error(
    get_incidence(data, groups = "dummy")
  )
})

test_that("Epidemic summary: basic expectations", {
  country = daedalus_country("Canada")
  data <- daedalus(country, daedalus_infection("sars_cov_1", rho = 0))
  
  expect_no_condition(
    {data_summary = get_epidemic_summary(data)}
  )
  checkmate::expect_data_frame(
    data_summary, any.missing = FALSE,
    nrows = 3L, ncols = 2L
  )
  expect_setequal(
    unique(data_summary$measure),
    c("epidemic_size", "total_deaths", "total_hospitalisations")
  )
  expect_gte(
    min(data_summary$value), 0.0
  )
  # NOTE: expectations on final size only valid when reinfection = 0
  expect_lt(
    max(data_summary$value),
    sum(get_data(country, "demography"))
  )
  
  expect_error(
    get_epidemic_summary(data, measures = "dummy")
  )
  expect_error(
    get_epidemic_summary(data, groups = "dummy")
  )
})
