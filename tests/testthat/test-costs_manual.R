# Tests on model cost function
test_that("Daedalus costs method for data.frame", {
  # use daedalus model as base
  output <- daedalus("Canada", "influenza_1918", time_end = 100)

  data <- get_data(output)
  comp_non_working <- c(
    "infect_symp",
    "hospitalised_recov",
    "hospitalised_death",
    "dead"
  )
  comp_infected <- "infect_symp"
  comp_dead <- "dead"

  daily_gva <- output$country_parameters$gva
  workforce <- output$country_parameters$workers
  vsl_by_age <- output$country_parameters$vsl
  life_expectancy <- output$country_parameters$life_expectancy

  value_school_year <- get_value_school_year(output$country_parameters$gni)
  n_students <- output$country_parameters$demography[i_SCHOOL_AGE]

  edu_effectiveness_remote <- EDU_EFFECTIVENESS_REMOTE

  productivity_loss_infection <- 1.0

  costs_manual <- get_costs(
    data,
    comp_non_working,
    comp_infected,
    comp_dead,
    daily_gva,
    workforce,
    vsl_by_age,
    life_expectancy,
    value_school_year,
    n_students,
    productivity_loss_infection = productivity_loss_infection,
    edu_effectiveness_remote = edu_effectiveness_remote
  )
  costs_auto <- get_costs(output)

  expect_identical(
    costs_manual,
    costs_auto
  )
})
