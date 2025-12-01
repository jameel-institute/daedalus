# Tests on model cost function
test_that("Daedalus costs method for data.frame", {
  # use daedalus model as base
  output <- daedalus("Canada", "influenza_1918", time_end = 100)

  x <- get_data(output)
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
    x,
    comp_non_working,
    comp_infected,
    comp_dead,
    workforce,
    daily_gva,
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

test_that("Daedalus costs for data.frame with reactive NPI", {
  # use daedalus model as base
  cty <- "CAN"
  infection <- "influenza_1918"

  npi <- daedalus_npi("elimination", cty, infection)
  output <- daedalus(cty, infection, response_strategy = npi, time_end = 100)

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
  names(n_students) <- NULL

  edu_effectiveness_remote <- EDU_EFFECTIVENESS_REMOTE

  productivity_loss_infection <- 1.0

  npi_data <- c(
    output$response_data$npi_info,
    list(openness = first(output$response_data$openness))
  )

  expect_no_condition(
    get_costs(
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
      npi_data = npi_data,
      productivity_loss_infection = productivity_loss_infection,
      edu_effectiveness_remote = edu_effectiveness_remote
    )
  )
})

test_that("Daedalus costs for data.frame with timed NPI", {
  # use daedalus model as base
  cty <- "CAN"
  infection <- "influenza_1918"

  npi <- daedalus_timed_npi(
    start_time = c(10, 60),
    end_time = c(40, 80),
    openness = list(
      rep(0.5, 45),
      rep(0.3, 45)
    ),
    cty
  )
  output <- daedalus(cty, infection, response_strategy = npi, time_end = 100)

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
  names(n_students) <- NULL

  edu_effectiveness_remote <- EDU_EFFECTIVENESS_REMOTE

  productivity_loss_infection <- 1.0

  npi_data <- c(
    output$response_data$npi_info,
    list(openness = output$response_data$openness)
  )

  expect_no_condition(
    get_costs(
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
      npi_data = npi_data,
      productivity_loss_infection = productivity_loss_infection,
      edu_effectiveness_remote = edu_effectiveness_remote
    )
  )
})

test_that("Daedalus costs methods: errors and messages", {
  # use daedalus model as base
  cty <- "CAN"
  infection <- "influenza_1918"

  npi <- daedalus_npi("elimination", cty, infection)
  output <- daedalus(cty, infection, response_strategy = npi, time_end = 100)

  data <- get_data(output)

  # check errors on input data
  expect_error(
    get_costs(data[-1L]),
    regexp = "(x)*(missing elements)*('time')}"
  )
  expect_error(
    get_costs(data[c(-1, -2)]),
    regexp = "(x)*(Must have at least 5 cols, but has 4 cols.)"
  )

  data_ <- data
  data_$age_group <- as.factor(data_$age_group)
  expect_error(
    get_costs(data_),
    regexp = "(x)*(age_group)*(has type 'factor')"
  )

  data_ <- data
  data_$time <- as.character(data_$time)
  data_$value <- as.character(data_$value)
  expect_error(
    get_costs(data_),
    regexp = "(x)*(time)*(Must be of type 'numeric')"
  )

  data_ <- data
  data_$value <- as.character(data_$value)
  expect_error(
    get_costs(data_),
    regexp = "(x)*(value)*(Must be of type 'numeric')"
  )

  comp_non_working <- c(
    "infect_symp",
    "hospitalised_recov",
    "hospitalised_death",
    "dead"
  )
  comp_infected <- "infect_symp"
  comp_dead <- "dead"

  # check compartment names
  expect_error(
    get_costs(
      data,
      comp_infected = as.factor(comp_infected)
    ),
    regexp = "(comp_infected)*(Must be of type 'character')"
  )

  expect_error(
    get_costs(
      data,
      comp_infected = comp_infected,
      comp_dead = as.factor(comp_dead)
    ),
    regexp = "(comp_dead)*(Must be of type 'character')"
  )

  expect_error(
    get_costs(
      data,
      comp_infected = comp_infected,
      comp_dead = comp_dead,
      comp_non_working = as.factor(comp_non_working)
    ),
    regexp = "(comp_non_working)*(Must be of type 'character')"
  )

  expect_error(
    get_costs(
      data,
      comp_infected = comp_infected,
      comp_dead = "dead_excess",
      comp_non_working = comp_non_working
    ),
    regexp = "(comp_infected, comp_dead)*(has additional elements)"
  )

  # econ parameters
  workforce <- output$country_parameters$workers
  daily_gva <- output$country_parameters$gva

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce * -1
    ),
    regexp = "(workforce)*(Element 1 is not >= 0)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce * Inf
    ),
    regexp = "(workforce)*(Must be finite)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce * NA_real_
    ),
    regexp = "(workforce)*(Contains missing values)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva * -1
    ),
    regexp = "(daily_gva)*(Element 1 is not >= 0)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva * Inf
    ),
    regexp = "(daily_gva)*(Must be finite)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva * NA_real_
    ),
    regexp = "(daily_gva)*(Contains missing values)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva[-1]
    ),
    regexp = "(daily_gva)*(Must have length)"
  )

  # productivity loss due to infection
  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection = -1
    ),
    regexp = "(productivity_loss_infection)*(Element 1 is not >= 0)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection = 2
    ),
    regexp = "(productivity_loss_infection)*(Element 1 is not <= 1)"
  )

  # check VSL errors
  productivity_loss_infection <- 1.0
  vsl_by_age <- output$country_parameters$vsl
  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age * -1
    ),
    regexp = "(vsl_by_age)*(Element 1 is not >= 0)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age * Inf
    ),
    regexp = "(vsl_by_age)*(Must be finite)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age * NA_real_
    ),
    regexp = "(vsl_by_age)*(missing values)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age[-1]
    ),
    regexp = "(vsl_by_age)*(Must have length)"
  )

  # life expectancy
  life_expectancy <- output$country_parameters$life_expectancy

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy * -1
    ),
    regexp = "(life_expectancy)*(Element 1 is not >= 0)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy * NA_real_
    ),
    regexp = "(life_expectancy)*(missing values)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy * Inf
    ),
    regexp = "(life_expectancy)*(Must be finite)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy[-1]
    ),
    regexp = "(life_expectancy)*(Must have length)"
  )

  # value school year
  value_school_year <- get_value_school_year(output$country_parameters$gni)

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy,
      value_school_year * -1
    ),
    regexp = "(value_school_year)*(not >= 0)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy,
      value_school_year * -Inf
    ),
    regexp = "(value_school_year)*(Must be finite)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy,
      as.character(value_school_year)
    ),
    regexp = "(value_school_year)*(Must be of type 'number')"
  )

  # students
  n_students <- output$country_parameters$demography[i_SCHOOL_AGE]

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy,
      value_school_year,
      n_students * -1
    ),
    regexp = "(n_students)*(not >= 0)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy,
      value_school_year,
      n_students * -Inf
    ),
    regexp = "(n_students)*(Must be finite)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy,
      value_school_year,
      as.character(n_students)
    ),
    regexp = "(n_students)*(Must be of type 'number')"
  )

  edu_effectiveness_remote <- 0.33

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy,
      value_school_year,
      n_students,
      edu_effectiveness_remote * -1
    ),
    regexp = "(edu_effectiveness_remote)*(Element 1 is not >= 0)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy,
      value_school_year,
      n_students,
      edu_effectiveness_remote * 10
    ),
    regexp = "(edu_effectiveness_remote)*(Element 1 is not <= 1)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy,
      value_school_year,
      n_students,
      edu_effectiveness_remote * NA_real_
    ),
    regexp = "(edu_effectiveness_remote)*(May not be NA)"
  )

  npi_data <- c(
    output$response_data$npi_info,
    list(openness = first(output$response_data$openness))
  )

  # errors on NPI data
  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy,
      value_school_year,
      n_students,
      edu_effectiveness_remote,
      NA
    ),
    regexp = "(npi_data)*(Must be of type 'list')"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy,
      value_school_year,
      n_students,
      edu_effectiveness_remote,
      list(
        obj = character(1)
      )
    ),
    regexp = "(npi_data)*(May only contain the following types)"
  )

  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy,
      value_school_year,
      n_students,
      edu_effectiveness_remote,
      npi_data[-1]
    ),
    regexp = "(npi_data)*(Names must include the elements)"
  )

  npi_data_ <- npi_data
  npi_data_$npi_times_start <- -1
  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy,
      value_school_year,
      n_students,
      edu_effectiveness_remote,
      npi_data_
    ),
    regexp = "(npi_durations)*(npi_times_start)*(npi_times_end)*(npi_periods)"
  )

  npi_data_ <- npi_data
  npi_data_$openness <- npi_data_$openness[-1]
  expect_error(
    get_costs(
      data,
      comp_non_working,
      comp_infected,
      comp_dead,
      workforce,
      daily_gva,
      productivity_loss_infection,
      vsl_by_age,
      life_expectancy,
      value_school_year,
      n_students,
      edu_effectiveness_remote,
      npi_data_
    ),
    regexp = "(npi_data$openness)*(Must have length)"
  )
})
