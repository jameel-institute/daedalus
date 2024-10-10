# Tests for daedalus() with vaccination active
test_that("Vaccination: basic expectations", {
  vax_level <- daedalus_vaccination("medium")
  expect_no_condition({
    output <- daedalus("Canada", "sars_cov_1", vaccine_investment = vax_level)
  })
  data <- get_data(output)

  data_vaccinated <- data[data$vaccine_group == "vaccinated", ]
  expect_gt(
    max(data_vaccinated$value), 0
  )

  # expect that there are no vaccinations before the vaccination start time
  t_vax <- get_data(vax_level, "vax_start_time")
  vax_per_day <- tapply(data_vaccinated$value, data_vaccinated$time, sum)
  expect_identical(
    max(vax_per_day[seq_len(t_vax)]), 0.0
  )


  # expect that vaccination does not affect population size
  data_start <- data[data$time == min(data$time), ]
  data_end <- data[data$time == max(data$time), ]

  expect_identical(
    sum(
      data[data$time == max(data$time) &
        data$compartment %in% COMPARTMENTS[i_EPI_COMPARTMENTS] &
        data$vaccine_group != "new_vaccinations", ]$value
    ),
    sum(
      data[data$time == min(data$time) &
        data$compartment %in% COMPARTMENTS[i_EPI_COMPARTMENTS] &
        data$vaccine_group != "new_vaccinations", ]$value
    ),
    tolerance = 1
  )
})

# NOTE: testing that higher vaccine investment leads to fewer deaths
# but not lower costs, as vaccination lifts social distancing which leads
# to more infections and deaths, leading to higher life-years lost
test_that("Vaccination: basic statistical correctness", {
  # expect that higher vaccination investment leads to fewer deaths
  # set disease to lower R0 to make it run longer
  data_list <- lapply(
    daedalus::vaccination_scenario_names,
    function(v) {
      daedalus(
        country = "Canada",
        infection = daedalus_infection("sars_cov_1", r0 = 1.1),
        response_time = 10, vaccine_investment = v
      )
    }
  )

  deaths <- vapply(
    data_list, function(df) {
      get_epidemic_summary(df, "deaths")$value
    }, numeric(1L)
  )

  expect_lt(
    min(diff(deaths)), 0.0
  )
})
