# Tests for daedalus() with vaccination active
test_that("Vaccination: basic expectations", {
  expect_no_condition({
    output <- daedalus("Canada", "sars_cov_1", vaccine_investment = "medium")
  })
  data <- get_data(output)

  data_vaccinated <- data[data$vaccine_group == "vaccinated", ]
  expect_gt(
    max(data_vaccinated$value), 0
  )

  # expect that vaccination does not affect population size
  data_start <- data[data$time == min(data$time), ]
  data_end <- data[data$time == max(data$time), ]

  expect_identical(
    sum(
      data[data$time == max(data$time) &
        data$compartment %in% COMPARTMENTS[i_EPI_COMPARTMENTS], ]$value
    ),
    sum(
      data[data$time == min(data$time) &
        data$compartment %in% COMPARTMENTS[i_EPI_COMPARTMENTS], ]$value
    ),
    tolerance = 1
  )
})

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
