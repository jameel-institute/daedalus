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
  time_end <- 300
  # expect that higher vaccination rate leads to fewer deaths
  data_low_vax <- get_data(
    daedalus(
      "Canada", "sars_cov_1",
      response_time = 10,
      vaccine_investment = "medium",
      time_end = time_end
    )
  )

  data_high_vax <- get_data(
    daedalus(
      "Canada", "sars_cov_1",
      response_time = 10,
      vaccine_investment = "high",
      time_end = time_end
    )
  )

  deaths_low_vax <- sum(
    data_low_vax[
      data_low_vax$time == time_end &
        data_low_vax$compartment == "dead",
    ]$value
  )
  deaths_high_vax <- sum(
    data_high_vax[
      data_high_vax$time == time_end &
        data_high_vax$compartment == "dead",
    ]$value
  )
  expect_gt(
    deaths_low_vax, deaths_high_vax
  )
})
