# Tests for unexpected behaviour related to events
# These may be moved to more appropriate locations later
test_that("Vaccination does not trigger hospital-capacity based events", {
  vax_time <- 10
  early_vax <- daedalus_vaccination(
    "high",
    "GBR",
    vax_time
  )

  output <- daedalus(
    "GBR",
    "influenza_2009",
    "elimination",
    vaccine_investment = early_vax,
    response_time = 100, # prevent NPI on time
    time_end = 300
  )

  npi_state_on_time <- round(output$event_data[
    output$event_data$name == "npi_state_on",
    "time"
  ])

  hosp_cap_exceeded_time <- round(output$event_data[
    output$event_data$name == "hosp_cap_exceeded_state_on",
    "time"
  ])

  expect_gt(
    npi_state_on_time,
    vax_time
  )
  expect_gt(
    hosp_cap_exceeded_time,
    vax_time
  )
  expect_identical(
    hosp_cap_exceeded_time,
    npi_state_on_time
  )
})
