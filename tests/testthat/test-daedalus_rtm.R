test_that("daedalus real-time modelling works", {
  expect_no_condition(
    daedalus_rtm("GBR", "influenza_2009")
  )

  infection <- daedalus_infection("influenza_1918")
  expect_no_condition(
    daedalus_rtm("GBR", infection)
  )

  invisible(
    lapply(
      daedalus::country_codes_iso2c, function(x) {
        expect_no_condition(
          daedalus_rtm(x, infection, time_end = 10)
        )
      }
    )
  )

  output <- daedalus_rtm("GBR", infection)
  expect_s3_class(output, "daedalus_output")

  # function runs on a list of infections
  infection_list <- lapply(
    epidemic_names, daedalus_infection
  )

  expect_no_condition(
    daedalus_rtm("GBR", infection_list)
  )
  output_list <- daedalus_rtm("GBR", infection_list)
  checkmate::expect_list(
    output_list, "daedalus_output"
  )
})

test_that("daedalus real-time modelling: downstream func compat", {
  infection <- daedalus_infection("influenza_2009")
  output <- daedalus_rtm("GBR", infection)

  # getting basic data
  expect_no_condition(
    get_data(output)
  )
  expect_no_condition(
    get_data(output, "response_data")
  )
  data <- get_data(output)
  checkmate::expect_data_frame(data)

  # get incidence
  expect_no_condition(
    get_incidence(output)
  )
  expect_no_condition(
    get_incidence(output, groups = "age_group")
  )
  # NOTE: expect error when vaccine groups requested as these are not modelled
  expect_error(
    get_incidence(output, groups = "vaccine_group")
  )
  # NOTE: expect empty data on downstream func for new vaccinations
  checkmate::expect_data_frame(
    get_new_vaccinations(output),
    nrows = 0L
  )

  # epidemic summary
  expect_no_condition(
    get_epidemic_summary(output)
  )
  expect_snapshot(
    get_epidemic_summary(output)
  )

  # costs
  expect_no_condition(
    get_costs(output)
  )
  expect_snapshot(
    get_costs(output)
  )
  expect_no_condition(
    get_costs(output, "none")
  )
  expect_no_condition(
    get_costs(output, "domain")
  )
  expect_no_condition(
    get_costs(output, "total")
  )

  # costs with closures
  output <- daedalus_rtm(
    "GBR", infection,
    response_strategy = "elimination",
    response_time_start = 14, response_time_end = 60
  )
  expect_no_condition(
    get_costs(output)
  )
  expect_no_condition(
    get_costs(output, "domain")
  )
  expect_no_condition(
    get_costs(output, "total")
  )

  output <- daedalus_rtm(
    "GBR", list(infection, infection),
    response_strategy = "elimination",
    response_time_start = 14, response_time_end = 60
  )
  expect_no_condition(
    lapply(output, get_costs)
  )
  expect_no_condition(
    lapply(output, get_costs, "domain")
  )
  expect_no_condition(
    lapply(output, get_costs, "total")
  )
})
