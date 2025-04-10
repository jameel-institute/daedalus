# expectations that interventions work
response_names <- c(
  "none",
  "elimination",
  "economic_closures",
  "school_closures"
)
country_x <- daedalus_country("Thailand")

test_that("Closures: basic expectations: runs without errors", {
  # test all responses
  expect_no_condition({
    output_list <- lapply(response_names, function(x) {
      daedalus(country_x, "sars_cov_1", response_strategy = x)
    })
  })

  # test combinations of responses and some countries
  invisible({
    Map(
      response_names,
      daedalus::country_names[seq_along(response_names)],
      f = function(response, country) {
        expect_no_condition(daedalus(
          country,
          "sars_cov_1",
          response_strategy = response
        ))
      }
    )
  })
})

# expect that applying closures reduces epidemic size
test_that("Closures: basic statistical correctness: reduces epidemic size", {
  output_list <- lapply(response_names, function(x) {
    daedalus(
      country_x,
      daedalus_infection("sars_cov_1", rho = 0.0),
      response_strategy = x
    )
  })

  epidemic_sizes <- vapply(
    output_list,
    function(x) {
      get_epidemic_summary(x, "infections")[["value"]]
    },
    FUN.VALUE = numeric(1L)
  )

  expect_true(all(epidemic_sizes[-1] < epidemic_sizes[1]))
})

# expect that earlier closures reduce epidemic size
test_that("Closures: earlier closures reduce epidemic size", {
  response_times <- c(200, 30)
  response_threshold <- 1e9 # very high to prevent auto-activation
  output_list <- lapply(response_times, function(x) {
    daedalus(
      country_x,
      daedalus_infection("sars_cov_1", rho = 0.0),
      response_strategy = "elimination",
      response_time = x,
      response_threshold = response_threshold
    )
  })

  epidemic_sizes <- vapply(
    output_list,
    function(x) {
      get_epidemic_summary(x, "infections")[["value"]]
    },
    FUN.VALUE = numeric(1L)
  )

  expect_lt(epidemic_sizes[2], epidemic_sizes[1])
})

# expect that lower activation threshold reduces epidemic size
test_that("Closures: lower threshold reduces epidemic size", {
  # NOTE: the relationship between repsonse threshold and the epidemic size
  # seems non-linear --- will open an issue for this
  response_thresholds <- c(1e7, 100)
  output_list <- lapply(response_thresholds, function(x) {
    daedalus(
      country_x,
      daedalus_infection("sars_cov_1", rho = 0.0),
      response_strategy = "elimination",
      response_threshold = x,
      response_time = 200 # artificially high
    )
  })

  epidemic_sizes <- vapply(
    output_list,
    function(x) {
      get_epidemic_summary(x, "infections")[["value"]]
    },
    FUN.VALUE = numeric(1L)
  )

  expect_lt(epidemic_sizes[2], epidemic_sizes[1])
})

# check that closures are logged correctly
# NOTE: we can only really test closure start times, as closures either
# end reactively or do not end at all
test_that("Closures: correct logging of time limits", {
  response_threshold <- 1e9 # artificially large
  response_time <- 10 # arbitrary value
  time_end <- 25 # low to prematurely end simulation
  dummy_vax <- daedalus_vaccination("none", start_time = 15)
  output <- daedalus(
    "Thailand",
    daedalus_infection("influenza_1918", r0 = 1.1),
    response_strategy = "elimination",
    response_threshold = response_threshold,
    response_time = response_time,
    vaccine_investment = dummy_vax,
    time_end = time_end
  )

  response_data <- get_data(output, "response_data")

  expect_identical(
    response_data[["closure_info"]][["closure_time_start"]],
    response_time
  )
  expect_identical(
    response_data[["closure_info"]][["closure_time_end"]],
    time_end
  )

  # Check that closures are terminated at the start time when
  # the epidemic is not growing
  dvx <- daedalus_vaccination("low", start_time = 90)
  tend <- 100
  response_time <- seq(10, 80, 10)

  # run scenario with very few susceptibles
  invisible(lapply(response_time, function(x) {
    data <- daedalus(
      "United States",
      "influenza_1957",
      initial_state_manual = list(p_infectious = 0.9999),
      response_strategy = "elimination",
      response_time = x,
      vaccine_investment = dvx,
      time_end = tend
    )

    closure_info <- data$response_data$closure_info

    expect_identical(
      closure_info$closure_time_end,
      closure_info$closure_time_start
    )
    expect_identical(closure_info$closure_time_end, x)
    expect_identical(closure_info$closure_duration, 0.0)
  }))
})
