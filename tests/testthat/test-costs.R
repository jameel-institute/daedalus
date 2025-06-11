# Tests on model cost function
test_that("Costs: basic expectations", {
  output <- daedalus("Canada", "influenza_1918")
  expect_no_condition({
    costs <- get_costs(output)
  })
  expect_no_condition({
    costs_total <- get_costs(output, "total")
  })
  expect_no_condition({
    costs_domain <- get_costs(output, "domain")
  })
  expect_snapshot(
    costs
  )

  checkmate::expect_list(costs, c("numeric", "list"), any.missing = FALSE)
  checkmate::expect_number(costs_total, lower = 0, finite = TRUE)
  checkmate::expect_numeric(
    costs_domain,
    lower = 0,
    finite = TRUE,
    any.missing = FALSE
  )

  expected_names <- c(
    "total_cost",
    "economic_cost",
    "education_cost",
    "life_years_lost"
  )
  checkmate::expect_numeric(
    unlist(costs),
    lower = 0,
    finite = TRUE,
    any.missing = FALSE
  )
})

test_that("Costs: scenario expectations", {
  ## when response = "none"
  output <- daedalus("Canada", "influenza_2009", time_end = 400)
  costs <- get_costs(output)

  expect_identical(costs$economic_costs$economic_cost_closures, 0)
  expect_identical(costs$education_costs$education_cost_closures, 0)

  # expect life years lost costs in response scenarios are higher than no resp.
  x <- c("none", "school_closures", "economic_closures", "elimination")

  o <- lapply(
    x,
    daedalus,
    country = "United Kingdom",
    infection = "influenza_1957",
    response_time = 10
  )

  a <- lapply(o, get_costs, "domain")

  v <- vapply(a, `[[`, FUN.VALUE = 1, "life_years")

  expect_true(all(v[-1] < v[1]))

  ## expect that closure costs are non-zero
  response_names <- c("elimination", "economic_closures", "school_closures")
  invisible({
    lapply(response_names, function(x) {
      output <- daedalus("Canada", "influenza_1918", response_strategy = x)
      costs <- get_costs(output)

      # closure costs must be at least one day of reduced GVA
      expected_cost_closures <- output$country_parameters$gva *
        (1 - output$response_data$openness) *
        output$response_data$closure_info$closure_duration

      expect_identical(
        costs$economic_costs$economic_cost_closures,
        sum(expected_cost_closures)
      )
    })
  })
})

test_that("Expectations on education costs", {
  # tests to check that education costs are correctly handled
  output <- daedalus("GBR", "sars_cov_1")
  costs <- get_costs(output)

  # to check that matrix mult and colsums is correct in `get_costs()`
  expect_gt(
    round(costs$education_costs$education_cost_absences),
    1
  )

  # expect absences in education lead to higher losses
  # this probably works for UK due to size of education sector
  expect_gt(
    costs$education_costs$education_cost_absences,
    costs$economic_costs$sector_cost_absences[1]
  )

  output <- daedalus("GBR", "sars_cov_1")
  costs <- get_costs(output)

  # exepct that costs due to closures are non-zero, in scenarios with schools
  # closed
  x <- c("none", "school_closures", "elimination")

  o <- lapply(
    x,
    daedalus,
    country = "United Kingdom",
    infection = daedalus_infection("sars_cov_1")
  )

  a <- vapply(
    o,
    function(x) {
      y <- get_costs(x)
      y[["education_costs"]][["education_cost_closures"]]
    },
    1
  )
  names(a) <- x

  checkmate::expect_numeric(
    a[setdiff(x, c("none", "economic_closures"))],
    lower = 1
  )
})
