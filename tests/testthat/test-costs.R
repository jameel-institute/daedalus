# Tests on model cost function
test_that("Costs: basic expectations", {
  output <- daedalus("Canada", "influenza_1918")
  expect_no_condition(
    {
      costs <- get_costs(output)
      costs_total <- get_costs(output, "total")
      costs_domain <- get_costs(output, "domain")
    } # nolint allow assignment
  )
  checkmate::expect_list(costs, c("numeric", "list"), any.missing = FALSE)
  checkmate::expect_number(costs_total, lower = 0, finite = TRUE)
  checkmate::expect_numeric(
    costs_domain,
    lower = 0, finite = TRUE, any.missing = FALSE
  )

  expected_names <- c(
    "total_cost", "economic_cost", "education_cost", "life_years_lost"
  )
  checkmate::expect_numeric(
    unlist(costs),
    lower = 0, finite = TRUE, any.missing = FALSE
  )
})

test_that("Costs: scenario expectations", {
  ## when response = "none"
  output <- daedalus("Canada", "influenza_1918")
  costs <- get_costs(output)

  expect_identical(
    costs$economic_costs$economic_cost_closures, 0
  )
  expect_identical(
    costs$education_costs$education_cost_closures, 0
  )
  # store life years lost for later use
  life_value_lost_noresp <- costs$life_years_lost$life_years_lost_total

  ## when there is a response
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
        sum(expected_cost_closures[-i_EDUCATION_SECTOR])
      )
      expect_identical(
        costs$education_costs$education_cost_closures,
        sum(expected_cost_closures[i_EDUCATION_SECTOR])
      )

      # expect lives lost cost is lower
      expect_lt(
        costs$life_years_lost$life_years_lost_total,
        life_value_lost_noresp
      )
    })
  })
})
