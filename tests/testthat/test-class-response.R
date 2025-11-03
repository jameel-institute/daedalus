# Most checks are pushed to sub-classes; this file tests the general validator
test_that("daedalus_response: Validator checks", {
  expect_error(
    new_daedalus_response(
      "low",
      "dummy_class"
    ),
    "`class` must be one of"
  )

  y <- new_daedalus_response(
    "low",
    "daedalus_npi"
  )
  y$dummy_field <- "dummy_value"
  expect_error(
    validate_daedalus_response(y),
    "has the extra or missing fields"
  )

  expect_error(
    new_daedalus_response(
      "low",
      "daedalus_vaccination",
      time_on = -1
    ),
    "(time_on)*(must be a vector of integer-ish numbers)"
  )
  expect_error(
    new_daedalus_response(
      "low",
      "daedalus_vaccination",
      time_on = 1.5
    ),
    "(time_on)*(must be a vector of integer-ish numbers)"
  )
  expect_error(
    new_daedalus_response(
      "low",
      "daedalus_vaccination",
      time_on = c(1, NA_integer_, 2)
    ),
    "(time_on)*(must be a vector of integer-ish numbers)"
  )

  expect_error(
    new_daedalus_response(
      "low",
      "daedalus_vaccination",
      max_duration = -1
    ),
    "(max_duration)*(must be a single integer-ish number)"
  )
  expect_error(
    new_daedalus_response(
      "low",
      "daedalus_vaccination",
      max_duration = 1.5
    ),
    "(max_duration)*(must be a single integer-ish number)"
  )
  expect_error(
    new_daedalus_response(
      "low",
      "daedalus_vaccination",
      max_duration = c(1, NA_integer_, 2)
    ),
    "(max_duration)*(must be a single integer-ish number)"
  )
  expect_error(
    new_daedalus_response(
      "low",
      "daedalus_vaccination",
      time_on = c(10, 12),
      time_off = 1
    ),
    "(time_off)*(must be a vector)*(of length 2)"
  )

  expect_error(
    new_daedalus_response(
      "low",
      "daedalus_vaccination",
      id_state_on = -1
    ),
    "(id_state_on)*(must be a vector of integer-ish numbers)"
  )
  expect_error(
    new_daedalus_response(
      "low",
      "daedalus_vaccination",
      id_state_on = 1.5
    ),
    "(id_state_on)*(must be a vector of integer-ish numbers)"
  )

  expect_error(
    new_daedalus_response(
      "low",
      "daedalus_vaccination",
      id_state_off = -1
    ),
    "(id_state_off)*(must be a vector of integer-ish numbers)"
  )
  expect_error(
    new_daedalus_response(
      "low",
      "daedalus_vaccination",
      id_state_off = 1.5
    ),
    "(id_state_off)*(must be a vector of integer-ish numbers)"
  )

  expect_error(
    new_daedalus_response(
      "low",
      "daedalus_vaccination",
      value_state_on = -1.0
    ),
    "(value_state_on)*(must be a numeric vector)"
  )

  expect_error(
    new_daedalus_response(
      "low",
      "daedalus_vaccination",
      value_state_off = -1.0
    ),
    "(value_state_off)*(must be a numeric vector)"
  )
})
