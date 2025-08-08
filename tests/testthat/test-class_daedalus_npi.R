# Tests for the <daedalus_npi> class

test_that("class <daedalus_npi>: basic expectations pass", {
  expect_no_condition(
    daedalus_npi("elimination", "GBR", "sars_cov_1")
  )

  expect_no_condition(
    lapply(
      daedalus.data::closure_strategy_names,
      daedalus_npi,
      country = "GBR",
      infection = "sars_cov_1"
    )
  )

  cty <- "GBR"
  infection <- "influenza_1918"
  npi <- daedalus_npi("school_closures", cty, infection)
  expect_no_condition(
    daedalus(cty, infection, npi)
  )

  expect_snapshot(npi)

  expect_no_condition(
    get_data(npi, "openness")
  )
})

test_that("class <daedalus_npi>: throws expected errors", {
  expect_error(
    daedalus_npi(
      "elimination",
      "THA",
      "sars_cov_1",
      rep(1, 45)
    ),
    "pre-defined NPI strategy passed to `name` and custom `openness` values"
  )

  expect_error(
    daedalus_npi(
      NA_character_,
      "THA",
      "sars_cov_1",
      rep(2, 45)
    ),
    "values between 0.0 and 1.0"
  )

  expect_error(
    get_data(
      daedalus_npi("elimination", "THA", "sars_cov_1"),
      "dummy_param"
    ),
    "`to_get` must be a string available"
  )

  expect_error(
    validate_daedalus_npi(
      daedalus_country("GBR")
    ),
    "check class assignment"
  )

  x <- daedalus_npi(NA, "THA", "sars_cov_1", rep(0.1, 45))
  x$parameters$openness <- rep(2, 34)
  expect_error(
    validate_daedalus_npi(x),
    "must be a numeric of length 45, with values between 0.0 and 1.0"
  )

  x <- list(parameters = list(openness = rep(1, 45), dummy = NA))
  class(x) <- "daedalus_npi"
  expect_error(
    validate_daedalus_npi(x),
    "does not have the correct attributes"
  )

  expect_error(
    validate_npi_input(
      list(dummy = "dummy"),
      "THA",
      "sars_cov_1",
      30,
      60
    ),
    "Got an unexpected value of class"
  )
})
