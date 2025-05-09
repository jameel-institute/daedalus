# Tests for the <daedalus_country> class
test_that("class <daedalus_country>: basic expectations", {
  expect_no_condition({
    country_x <- daedalus_country("Canada") # nolint saves extra assignment
  })
  expect_s3_class(country_x, "daedalus_country")
  checkmate::expect_list(
    country_x,
    c("character", "numeric"),
    any.missing = FALSE
  )
  expect_snapshot(country_x)

  # model function can handle either string or class input
  expect_identical(
    daedalus("Canada", "influenza_1918"),
    daedalus(daedalus_country("Canada"), "influenza_1918"),
    tolerance = 1e-6
  )
})

test_that("class <daedalus_country>`: works for all country names", {
  expect_no_condition({
    countries <- lapply(daedalus.data::country_names, daedalus_country) # nolint
  })
  checkmate::expect_list(countries, "daedalus_country")
})

test_that("class <daedalus_country>`: works for all country ISO2 codes", {
  expect_no_condition({
    countries <- lapply(daedalus.data::country_codes_iso2c, daedalus_country) # nolint
  })
  checkmate::expect_list(countries, "daedalus_country")
})

test_that("class <daedalus_country>`: works for all country ISO3 codes", {
  expect_no_condition({
    countries <- lapply(daedalus.data::country_codes_iso3c, daedalus_country) # nolint
  })
  checkmate::expect_list(countries, "daedalus_country")
})

test_that("class <daedalus_country>: access and assignment", {
  name <- "Canada"
  country_x <- daedalus_country(name)

  # expectations on `[`
  expect_no_condition({
    x <- country_x[1] # nolint
  })
  checkmate::expect_list(x, "character", len = 1L)

  # expectations on `[[`
  expect_no_condition({
    x <- country_x[["name"]] # nolint
  })
  checkmate::expect_string(x)
  expect_identical(x, name)

  # expectations on `[[<-` and `$<-` and preservation of class
  expect_no_condition({
    country_x[["name"]] <- "China" # nolint
  })
  expect_s3_class(country_x, "daedalus_country")

  country_x <- daedalus_country("Canada")
  expect_no_condition({
    country_x$name <- "Ghana" # nolint
  })
  expect_s3_class(country_x, "daedalus_country")
})

test_that("class <daedalus_country>`: getting parameters", {
  country_x <- daedalus_country("China")
  expect_no_condition({
    x <- get_data(country_x, "demography") # nolint
    y <- get_data(country_x, "contact_matrix") # nolint
  })
  checkmate::expect_numeric(x, len = N_AGE_GROUPS)
  checkmate::expect_matrix(
    y,
    "numeric",
    nrows = N_AGE_GROUPS,
    ncols = N_AGE_GROUPS
  )

  expect_error(
    get_data(country_x, c("demography", "contact_matrix")),
    regexp = "must be a string"
  )
})

test_that("class <daedalus_country>`: setting parameters", {
  country_x <- daedalus_country("China")
  expect_no_condition({
    # nolint begin
    country_x <- set_data(
      country_x,
      contact_matrix = matrix(1, N_AGE_GROUPS, N_AGE_GROUPS),
      contacts_consumer_worker = matrix(1, N_ECON_SECTORS, N_AGE_GROUPS)
    )
    # nolint end
  })
  expect_s3_class(country_x, "daedalus_country")

  country_x <- daedalus_country("China")
  expect_error(
    set_data(country_x, demography = rep(1, N_AGE_GROUPS)),
    regexp = "Found a disallowed parameter substitution"
  )

  # expect that validator prevents setting data that invalidates class
  # the exact error is not very important and could change
  expect_error(set_data(
    country_x,
    contact_matrix = matrix(1, N_AGE_GROUPS - 1, N_AGE_GROUPS)
  ))
})

# tests for elements of the validator not caught elsewhere
test_that("class <daedalus_country>: validator", {
  x <- daedalus_country("China")
  x <- unclass(x)
  expect_error(validate_daedalus_country(x))

  x <- list(name = "Ghana")
  class(x) <- "daedalus_country"
  expect_error(
    validate_daedalus_country(x),
    regexp = "does not have the correct attributes"
  )
})

test_that("class <daedalus_country>`: errors", {
  # invalid name
  expect_error(daedalus_country("dummy"), regexp = "`country` must be one of")

  # invalid ISO2
  expect_error(daedalus_country("ZZ"), regexp = "`code` must be one of")

  # invalid ISO3
  expect_error(daedalus_country("ZZZ"), regexp = "`code` must be one of")

  # invalid `parameter` type
  expect_error(
    daedalus_country("Canada", parameters = "dummy values"),
    regexp = "Must be of type 'list'"
  )

  # invalid parameter types
  expect_error(
    daedalus_country("Canada", list(dummy_param = "dummy values")),
    regexp = "May only contain.*numeric,matrix,NULL"
  )

  # invalid parameter names
  expect_error(
    daedalus_country("Canada", list(dummy_param = matrix(1))),
    regexp = "Found unexpected values in `parameters`"
  )

  # invalid parameter dims or values: contact matrix
  expect_error(
    daedalus_country("Canada", list(contact_matrix = matrix(1))),
    regexp = "Expected.*numeric matrix"
  )
  expect_error(
    daedalus_country("Canada", list(contact_matrix = matrix("1"))),
    regexp = "Expected.*numeric matrix"
  )
  expect_error(
    daedalus_country(
      "Canada",
      list(contact_matrix = matrix(-1, 4, 4)) # right dims, negatives
    ),
    regexp = "Expected.*numeric matrix"
  )
  expect_error(
    daedalus_country(
      "Canada",
      list(contact_matrix = matrix(NA_real_, 4, 4)) # right dims, NAs
    ),
    regexp = "Expected.*numeric matrix"
  )

  # invalid parameter dims or values: contacts workplace
  expect_error(
    daedalus_country(
      "Canada",
      list(contacts_workplace = rep(1, N_ECON_SECTORS - 1)) # wrong dims
    ),
    regexp = "Expected.*45-element numeric vector"
  )
  expect_error(
    daedalus_country(
      "Canada",
      list(contacts_workplace = rep(-1, N_ECON_SECTORS)) # negatives
    ),
    regexp = "Expected.*numeric vector.*positive"
  )
  expect_error(
    daedalus_country(
      "Canada",
      list(contacts_workplace = rep(NA_real_, N_ECON_SECTORS)) # NAs
    ),
    regexp = "Expected.*numeric vector.*positive"
  )

  # invalid parameter dims or values: contacts consumer-to-worker
  expect_error(
    daedalus_country("Canada", list(contacts_consumer_worker = matrix(1))),
    regexp = "Expected.*45x4 numeric matrix"
  )
  expect_error(
    daedalus_country("Canada", list(contacts_consumer_worker = matrix("1"))),
    regexp = "Expected.*numeric matrix"
  )
  expect_error(
    daedalus_country(
      "Canada",
      list(contacts_consumer_worker = matrix(-1, 4, 4))
    ),
    regexp = "Expected.*numeric matrix.*positive.*values"
  )
  expect_error(
    daedalus_country(
      "Canada",
      list(contacts_consumer_worker = matrix(NA_real_, 4, 4)) # NAs
    ),
    regexp = "Expected.*numeric matrix"
  )
})
