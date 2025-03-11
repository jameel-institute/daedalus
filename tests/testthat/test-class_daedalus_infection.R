# Tests for the <daedalus_infection> class
test_that("class <daedalus_infection>: basic expectations", {
  expect_no_condition({
    infection_x <- daedalus_infection("sars_cov_1") # nolint saves extra assignment
  })
  expect_s3_class(infection_x, "daedalus_infection")
  checkmate::expect_list(
    infection_x,
    c("character", "numeric"),
    any.missing = FALSE
  )
  expect_snapshot(infection_x)

  # model function can handle either string or class input
  expect_identical(
    daedalus("Canada", "influenza_1918"),
    daedalus("Canada", daedalus_infection("influenza_1918"))
  )
})

test_that("class <daedalus_infection>: works for all epidemics", {
  expect_no_condition({
    infections <- lapply(daedalus::epidemic_names, daedalus_infection) # nolint
  })
  checkmate::expect_list(infections, "daedalus_infection")
})

test_that("class <daedalus_infection>: access and assignment", {
  name <- "influenza_1918"
  infection_x <- daedalus_infection(name)

  # expectations on `[`
  expect_no_condition({
    x <- infection_x[1] # nolint
  })
  checkmate::expect_list(x, "character", len = 1L) # first element is "name"

  # expectations on `[[`
  expect_no_condition({
    x <- infection_x[["name"]] # nolint
  })
  checkmate::expect_string(x)
  expect_identical(x, name)

  # expectations on `[[<-` and `$<-` and preservation of class
  expect_no_condition({
    infection_x[["name"]] <- "influenza_1918" # nolint
  })
  expect_s3_class(infection_x, "daedalus_infection")

  infection_x <- daedalus_infection("sars_cov_1")
  expect_no_condition({
    infection_x$name <- "influenza_2009" # nolint
  })
  expect_s3_class(infection_x, "daedalus_infection")
})

test_that("class <daedalus_infection>: getting parameters", {
  infection_x <- daedalus_infection("influenza_1918")
  expect_no_condition({
    x <- get_data(infection_x, "r0") # nolint
    y <- get_data(infection_x, "omega") # nolint
  })
  checkmate::expect_number(x, lower = 0, finite = TRUE)
  checkmate::expect_numeric(y, len = N_AGE_GROUPS, lower = 0, finite = TRUE)

  expect_error(
    get_data(infection_x, c("r0", "omega")),
    regexp = "must be a string"
  )
})

test_that("class <daedalus_infection>: setting parameters", {
  infection_x <- daedalus_infection("sars_cov_2_pre_alpha")
  expect_no_condition({
    # nolint begin
    infection_x <- set_data(infection_x, r0 = 3.0, rho = 1 / 60)
    # nolint end
  })
  expect_s3_class(infection_x, "daedalus_infection")

  # expect that validator prevents setting data that invalidates class
  # the exact error is not very important and could change
  infection_x <- daedalus_infection("sars_cov_2_pre_alpha")
  expect_error(set_data(infection_x, r0 = rep(1.3, N_AGE_GROUPS)))

  expect_error(
    set_data(daedalus_infection("influenza_1918"), beta = 0.018),
    regexp = "Found a disallowed parameter substitution"
  )
})

# tests for elements of the validator not caught elsewhere
test_that("class <daedalus_infection>: validator", {
  x <- daedalus_infection("influenza_1918")
  x <- unclass(x)
  expect_error(validate_daedalus_infection(x))

  x <- list(name = "influenza_1918")
  class(x) <- "daedalus_infection"
  expect_error(
    validate_daedalus_infection(x),
    regexp = "does not have the correct attributes"
  )

  x <- daedalus_infection("influenza_1918")
  x <- unclass(x)
  x$omega <- rep(1, N_AGE_GROUPS - 1)
  class(x) <- "daedalus_infection"
  expect_error(validate_daedalus_infection(x))
})

test_that("class <daedalus_infection>: errors", {
  # invalid name
  expect_error(daedalus_infection("dummy"), regexp = "`name` must be one of")

  # invalid `...` types
  expect_error(
    daedalus_infection("influenza_1918", r0 = "1.3"),
    regexp = "May only contain the following types:.*numeric"
  )
  expect_error(
    daedalus_infection("influenza_1918", list(r0 = "dummy values")),
    regexp = "May only contain the following types:.*numeric"
  )
  expect_error(
    daedalus_infection("influenza_1918", dummy_param = 1.3),
    regexp = "Found unexpected values in `...`"
  )

  expect_error(
    daedalus_infection("sars_cov_1", omega = rep(1, N_AGE_GROUPS - 1)),
    regexp = "Expected.*following parameters.*to be numeric.*length 4"
  )

  expect_error(
    daedalus_infection("sars_cov_1", r0 = c(1, 3, 4)),
    regexp = "Expected the following parameters.*to be a single.*number"
  )
})
