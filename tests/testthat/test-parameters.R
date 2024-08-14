# # Basic unit tests for the default model parameter expectations
# NOTE: testing that parameter errors are bubbled up from `daedalus()`
test_that("Errors on model parameters passed to `daedalus()`:", {
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      infect_params_manual = list(r0 = "0.01")
    ),
    regexp = "Expected.*`infect_params_manual`.*single positive.*finite number"
  )
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      infect_params_manual = list(r0 = c(0.1, 0.1))
    ),
    regexp = "Expected.*`infect_params_manual`.*single positive.*finite number"
  )
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      infect_params_manual = list(r0 = -0.1)
    ),
    regexp = "Expected.*`infect_params_manual`.*single positive.*finite number"
  )
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      infect_params_manual = list(r0 = Inf)
    ),
    regexp = "Expected.*`infect_params_manual`.*single positive.*finite number"
  )

  # checks on expected numerics of correct length
  param_passed <- "eta"
  expected_error <- glue::glue(
    "Expected the following parameters passed in `infect_params_manual` to be \\
    numeric vectors of length {N_AGE_GROUPS} with positive and finite values: \\
    {glue::double_quote(param_passed)}"
  )

  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      infect_params_manual = list(eta = c(0.1, 0.1))
    ),
    regexp = expected_error
  )
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      infect_params_manual = list(eta = 0.1)
    ),
    regexp = expected_error
  )
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      infect_params_manual = list(eta = rep(Inf, 4))
    ),
    regexp = expected_error
  )
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      infect_params_manual = list(eta = rep(-1.0, 4))
    ),
    regexp = expected_error
  )

  # checks on extra infection parameters - now bumped up to error
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      infect_params_manual = list(dummy_param = 0.1)
    ),
    regexp = "`infect_params_manual` found parameters that are not allowed"
  )

  # specific checks on poorly specified community contact matrix
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = list(
        contact_matrix = matrix("a", N_AGE_GROUPS, N_AGE_GROUPS)
      )
    ),
    regexp = "Expected user-provided `contact_matrix` to be a numeric matrix"
  )
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = list(
        contact_matrix = list(matrix(1.0, N_AGE_GROUPS, N_AGE_GROUPS))
      )
    ),
    regexp = "Expected user-provided `contact_matrix` to be a numeric matrix"
  )

  cm <- matrix(1.0, N_AGE_GROUPS, N_AGE_GROUPS)
  cm[1L, ] <- NA_real_
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = list(contact_matrix = cm)
    ),
    regexp = "Expected.*`contact_matrix`.*no missing elements"
  )

  cm <- matrix(1.0, N_AGE_GROUPS - 1L, N_AGE_GROUPS)
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = list(contact_matrix = cm)
    ),
    regexp = "Expected user-provided `contact_matrix` to be a square matrix"
  )

  cm <- matrix(1.0, N_AGE_GROUPS, N_AGE_GROUPS - 1L)
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = list(contact_matrix = cm)
    ),
    regexp = "Expected user-provided `contact_matrix` to be a square matrix"
  )

  # expectations on the within-sector workplace contacts
  cw <- rep("1.0", N_ECON_SECTORS)
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = list(contacts_workplace = cw)
    ),
    regexp = "Expected.*`contacts_workplace`.*numeric vector"
  )
  cw <- matrix(1.0, N_ECON_SECTORS, N_ECON_SECTORS)
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = list(contacts_workplace = cw)
    ),
    regexp = "Expected.*`contacts_workplace`.*numeric vector"
  )

  cw <- rep(1.0, N_ECON_SECTORS - 1L)
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = list(contacts_workplace = cw)
    ),
    regexp = "Expected.*`contacts_workplace`.*numeric vector"
  )
  cw <- rep(-1.0, N_ECON_SECTORS)
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = list(contacts_workplace = cw)
    ),
    regexp = "Expected.*`contacts_workplace`.*numeric vector"
  )

  # expectations for consumer-worker contacts
  cmcw <- matrix(
    "1.0", N_ECON_SECTORS, N_AGE_GROUPS
  )
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = list(contacts_consumer_worker = cmcw)
    ),
    regexp = "Expected.*`contacts_consumer_worker`.*numeric matrix"
  )

  cmcw <- rep(1.0, N_ECON_SECTORS)
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = list(contacts_consumer_worker = cmcw)
    ),
    regexp = "Expected.*`contacts_consumer_worker`.*numeric matrix"
  )

  expected_error <- glue::glue(
    "Expected user-provided `contacts_consumer_worker` to have \\
    {N_ECON_SECTORS} rows and {N_AGE_GROUPS} columns"
  )
  cmcw <- matrix(
    1.0, N_ECON_SECTORS - 1L, N_AGE_GROUPS - 1L
  )
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = list(contacts_consumer_worker = cmcw)
    ),
    regexp = expected_error
  )

  # expectations for worker-worker contacts betweens sectors
  cm_ww <- matrix("1.0", N_ECON_SECTORS, N_ECON_SECTORS)
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = list(contacts_between_sectors = cm_ww)
    ),
    regexp = "Expected.*`contacts_between_sectors`.*numeric matrix"
  )

  cm_ww <- matrix(1.0, N_ECON_SECTORS - 2L, N_ECON_SECTORS - 2L)
  expected_error <- glue::glue(
    "Expected `contacts_between_sectors` to be a square numeric \\
    matrix with {N_ECON_SECTORS} rows and columns"
  )
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = list(contacts_between_sectors = cm_ww)
    ),
    regexp = as.character(expected_error), perl = TRUE
  )

  cm_ww <- matrix(1.0, N_ECON_SECTORS, N_ECON_SECTORS)
  cm_ww[, 1L] <- NA_real_
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = list(contacts_between_sectors = cm_ww)
    ),
    regexp =
      "Expected.*numeric matrix.*with no missing elements"
  )

  cm_ww <- matrix(1.0, N_ECON_SECTORS, N_ECON_SECTORS)
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = list(contacts_between_sectors = cm_ww)
    ),
    regexp = "Expected.*diagonal entries are all zero"
  )

  # expect errors for inadmissible country parameters
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      country_params_manual = list(dummy_value = matrix(1, N_AGE_GROUPS))
    ),
    regexp =
      "`country_params_manual` found.*parameters that are not.*allowed"
  )
})

# special test that `make_country_parameters()` processes between-sector matrix
test_that("`make_country_parameters` scales between-sector matrix", {
  cm_ww <- matrix(1, N_ECON_SECTORS, N_ECON_SECTORS)
  diag(cm_ww) <- 0.0

  p <- make_country_parameters("Canada", list(contacts_between_sectors = cm_ww))

  expect_identical(
    max(Re(eigen(p[["contacts_between_sectors"]])$values)),
    1.0,
    tolerance = 1e-12
  )
})

# adding snapshot test on all infection parameter values to catch
# potentially unwanted raw data changes
test_that("infection_data: Snapshot tests", {
  expect_snapshot(
    infection_data
  )
})
