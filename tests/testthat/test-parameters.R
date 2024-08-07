# Basic unit tests for the default model parameter expectations
test_that("make_parameters: basic expectations", {
  # expect no conditions on commonly expected function usage
  expect_no_condition(make_parameters("Canada", epidemic = "influenza_1918"))
  expect_no_condition(
    make_parameters("Canada", epidemic = "influenza_1918", r0 = 999)
  )
  expect_no_condition(
    make_parameters(
      "Canada",
      epidemic = "influenza_1918",
      contact_matrix = matrix(2, N_AGE_GROUPS, N_AGE_GROUPS)
    )
  )

  expect_warning(
    make_parameters("Canada", epidemic = "influenza_1918", alpha = 999),
    regexp = "User provided parameters that are not among the model parameters"
  )

  # NOTE: warning about extra parameters suppressed
  suppressWarnings({
    modified_param_names <- names(
      make_parameters("Canada", epidemic = "influenza_1918", alpha = 999)
    )
  })
  expect_identical(
    modified_param_names,
    names(make_parameters("Canada", epidemic = "influenza_1918"))
  )
})

# adding snapshot test on all infection parameter values to catch
# potentially unwanted raw data changes
test_that("infection_data: Snapshot tests", {
  expect_snapshot(
    infection_data
  )
})

# NOTE: testing that `...` parameter errors are bubbled up from `daedalus()`
test_that("Errors on `...` passed to `daedalus()`:", {
  expect_error(
    daedalus("Canada", r0 = "0.01"),
    regexp = "Model options.*`\\.\\.\\.`.*may only be numeric"
  )
  expect_error(
    daedalus("Canada", r0 = c(0.1, 0.1)),
    regexp = "Expected the following parameters.*`\\.\\.\\..*single positive"
  )
  expect_error(
    daedalus("Canada", r0 = -0.1),
    regexp = "Expected the following parameters.*`\\.\\.\\..*single positive"
  )
  expect_error(
    daedalus("Canada", r0 = Inf),
    regexp = "Expected the following parameters.*`\\.\\.\\..*single positive"
  )

  # checks on allowed numeric vector inputs
  "Expected the following parameters passed in `...` to be numeric
          vectors of length {N_AGE_GROUPS} with positive and finite values:
          {.str {intersect(names(user_params), allowed_numerics)}}"

  param_passed <- "eta"
  expected_error <- glue::glue(
    "Expected the following parameters passed in `...` to be numeric \\
    vectors of length {N_AGE_GROUPS} with positive and finite values: \\
    {glue::double_quote(param_passed)}"
  )

  expect_error(
    daedalus("Canada", eta = c(0.1, 0.1)),
    regexp = expected_error
  )
  expect_error(
    daedalus("Canada", eta = 0.1),
    regexp = expected_error
  )
  expect_error(
    daedalus("Canada", eta = rep(Inf, 4)),
    regexp = expected_error
  )
  expect_error(
    daedalus("Canada", eta = rep(-1.0, 4)),
    regexp = expected_error
  )

  # specific checks on poorly specified community contact matrix
  expect_error(
    daedalus(
      "Canada",
      contact_matrix = matrix("a", N_AGE_GROUPS, N_AGE_GROUPS)
    ),
    regexp = "Model options.*`\\.\\.\\.`.*may only be numeric"
  )
  expect_error(
    daedalus(
      "Canada",
      contact_matrix = list(matrix(1.0, N_AGE_GROUPS, N_AGE_GROUPS))
    ),
    regexp = "Model options.*`\\.\\.\\.`.*may only be numeric"
  )

  cm <- matrix(1.0, N_AGE_GROUPS, N_AGE_GROUPS)
  cm[1L, ] <- NA_real_
  expect_error(
    daedalus("Canada", contact_matrix = cm),
    regexp = "Expected.*`contact_matrix`.*have no missing elements"
  )

  cm <- matrix(1.0, N_AGE_GROUPS - 1L, N_AGE_GROUPS)
  expect_error(
    daedalus(
      "Canada",
      contact_matrix = cm
    ),
    regexp = "Expected user-provided `contact_matrix` to be a square matrix"
  )

  cm <- matrix(1.0, N_AGE_GROUPS, N_AGE_GROUPS - 1L)
  expect_error(
    daedalus(
      "Canada",
      contact_matrix = cm
    ),
    regexp = "Expected user-provided `contact_matrix` to be a square matrix"
  )

  # expectations on the within-sector workplace contacts
  cw <- rep("1.0", N_ECON_SECTORS)
  expect_error(
    daedalus("Canada", contacts_workplace = cw),
    regexp = "Model options passed as `...` may only be numeric elements"
  )
  cw <- matrix(1.0, N_ECON_SECTORS, N_ECON_SECTORS)
  expect_error(
    daedalus("Canada", contacts_workplace = cw),
    regexp = "Expected.*`contacts_workplace`.*numeric vector"
  )

  cw <- rep(1.0, N_ECON_SECTORS - 1L)
  expect_error(
    daedalus("Canada", contacts_workplace = cw),
    regexp = "Expected.*`contacts_workplace`.*numeric vector"
  )
  cw <- rep(-1.0, N_ECON_SECTORS)
  expect_error(
    daedalus("Canada", contacts_workplace = cw),
    regexp = "Expected.*`contacts_workplace`.*numeric vector"
  )

  # expectations for consumer-worker contacts
  cmcw <- matrix(
    "1.0", N_ECON_SECTORS, N_AGE_GROUPS
  )
  expect_error(
    daedalus("Canada", contacts_consumer_worker = cmcw),
    regexp = "Model options passed as `...` may only be numeric elements"
  )

  cmcw <- rep(1.0, N_ECON_SECTORS)
  expect_error(
    daedalus("Canada", contacts_consumer_worker = cmcw),
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
    daedalus("Canada", contacts_consumer_worker = cmcw),
    regexp = expected_error
  )

  # expectations for worker-worker contacts betweens sectors
  cm_ww <- matrix("1.0", N_ECON_SECTORS, N_ECON_SECTORS)
  expect_error(
    daedalus("Canada", contacts_between_sectors = cm_ww),
    regexp = "Model options passed as `...` may only be numeric elements"
  )

  cm_ww <- matrix(1.0, N_ECON_SECTORS - 2L, N_ECON_SECTORS - 2L)
  expected_error <- glue::glue(
    "Expected `contacts_between_sectors` to be a square numeric \\
    matrix with {N_ECON_SECTORS} rows and columns"
  )
  expect_error(
    daedalus("Canada", contacts_between_sectors = cm_ww),
    regexp = as.character(expected_error), perl = TRUE
  )

  cm_ww <- matrix(1.0, N_ECON_SECTORS, N_ECON_SECTORS)
  cm_ww[, 1L] <- NA_real_
  expect_error(
    daedalus("Canada", contacts_between_sectors = cm_ww),
    regexp =
      "Expected.*numeric matrix.*with no missing elements"
  )

  cm_ww <- matrix(1.0, N_ECON_SECTORS, N_ECON_SECTORS)
  expect_error(
    daedalus("Canada", contacts_between_sectors = cm_ww),
    regexp = "Expected.*diagonal entries are all zero"
  )
})
