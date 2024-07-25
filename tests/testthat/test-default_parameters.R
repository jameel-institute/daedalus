# Basic tests for the default model parameter expectations
test_that("default_parameters: basic expectations", {
  # expect no conditions on commonly expected function usage
  expect_no_condition(default_parameters())
  expect_no_condition(default_parameters(beta = 999))
  expect_no_condition(default_parameters(contact_matrix = matrix(2, 4L, 4L)))

  expect_warning(
    default_parameters(alpha = 999),
    regexp = "parameters were passed that are not among the model parameters"
  )

  # NOTE: warning about extra parameters suppressed
  suppressWarnings({
    modified_param_names <- names(default_parameters(alpha = 999))
  })
  expect_identical(
    modified_param_names,
    names(default_parameters())
  )
})

test_that("default_parameters: errors and warnings", {
  expect_error(
    default_parameters(dummy_param = -1.0),
    regexp = "(Expected)*(to be a single positive and finite number)"
  )
  expect_error(
    default_parameters(dummy_param = Inf),
    regexp = "(Expected)*(to be a single positive and finite number)"
  )
  expect_error(
    default_parameters(dummy_param = numeric(5L)),
    regexp = "(Expected)*(to be a single positive and finite number)"
  )

  cm <- matrix(1.0, N_AGE_GROUPS, N_AGE_GROUPS)
  cm[1L, ] <- NA_real_

  expect_error(
    default_parameters(
      contact_matrix = cm
    ),
    regexp = "(Expected)*(`contact_matrix` to have no missing elements.)"
  )

  cm <- matrix(1.0, N_AGE_GROUPS - 1L, N_AGE_GROUPS)
  expect_error(
    default_parameters(
      contact_matrix = cm
    ),
    regexp = "Expected user-provided `contact_matrix` to be a square matrix"
  )
  cm <- matrix(1.0, N_AGE_GROUPS, N_AGE_GROUPS - 1L)
  expect_error(
    default_parameters(
      contact_matrix = cm
    ),
    regexp = "Expected user-provided `contact_matrix` to be a square matrix"
  )

  # expectations on the within-sector workplace contacts
  cw <- rep("1.0", N_ECON_SECTORS)
  expect_error(
    default_parameters(contacts_workplace = cw),
    regexp = "(Expected)*(`contacts_workplace`)*(to be a numeric vector)"
  )
  cw <- rep(1.0, N_ECON_SECTORS - 1L)
  expect_error(
    default_parameters(contacts_workplace = cw),
    regexp = "(Expected)*(`contacts_workplace`)*(to be a numeric vector)"
  )
  cw <- rep(-1.0, N_ECON_SECTORS)
  expect_error(
    default_parameters(contacts_workplace = cw),
    regexp = "(Expected)*(`contacts_workplace`)*(to be a numeric vector)"
  )

  # expectations for consumer-worker contacts
  cmcw <- matrix(
    "1.0", N_ECON_SECTORS, N_AGE_GROUPS
  )
  expect_error(
    default_parameters(contacts_consumer_worker = cmcw),
    regexp = "(Expected)*(`contacts_consumer_worker`)*(to be a numeric matrix)"
  )

  expected_error <- glue::glue(
    "Expected user-provided `contacts_consumer_worker` to have \\
    {N_ECON_SECTORS} rows and {N_AGE_GROUPS} columns"
  )
  cmcw <- matrix(
    1.0, N_ECON_SECTORS - 1L, N_AGE_GROUPS - 1L
  )
  expect_error(
    default_parameters(contacts_consumer_worker = cmcw),
    regexp = expected_error
  )

  # expectations for worker-worker contacts betweens sectors
  cm_ww <- matrix("1.0", N_ECON_SECTORS, N_ECON_SECTORS)
  expect_error(
    default_parameters(contacts_between_sectors = cm_ww),
    regexp = "(Expected)*(contacts_between_sectors)*(numeric matrix)"
  )

  cm_ww <- matrix(1.0, N_ECON_SECTORS - 2L, N_ECON_SECTORS - 2L)
  expected_error <- glue::glue(
    "Expected user-provided `contacts_between_sectors` to be a square numeric \\
    matrix with {N_ECON_SECTORS} rows and columns"
  )
  expect_error(
    default_parameters(contacts_between_sectors = cm_ww),
    regexp = expected_error
  )

  cm_ww <- matrix(1.0, N_ECON_SECTORS, N_ECON_SECTORS)
  expect_error(
    default_parameters(contacts_between_sectors = cm_ww)
  )
})
