# Basic tests for the DAEDALUS model
# Prepare some data
polymod <- socialmixr::polymod
# suppress messages that clog up test output
suppressMessages(
  contact_data <- socialmixr::contact_matrix(
    polymod,
    countries = "United Kingdom",
    age.limits = c(0, 5, 20, 65),
    symmetric = TRUE
  )
)

# get demography vector
demography <- contact_data[["demography"]][["population"]]

# prepare contact matrix
contact_matrix <- t(contact_data[["matrix"]]) / demography

# initial state: one in every 1 million is infected
initial_i <- 1e-6
initial_state <- c(
  S = 1.0 - initial_i, E = 0.0,
  Is = initial_i, Ia = 0.0,
  H = 0.0, R = 0, D = 0
)

# build for all age groups
initial_state <- rbind(
  initial_state,
  initial_state,
  initial_state,
  initial_state
)

# multiply by demography vector for absolute values
initial_state <- initial_state * demography

test_that("daedalus: basic expectations", {
  # expect no conditions
  expect_no_condition(
    daedalus(
      initial_state,
      parameters = default_parameters(contact_matrix = contact_matrix)
    )
  )

  output <- daedalus(
    initial_state,
    parameters = default_parameters(contact_matrix = contact_matrix)
  )

  # expect classed output, type double, and non-negative
  expect_s3_class(output, "deSolve")
  expect_type(output, "double")
  expect_true(all(output >= 0.0))

  # expect closed population with no change in total size
  # NOTE: disregard first column holding time
  # NOTE: set tolerance to a reasonable value
  expect_identical(
    sum(tail(output[, -1L], 1)),
    sum(head(output[, -1L], 1)),
    tolerance = 1e-12
  )
})


test_that("daedalus: errors and warnings", {
  # expect errors on poorly specified initial state
  expect_error(
    daedalus(initial_state = as.data.frame(initial_state)),
    regexp = "Must be of type 'matrix'"
  )
  expect_error(
    daedalus(initial_state = matrix("1", 4L, 7L)),
    regexp = "Must store numerics"
  )

  expect_error(
    daedalus(initial_state = initial_state[, -1L]),
    regexp = "Must have exactly 7 cols"
  )
  expect_error(
    daedalus(initial_state = initial_state[-1L, ]),
    regexp = "Must have exactly 4 rows"
  )

  initial_state_ <- initial_state
  initial_state_[1L, ] <- NA_real_
  expect_error(
    daedalus(initial_state_),
    regexp = "Contains missing values"
  )

  # expect errors on poorly specified time_end
  expect_error(
    daedalus(initial_state, time_end = -1)
  )
  expect_error(
    daedalus(initial_state, time_end = 100.5)
  )
  expect_error(
    daedalus(initial_state, time_end = Inf)
  )

  # expect errors on poorly specified parameter list
  expect_error(
    daedalus(initial_state, parameters = "parameters")
  )
  expect_error(
    daedalus(
      initial_state,
      parameters = lapply(default_parameters(), as.character)
    ),
    regexp = "(May only contain)*(numeric)"
  )
})
