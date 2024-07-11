# Tests for prepare_output
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
) * demography

# define end time
time_end <- 2L
n_age_groups <- 4L
n_compartments <- 7L

# basic output
output <- daedalus(initial_state,
  time_end = time_end,
  parameters = default_parameters(contact_matrix = contact_matrix)
)

test_that("prepare_output: basic expectations", {
  # expect no conditions
  expect_no_condition(
    prepare_output(output)
  )
  # expect data structure
  data <- prepare_output(output)
  expect_s3_class(data, "data.frame")
  expect_length(data, 4L)
  expect_identical(
    nrow(data),
    time_end * n_age_groups * n_compartments
  )
  expect_named(
    data,
    c("time", "age_group", "compartment", "value")
  )
  expect_type(
    data[["time"]], "double"
  )
  expect_type(
    data[["age_group"]], "character"
  )
  expect_type(
    data[["compartment"]], "character"
  )
  expect_type(
    data[["value"]], "double"
  )

  # expect snapshot is identical
  expect_snapshot(
    head(
      prepare_output(output),
      50L
    )
  )
})
