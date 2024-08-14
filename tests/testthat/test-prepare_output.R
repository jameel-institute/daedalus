# Tests for prepare_output
# define end time
time_end <- 2L

# basic output
output <- daedalus("Canada", time_end = time_end)
test_that("prepare_output: basic expectations with re-infections", {
  # expect no conditions
  expect_no_condition(
    prepare_output(output)
  )
  # expect data structure
  data <- prepare_output(output)
  expect_s3_class(data, "data.frame")
  expect_length(data, N_OUTPUT_COLS)
  expect_identical(
    nrow(data),
    time_end * N_AGE_GROUPS * N_EPI_COMPARTMENTS * N_ECON_STRATA
  )
  expect_named(
    data,
    c("time", "age_group", "compartment", "econ_sector", "value")
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
})
