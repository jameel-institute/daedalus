# Tests for the <daedalus_output> class
test_that("class <daedalus_output>: basic expectations", {
  expect_no_condition({
    output <- daedalus("Canada", "influenza_1918") # nolint saves extra assignment
  })
  expect_s3_class(output, "daedalus_output")
  checkmate::expect_list(
    output, c("data.frame", "list"),
    any.missing = FALSE
  )
  expect_snapshot(
    output
  )

  checkmate::expect_data_frame(
    get_data(output), ncols = 5L
  )
  checkmate::expect_list(
    get_data(output, "response_data")
  )
  expect_error(
    get_data(output, c("model_data", "response_data")),
    regexp = "must be a single string"
  )
})
