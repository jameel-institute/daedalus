# Tests for the <daedalus_output> class
test_that("class <daedalus_output>: basic expectations", {
  expect_no_condition({
    output <- daedalus("Canada", "influenza_1918") # nolint saves assignment
  })
  expect_s3_class(output, "daedalus_output")
  checkmate::expect_list(
    output,
    c("data.frame", "list", "numeric"),
    any.missing = FALSE
  )
  expect_snapshot(output)

  checkmate::expect_data_frame(get_data(output), ncols = N_OUTPUT_COLS)
  checkmate::expect_list(get_data(output, "response_data"))
  checkmate::expect_list(get_data(output, "country_parameters"))
  # NOTE: tests pass but get_data.daedalus_output() is flagged as not tested
  expect_error(
    get_data(output, c("model_data", "response_data")),
    regexp = "must be a single string naming an element of `x`"
  )
  expect_error(
    get_data(output, "dummy_data"),
    regexp = "must be a single string naming an element of `x`"
  )
})
