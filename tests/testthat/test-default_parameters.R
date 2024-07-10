# Basic tests for the default model parameter expectations
test_that("default_parameters: basic expectations", {
  # expect no conditions on commonly expected function usage
  expect_no_condition(default_parameters())
  expect_no_condition(default_parameters(beta = 999))
  expect_no_condition(default_parameters(contact_matrix = matrix(2, 4L, 4L)))
  expect_no_condition(default_parameters(alpha = 999)) # extra parameter

  expect_identical(
    names(default_parameters(alpha = 999)),
    names(default_parameters())
  )
})

test_that("default_parameters: errors and warnings", {
  expect_error(
    default_parameters(data = data.frame()),
    regexp = "(May only contain)*(numeric)"
  )
})
