# Test that errors from initial state preparation are bubbled up
test_that("Initial state preparation:", {
  expect_error(
    daedalus("Canada", p_infectious = "1.0"),
    regexp = "Model options passed as `...` may only be numeric elements"
  )
  expect_error(
    daedalus("Canada", p_infectious = -1.0),
    regexp =
      "`p_infectious` must be a single number in the range \\[0.0, 1.0\\]"
  )
  expect_error(
    daedalus("Canada", p_infectious = 1.000001),
    regexp =
      "`p_infectious` must be a single number in the range \\[0.0, 1.0\\]"
  )
  expect_error(
    daedalus("Canada", p_infectious = c(0.5, 0.5)),
    regexp =
      "`p_infectious` must be a single number in the range \\[0.0, 1.0\\]"
  )

  expect_error(
    daedalus("Canada", p_asymptomatic = "1.0"),
    regexp = "Model options passed as `...` may only be numeric elements"
  )
  expect_error(
    daedalus("Canada", p_asymptomatic = -1.0),
    regexp =
      "`p_asymptomatic` must be a single number in the range \\[0.0, 1.0\\]"
  )
  expect_error(
    daedalus("Canada", p_asymptomatic = 1.000001),
    regexp =
      "`p_asymptomatic` must be a single number in the range \\[0.0, 1.0\\]"
  )
  expect_error(
    daedalus("Canada", p_asymptomatic = c(0.5, 0.5)),
    regexp =
      "`p_asymptomatic` must be a single number in the range \\[0.0, 1.0\\]"
  )
})
