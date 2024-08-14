# Test that errors from initial state preparation are bubbled up
test_that("Initial state preparation:", {
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      initial_state_manual = list(p_infectious = "0.1")
    ),
    regexp = "must be a single number in the range \\[0.0, 1.0\\]"
  )
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      initial_state_manual = list(p_infectious = -0.1)
    ),
    regexp =
      "`p_infectious` must be a single number in the range \\[0.0, 1.0\\]"
  )
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      initial_state_manual = list(p_infectious = 1.0001)
    ),
    regexp =
      "`p_infectious` must be a single number in the range \\[0.0, 1.0\\]"
  )
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      initial_state_manual = list(p_infectious = c(0.1, 0.5))
    ),
    regexp =
      "`p_infectious` must be a single number in the range \\[0.0, 1.0\\]"
  )

  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      initial_state_manual = list(p_asymptomatic = "0.1")
    ),
    regexp =
      "`p_asymptomatic` must be a single number in the range \\[0.0, 1.0\\]"
  )
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      initial_state_manual = list(p_asymptomatic = -1)
    ),
    regexp =
      "`p_asymptomatic` must be a single number in the range \\[0.0, 1.0\\]"
  )
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      initial_state_manual = list(p_asymptomatic = 1.00001)
    ),
    regexp =
      "`p_asymptomatic` must be a single number in the range \\[0.0, 1.0\\]"
  )
  expect_error(
    daedalus(
      "Canada", "influenza_1918",
      initial_state_manual = list(p_asymptomatic = c(0.1, 0.5))
    ),
    regexp =
      "`p_asymptomatic` must be a single number in the range \\[0.0, 1.0\\]"
  )
})
