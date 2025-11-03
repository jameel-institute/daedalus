# Test that errors from initial state preparation are bubbled up
test_that("Initial vaccinated proportion is correct", {
  p_immune <- 0.5
  state <- make_initial_state(daedalus_country("GB"), list(p_immune = p_immune))

  n_comps <- (N_AGE_GROUPS + N_ECON_SECTORS) * N_MODEL_COMPARTMENTS
  i_unvaxxed <- 1:n_comps
  i_vaxxed <- (n_comps + 1):(n_comps * 2L)

  expect_identical(
    state[i_unvaxxed],
    state[i_vaxxed]
  )

  p_immune <- 1 / 4
  state <- make_initial_state(daedalus_country("GB"), list(p_immune = p_immune))
  expect_identical(
    state[i_vaxxed],
    state[i_unvaxxed] / 3,
    tolerance = 1e-6
  )

  # initialising with some vaccinated works
  expect_no_condition(
    daedalus(
      "GB",
      "sars_cov_1",
      initial_state_manual = list(p_immune = p_immune),
      time_end = 100
    )
  )
  expect_no_condition(
    daedalus(
      "GB",
      "sars_cov_1",
      initial_state_manual = list(p_immune = p_immune),
      vaccine_investment = "high",
      time_end = 300
    )
  )

  # phenomenological correctness --- final size is smaller
  out_1 <- daedalus(
    "GB",
    "sars_cov_1",
    initial_state_manual = list(p_immune = p_immune),
    time_end = 100
  )
  out_2 <- daedalus(
    "GB",
    "sars_cov_1",
    time_end = 100
  )
  fs_1 <- get_epidemic_summary(out_1, "infections")$value
  fs_2 <- get_epidemic_summary(out_2, "infections")$value
  expect_lt(
    fs_1,
    fs_2
  )

  # initialising with age-specific p_immune
  p_immune <- c(0, 0, 0, 0.4)
  expect_no_condition(
    daedalus(
      "GB",
      "sars_cov_1",
      initial_state_manual = list(p_immune = p_immune),
      time_end = 100
    )
  )

  out_1 <- daedalus(
    "GB",
    "sars_cov_1",
    initial_state_manual = list(p_immune = p_immune),
    time_end = 100
  )
  fs_1 <- get_epidemic_summary(out_1, "infections")$value
  expect_lt(
    fs_1,
    fs_2
  )
})

test_that("Initial state preparation:", {
  country_canada <- daedalus_country("Canada")
  expect_error(
    daedalus(
      country_canada,
      "influenza_1918",
      initial_state_manual = list(p_infectious = "0.1")
    ),
    regexp = "must be a single number in the range \\[0.0, 1.0\\]"
  )
  expect_error(
    daedalus(
      country_canada,
      "influenza_1918",
      initial_state_manual = list(p_infectious = -0.1)
    ),
    regexp = "(p_infectious)*(single number in the range \\[0.0, 1.0\\])"
  )
  expect_error(
    daedalus(
      country_canada,
      "influenza_1918",
      initial_state_manual = list(p_infectious = 1.0001)
    ),
    regexp = "(p_infectious)*(single number in the range \\[0.0, 1.0\\])"
  )
  expect_error(
    daedalus(
      country_canada,
      "influenza_1918",
      initial_state_manual = list(p_infectious = c(0.1, 0.5))
    ),
    regexp = "(p_infectious)*(single number in the range \\[0.0, 1.0\\])"
  )

  expect_error(
    daedalus(
      country_canada,
      "influenza_1918",
      initial_state_manual = list(p_asymptomatic = "0.1")
    ),
    regexp = "(p_asymptomatic)*(single number in the range \\[0.0, 1.0\\])"
  )
  expect_error(
    daedalus(
      country_canada,
      "influenza_1918",
      initial_state_manual = list(p_asymptomatic = -1)
    ),
    regexp = "(p_asymptomatic)*(single number in the range \\[0.0, 1.0\\])"
  )
  expect_error(
    daedalus(
      country_canada,
      "influenza_1918",
      initial_state_manual = list(p_asymptomatic = 1.00001)
    ),
    regexp = "(p_asymptomatic)*(single number in the range \\[0.0, 1.0\\])"
  )
  expect_error(
    daedalus(
      country_canada,
      "influenza_1918",
      initial_state_manual = list(p_asymptomatic = c(0.1, 0.5))
    ),
    regexp = "(p_asymptomatic)*(single number in the range \\[0.0, 1.0\\])"
  )
})
