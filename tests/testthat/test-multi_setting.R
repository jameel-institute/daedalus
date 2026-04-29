test_that("Daedalus works with multiple contact settings", {
  x <- daedalus_country("GB")
  cm <- x$contact_matrix$community
  cty <- daedalus_country(
    "GB",
    contact_matrix = list(
      s1 = cm / 2,
      s2 = cm / 2
    )
  )

  # check for internal bounds errors
  expect_no_condition(
    daedalus(cty, "sars_cov_1", time_end = 20)
  )

  # check for correctness: redistributing contacts into multiple settings
  # should not affect the epidemic
  # check beta is correctly calculated
  inf <- daedalus_infection("sars_cov_1")
  expect_identical(
    get_beta(inf, cty),
    get_beta(inf, x)
  )

  # check epidemic size identical
  expect_identical(
    get_epidemic_summary(
      daedalus(cty, "sars_cov_1", time_end = 100)
    ),
    get_epidemic_summary(
      daedalus(x, "sars_cov_1", time_end = 100)
    )
  )

  # check that passing timed NPI on one or both settings is identical
  npi_1_setting <- daedalus_timed_npi(
    30,
    90,
    list(rep(0.5, 45)),
    x
  )
  npi_2_setting <- daedalus_timed_npi(
    30,
    90,
    list(rep(0.5, 45)),
    cty
  )

  expect_identical(
    get_epidemic_summary(
      daedalus(cty, "sars_cov_1", npi_2_setting, time_end = 100)
    ),
    get_epidemic_summary(
      daedalus(x, "sars_cov_1", npi_1_setting, time_end = 100)
    )
  )
})
