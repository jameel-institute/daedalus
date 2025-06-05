test_that("daedalus: Auto-social distancing works", {
  expect_no_condition(
    daedalus(
      "GBR",
      "sars_cov_1",
      "elimination",
      auto_social_distancing = "off",
      time_end = 100
    )
  )
  expect_no_condition(
    daedalus(
      "GBR",
      "sars_cov_1",
      "elimination",
      auto_social_distancing = "independent",
      time_end = 100
    )
  )
  expect_no_condition(
    daedalus(
      "GBR",
      "sars_cov_1",
      "elimination",
      auto_social_distancing = "npi_linked",
      time_end = 100
    )
  )

  # check for effects
  data_sd_off <- daedalus(
    "GBR",
    "sars_cov_1",
    "none",
    auto_social_distancing = "off",
    time_end = 100
  )
  data_sd_ind <- daedalus(
    "GBR",
    "sars_cov_1",
    "none",
    auto_social_distancing = "independent",
    time_end = 100
  )
  data_sd_npi <- daedalus(
    "GBR",
    "sars_cov_1",
    "none",
    auto_social_distancing = "npi_linked",
    time_end = 100
  )

  fs_sd_off <- get_epidemic_summary(data_sd_off, "infections")
  fs_sd_ind <- get_epidemic_summary(data_sd_ind, "infections")
  fs_sd_npi <- get_epidemic_summary(data_sd_npi, "infections")

  # npi-linked on "none" is same as off
  expect_identical(
    fs_sd_npi$value,
    fs_sd_off$value
  )
  expect_lt(
    fs_sd_ind$value,
    fs_sd_off$value
  )

  # auto SD adds to NPI effect
  data_sd_off <- daedalus(
    "GBR",
    "sars_cov_1",
    "elimination",
    auto_social_distancing = "off",
    time_end = 100
  )
  data_sd_npi <- daedalus(
    "GBR",
    "sars_cov_1",
    "elimination",
    auto_social_distancing = "npi_linked",
    time_end = 100
  )

  fs_sd_off <- get_epidemic_summary(data_sd_off, "infections")
  fs_sd_npi <- get_epidemic_summary(data_sd_npi, "infections")

  expect_lt(
    fs_sd_npi$value,
    fs_sd_off$value
  )
})

test_that("daedalus_multi_infection: Auto-social distancing works", {
  infection_list <- list(
    daedalus_infection("sars_cov_1"),
    daedalus_infection("influenza_2009")
  )
  expect_no_condition(
    daedalus_multi_infection(
      "GBR",
      infection_list,
      "elimination",
      auto_social_distancing = "off",
      time_end = 100
    )
  )
  expect_no_condition(
    daedalus_multi_infection(
      "GBR",
      infection_list,
      "elimination",
      auto_social_distancing = "independent",
      time_end = 100
    )
  )
  expect_no_condition(
    daedalus_multi_infection(
      "GBR",
      infection_list,
      "elimination",
      auto_social_distancing = "npi_linked",
      time_end = 100
    )
  )
})
