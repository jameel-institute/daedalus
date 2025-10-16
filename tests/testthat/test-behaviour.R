test_that("Behaviour mechanisms: work with daedalus()", {
  expect_no_condition(
    daedalus(
      "GBR",
      "sars_cov_1",
      time_end = 200
    )
  )
  expect_no_condition(
    daedalus(
      "GBR",
      "sars_cov_1",
      behaviour = daedalus_old_behaviour(),
      time_end = 200
    )
  )
  expect_no_condition(
    daedalus(
      "GBR",
      "sars_cov_1",
      behaviour = daedalus_new_behaviour(
        daedalus_country("GBR")
      ),
      time_end = 200
    )
  )

  # check for effects
  data_behav_off <- daedalus(
    "GBR",
    "sars_cov_1",
    time_end = 200
  )
  data_behav_old <- daedalus(
    "GBR",
    "sars_cov_1",
    behaviour = daedalus_old_behaviour(),
    time_end = 200
  )
  data_behav_new <- daedalus(
    "GBR",
    "sars_cov_1",
    behaviour = daedalus_new_behaviour(daedalus_country("GBR")),
    time_end = 200
  )

  fs_behav_off <- get_epidemic_summary(data_behav_off, "infections")
  fs_behav_old <- get_epidemic_summary(data_behav_old, "infections")
  fs_behav_new <- get_epidemic_summary(data_behav_new, "infections")

  expect_lt(
    fs_behav_old$value,
    fs_behav_off$value
  )
  expect_lt(
    fs_behav_new$value,
    fs_behav_off$value
  )
})

test_that("Behaviour mechanisms work with daedalus_multi_infection()", {
  infection_list <- list(
    daedalus_infection("sars_cov_1"),
    daedalus_infection("influenza_2009")
  )
  expect_no_condition(
    daedalus_multi_infection(
      "GBR",
      infection_list,
      time_end = 20
    )
  )
  expect_no_condition(
    daedalus_multi_infection(
      "GBR",
      infection_list,
      behaviour = daedalus_old_behaviour(),
      time_end = 20
    )
  )
  expect_no_condition(
    daedalus_multi_infection(
      "GBR",
      infection_list,
      behaviour = daedalus_new_behaviour(
        daedalus_country("GBR")
      ),
      time_end = 20
    )
  )
})
