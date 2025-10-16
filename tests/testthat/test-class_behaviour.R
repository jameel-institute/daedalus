test_that("Class <daedalus_behaviour> works for original mechanism", {
  expect_no_condition(
    daedalus_old_behaviour()
  )

  behav <- daedalus_old_behaviour()
  expect_s3_class(behav, c("daedalus_behaviour", "daedalus_response"))
  expect_identical(
    behav$identifier,
    "old_behaviour"
  )
  expect_snapshot(behav)

  expect_no_condition(
    daedalus("GB", "sars_cov_1", behaviour = behav, time_end = 100)
  )
})

test_that("Class <daedalus_behaviour> works for new mechanism", {
  expect_no_condition(
    daedalus_new_behaviour(1e4)
  )
  expect_no_condition(
    daedalus_new_behaviour(daedalus_country("GB"))
  )

  behav <- daedalus_new_behaviour(1e4)
  expect_s3_class(behav, c("daedalus_behaviour", "daedalus_response"))
  expect_identical(
    behav$identifier,
    "new_behaviour"
  )
  expect_snapshot(behav)

  expect_no_condition(
    daedalus("GB", "sars_cov_1", behaviour = behav, time_end = 100)
  )
})
