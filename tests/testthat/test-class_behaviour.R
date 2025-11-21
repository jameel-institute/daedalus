test_that("Class <daedalus_behaviour> works for original mechanism", {
  expect_no_condition(
    daedalus_old_behaviour()
  )

  behav <- daedalus_old_behaviour()
  expect_s3_class(behav, c("daedalus_behaviour", "daedalus_response"))
  expect_identical(
    behav$identifier,
    "old behaviour"
  )
  expect_snapshot(behav)

  expect_no_condition(
    daedalus("GB", "sars_cov_1", behaviour = behav, time_end = 100)
  )
})

test_that("Class <daedalus_behaviour> works for new mechanism", {
  expect_no_condition(
    daedalus_new_behaviour()
  )

  behav <- daedalus_new_behaviour()
  expect_s3_class(behav, c("daedalus_behaviour", "daedalus_response"))
  expect_identical(
    behav$identifier,
    "new behaviour"
  )
  expect_snapshot(behav)

  expect_no_condition(
    daedalus("GB", "sars_cov_1", behaviour = behav, time_end = 100)
  )
})

test_that("Class <daedalus_behaviour>: errors correctly", {
  # errors from daedalus_old_behaviour()
  expect_error(
    daedalus_old_behaviour(-1),
    "Assertion on 'rate' failed: Element 1 is not >= 0"
  )
  expect_error(
    daedalus_old_behaviour(c(0.5, 0.5)),
    "Assertion on 'rate' failed: Must have length 1."
  )
  expect_error(
    daedalus_old_behaviour("0.5"),
    "Assertion on 'rate' failed: Must be of type 'number', not 'character'."
  )

  expect_error(
    daedalus_old_behaviour(0.001, -1),
    "Assertion on 'lower_limit' failed: Element 1 is not >= 0"
  )
  expect_error(
    daedalus_old_behaviour(0.001, c(0.5, 0.5)),
    "Assertion on 'lower_limit' failed: Must have length 1."
  )
  expect_error(
    daedalus_old_behaviour(0.001, "0.5"),
    "Assertion on 'lower_limit' failed: Must be of type 'number'"
  )

  # errors from daedalus_new_behaviour()
  expect_error(
    daedalus_new_behaviour(-0.5),
    "Assertion on 'behav_effectiveness' failed: Element 1 is not >= 0"
  )
  expect_error(
    daedalus_new_behaviour(c(0.5, 0.5)),
    "Assertion on 'behav_effectiveness' failed: Must have length 1."
  )
  expect_error(
    daedalus_new_behaviour("0.5"),
    "Assertion on 'behav_effectiveness' failed: Must be of type 'number'"
  )

  expect_error(
    daedalus_new_behaviour(0.5, -1),
    "Assertion on 'baseline_optimism' failed: Element 1 is not >= 0"
  )
  expect_error(
    daedalus_new_behaviour(0.5, c(1, 1)),
    "Assertion on 'baseline_optimism' failed: Must have length 1."
  )
  expect_error(
    daedalus_new_behaviour(0.5, c("1", "1")),
    "Assertion on 'baseline_optimism' failed: Must be of type 'number'"
  )

  expect_error(
    daedalus_new_behaviour(0.5, 0.5, 1.5, -4),
    "Assertion on 'k0' failed: Element 1 is not >= 0"
  )
  expect_error(
    daedalus_new_behaviour(0.5, 0.5, 1.5, c(4, 4)),
    "Assertion on 'k0' failed: Must have length 1."
  )
  expect_error(
    daedalus_new_behaviour(0.5, 0.5, 1.5, "4"),
    "Assertion on 'k0' failed: Must be of type 'number'"
  )

  expect_error(
    daedalus_new_behaviour(0.5, 0.5, 1.5, 4, 1),
    "Assertion on 'k1' failed: Element 1 is not <= 0"
  )
  expect_error(
    daedalus_new_behaviour(0.5, 0.5, 1.5, 4, c(-9, -9)),
    "Assertion on 'k1' failed: Must have length 1."
  )
  expect_error(
    daedalus_new_behaviour(0.5, 0.5, 1.5, 4, "-9"),
    "Assertion on 'k1' failed: Must be of type 'number'"
  )
})
