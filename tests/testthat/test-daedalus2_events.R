# check for vaccination mechanism
test_that("daedalus2: events can start at model start time", {
  # test on events starting at model start time
  vax <- daedalus_vaccination("none", 0, 0.5, 100)
  output <- daedalus2("THA", "sars_cov_1", vaccine_investment = vax)

  # expect vaccination group is non-zero
  expect_gt(max(output$S_vax), 0.0)
})

test_that("daedalus2: root-finding events launch at each appropriate root", {
  output <- daedalus2(
    "THA",
    "sars_cov_1",
    response_strategy = "elimination",
    time_end = 600
  )

  # expect 3 distinct response start times
  # known value from visual inspection + 1 for zero
  # NOTE: need way to log events that really begin at time = 0
  expect_length(
    unique(as.vector(output$resp_start)),
    3L
  )
})

test_that("daedalus2: events end at defined time", {
  # response should end in 60 days
  output <- daedalus2(
    "THA",
    "sars_cov_1",
    response_strategy = "elimination",
    time_end = 200
  )

  # expect response start and end value is set
  expect_gt(
    max(output$resp_start),
    0.0
  )
  expect_gt(
    max(output$resp_end),
    0.0
  )
  expect_identical(
    max(output$resp_end),
    max(output$resp_start) + 60.0 # arbitrary duration set internally
  )
})
