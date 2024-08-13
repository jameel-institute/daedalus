# snapshot tests for country data to flag unexpected changes
seq <- seq(1, length(daedalus::country_data), 20) # approx 10
test_that("daedalus::country_data: snapshot tests", {
  expect_snapshot(
    daedalus::country_data[seq]
  )

  expect_identical(
    daedalus::country_data[seq],
    daedalus::country_data[daedalus::country_names[seq]]
  )

  expect_snapshot(
    daedalus::country_data[["Canada"]]
  )
})
