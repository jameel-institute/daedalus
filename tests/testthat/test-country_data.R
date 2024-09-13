# snapshot tests for country data to flag unexpected changes
# check some countries
test_countries <- c("China", "United Kingdom", "Canada")
test_that("daedalus::country_data: snapshot tests", {
  expect_snapshot(
    daedalus::country_data[test_countries]
  )

  expect_snapshot(
    daedalus::country_data[["Canada"]]
  )
})
