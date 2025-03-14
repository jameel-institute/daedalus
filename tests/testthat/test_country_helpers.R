test_that("country_name_from_arg looks up name from iso2", {
  expect_identical(country_name_from_arg("CA"), "Canada")
})

test_that("country_name_from_arg looks up name from iso3", {
  expect_identical(country_name_from_arg("CAN"), "Canada")
})

test_that("country_name_from_arg passes through supported country name", {
  expect_identical(country_name_from_arg("Canada"), "Canada")
})

test_that("country_name_from_arg errors on unsupported iso2", {
  expect_error(country_name_from_arg("ZZ"), regexp = "must be one of")
})

test_that("country_name_from_arg errors on unsupported iso3", {
  expect_error(country_name_from_arg("ZZZ"), regexp = "must be one of")
})

test_that("country_name_from_arg errors on unsupported country name", {
  expect_error(country_name_from_arg("Narnia"), regexp = "must be one of")
})
