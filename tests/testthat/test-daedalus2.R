test_that("daedalus2 works", {
  expect_no_condition(
    daedalus2(10)
  )
  expect_no_condition(
    daedalus2(10, n_strata = 2)
  )

  output <- daedalus2(10, n_strata = 2)
  expect_list(output, "numeric")
})
