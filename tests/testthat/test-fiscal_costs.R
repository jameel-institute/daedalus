test_that("Calculating fiscal costs works", {
  expect_no_condition(
    get_fiscal_costs(
      daedalus("CAN", "sars_cov_1")
    )
  )
  expect_no_condition(
    get_fiscal_costs(
      daedalus("GBR", "sars_cov_1", "economic_closures")
    )
  )
  expect_no_condition(
    get_fiscal_costs(
      daedalus("CAN", "sars_cov_1", vaccine_investment = "high")
    )
  )
})
