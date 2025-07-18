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

  # expect fiscal costs with intervention are higher than without
  o1 <- daedalus("GBR", "sars_cov_1", "none", time_end = 100)
  o2 <- daedalus("GBR", "sars_cov_1", "elimination", time_end = 100)

  fc1 <- get_fiscal_costs(o1)
  fc2 <- get_fiscal_costs(o2)

  expect_gt(
    tail(fc2$fiscal_costs$fiscal_cost, 1),
    tail(fc1$fiscal_costs$fiscal_cost, 1)
  )

  # expect npi costs of no intervention are zero
  expect_identical(
    sum(fc1$fiscal_costs$npi_support),
    0.0
  )

  # expect npi support costs are higher when npi is applied
  expect_gt(
    sum(fc2$fiscal_costs$npi_support),
    sum(fc1$fiscal_costs$npi_support)
  )

  # expect vaccination costs are higher when applied
  vax <- daedalus_vaccination("high", 10)
  o2 <- daedalus(
    "GBR",
    "sars_cov_1",
    vaccine_investment = vax,
    time_end = 100
  )
  fc2 <- get_fiscal_costs(o2)

  expect_gt(
    sum(fc2$fiscal_costs$vax_support),
    sum(fc1$fiscal_costs$vax_support)
  )
})
