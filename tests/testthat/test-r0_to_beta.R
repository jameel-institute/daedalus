test_that("Calculating beta from R0", {
  infection <- daedalus_infection("sars_cov_1")
  country <- daedalus_country("United Kingdom")

  expect_no_condition(get_beta(infection, country))

  expect_snapshot(get_beta(infection, country))
})

test_that("Getting the next-generation matrix", {
  infection <- daedalus_infection("sars_cov_1")
  country <- daedalus_country("United Kingdom")

  expect_no_condition(get_ngm(country, infection))
  expect_snapshot(get_ngm(country, infection))

  # correctness: leading eigenvalue should be infection R0
  ngm <- get_ngm(country, infection)
  expect_identical(
    max(Re(eigen(ngm)$values)),
    infection$r0,
    tolerance = 1e-6
  )

  # expect that p_susc = 0.5 gives half R0
  ratio <- 0.5
  ngm <- get_ngm(country, infection, p_susc = ratio)
  expect_identical(
    max(Re(eigen(ngm)$values)),
    infection$r0 * ratio,
    tolerance = 1e-6
  )
})
