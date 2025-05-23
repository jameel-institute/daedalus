test_that("daedalus can handle multiple infection objects", {
  # NOTE: only for testing, the intention is to accommodate uncertainty in
  # single infection's parameters, not to compare across infections
  infection <- lapply(
    daedalus.data::epidemic_names,
    daedalus_infection
  )

  expect_no_condition(
    daedalus(
      "GBR",
      infection,
      "elimination",
      time_end = 100
    )
  )

  output <- daedalus(
    "GBR",
    infection,
    "elimination",
    time_end = 100
  )
  checkmate::expect_list(output, "daedalus_output")

  expect_identical(
    vapply(output, function(x) x$infection_parameters$name, character(1)),
    daedalus.data::epidemic_names
  )
})
