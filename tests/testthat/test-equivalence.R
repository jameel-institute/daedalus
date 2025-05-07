test_that("daedalus() and daedalus2() are equivalent", {
  # Excluding economic infections
  x <- daedalus_infection("sars_cov_1", rho = 0.0)
  ct <- daedalus_country("GBR")
  ct$hospital_capacity <- 1e7
  # set workers to 1 to discount effect
  ct$workers <- rep(1, length(ct$workers))
  # modify vaccination to fit needs
  vax <- daedalus_vaccination(
    "none",
    rate = 0.0,
    start_time = 5,
    uptake_limit = 0
  )

  # get outputs
  # NOTE: response specified as 'none' rather than NULL because `daedalus()`
  # triggers public-concern distancing even when response is 'none'
  output_daedalus2 <- daedalus2(
    ct,
    x,
    response_strategy = "elimination",
    response_time = 2,
    response_duration = 400,
    time_end = 399
  ) # one less timestep
  output_daedalus <- daedalus(
    ct,
    x,
    "elimination",
    vax,
    response_time = 2,
    time_end = 400,
    initial_state_manual = list(p_infectious = 1e-7)
  )

  # compare final sizes
  fs_daedalus <- get_epidemic_summary(output_daedalus, "infections")
  fs_daedalus2 <- get_epidemic_summary(output_daedalus2, "infections")

  # a small difference is okay
  expect_identical(fs_daedalus2$value, fs_daedalus$value, tolerance = 1e-5)
})
