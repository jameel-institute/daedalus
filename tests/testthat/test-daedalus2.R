# Tests for daedalus2() mirror `test-daedalus.R`
country_canada <- daedalus_country("Canada")

test_that("daedalus2: basic expectations", {
  # expect no conditions
  expect_no_condition({
    daedalus2(country_canada, "influenza_1918")
  })
  output <- daedalus2(country_canada, "influenza_1918")

  checkmate::expect_list(
    output,
    len = 2L
  )

  # expect list is type double and non-negative
  output <- output$data
  checkmate::expect_list(
    output,
    "numeric",
    len = N_MODEL_COMPARTMENTS * N_VACCINE_STRATA + N_FLAGS
  )
  expect_true(all(vapply(
    output,
    function(x) all(x >= 0.0),
    FUN.VALUE = logical(1)
  )))

  # expect vaccination groups are zero
  checkmate::expect_numeric(output$S_vax, lower = 0, upper = 0)

  # expect closed population with no change in total size
  pop_sizes_time <- Reduce(
    x = lapply(output[i_EPI_COMPARTMENTS], colSums),
    f = `+`
  )
  pop_size <- sum(get_data(country_canada, "demography"))
  expect_length(unique(pop_size), 1L)
  expect_true(all(abs(pop_sizes_time - pop_size) <= 1e-6))
})

test_that("daedalus2: Can run with ISO2 country parameter", {
  expect_no_condition({
    daedalus2("CA", "influenza_1918")
  })

  output <- daedalus2("CA", "influenza_1918")$data

  # expect list is type double and non-negative
  checkmate::expect_list(
    output,
    "numeric",
    len = N_MODEL_COMPARTMENTS * N_VACCINE_STRATA + N_FLAGS
  )
  expect_true(all(vapply(
    output,
    function(x) all(x >= 0.0),
    FUN.VALUE = logical(1)
  )))
})

test_that("daedalus2: Can run with ISO3 country parameter", {
  expect_no_condition({
    daedalus2("CAN", "influenza_1918")
  })

  output <- daedalus2("CAN", "influenza_1918")$data

  # expect list is type double and non-negative
  checkmate::expect_list(
    output,
    "numeric",
    len = N_MODEL_COMPARTMENTS * N_VACCINE_STRATA + N_FLAGS
  )
  expect_true(all(vapply(
    output,
    function(x) all(x >= 0.0),
    FUN.VALUE = logical(1)
  )))
})

# test that daedalus runs for all epidemic infection parameter sets
test_that("daedalus2: Runs for all country x infection x response", {
  country_infection_combos <- data.table::CJ(
    country = daedalus::country_names,
    infection = daedalus::epidemic_names
  )

  time_end <- 10

  # expect no conditions
  invisible(Map(
    country_infection_combos$country,
    country_infection_combos$infection,
    f = function(x, y) {
      expect_no_condition(
        daedalus2(x, y, time_end = time_end, response_time = 5)
      )
    }
  ))
})

# check for vaccination mechanism
test_that("daedalus2: vaccination works", {
  # NOTE: test for truly no vaccination are in default daedalus2 test above

  # NOTE: event starting at t = 0 does not work
  vax <- daedalus_vaccination("none", 10, 0.1, 100)
  expect_no_condition(
    daedalus2("THA", "sars_cov_1", vaccine_investment = vax)
  )
  output <- daedalus2("THA", "sars_cov_1", vaccine_investment = vax)$data

  # expect vaccination group is non-zero
  expect_true(any(output$S_vax > 0))

  # test that vaccination infection pathways are active
  expect_true(any(output$E_vax > 0))

  # expect that vaccination reduces final size
  output_novax <- daedalus2("THA", "sars_cov_1")$data
  fs_daedalus2 <- sum(output$new_inf)
  fs_daedalus2_novax <- sum(output_novax$new_inf)

  expect_lt(fs_daedalus2, fs_daedalus2_novax)

  # see `../test-equivalence.R` for tests that no vaccination in
  # `daedalus2()` is equivalent to no vaccination in `daedalus()`
})

test_that("daedalus2: advanced vaccination features", {
  # use dummy scenarios to check that vaccination uptake limit is respected
  x <- daedalus_country("THA")
  disease_x <- daedalus_infection("sars_cov_1", r0 = 0)

  uptake_limit <- 40
  popsize <- sum(get_data(x, "demography"))
  vax <- daedalus_vaccination("high", uptake_limit = uptake_limit)
  # final size is zero
  output <- daedalus2(
    "THA",
    disease_x,
    vaccine_investment = vax,
    time_end = 600
  )$data

  n_vax <- tail(colSums(output$S_vax) + colSums(output$R_vax), 1)

  # higher tolerance as vaccination is expected to be asymptotic
  expect_identical(
    n_vax,
    uptake_limit * popsize / 100,
    tolerance = 1
  )
})

test_that("daedalus2: responses triggered by hospital capacity event", {
  # with absolutely no response
  expect_no_condition(
    daedalus2("GBR", "sars_cov_1")
  )

  # with named responses (none = absolutely no resp)
  invisible(
    lapply(
      names(daedalus::closure_data),
      function(x) {
        expect_no_condition({
          daedalus2("GBR", "sars_cov_1", x)
        })
      }
    )
  )

  # expect lower final sizes for all interventions
  # very low hosp capacity trigger
  x <- daedalus_country("GBR")
  x$hospital_capacity <- 1e4

  output_list <- lapply(
    names(daedalus::closure_data),
    daedalus2,
    country = x,
    infection = "sars_cov_1"
  )
  resp_scenario_names <- names(daedalus::closure_data)
  output_fs <- vapply(
    output_list,
    function(x) {
      sum(x$data$new_inf)
    },
    FUN.VALUE = numeric(1)
  )
  names(output_fs) <- resp_scenario_names

  invisible(
    lapply(output_fs[names(output_fs) != "none"], expect_lt, output_fs["none"])
  )
})

skip("Root jumping causes test to fail")
test_that("daedalus2: responses ended by epidemic growth", {
  # start response early
  time_end <- 100
  x <- daedalus_country("GBR")
  x$hospital_capacity <- 1e3

  d <- daedalus_infection("influenza_2009")

  output <- daedalus2(
    x,
    "influenza_2009",
    "elimination",
    time_end = time_end,
    response_time = 98
  )

  event_data <- output$event_data
  output <- output$data
  # check that epidemic stops growing by IPR method; IPR < gamma
  ipr <- colSums(output$new_inf) / colSums(output$Is + output$Ia)
  expect_lt(
    min(ipr - d$gamma_Is),
    0.0
  )

  # find end idx
  end_time <- which.min(abs(ipr - d$gamma_Ia)) + 1

  # check that response is switched off at expected time
  expect_identical(
    event_data[event_data$name == "npi_state_off", "time"],
    end_time
  )
})
