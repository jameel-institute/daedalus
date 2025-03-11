# Tests for daedalus2() mirror `test-daedalus.R`
country_canada <- daedalus_country("Canada")

test_that("daedalus2: basic expectations", {
  # expect no conditions
  expect_no_condition({
    daedalus2(country_canada, "influenza_1918")
  })
  output <- daedalus2(country_canada, "influenza_1918")

  # expect list is type double and non-negative
  checkmate::expect_list(
    output, "numeric",
    len = N_MODEL_COMPARTMENTS * N_VACCINE_STRATA
  )
  expect_true(
    all(
      vapply(output, function(x) all(x >= 0.0), FUN.VALUE = logical(1))
    )
  )

  # expect vaccination groups are zero
  checkmate::expect_numeric(
    output$S_vax,
    lower = 0, upper = 0
  )

  # expect closed population with no change in total size
  pop_sizes_time <- Reduce(
    x = lapply(output[i_EPI_COMPARTMENTS], colSums), f = `+`
  )
  pop_size <- sum(get_data(country_canada, "demography"))
  expect_length(
    unique(pop_size), 1L
  )
  expect_true(
    all(abs(pop_sizes_time - pop_size) <= 1e-6)
  )
})

test_that("daedalus2: Can run with ISO2 country parameter", {
  expect_no_condition({
    daedalus2("CA", "influenza_1918")
  })

  output <- daedalus2("CA", "influenza_1918")

  # expect list is type double and non-negative
  checkmate::expect_list(
    output, "numeric",
    len = N_MODEL_COMPARTMENTS * N_VACCINE_STRATA
  )
  expect_true(
    all(
      vapply(output, function(x) all(x >= 0.0), FUN.VALUE = logical(1))
    )
  )
})

test_that("daedalus2: Can run with ISO3 country parameter", {
  expect_no_condition({
    daedalus2("CAN", "influenza_1918")
  })

  output <- daedalus2("CAN", "influenza_1918")

  # expect list is type double and non-negative
  checkmate::expect_list(
    output, "numeric",
    len = N_MODEL_COMPARTMENTS * N_VACCINE_STRATA
  )
  expect_true(
    all(
      vapply(output, function(x) all(x >= 0.0), FUN.VALUE = logical(1))
    )
  )
})

# test that daedalus runs for all epidemic infection parameter sets
test_that("daedalus2: Runs for all country x infection x response", {
  country_infection_combos <- data.table::CJ(
    country = daedalus::country_names,
    infection = daedalus::epidemic_names
  )

  time_end <- 10

  # expect no conditions
  invisible(
    Map(
      country_infection_combos$country,
      country_infection_combos$infection,
      f = function(x, y) {
        expect_no_condition(
          daedalus2(
            x, y,
            time_end = time_end
          )
        )
      }
    )
  )
})

# check for vaccination mechanism
test_that("daedalus2: vaccination works", {
  vax <- daedalus_vaccination(
    "none", 0, 0.1, 100
  )
  expect_no_condition(
    daedalus2("THA", "sars_cov_1", vax)
  )
  output <- daedalus2("THA", "sars_cov_1", vax)

  # expect vaccination group is non-zero
  expect_true(
    any(output$S_vax > 0)
  )

  # test that vaccination infection pathways are active
  expect_true(
    any(output$E_vax > 0)
  )

  # expect that vaccination reduces final size
  output_novax <- daedalus2("THA", "sars_cov_1")
  fs_daedalus2 <- sum(output$new_inf)
  fs_daedalus2_novax <- sum(output_novax$new_inf)

  expect_lt(
    fs_daedalus2, fs_daedalus2_novax
  )

  # see `../test-equivalence.R` for tests that no vaccination in
  # `daedalus2()` is equivalent to no vaccination in `daedalus()`
})
