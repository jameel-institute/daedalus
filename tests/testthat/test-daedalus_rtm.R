test_that("multiplication works", {
  expect_no_condition(
    daedalus_rtm("GBR", "influenza_1918")
  )
})

bench::mark(
  daedalus_rtm("GBR", "influenza_2009", time_end = 50, 
               initial_state_manual = list(p_infected = 1e-7))
)
