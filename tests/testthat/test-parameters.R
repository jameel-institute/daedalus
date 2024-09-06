# # Basic unit tests for the default model parameter expectations
# NOTE: testing that parameter errors are bubbled up from `daedalus()`
# country_canada <- country("Canada")
# test_that("Errors on model parameters passed to `daedalus()`:", {
#   expect_error(
#     daedalus(
#       country_canada, "influenza_1918",
#       infect_params_manual = list(r0 = "0.01")
#     ),
#     regexp = "Expected.*`infect_params_manual`.*single positive.*finite number"
#   )
#   expect_error(
#     daedalus(
#       country_canada, "influenza_1918",
#       infect_params_manual = list(r0 = c(0.1, 0.1))
#     ),
#     regexp = "Expected.*`infect_params_manual`.*single positive.*finite number"
#   )
#   expect_error(
#     daedalus(
#       country_canada, "influenza_1918",
#       infect_params_manual = list(r0 = -0.1)
#     ),
#     regexp = "Expected.*`infect_params_manual`.*single positive.*finite number"
#   )
#   expect_error(
#     daedalus(
#       country_canada, "influenza_1918",
#       infect_params_manual = list(r0 = Inf)
#     ),
#     regexp = "Expected.*`infect_params_manual`.*single positive.*finite number"
#   )

#   # checks on expected numerics of correct length
#   param_passed <- "eta"
#   expected_error <- glue::glue(
#     "Expected the following parameters passed in `infect_params_manual` to be \\
#     numeric vectors of length {N_AGE_GROUPS} with positive and finite values: \\
#     {glue::double_quote(param_passed)}"
#   )

#   expect_error(
#     daedalus(
#       country_canada, "influenza_1918",
#       infect_params_manual = list(eta = c(0.1, 0.1))
#     ),
#     regexp = expected_error
#   )
#   expect_error(
#     daedalus(
#       country_canada, "influenza_1918",
#       infect_params_manual = list(eta = 0.1)
#     ),
#     regexp = expected_error
#   )
#   expect_error(
#     daedalus(
#       country_canada, "influenza_1918",
#       infect_params_manual = list(eta = rep(Inf, 4))
#     ),
#     regexp = expected_error
#   )
#   expect_error(
#     daedalus(
#       country_canada, "influenza_1918",
#       infect_params_manual = list(eta = rep(-1.0, 4))
#     ),
#     regexp = expected_error
#   )

#   # checks on extra infection parameters - now bumped up to error
#   expect_error(
#     daedalus(
#       country_canada, "influenza_1918",
#       infect_params_manual = list(dummy_param = 0.1)
#     ),
#     regexp = "`infect_params_manual` found parameters that are not allowed"
#   )
# })

# # adding snapshot test on all infection parameter values to catch
# # potentially unwanted raw data changes
# test_that("infection_data: Snapshot tests", {
#   expect_snapshot(
#     infection_data
#   )
# })
