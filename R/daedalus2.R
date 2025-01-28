#' Internal function for daedalus2
#'
#' @return A list of state values as returned by `dust2::dust_unpack_state()`.
#' @keywords internal
daedalus2_internal <- function(time_end, params) {
  # NOTE: sys params assumed suitable for `do.call()`
  sys_params <- list(daedalus_ode, pars = params)
  sys <- do.call(
    dust2::dust_system_create, sys_params
  )

  dust2::dust_system_set_state_initial(sys)

  state <- dust2::dust_system_simulate(sys, seq(0, time_end))
  dust2::dust_unpack_state(sys, state)
}

#' DAEDALUS model implemented with dust
#'
#' `daedalus::daedalus2()` will eventually replace `daedalus::daedalus()` with
#' the model implemented in C++ to integrate with \pkg{dust2}
#' (and in future \pkg{dust}),
#'
#' @inheritParams daedalus
#' @export
daedalus2 <- function(time_end = 100, ...) {
  # TODO: input checking
  # TODO: parameter filtering
  params <- rlang::list2(...)
  output <- daedalus2_internal(time_end, params)

  # TODO: needs to be compatible with `<daedalus_output>`
  # or equivalent from `{daedalus.compare}`
  output
}
