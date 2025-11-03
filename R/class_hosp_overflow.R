#' Make a hospital overflow response
#'
#' @description
#' Intended to be a helper function for internal use only. No parameters are
#' carried although this may change in future.
#'
#' @param country A `<daedalus_country>`.
#'
#' @return An object of class `<daedalus_hosp_overflow>` that inherits from
#' `<daedalus_response>`.
#'
#' @keywords internal
new_daedalus_hosp_overflow <- function(country) {
  identifier <- "hosp_cap_exceeded"
  hospital_capacity <- get_data(country, "hospital_capacity")

  id_state_hosp <- c(
    get_state_indices("hospitalised_recov", country),
    get_state_indices("hospitalised_death", country)
  )

  new_daedalus_response(
    "hosp_cap_exceeded",
    "daedalus_hosp_overflow",
    parameters = list(),
    identifier = identifier,
    id_flag = get_flag_index("hosp_overflow_flag", country),
    id_state_on = id_state_hosp,
    id_state_off = id_state_hosp,
    value_state_on = hospital_capacity,
    value_state_off = hospital_capacity,
    id_time_log = get_flag_index("hosp_overflow_start_time", country)
  )
}
