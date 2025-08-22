#' Represent a time-limited intervention
#'
#' @name class_npi
#'
#' @description
#' Helper function to create a `<daedalus_npi>` that is only trigged by time,
#' and is not responsive to state variables. Primarily intended for use in real
#' time modelling.
#'
#' @inheritParams daedalus
#'
#' @inheritParams daedalus_npi
#'
#' @param openness A list of numeric vectors giving the openness coefficients in
#' each interval specified by corresponding elements of `start_time` and
#' `end_time`. List elements must be vectors where all values are in the range
#' \eqn{[0, 1]}, giving the openness of each economic sector in the model.
#' Expected to have a length of `N_ECON_SECTORS` (currently 45).
#'
#' @return A `<daedalus_npi>` class object which specifies time-limited
#' interventions only.
#'
#' @export
daedalus_timed_npi <- function(
  start_time,
  end_time,
  openness,
  country
) {
  country <- validate_country_input(country)

  checkmate::assert_numeric(start_time, 0, finite = TRUE)
  checkmate::assert_numeric(
    end_time,
    0,
    finite = TRUE,
    len = length(start_time)
  )
  checkmate::assert_list(
    openness,
    "numeric",
    FALSE,
    len = length(start_time)
  )

  new_daedalus_npi(
    list(openness = openness),
    identifier = "custom_timed",
    id_flag = get_flag_index("npi_flag", country),
    time_on = start_time,
    time_off = end_time,
    id_time_log = get_flag_index("npi_start_time", country)
  )
}
