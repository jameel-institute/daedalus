#' Coefficient to scale transmission by public concern about the pandemic
#'
#' @description The social distancing coefficient is a value between 1.0 and an
#' arbitrary lower limit, which is used to scale the transmission rate and
#' represents public concern around the pandemic. The coefficient is 1.0 when
#' there are no deaths, and reduces asymptotically to the lower limit as deaths
#' increase. Social distancing begins when closures begin, and ends when
#' vaccination is complete (*not yet implemented*).
#'
#' @param new_deaths The number of new deaths at time `t`.
#' @param rate The proportional reduction of social contacts due to each
#' additional death. Defaults to 0.01, or a 0.1% decrease for each additional
#' death.
#' @param lower_limit The lower limit to which social contacts can be reduced.
#' Defaults to 0.2, which is equivalent to a greater than a 50% reduction in
#' incoming and outgoing contacts.
#'
#' @return A single number between 1 and `lower_limit` for the social distancing
#' coefficient.
#' @keywords internal
get_distancing_coefficient <- function(
  new_deaths,
  rate = 0.001,
  lower_limit = 0.2
) {
  # NOTE: no input checks on this internal function
  (1 - rate)^new_deaths * (1 - lower_limit) + lower_limit
}
