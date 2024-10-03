#' @title Vaccine investment scenario parameters
#'
#' @name vaccine_scenarios
#' @rdname vaccine_scenarios
#'
#' @description Vaccination parameters for four main pre-pandemic scenarios of
#' pre-pandemic investments in vaccine development and delivery.
#'
#' `vaccine_scenario_names` provides the names by which the advance vaccine
#' investment scenarios are known.
#'
#' @details
#' Each scenario has three parameters; parameters do not differ across age or
#' other groups.
#'
#' - Vaccination start time: The time in days at which vaccination begins.
#'
#' - Vaccination rate: The rate, as a percentage of the population vaccinated
#' per day.
#'
#' - Vaccination uptake limit: The percentage of the population that is willing
#' to accept vaccination.
#'
#' @format
#'
#' ## `vaccination_scenario_data`
#' A list with 4 elements, each corresponding to an advance vaccine investment
#' strategy. Each element is a list of three values (see **Details**).
#'
#' - `vax_start_time`: A single number for the vaccination start time in days.
#'
#' - `nu`: A single number for the daily vaccination rate as a percentage
#' of the population vaccinated.
#'
#' - `vax_uptake_limit`: A single number for the percentage of the population
#' that is willing to be vaccinated.
#'
#' ## `vaccination_scenario_names`
#'
#' A character vector of four elements giving the identifier for each scenario.
#'
#' ## `vaccination_parameter_names`
#'
#' A character vector of three elements giving the identifier for vaccination
#' parameters.
#'
#' @source See processing details in `data-raw/vaccination_data.R
#' @examples
#' # check vaccination scenarios
#' vaccination_scenario_names
#'
#' vaccination_scenario_data
"vaccination_scenario_data"

#' @name vaccine_scenarios
"vaccination_scenario_names"

#' @name vaccine_scenarios
"vaccination_parameter_names"
