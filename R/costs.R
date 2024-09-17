#' Get epidemic costs from a DAEDALUS model run
#'
#' @param x A `<daedalus_output>` object from a call to [daedalus()].
#' @return A list of different cost values, including the total cost. See
#' **Details** for more information.
#'
#' @details
#'
#' The total cost in million dollars is returned as `total_cost`. This is
#' comprised of the following costs.
#'
#' ## Economic costs
#'
#' A three element list of `economic_cost_total`, the total costs from pandemic
#' impacts on economic sectors, including both costs of lost gross value added
#' (GVA) due to pandemic-control restrictions or closures
#' (`economic_cost_closures`), and pandemic-related absences due to illness and
#' death (`economic_cost_absences`).
#'
#' ## Educational costs
#'
#' A three element list of `education_cost_total`, the total costs from pandemic
#' impacts on education due to pandemic-control restrictions or closures
#' (`education_cost_closures`), and pandemic-related absences due to illness and
#' death (`education_cost_absences`).
#'
#' ## Life-years lost
#'
#' A four-element vector (for the number of age groups) giving the value of
#' life-years lost per age group. This is calculated as the life-expectancy of
#' each age group times the value of a statistical life, with all years assumed
#' to have the same value.
#'
#' @examples
#' output <- daedalus("Canada", "influenza_1918")
#'
#' get_costs(output)
#' @export
get_costs <- function(x) {
  checkmate::assert_class(x, "daedalus_output")

  compartment <- NULL
  prop_absent <- NULL
  gva_loss <- NULL
  total_absent <- NULL
  econ_sector <- NULL
  value <- NULL

  closure_duration <- x$response_data$closure_info$closure_duration
  closure_start <- x$response_data$closure_info$closure_time_start
  closure_end <- x$response_data$closure_info$closure_time_end

  gva <- x$country_parameters$gva
  openness <- x$response_data$openness

  # cost of closures
  economic_cost_closures <- sum(
    gva[-i_EDUCATION_SECTOR] * (1 - openness[-i_EDUCATION_SECTOR]) *
      closure_duration
  )

  education_cost_closures <- sum(
    gva[i_EDUCATION_SECTOR] * (1 - openness[i_EDUCATION_SECTOR]) *
      closure_duration
  )

  # absences due to infection, hospitalisation, death
  model_data <- x$model_data
  data.table::setDT(model_data)
  worker_absences <- model_data[
    grepl("infect|dead|hosp", compartment) &
      econ_sector != "sector_00",
    list(total_absent = sum(value)),
    by = c("time", "econ_sector")
  ]

  workforce <- x$country_parameters$workers

  worker_absences[, prop_absent := total_absent / workforce, by = "time"]
  worker_absences[, gva_loss := gva * prop_absent, by = "time"]
  worker_absences[data.table::between(time, closure_start, closure_end),
    gva_loss := gva_loss * openness,
    by = "time"
  ]

  worker_absences <- worker_absences[, list(gva_loss = sum(gva_loss)),
    by = {
      sector <- data.table::fcase(
        econ_sector == sprintf("sector_%02i", i_EDUCATION_SECTOR), "education",
        default = "non_edu"
      )
      sector
    }
  ]

  education_cost_absences <- worker_absences[
    worker_absences$sector == "education",
  ]$gva_loss
  economic_cost_absences <- worker_absences[
    worker_absences$sector == "non_edu",
  ]$gva_loss

  # calculate total deaths and multiply by VSL
  total_deaths <- model_data[compartment == "dead" & time == max(time),
    list(deaths = sum(value)),
    by = "age_group"
  ]$deaths

  # NOTE: in million $s
  life_years_lost <- x$country_parameters$vsl * total_deaths / 1e6

  cost_list <- list(
    total_cost = NA,
    economic_costs = list(
      economic_cost_total = economic_cost_closures + economic_cost_absences,
      economic_cost_closures = economic_cost_closures,
      economic_cost_absences = economic_cost_absences
    ),
    education_costs = list(
      education_cost_total = education_cost_closures + education_cost_absences,
      education_cost_closures = education_cost_closures,
      education_cost_absences = education_cost_absences
    ),
    life_years_lost = life_years_lost
  )

  cost_list$total_cost <- sum(rapply(cost_list, sum), na.rm = TRUE)

  cost_list
}
