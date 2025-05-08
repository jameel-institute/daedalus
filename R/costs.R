#' Get epidemic costs from a DAEDALUS model run
#'
#' @param x A `<daedalus_output>` object from a call to [daedalus()].
#' @param summarise_as A string from among "none", "total", or "domain", for how
#' the costs should be returned. Select "none", the default, for the raw costs
#' along with overall and domain-specific totals; "total" for the overall cost,
#' and "domain" for the total costs per domain; the domains are 'economic',
#' 'education', and 'life years'.
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
#' ## Life-value lost
#'
#' A four-element vector (for the number of age groups) giving the value of
#' life-years lost per age group. This is calculated as the life-expectancy of
#' each age group times the value of a statistical life, with all years assumed
#' to have the same value.
#'
#' ## Life-years lost
#'
#' A four-element vector (for the number of age groups) giving the value of
#' life-years lost per age group. This is calculated as the life-expectancy of
#' each age group times the number of deaths in that age group. No quality
#' adjustment is applied.
#'
#' @examples
#' output <- daedalus("Canada", "influenza_1918")
#'
#' get_costs(output)
#' @export
get_costs <- function(x, summarise_as = c("none", "total", "domain")) {
  checkmate::assert_class(x, "daedalus_output")

  gva <- x$country_parameters$gva
  openness <- x$response_data$openness

  # NOTE: might be good to split these into separate functions for different
  # cost domains, but might end up replicating a good bit of code
  # value of a school year, school day (vsd), and N students
  # convert vsd to be in million dollars
  # NOTE: assuming all in school, need data for in-school proportion
  vsy <- get_value_school_year(x$country_parameters$gni)
  vsd <- vsy / 365
  vsd <- vsd / 1e6 # for uniformity with daily GVA
  n_students <- x$country_parameters$demography[i_SCHOOL_AGE]

  # absences due to infection, hospitalisation, death
  model_data <- get_data(x)
  worker_absences <- model_data[
    model_data$compartment %in%
      c("infect_symp", "infect_asymp", "hospitalised", "dead") &
      model_data$econ_sector != "sector_00",
  ]
  worker_absences <- tapply(
    worker_absences$value,
    list(worker_absences$time, worker_absences$econ_sector),
    sum
  )
  workforce <- x$country_parameters$workers

  # calculate daily GVA loss due to illness-related absencees;
  # scale GVA loss by openness when closures are active
  # assuming no working from home
  gva_loss <- worker_absences %*% diag(gva / workforce)

  # NOTE: education costs of absences are ONLY related to the absence of
  # educational workers, not students - may need to be updated
  education_cost_absences <- gva_loss[i_EDUCATION_SECTOR]
  economic_cost_absences <- sum(gva_loss[-i_EDUCATION_SECTOR])

  # calculate total deaths and multiply by VSL
  total_deaths <- model_data[
    model_data$compartment == "dead" & model_data$time == max(model_data$time),
  ]

  # set factor levels to keep order
  total_deaths$age_group <- factor(
    total_deaths$age_group,
    levels = unique(total_deaths$age_group)
  )
  total_deaths <- tapply(total_deaths$value, total_deaths$age_group, sum)
  life_years_lost <- x$country_parameters$life_expectancy * total_deaths

  # NOTE: in million $s
  life_value_lost <- x$country_parameters$vsl * total_deaths / 1e6

  closure_duration <- x$response_data$closure_info$closure_duration

  # handle daedalus2
  if (is.na(closure_duration)) {
    economic_cost_closures <- 0
    education_cost_closures <- 0
    sector_cost_closures <- rep(0, length(x$country_parameters$workers))
  } else {
    closure_start <- x$response_data$closure_info$closure_time_start
    closure_end <- x$response_data$closure_info$closure_time_end

    # cost of closures, per sector and total
    sector_cost_closures <- gva * (1 - openness) * closure_duration
    economic_cost_closures <- sum(sector_cost_closures)

    education_cost_closures <- sum(
      vsd *
        n_students *
        (1 - openness[i_EDUCATION_SECTOR]) *
        (1 - edu_effectiveness_remote) *
        closure_duration
    )

    # multiply loss due to closures by loss due to absences
    gva_loss[seq(closure_start, closure_end), ] <- gva_loss[
      seq(closure_start, closure_end),
    ] %*%
      diag(openness)
  }

  gva_loss <- colSums(gva_loss)

  cost_list <- list(
    total_cost = NA,
    economic_costs = list(
      economic_cost_total = economic_cost_closures + economic_cost_absences,
      economic_cost_closures = economic_cost_closures,
      economic_cost_absences = economic_cost_absences,
      sector_cost_closures = sector_cost_closures,
      sector_cost_absences = gva_loss
    ),
    education_costs = list(
      education_cost_total = education_cost_closures + education_cost_absences,
      education_cost_closures = education_cost_closures,
      education_cost_absences = education_cost_absences
    ),
    life_value_lost = list(
      life_value_lost_total = sum(life_value_lost),
      life_value_lost_age = life_value_lost
    ),
    life_years_lost = list(
      life_years_lost_total = sum(life_years_lost),
      life_years_lost_age = life_years_lost
    )
  )

  # probably a neater way of doing this
  cost_list$total_cost <- economic_cost_closures +
    economic_cost_absences +
    education_cost_closures +
    education_cost_absences +
    sum(life_value_lost)

  # return summary if requested, defaults to no summary
  summarise_as <- rlang::arg_match(summarise_as)

  costs <- switch(
    summarise_as,
    none = cost_list,
    total = cost_list[["total_cost"]],
    domain = {
      cost_list[["total_cost"]] <- NULL
      vec_costs <- vapply(cost_list, `[[`, 1L, FUN.VALUE = numeric(1))
      names(vec_costs) <- c("economic", "education", "life_value", "life_years")

      vec_costs
    }
  )

  costs
}

#' Calculate the present value of lost earnings due to educational disruption
#'
#' @keywords internal
#' @return A single number (around 20) that gives a coefficient of lost
#' earnings.
# NOTE: this could be a model constant as it seems to be a fixed value. Unclear
# why this is not being treated as a constant.
# NOTE: simplifying assumption of single age group with a mean age of 12.5
get_value_lost_earnings <- function() {
  mean_age <- mean(c(5, 20))
  (1 - (1 + earnings_loss_discount)^(-(work_expected_years + 20 - mean_age))) /
    earnings_loss_discount -
    (1 - (1 + earnings_loss_discount)^(-(20 - mean_age))) /
      earnings_loss_discount
}

#' Calculate the value of a school year
#'
#' @param gni The GNI per capita of a country. Must be a single value. See
#' [daedalus::country_gni] for values.
#'
#' @return A single value giving the value of a school year given the country
#' GNI per capita and an expected work period of 45 years.
#' @keywords internal
get_value_school_year <- function(gni) {
  # no checking on GNI for this internal function
  get_value_lost_earnings() * gni * edu_annual_ror
}
