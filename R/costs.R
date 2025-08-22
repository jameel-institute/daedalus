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
  openness <- last(x$response_data$openness)

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
  sector_cost_absences <- worker_absences %*% diag(gva / workforce)

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

  closure_duration <- sum(x$response_data$closure_info$closure_durations)

  # handle daedalus
  if (all(is.na(closure_duration))) {
    economic_cost_closures <- 0
    education_cost_closures <- 0
    sector_cost_closures <- rep(0, length(x$country_parameters$workers))
  } else {
    closure_periods <- x$response_data$closure_info$closure_periods

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
    # to get true loss due to absences
    sector_cost_absences[closure_periods, ] <-
      sector_cost_absences[
        closure_periods,
      ] %*%
      diag(openness)
  }

  # NOTE: sum costs of illness related absence after accounting for
  # sector openness
  sector_cost_absences <- colSums(sector_cost_absences)

  # NOTE: education costs of absences are ONLY related to the absence of
  # educational workers, not students - may need to be updated
  education_cost_absences <- sector_cost_absences[i_EDUCATION_SECTOR]
  economic_cost_absences <- sum(sector_cost_absences[-i_EDUCATION_SECTOR])

  cost_list <- list(
    total_cost = NA,
    economic_costs = list(
      economic_cost_total = economic_cost_closures + economic_cost_absences,
      economic_cost_closures = economic_cost_closures,
      economic_cost_absences = economic_cost_absences,
      sector_cost_closures = sector_cost_closures,
      sector_cost_absences = sector_cost_absences
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
#' [daedalus.data::country_gni] for values.
#'
#' @return A single value giving the value of a school year given the country
#' GNI per capita and an expected work period of 45 years.
#' @keywords internal
get_value_school_year <- function(gni) {
  # no checking on GNI for this internal function
  get_value_lost_earnings() * gni * edu_annual_ror
}

#' Get pandemic fiscal costs from a model run
#'
#' @description
#' A helper function that post-processes a `<daedalus_output>` object to
#' calculate the costs to a national government (fiscal costs) of responding to
#' a pandemic. Includes costs of economic support, vaccinations given, and NPIs
#' administered or implemented.
#'
#' @param x A `<daedalus_output>` object.
#'
#' @param support_level The proportion of pandemic-related economic losses that
#' a government compensates, as a proportion.
#'
#' @param price_vax The per-dose price of vaccination.
#'
#' @param price_npi The per-day and per-person price of implementing any
#' pandemic response. May include costs such as testing or masks.
#'
#' @param uptake_npi The extent to which NPIs are taken up by the population;
#' essentially a number that modifies (reduces) the total cost of implementing
#' an NPI.
#'
#' @param interest_rate The annual interest rate on government borrowing for
#' pandemic response.
#'
#' @param tax_rate The annual mean tax rate on economic output; used to
#' calculate government revenue.
#'
#' @param spending_rate The annual mean rate of public spending as a percentage
#' of GDP.
#'
#' @param starting_debt The value of national starting debt. Currently assumed
#' to be zero while country-specific data are collected.
#'
#' @return A two-element list giving:
#'
#' - A list of `fiscal_costs` with elements giving the fiscal costs
#' \eqn{\text{TCG_t}} and a breakdown of these costs, as well as interest;
#'
#' - A list of `public_debt` of the public debt \eqn{b_t}, which is
#' the net of baseline public spending, pandemic response costs, and
#' pandemic-impacted revenue.
#'
#' @details
#' Default argument values are intended to be representative. This
#' function is intended to be called after [daedalus()] and parameters
#' required to compute fiscal costs may need to be bundled along with model
#' outputs.
#' Note that all rates (interest rate, spending rate, and tax rate) are given as
#' annual percentages, and are simply divided by 365 to give the daily
#' rate.
#'
#' ## Public spending
#'
#' Spending is calculated as:
#' \eqn{G_t = \sigma(\text{GVA}^* - \text{GVA}_t) + Cv_t + Cp_t}
#'
#' where \eqn{Cv_t} is the time-specific cost of vaccination, and is calculated
#' as the cost of new vaccinations in each timestep: \eqn{P_v \times \delta V_t}
#' .
#'
#' \eqn{Cp_t} is the time-specific cost of implementing pandemic response,
#' and is calculated as \eqn{\psi (\bar N - D_t) P_p}, where
#' \eqn{\bar N - D_t} is the remaining number of individuals in the population,
#' and \eqn{\psi} is the proportion taking up any protection offered by the
#' response.
#'
#' The total cost to the public of the pandemic over each day of the pandemic
#' is assumed to be made up of borrowing, and is then
#' \eqn{\text{TCG}_t = G_t + (1 + R_t^T) \text{TCG}_{t - 1}}, where
#' \eqn{R_t^T} is the daily rate of interest to be paid on the borrowed amount.
#' This rate is currently modelled as being constant over time.
#'
#' The total public debt at the end of the pandemic \eqn{b_t} is then the sum of
#' total public spending on the pandemic \eqn{\text{TCG}_t},
#' existing day-to-day public spending \eqn{\bar G} which is assumed to be a
#' fraction of daily GDP \eqn{\nu \text{GDP}}, existing debt owed due to past
#' daily spending, interest to be paid on the debt, less the revenues collected
#' \eqn{\mu \text{GVA}_{t-1}}. The daily GVA is the pre-pandemic GVA scaled by
#' the available labour supply during the pandemic, taking into account labour
#' restrictions due to illness-related absences and deaths, and response-related
#' restrictions.
#'
#' GDP is calculated as the sum of sector-specific daily GVA, and existing debt
#' is currently assumed to be zero and is not included in the equation.
#'
#' \eqn{b_t = \bar G + \text{TCG}_{t-1} + (1 + R_{t-1}^T) b_{t-1} -
#'   \mu \text{GVA}_{t-1}
#' }
#'
#' @export
#'
#' @examples
#' # get fiscal costs for UK with SARS-CoV-2
#' # and both closures and vaccinations
#' o <- daedalus(
#'   "GBR", "sars_cov_2_pre_alpha",
#'   "economic_closures", "high",
#'   time_end = 100
#' )
#' fc <- get_fiscal_costs(o)
#'
#' # also works when no closures are applied
#' o <- daedalus(
#'   "CAN", "influenza_2009",
#'   time_end = 30
#' )
#' get_fiscal_costs(o)
get_fiscal_costs <- function(
  x,
  support_level = 0.2,
  price_vax = 1.0,
  price_npi = 1.0,
  uptake_npi = 1.0,
  interest_rate = 4.0,
  tax_rate = 35.0,
  spending_rate = 45.0,
  starting_debt = 0.0
) {
  # Needs better error messages
  checkmate::assert_class(x, "daedalus_output")
  checkmate::assert_number(
    support_level,
    lower = 0,
    upper = 1
  )
  checkmate::assert_number(
    price_vax,
    lower = 0,
    finite = TRUE
  )
  checkmate::assert_number(
    price_npi,
    lower = 0,
    finite = TRUE
  )
  checkmate::assert_number(
    uptake_npi,
    lower = 0,
    upper = 1
  )
  checkmate::assert_number(
    interest_rate,
    lower = 0,
    upper = 100
  )
  checkmate::assert_number(
    tax_rate,
    lower = 0,
    upper = 100
  )
  checkmate::assert_number(
    spending_rate,
    lower = 0,
    upper = 100
  )
  checkmate::assert_number(
    starting_debt,
    finite = TRUE
  )

  gva <- x$country_parameters$gva
  openness <- last(x$response_data$openness)

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

  # calculate labour available after absences and closures
  # during closures, available labour is the lesser of healthy workers and
  # mandated capacity
  avail_labour <- 1.0 - worker_absences %*% diag(1.0 / workforce)

  # calculate closure duration if any
  closure_duration <- sum(x$response_data$closure_info$closure_durations)

  if (is.na(closure_duration) || closure_duration == 0) {
    npi_support <- 0.0
  } else {
    closure_periods <- x$response_data$closure_info$closure_periods

    # cost of getting NPIs to work: price_npi * number of alive individuals *
    # some uptake param
    npi_support <- sum(x$country_parameters$demography) -
      get_incidence(x, "deaths")$value * uptake_npi * price_npi
    npi_support[-closure_periods] <- 0.0
    npi_support <- npi_support / 1e6

    avail_labour[closure_periods, ] <- t(apply(
      avail_labour[closure_periods, ],
      1L,
      pmin,
      openness
    ))
  }

  # cost of support for GVA loss per sector per day
  gva_achieved <- weighted_rowsums(avail_labour, gva)
  gva_support <- weighted_rowsums(1.0 - avail_labour, support_level * gva)

  # cost of vaccination assumed to be instantaneous
  # NOTE: this is only over the model horizon!
  vax_support <- get_new_vaccinations(x)$new_vaccinations * price_vax / 1e6

  total_support <- gva_support + vax_support + npi_support
  names(total_support) <- NULL
  names(gva_support) <- NULL

  # get total fiscal cost with interest rate
  interest_rate <- interest_rate / 365.0
  fiscal_cost <- numeric(x$total_time + 1L)
  for (i in seq_len(x$total_time)) {
    fiscal_cost[i + 1] <- interest_accumulation(
      fiscal_cost[i],
      total_support[i],
      interest_rate
    )
  }

  # get total public debt, net of spending and revenue
  daily_spending <- sum(gva) * spending_rate / 365.0 # mean daily spend
  tax_rate <- tax_rate / 365.0

  public_debt <- numeric(x$total_time + 1L)
  public_debt[1L] <- starting_debt

  # iterate over 1:150, but public_debt has length 151 as t=0 included
  for (i in seq_len(x$total_time)) {
    public_debt[i + 1] <- daily_spending +
      interest_accumulation(public_debt[i], total_support[i], interest_rate) -
      (tax_rate * gva_achieved[i])
  }

  # return cost timeseries
  list(
    fiscal_costs = list(
      fiscal_cost = fiscal_cost,
      gva_support = gva_support,
      vax_support = vax_support,
      npi_support = npi_support,
      interest_value = sum(fiscal_cost - total_support)
    ),
    public_debt = list(
      public_debt = public_debt,
      added_public_debt = last(public_debt) - first(public_debt)
    )
  )
}
