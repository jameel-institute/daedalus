#' @title Generate a default initial state for DAEDALUS
#' @description Function to prepare the model initial state.
#'
#' @param country A string for the country name.
#' @param p_infectious A single value between 0.0 and 1.0 for the proportion of
#' individuals infectious This is applied to individuals in all economic
#' sectors.
#' @param p_asymptomatic A single value between 0.0 and 1.0 for the proportion
#' of infections that are asymptomatic.
#'
#' @return AN array with as many dimensions as `N_ECON_STRATA` (currently, 46)
#' with each layer giving the proportion of individuals of each group in each
#' epidemiological compartment.
#' @keywords internal
make_initial_state <- function(country,
                               p_infectious = 1e-6, p_asymptomatic = 0.0) {
  # NOTE: no checks on country as this is tested in top-level fn `daedalus()`
  # check other inputs
  is_good_p_infectious <- checkmate::test_number(
    p_infectious,
    lower = 0.0, upper = 1.0
  )
  if (!is_good_p_infectious) {
    cli::cli_abort(
      "`p_infectious` must be a single number in the range [0.0, 1.0]."
    )
  }

  is_good_p_asymp <- checkmate::test_number(
    p_asymptomatic,
    lower = 0.0, upper = 1.0
  )
  if (!is_good_p_asymp) {
    cli::cli_abort(
      "`p_asymptomatic` must be a single number in the range [0.0, 1.0]."
    )
  }

  initial_state <- c(
    S = 1.0 - p_infectious, E = 0.0,
    Is = p_infectious * (1.0 - p_asymptomatic),
    Ia = p_infectious * p_asymptomatic,
    H = 0.0, R = 0.0, D = 0.0
  )

  # build for all age groups
  initial_state <- array(
    rep(initial_state, each = N_AGE_GROUPS),
    c(N_AGE_GROUPS, N_EPI_COMPARTMENTS, N_ECON_STRATA)
  )

  # get demography and sector workforce
  demography <- daedalus::country_data[[country]][["demography"]]
  sector_workforce <- daedalus::country_data[[country]][["workers"]]

  # calculate non-working working-age, and prepare initial state accounting
  # for distribution of working age into economic sectors
  inactive_workers <- demography[i_WORKING_AGE] - sum(sector_workforce)

  initial_state[-i_WORKING_AGE, , i_NOT_WORKING] <-
    initial_state[-i_WORKING_AGE, , i_NOT_WORKING] *
      demography[-i_WORKING_AGE]

  initial_state[i_WORKING_AGE, , i_NOT_WORKING] <-
    initial_state[i_WORKING_AGE, , i_NOT_WORKING] * inactive_workers

  initial_state[i_WORKING_AGE, , -i_NOT_WORKING] <-
    initial_state[i_WORKING_AGE, , -i_NOT_WORKING] * sector_workforce

  initial_state
}
