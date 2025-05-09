#' Prepare susceptibility matrix for a vaccine-country pair
#'
#' @description
#' Helper function to prepare a susceptibility matrix to be used internally to
#' modulate the number of infections in vaccinated groups.
#'
#' @inheritParams daedalus
#'
#' @returns A matrix with dimensions as follows:
#' - Rows: number of age and economic groups in `country`;
#' - Cols: number of vaccination strata in the DAEDALUS model, given as
#' `N_VAX_STRATA`.
#'
#' @keywords internal
make_susc_matrix <- function(vaccination, country) {
  # no input checks for this internal function
  efficacy <- get_data(vaccination, "efficacy")
  tau <- 1.0 - efficacy / 100.0

  n_strata <- get_data(country, "n_strata")

  # default assumption is full susceptibility
  susc_matrix <- matrix(1, n_strata, N_VACCINE_STRATA)
  susc_matrix[, i_VACCINATED_STRATUM] <- rep(tau, n_strata)

  susc_matrix
}
