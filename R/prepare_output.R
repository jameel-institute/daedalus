#' @title Prepare DAEDALUS data
#' @description Convert DAEDALUS data into a long-format `<data.frame>`.
#' @param output Output from [daedalus()] of the class `<deSolve>`.
#' @return A `<data.frame>` in long or 'tidy' format with the columns
#' "time", "age_group", "compartment", "econ_sector", and "value", for the
#' age-group specific value of the number of individuals in each economic sector
#' and epidemiological compartment in each timestep.
#'
#' @details
#'
#' There are 45 economic sectors, given by `N_ECON_SECTORS`, with the first
#' 'sector' representing non-working individuals. All age groups that are
#' considered non-working are in this first sector (children and retirees),
#' while working-age individuals may be distributed flexibly into the various
#' economic sectors (including non-working).
#' @keywords internal
prepare_output <- function(output) {
  # NOTE: no checks on this internal function

  # convert output to 5D array with the following mapping to dimensions
  # mapping: time, age group, epi compartment, econ stratum, vaccine stratum
  n_times <- max(output[, "time"])
  data <- array(
    output[, setdiff(colnames(output), "time")],
    c(
      n_times, N_AGE_GROUPS, N_MODEL_COMPARTMENTS,
      N_ECON_STRATA, N_VACCINE_STRATA
    )
  )
  data <- array2DF(data)

  # time labels
  data$Var1 <- rep(
    seq(n_times),
    N_AGE_GROUPS * N_MODEL_COMPARTMENTS * N_ECON_STRATA * N_VACCINE_STRATA
  )

  # age group labels
  age_groups <- rep(AGE_GROUPS, each = n_times)
  age_groups <- rep(
    age_groups,
    N_MODEL_COMPARTMENTS * N_ECON_STRATA * N_VACCINE_STRATA
  )
  data$Var2 <- age_groups

  # model compartment labels
  compartments <- rep(COMPARTMENTS, each = N_AGE_GROUPS * n_times)
  compartments <- rep(compartments, N_ECON_STRATA * N_VACCINE_STRATA)
  data$Var3 <- compartments

  # economic stratum labels
  # NOTE: sector 0 indicates not-in-work; consider alternatives
  # padding sectors with zeros
  econ_sector <- rep(
    sprintf("sector_%02i", seq.int(0L, N_ECON_SECTORS)),
    each = N_AGE_GROUPS * N_MODEL_COMPARTMENTS * n_times
  )
  econ_sector <- rep(
    econ_sector, N_VACCINE_STRATA
  )
  data$Var4 <- econ_sector

  # vaccine group labels
  data$Var5 <- rep(
    VACCINE_GROUPS,
    each = n_times * N_MODEL_COMPARTMENTS * N_AGE_GROUPS * N_ECON_STRATA
  )

  # set column names
  colnames(data) <- c(
    "time", "age_group", "compartment",
    "econ_sector", "vaccine_group", "value"
  )

  # remove economic sectors for non-working age groups
  data <- data[!(data$age_group != "20-65" & data$econ_sector != "sector_00"), ]

  # return data
  data
}
