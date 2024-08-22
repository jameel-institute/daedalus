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
  # no input checking on internal function
  # NOTE: keeping this dependency free for now, easier implementations using
  # `{data.table}` or `{tidyr}` are available
  times <- output[, "time"]
  time <- rep(times, N_AGE_GROUPS * N_EPI_COMPARTMENTS * N_ECON_STRATA)

  age_groups <- rep(AGE_GROUPS, each = length(times))
  age_groups <- rep(age_groups, N_EPI_COMPARTMENTS)
  age_groups <- rep(age_groups, N_ECON_STRATA)

  compartments <- rep(COMPARTMENTS, each = N_AGE_GROUPS * length(times))
  compartments <- rep(compartments, N_ECON_STRATA)

  # NOTE: sector 0 indicates not-in-work; consider alternatives
  # padding sectors with zeros
  econ_sector <- rep(
    sprintf("sector_%02i", seq.int(0L, N_ECON_SECTORS)),
    each = N_AGE_GROUPS * N_EPI_COMPARTMENTS * max(time)
  )

  # data values
  values <- c(output[, setdiff(colnames(output), c("time", "switch"))])

  data <- data.frame(
    time = time,
    age_group = age_groups,
    compartment = compartments,
    econ_sector = econ_sector,
    value = values
  )

  # remove economic sectors for non-working age groups
  data <- data[!(data$age_group != "20-65" & data$econ_sector != "sector_00"), ]

  # return data
  data
}
