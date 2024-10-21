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

  # prepare times and labels for model groups - age, econ sector, vaccine status
  n_times <- max(output[, "time"])

  # compartment labels
  compartment_labels <- rep(
    COMPARTMENTS,
    each = (N_AGE_GROUPS + N_ECON_SECTORS) * n_times
  )
  compartment_labels <- rep(compartment_labels, N_VACCINE_DATA_GROUPS)

  # economic sector labels including non-working
  econ_sectors <- sprintf("sector_%02i", seq.int(N_ECON_SECTORS))
  econ_labels <- rep(
    c(rep("sector_00", N_AGE_GROUPS), econ_sectors),
    each = n_times
  )
  econ_labels <- rep(
    econ_labels, N_MODEL_COMPARTMENTS * N_VACCINE_DATA_GROUPS
  )

  # age group labels
  age_labels <- rep(
    c(AGE_GROUPS, rep(AGE_GROUPS[i_WORKING_AGE], N_ECON_SECTORS)),
    each = n_times
  )
  age_labels <- rep(age_labels, N_MODEL_COMPARTMENTS * N_VACCINE_DATA_GROUPS)

  # vaccine group labels
  vaccine_labels <- rep(
    VACCINE_GROUPS,
    each = (N_AGE_GROUPS + N_ECON_SECTORS) * N_MODEL_COMPARTMENTS * n_times
  )

  # make data.frame, then data.table, pivot longer and assign labels
  data <- as.data.frame(output)
  data.table::setDT(data)
  data <- data.table::melt(data, id.vars = "time")
  data[, c(
    "age_group", "econ_sector", "vaccine_group",
    "compartment"
  ) := list(
    age_labels, econ_labels, vaccine_labels,
    compartment_labels
  )]
  data$variable <- NULL

  # reset to DF and return data
  data.table::setDF(data)

  # new vaccinations only in susceptible and recovered epi compartments
  data <- data[
    !(data$vaccine_group == "new_vaccinations" &
      !data$compartment %in% c("susceptible", "recovered")),
  ]
  data
}
