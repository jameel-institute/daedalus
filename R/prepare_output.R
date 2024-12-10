#' @title Prepare DAEDALUS data
#'
#' @name prepare_output
#' @rdname prepare_output
#'
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


#' Constants for `daedalus_rtm()`
#'
#' @name epi_constants
#' @keywords epi_constant
#' @return A vector of compartment names used in the C++ model.
COMPARTMENTS_cpp <- c(
  "susceptible", "exposed", "infect_symp", "infect_asymp",
  "hospitalised", "recovered", "dead", "vaccinated", "new_infections",
  "new_hosp"
)

#' Prepare output from the C++ model
#'
#' @name prepare_output
#'
#' @param output The output of `.model_daedalus_cpp()`.
prepare_output_cpp <- function(output) {
  times <- output[["time"]]
  n_times <- length(times)

  data <- do.call(rbind, output[["x"]])

  age_group_labels <- c(
    AGE_GROUPS, rep(AGE_GROUPS[i_WORKING_AGE], N_ECON_SECTORS)
  )
  econ_group_labels <- sprintf("sector_%02i", seq.int(N_ECON_SECTORS))
  non_working_label <- rep("sector_00", N_AGE_GROUPS)

  econ_group_labels <- c(non_working_label, econ_group_labels)

  data <- data.table::as.data.table(data)
  colnames(data) <- COMPARTMENTS_cpp

  # assign names and times
  data$age_group <- rep(age_group_labels, n_times)
  data$econ_sector <- rep(econ_group_labels, n_times)
  data$time <- rep(times, each = N_AGE_GROUPS + N_ECON_SECTORS)

  # make long format
  data <- data.table::melt(
    data,
    id.vars = c("time", "age_group", "econ_sector"),
    variable.name = "compartment"
  )

  data.table::setDF(data)
  data
}
