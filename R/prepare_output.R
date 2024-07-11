#' @title Prepare DAEDALUS data
#' @description Convert DAEDALUS data into a long-format `<data.frame>`.
#' @param output Output from [daedalus()] of the class `<deSolve>`.
#' @return A `<data.frame>` in long or 'tidy' format with the columns
#' "time", "age_group", "compartment", and "value", for the age-group specific
#' value of the number of individuals in each epidemiological compartment in
#' each timestep.
#' @export
prepare_output <- function(output) {
  # expect S3 type `deSolve`
  checkmate::assert_class(output, c("deSolve", "matrix"))

  n_age_groups <- 4L
  age_groups <- c("0-4", "5-19", "20-65", "65+")
  compartments <- c(
    "susceptible", "exposed", "infect_symp", "infect_asymp",
    "hospitalised", "recovered", "dead"
  )

  # NOTE: keeping this dependency free for now, easier implementations using
  # `{data.table}` or `{tidyr}` are available
  #
  # NOTE: hardcode the transformation from wide-format matrix to long-format
  # data.frame; assumes four age groups and seven epi compartments
  times <- unique(output[, "time"])
  time <- rep(times, times = n_age_groups * length(compartments))

  age_groups <- rep(age_groups, each = length(times))
  age_groups <- rep(age_groups, times = length(compartments))

  compartments <- rep(compartments, each = n_age_groups * length(times))

  data <- data.frame(
    time = time,
    age_group = age_groups,
    compartment = compartments,
    value = c(output[, -1L])
  )

  # return data
  data
}
