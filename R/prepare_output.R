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
#'
#' @export
prepare_output <- function(output) {
  # expect S3 type `deSolve`
  checkmate::assert_class(output, c("deSolve", "matrix"))

  # NOTE: keeping this dependency free for now, easier implementations using
  # `{data.table}` or `{tidyr}` are available
  times <- output[, "time"]
  time <- rep(times, times = N_AGE_GROUPS * N_EPI_COMPARTMENTS * N_ECON_SECTORS)

  age_groups <- rep(AGE_GROUPS, each = length(times))
  age_groups <- rep(age_groups, times = N_EPI_COMPARTMENTS)
  age_groups <- rep(age_groups, times = N_ECON_SECTORS)

  compartments <- rep(COMPARTMENTS, each = N_AGE_GROUPS * length(times))
  compartments <- rep(compartments, times = N_ECON_SECTORS)

  econ_sector <- rep(
    sprintf("sector_%i", seq.int(N_ECON_SECTORS)),
    each = N_AGE_GROUPS * N_EPI_COMPARTMENTS * max(time)
  )

  # data values
  values <- c(output[, setdiff(colnames(output), "time")])

  data <- data.frame(
    time = time,
    age_group = age_groups,
    compartment = compartments,
    econ_sector = econ_sector,
    value = values
  )

  # return data
  data
}
