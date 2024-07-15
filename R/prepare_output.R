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

  # NOTE: keeping this dependency free for now, easier implementations using
  # `{data.table}` or `{tidyr}` are available
  times <- output[, "time"]
  time <- rep(times, times = N_AGE_GROUPS * N_EPI_COMPARTMENTS)

  age_groups <- rep(AGE_GROUPS, each = length(times))
  age_groups <- rep(age_groups, times = N_EPI_COMPARTMENTS)

  compartments <- rep(COMPARTMENTS, each = N_AGE_GROUPS * length(times))

  data <- data.frame(
    time = time,
    age_group = age_groups,
    compartment = compartments,
    value = c(output[, setdiff(colnames(output), "time")])
  )

  # return data
  data
}
