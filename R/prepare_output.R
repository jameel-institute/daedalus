#' Reshape output data
#'
#' @description
#' An internal function to help [prepare_output()] process lists of data.
#'
#'
#' @param output A list-like object originating from [daedalus_internal()] and
#' partially processed within [prepare_output()].
#' @param new_vaccinations A list object of new vaccination data origination in
#' [daedalus_internal()].
#' @param timesteps A vector of timesteps, typically starting with 0.
#' @param labels A list of labels to be applied to the data.
#'
#' @keywords internal
#'
#' @return A `<data.frame>` suitable for passing to a `<daedalus_output>`
#' object.
out_list_to_df <- function(output, new_vaccinations, timesteps, labels) {
  # handle labels
  age_group_labels <- labels$age_group_labels
  age_group_labels_vax <- labels$age_group_labels_vax
  econ_group_labels <- labels$econ_group_labels
  econ_group_labels_vax <- labels$econ_group_labels_vax
  vaccine_labels <- labels$vaccine_labels
  compartment_labels <- labels$compartment_labels

  data_list <- lapply(output, function(x) {
    x <- t(x)
    x <- data.table::as.data.table(x)
    x[, "time" := timesteps]
    x <- data.table::melt(x, id.vars = "time")
    x
  })
  data <- data.table::rbindlist(data_list)
  data[,
    c("age_group", "econ_sector", "vaccine_group", "compartment") := list(
      age_group_labels,
      econ_group_labels,
      vaccine_labels,
      compartment_labels
    )
  ]
  data$variable <- NULL

  # handle new vaccinations data
  # NOTE: this could go into a dedicated function
  new_vaccinations <- t(new_vaccinations)
  new_vaccinations <- data.table::as.data.table(new_vaccinations)
  new_vaccinations[, "time" := timesteps]
  new_vaccinations <- data.table::melt(new_vaccinations, id.vars = "time")
  new_vaccinations$compartment <- "new_vax"
  new_vaccinations$vaccine_group <- "new_vaccinations" # compat with data fn
  new_vaccinations$age_group <- age_group_labels_vax
  new_vaccinations$econ_sector <- econ_group_labels_vax
  new_vaccinations$variable <- NULL

  # combine and return data converted to data.frame
  data <- data.table::rbindlist(
    list(data, new_vaccinations),
    use.names = TRUE
  )

  data.table::setDF(data)
}

#' @title Prepare DAEDALUS data
#'
#' @name prepare_output
#' @rdname prepare_output
#'
#' @description Convert DAEDALUS data into a long-format `<data.frame>`.
#'
#' @param output Output from [daedalus_internal()], expected to be a list.
#'
#' @param country A `<daedalus_country>` object from which to get data on the
#' number of demographic, economic, and vaccine groups.
#'
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
#' @keywords internal
prepare_output <- function(output, country) {
  # internal function: no input checking
  timesteps <- seq_len(tail(dim(output[[1]]), 1)) - 1
  n_times <- length(timesteps)

  # find groups if any
  n_groups <- 1
  if (length(dim(output[[1]])) == 3L) {
    n_groups <- dim(output[[1]])[[2]]
  }

  # remove flags and new vaccinations data
  NEW_VACCINATIONS_NAME <- "new_vax"
  new_vaccinations <- output[[NEW_VACCINATIONS_NAME]]
  output <- output[!names(output) %in% c(FLAG_NAMES, NEW_VACCINATIONS_NAME)]

  # get data from country
  demography <- get_data(country, "demography")
  n_age_groups <- length(demography)
  n_econ_sectors <- length(get_data(country, "workers"))
  age_group_names <- names(demography)
  i_working_age <- get_data(country, "group_working_age")

  # age group labels
  age_group_labels <- c(
    age_group_names,
    rep(age_group_names[i_working_age], n_econ_sectors)
  )
  age_group_labels_rep <- rep(
    age_group_labels,
    each = n_times
  )
  age_group_labels_rep <- rep(
    age_group_labels_rep,
    N_MODEL_COMPARTMENTS * N_VACCINE_STRATA
  )

  # econ sector labels
  econ_group_labels <- sprintf("sector_%02i", seq.int(n_econ_sectors))
  non_working_label <- rep("sector_00", n_age_groups)
  econ_group_labels <- c(non_working_label, econ_group_labels)
  econ_group_labels_rep <- rep(econ_group_labels, each = n_times)
  econ_group_labels_rep <- rep(
    econ_group_labels_rep,
    N_MODEL_COMPARTMENTS * N_VACCINE_STRATA
  )

  # vaccine group labels
  # NOTE: subsetting will be removed with replacement of daedalus w/ daedalus
  vaccine_labels <- rep(
    VACCINE_GROUPS[c(1, 2)],
    each = (n_age_groups + n_econ_sectors) * N_MODEL_COMPARTMENTS * n_times
  )

  # compartment labels
  compartment_labels <- rep(
    COMPARTMENTS,
    each = (n_age_groups + n_econ_sectors) * length(timesteps)
  )
  compartment_labels <- rep(compartment_labels, N_VACCINE_STRATA)

  labels <- list(
    age_group_labels = age_group_labels_rep,
    age_group_labels_vax = rep(age_group_labels, n_times),
    econ_group_labels = econ_group_labels_rep,
    econ_group_labels_vax = rep(econ_group_labels, n_times),
    vaccine_labels = vaccine_labels,
    compartment_labels = compartment_labels
  )

  if (n_groups > 1) {
    data_list <- lapply(output, asplit, 2)
    data_list <- data.table::transpose(data_list)

    new_vaccinations <- asplit(new_vaccinations, 2)

    data <- Map(
      f = function(x, y) {
        out_list_to_df(x, y, timesteps = timesteps, labels = labels)
      },
      data_list,
      new_vaccinations
    )
    data <- Map(
      f = function(x, i) {
        x$param_set <- i
        x
      },
      data,
      seq_along(data)
    )
  } else {
    data <- out_list_to_df(
      output,
      new_vaccinations,
      timesteps,
      labels
    )
  }

  # either a data.frame or a list of data.frames
  data
}
