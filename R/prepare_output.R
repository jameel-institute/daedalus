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
  econ_labels <- rep(econ_labels, N_MODEL_COMPARTMENTS * N_VACCINE_DATA_GROUPS)

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
  data[,
    c("age_group", "econ_sector", "vaccine_group", "compartment") := list(
      age_labels,
      econ_labels,
      vaccine_labels,
      compartment_labels
    )
  ]
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

#' Prepare output from the C++ model
#'
#' @name prepare_output
#'
#' @param country A `<daedalus_country>` object from which to get data on the
#' number of demographic, economic, and vaccine groups.
#'
#' @keywords internal
prepare_output_cpp <- function(output, country) {
  # internal function: no input checking
  timesteps <- seq_len(ncol(output[[1]])) - 1
  n_times <- length(timesteps)

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
  # NOTE: subsetting will be removed with replacement of daedalus w/ daedalus2
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
      age_group_labels_rep,
      econ_group_labels_rep,
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
  new_vaccinations$age_group <- rep(age_group_labels, each = n_times)
  new_vaccinations$econ_sector <- rep(econ_group_labels, each = n_times)
  new_vaccinations$variable <- NULL

  # combine and return data converted to data.frame
  data <- data.table::rbindlist(list(data, new_vaccinations), use.names = TRUE)

  data.table::setDF(data)
}
