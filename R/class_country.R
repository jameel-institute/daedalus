#' Constructor for the `<daedalus_country>` class
#'
#' @description Internal constructor function to make new objects of class
#' `<daedalus_country>`. See [daedalus_country()] for the user-facing helper
#' function which calls this function internally.
#'
#' @param name A daedalus_country name as a string.
#' @param parameters A named list of parameters for the daedalus_country.
#'
#' @return An object of the `<daedalus_country>` class, which inherits from a
#' `<list>`.
#' @keywords internal
#' @noRd
new_daedalus_country <- function(name, parameters) {
  # all input checking at top level
  x <- c(list(name = name), parameters)
  class(x) <- "daedalus_country"

  x
}

#' Represent countries and territories for DAEDALUS
#'
#' @name class_country
#' @rdname class_country
#'
#' @description Helper functions to create and work with S3 class
#' `<daedalus_country>` objects for use with [daedalus()].
#' These objects store country parameters for reuse and have methods for easy
#' parameter access and editing, as well as processing raw country
#' characteristics for the DAEDALUS model.
#'
#' @param country A string giving the country or territory name, or ISO2 or
#' ISO3 code; must be from among [daedalus.data::country_codes_iso2c] or
#' [daedalus.data::country_codes_iso3c] or [daedalus.data::country_names].
#'
#' @param parameters An optional named list of country parameters that are
#' allowed to be modified. Currently, users may only pass their own contact
#' matrix, workplace contacts, and consumer-worker contact matrix. If these are
#' not passed, default values are accessed from stored package data.
#'
#' @param group_working_age An optional value for the age-group that is
#' considered to be the working-age group. Defaults to `3`, which is taken from
#' an internal constant.
#'
#' @export
#' @return
#'
#' - `daedalus_country()` returns an object of the S3 class `<daedalus_country>`
#'
#' - `is_daedalus_country()` returns a logical for whether an object is a
#' `<daedalus_country>`.
#'
#' - `print.daedalus_country()` invisibly returns the `<daedalus_country>`
#' object `x`. Called for printing side-effects.
#'
#' @examples
#' x <- daedalus_country("Canada")
#'
#' x
#'
#' daedalus_country(
#'   "United Kingdom",
#'   parameters = list(contact_matrix = matrix(1, 4, 4))
#' )
#'
#' # check whether `x` is a <country> object
#' is_daedalus_country(x)
#'
#' # assign class members
#' # using set_data()
#' set_data(x, contact_matrix = matrix(99, 4, 4))
#'
#' # using assignment operators
#' x$contact_matrix <- matrix(99, 4, 4)
#' x
daedalus_country <- function(
  country,
  parameters = list(
    contact_matrix = NULL,
    contacts_workplace = NULL,
    contacts_consumer_worker = NULL
  ),
  group_working_age = NULL
) {
  # quick return as is if class is provided
  if (is_daedalus_country(country)) {
    return(country)
  }

  # input checking
  name <- country_name_from_arg(country)
  # check list but allow missing and NULL
  checkmate::assert_list(parameters, c("numeric", "matrix", "NULL"))
  # NOTE: not allowed to change demography, worker distribution, or
  # contacts between economic sectors (modelled as zero)
  allowed_params <- c(
    "contact_matrix",
    "contacts_workplace",
    "contacts_consumer_worker"
  )
  has_good_names <- checkmate::test_subset(
    names(parameters),
    allowed_params,
    empty.ok = TRUE
  )
  if (!has_good_names) {
    cli::cli_abort(
      "Found unexpected values in `parameters`; the only allowed list
      elements are: {.str {allowed_params}}"
    )
  }

  # check user-passed parameters
  is_good_contact_matrix <- checkmate::test_matrix(
    parameters$contact_matrix,
    "numeric",
    any.missing = FALSE,
    ncols = N_AGE_GROUPS,
    nrows = N_AGE_GROUPS,
    null.ok = TRUE
  ) &&
    checkmate::test_numeric(
      parameters$contact_matrix,
      lower = 0,
      finite = TRUE,
      null.ok = TRUE
    )

  is_good_contacts_workplace <- checkmate::test_numeric(
    parameters$contacts_workplace,
    lower = 0,
    finite = TRUE,
    any.missing = FALSE,
    len = N_ECON_SECTORS,
    null.ok = TRUE
  )

  is_good_contacts_cons_worker <- checkmate::test_matrix(
    parameters$contacts_consumer_worker,
    "numeric",
    any.missing = FALSE,
    nrows = N_ECON_SECTORS,
    ncols = N_AGE_GROUPS,
    null.ok = TRUE
  ) &&
    checkmate::test_numeric(
      parameters$contacts_consumer_worker,
      lower = 0,
      finite = TRUE,
      null.ok = TRUE
    )

  if (!is_good_contact_matrix) {
    cli::cli_abort(c(
      "Expected `parameters$contact_matrix` to be a 4x4 numeric matrix with
         positive, finite values.",
      i = "The number of rows and colums is the number of age groups."
    ))
  }
  if (!is_good_contacts_workplace) {
    cli::cli_abort(c(
      "Expected `parameters$contacts_workplace` to be a 45-element numeric
        vector with positive, finite values.",
      i = "The number of elements is the number of economic sectors."
    ))
  }
  if (!is_good_contacts_cons_worker) {
    cli::cli_abort(c(
      "Expected `parameters$contacts_consumer_worker` to be a 45x4 numeric
        matrix with positive, finite values.",
      i = "The number of rows is the number of economic sectors and the number
        of columns is the number of age groups."
    ))
  }

  # substitute defaults with non-NULL elements of parameters
  params <- daedalus.data::country_data[[name]]
  # add one worker to each sector to avoid division by zero
  params$workers <- params$workers + 1

  # add value of statistical life (VSL)
  life_expectancy <- daedalus.data::life_expectancy[[name]]
  vsl <- daedalus.data::life_value[[name]]
  gni <- daedalus.data::country_gni[[name]]

  # calculate consumer-worker contacts
  contacts_consumer_worker <- matrix(
    daedalus.data::economic_contacts[["contacts_workplace"]],
    N_ECON_SECTORS,
    N_AGE_GROUPS
  ) *
    params[["demography"]] /
    sum(params[["demography"]])

  if (is.null(group_working_age)) {
    group_working_age <- i_WORKING_AGE
  } else {
    checkmate::assert_integerish(
      group_working_age,
      lower = 1,
      upper = N_AGE_GROUPS,
      any.missing = FALSE
    )
  }

  params <- c(
    params,
    list(
      contacts_workplace = daedalus.data::economic_contacts[[
        "contacts_workplace"
      ]],
      contacts_consumer_worker = contacts_consumer_worker,
      contacts_between_sectors = daedalus.data::economic_contacts[[
        "contacts_between_sectors"
      ]],
      vsl = vsl,
      gni = gni,
      life_expectancy = life_expectancy,
      group_working_age = group_working_age
    )
  )
  parameters <- Filter(parameters, f = function(x) !is.null(x))
  params[names(parameters)] <- parameters

  # add total number of groups
  params["n_strata"] <- length(params$demography) + length(params$workers)

  x <- new_daedalus_country(name, params)

  validate_daedalus_country(x)

  x
}

#' Validator for the `<daedalus_country>` class
#'
#' @param x An object to be validated as a `<daedalus_country>` object.
#'
#' @keywords internal
#' @noRd
validate_daedalus_country <- function(x) {
  if (!is_daedalus_country(x)) {
    cli::cli_abort(
      "Object should be of class {.cls {daedalus_country}}; check class
      assignment."
    )
  }

  # check class members
  # NOTE: gva = gross value added; vsl = value of statistical life
  expected_fields <- c(
    "name",
    "demography",
    "contact_matrix",
    "contacts_workplace",
    "contacts_consumer_worker",
    "contacts_between_sectors",
    "workers",
    "gva",
    "vsl",
    "hospital_capacity",
    "gni",
    "life_expectancy",
    "n_strata",
    "group_working_age"
  )
  has_fields <- checkmate::test_names(
    attributes(x)$names,
    permutation.of = expected_fields
  )
  if (!has_fields) {
    cli::cli_abort("{.cls country} does not have the correct attributes")
  }

  # check class members, using asserts as this doesn't need to be
  # super informative
  # fmt: skip
  stopifnot(
    "Country `name` must be a string from `daedalus.data::country_names`" =
      checkmate::test_string(x$name) &&
        checkmate::test_subset(
          x$name, daedalus.data::country_names
        ),
    "Country `demography` must be a integer-ish vector of 4 positive values" =
      checkmate::test_integerish(
        x$demography,
        lower = 0,
        any.missing = FALSE, len = N_AGE_GROUPS,
        upper = 1e10 # large upper limit to prevent infinite values
      ),
    "Country `workers` must be integer-ish vector of 45 positive values" =
      checkmate::test_integerish(
        x$workers,
        lower = 0,
        any.missing = FALSE, len = N_ECON_SECTORS,
        upper = 1e10 # large upper limit to prevent infinite values
      ),
    "Country `gva` must be a numeric vector of 45 positive values" =
      checkmate::test_numeric(
        x$gva,
        lower = 0,
        any.missing = FALSE, len = N_ECON_SECTORS, finite = TRUE
      ),
    "Country `contact_matrix` must be a 4x4 numeric matrix of positive values" =
      checkmate::test_matrix(
        x$contact_matrix, "numeric",
        any.missing = FALSE, ncols = N_AGE_GROUPS, nrows = N_AGE_GROUPS
      ) && checkmate::test_numeric(
        x$contact_matrix,
        lower = 0, finite = TRUE
      ),
    "Country `contacts_workplace` must be a 45-length positive numeric vector" =
      checkmate::test_numeric(
        x$contacts_workplace,
        lower = 0, finite = TRUE,
        any.missing = FALSE, len = N_ECON_SECTORS
      ),
    "Country `contacts_consumer_worker` must be a 45x4 numeric matrix" =
      checkmate::test_matrix(
        x$contacts_consumer_worker, "numeric",
        any.missing = FALSE, nrows = N_ECON_SECTORS, ncols = N_AGE_GROUPS
      ) && checkmate::test_numeric(
        x$contacts_consumer_worker,
        lower = 0, finite = TRUE
      ),
    "Country `contacts_between_sectors` must be a 45x45 numeric matrix" =
      checkmate::test_matrix(
        x$contacts_between_sectors, "numeric",
        any.missing = FALSE, nrows = N_ECON_SECTORS, ncols = N_ECON_SECTORS
      ) && checkmate::test_numeric(
        x$contacts_between_sectors,
        lower = 0, finite = TRUE
      ),
    "Country `contacts_between_sectors` must have zero diagonal" =
      checkmate::test_matrix(
        x$contacts_between_sectors, "numeric",
        any.missing = FALSE, nrows = N_ECON_SECTORS, ncols = N_ECON_SECTORS
      ) && checkmate::test_subset(
        x$contacts_between_sectors, 0
      ),
    "Country `vsl` must be a 4-element vector of positive values" =
      checkmate::test_numeric(
        x$vsl,
        len = N_AGE_GROUPS, any.missing = FALSE,
        lower = 0, upper = 1e7 # reasonable upper limit
      ),
    "Country `hospital_capacity` must be a single positive number" =
      checkmate::test_count(
        x$hospital_capacity,
        positive = TRUE
      ),
    "Country `gni` must be a single positive number" =
      checkmate::test_count(
        x$gni,
        positive = TRUE
      ),
    "Country `life_expectancy` must be a 4-element vector of positive values" =
      checkmate::test_numeric(
        x$life_expectancy,
        len = N_AGE_GROUPS, any.missing = FALSE,
        lower = 0, upper = 100 # reasonable upper limit
      ),
    "Country index for working-age group must be a single positive number" =
      checkmate::test_integerish(
        x$group_working_age,
        lower = 1, upper = N_AGE_GROUPS,
        any.missing = FALSE
      )
  )

  invisible(x)
}

#' Check if an object is a `<daedalus_country>`
#' @name class_country
#'
#' @export
is_daedalus_country <- function(x) {
  inherits(x, "daedalus_country")
}

#' Print a `<daedalus_country>` object
#' @name class_country
#' @param x An object of the `<daedalus_country>` class.
#' @param ... Other parameters passed to [print()].
#' @export
print.daedalus_country <- function(x, ...) {
  validate_daedalus_country(x)
  format(x, ...)
}

#' Format a `<daedalus_country>` object
#'
#' @param x A `<daedalus_country>` object.
#' @param ... Other arguments passed to [format()].
#'
#' @return Invisibly returns the `<daedalus_country>` object `x`.
#' Called for printing side-effects.
#' @keywords internal
#' @noRd
format.daedalus_country <- function(x, ...) {
  chkDots(...)

  # NOTE: rough implementations, better scaling e.g. to millions could be added
  cli::cli_text("{.cls {class(x)}}")
  cli::cli_bullets(c(
    "*" = "Name: {cli::col_red(x$name)}",
    "*" = "Demography: {cli::cli_vec(x$demography)}",
    "*" = "Community contact matrix:"
  ))
  # No good way to print using {cli}
  print(get_data(x, "contact_matrix"))

  cli::cli_bullets(c(
    "*" = "GNI (PPP $): {cli::col_blue(x$gni)}",
    "*" = "Hospital capacity: {cli::col_green(x$hospital_capacity)}"
  ))
  invisible(x)
}

#' @name get_data
#' @export
get_data.daedalus_country <- function(x, to_get, ...) {
  chkDots(...)
  validate_daedalus_country(x)

  good_to_get <- checkmate::test_string(to_get) &&
    checkmate::test_subset(to_get, names(x))

  if (!good_to_get) {
    cli::cli_abort(c(
      "`to_get` must be a string naming an element of {.cls {class(x)}}",
      i = "Allowed values are {.str {names(x)}}"
    ))
  }

  x[[to_get]]
}

#' @name set_data
#' @export
set_data.daedalus_country <- function(x, ...) {
  to_set <- rlang::list2(...)
  checkmate::assert_list(to_set, "numeric", any.missing = FALSE)
  allowed_params <- c(
    "contact_matrix",
    "contacts_workplace",
    "contacts_consumer_worker"
  )

  is_good_subs <- checkmate::test_subset(names(to_set), allowed_params)
  if (!is_good_subs) {
    cli::cli_abort(c(
      "Found a disallowed parameter substitution in `set_data()`.",
      i = "Only the following country parameters can be set using
        `set_data()`: {.str {allowed_params}}. To set other parameters
        use standard assignment with `$<-` or `[[<-`."
    ))
  }

  x[names(to_set)] <- to_set

  validate_daedalus_country(x)

  x
}


#' Prepare country parameters for model
#'
#' @name prepare_parameters
#' @keywords internal
prepare_parameters.daedalus_country <- function(x, ...) {
  chkDots(...)
  validate_daedalus_country(x)

  cm <- make_conmat_large(x)
  cm_work <- make_work_contacts(x)
  cm_cons_work <- make_consumer_contacts(x)
  hospital_capacity <- get_data(x, "hospital_capacity")

  n_age_groups <- length(get_data(x, "demography"))
  n_econ_groups <- length(get_data(x, "workers"))

  list(
    cm = cm,
    cm_cons_work = cm_cons_work,
    cm_work = cm_work,
    n_age_groups = n_age_groups,
    n_econ_groups = n_econ_groups,
    popsize = sum(get_data(x, "demography")),
    hospital_capacity = hospital_capacity
  )
}

#' Validate country input
#'
#' @param x An object to be validated as suitable as input for the
#' `country` argument of [daedalus()].
#'
#' @keywords internal
#'
#' @return A `<daedalus_country>` object.
validate_country_input <- function(x) {
  # NOTE: names are case sensitive
  if (is_daedalus_country(x)) {
    x
  } else {
    daedalus_country(x)
  }
}
