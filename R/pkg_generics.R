#' Get parameters from DAEDALUS classes
#'
#' @description Generic and methods for S3 classes for safely getting class
#' parameters.
#' @name get_data
#' @rdname get_data
#'
#' @param x An S3 class object from the \pkg{daedalus} package of the
#' `<daedalus_country>` or `<infection>` class.
#' @param to_get A string giving the name of the element of `x` to return.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Other arguments to class
#' methods. Class methods do not currently support any other arguments.
#'
#' @return Returns a member of `x`, with the class preserved
#' (e.g. numeric vector for a country `"demography"`).
#'
#' For `<daedalus_output>` objects, returns the model timeseries data when no
#' element is specified.
#' @keywords generics
#' @export
#' @examples
#' # simple example of getting data
#' country_A <- daedalus_country("United Kingdom")
#' get_data(country_A, "demography")
#'
#' get_data(country_A, "contact_matrix")
#'
#' disease_x <- daedalus_infection("sars_cov_1", r0 = 1.9)
#' get_data(disease_x, "r0")
#'
#' # get model data
#' output <- daedalus("Canada", "influenza_1918")
#' head(
#'   get_data(output)
#' )
get_data <- function(x, ...) {
  UseMethod("get_data")
}

#' Set parameters in DAEDALUS classes
#'
#' @description Generic and methods for S3 classes for safely setting class
#' parameters. Only parameters considered safe to change -- mostly contact data
#' in the `<country>`, but all parameters in `<infection>` -- can be changed in
#' this way.
#' @name set_data
#' @rdname set_data
#'
#' @param x An S3 class object from the \pkg{daedalus} package of the
#' `<daedalus_country>` or `<infection>` class.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named optional arguments for
#' parameters to be changed, with their new values. The only values allowed for
#' `<daedalus_country>` objects are "contact_matrix", "contacts_workplace", and
#' "contacts_consumer_worker".
#'
#' @return An S3 object of the same class as input `x`.
#' @keywords generics
#' @export
#' @examples
#' # simple example of setting all contacts to 1
#' country_A <- daedalus_country("United Kingdom")
#' country_A
#'
#' country_A <- set_data(country_A, contact_matrix = matrix(1, 4, 4))
#' country_A
#'
#' disease_x <- daedalus_infection("sars_cov_1")
#' disease_x <- set_data(disease_x, r0 = 3.0)
#' disease_x
set_data <- function(x, ...) {
  UseMethod("set_data")
}

#' Prepare parameters for DAEDALUS
#'
#' @name prepare_parameters
#' @rdname prepare_parameters
#'
#' @description Generic for the [prepare_parameters] methods associated with
#' \pkg{daedalus} classes.
#'
#' @param x An S3 object with an appropriate method.
#' @param ... Not used; included for compatibility with methods.
#' @return A list of parameters suitable for the DAEDALUS model.
#' [prepare_parameters.daedalus_country()] returns the country parameters, while
#' [prepare_parameters.daedalus_infection()] returns infection parameters.
#'
#' @details
#'
#' ## Country parameters
#'
#' Country contact data is processed as follows:
#'
#' - `contact_matrix`: scaled by its leading eigenvalue, and with each column
#' `j` scaled by the `j`-th element of the country demography vector (i.e.,
#' scaling contacts from each age group by the size of that group).
#'
#' The returned parameter list consists of:
#'
#' - `demography`: the demography vector;
#'
#' - `contact_matrix`: the contact matrix;
#'
#' - `contacts_workplace`: the contacts in workplaces scaled by the number of
#' workers in each sector;
#'
#' - `contacts_consumer_worker`: contacts in workplaces distributed in
#' proportion to the demography distribution, and scaled by the largest singular
#' value (similar to eigenvalue for non-square matrices).
#'
#' ## Infection parameters
#'
#' Infection parameters are returned from `<daedalus_infection>` objects without
#' modification and only the name removed.
#'
#' @keywords internal
prepare_parameters <- function(x, ...) {
  UseMethod("prepare_parameters")
}
