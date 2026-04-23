#' Get parameters from DAEDALUS classes
#'
#' @description Generic and methods for S3 classes for safely getting class
#' parameters.
#'
#' @name get_data
#' @rdname get_data
#'
#' @param x An S3 class object from the \pkg{daedalus} package of the
#' `<daedalus_country>`, `<daedalus_infection>`, `<daedalus_npi>`,
#' `<daedalus_output>`, and `<daedalus_vaccination>` class.
#'
#' @param to_get A string giving the name of the element of `x` to return. For
#' `<daedalus_output>` the additional internal data for \eqn{R_\text{eff}} can
#' be accessed by passing `"rt_data"`.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Other arguments to class
#' methods. Class methods do not currently support any other arguments.
#'
#' @return Returns a member of `x`, with the class preserved
#' (e.g. numeric vector for a country `"demography"`).
#'
#' For `<daedalus_output>` objects, returns the model timeseries data when no
#' element is specified.
#'
#' @keywords generics
#'
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
#'
#' @export
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

#' Get epidemic losses from a DAEDALUS model run
#'
#' @description
#' Calculate the costs of an epidemic and any mitigation measures, either from a
#' Daedalus model run, or from a timeseries of epidemic data and relevant
#' demographic and economic parameters.
#'
#'
#' @name daedalus_costs
#' @rdname daedalus_costs
#'
#'
#' @param x A `<daedalus_output>` object from a call to [daedalus()], or a
#' `<data.frame>` of an epidemic timeseries.
#'
#' If `x` is a `<data.frame>`, it must have the columns `"time"`,
#' `"compartment"`, `"age_group"`, `"econ_sector"` and `"value"`, giving the
#' model time, the epidemiological compartment, and the number of individuals
#' of each age group and economic sector in each compartment at each time.
#' See **Details** for more on the expectations around how this dataset is
#' organised.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Other arguments to class
#' methods.
#'
#' @return A list of different cost values, including the total cost. See
#' **Details** for more information.
#'
#' @details
#'
#' ## Expectations for non-Daedalus model data
#'
#' When a `<data.frame> x` is passed, it must have at least one row, at least
#' some non-missing values, and:
#'
#' 1. Columns `"time"`, `"age_group"`, `"compartment"`, `"econ_sector"`, and
#' `"value"`, giving the model time (numeric), age group identifier (character),
#' epidemiological compartment identifier (character), economic sector
#' identifier (character), and the value of the number of individuals in each
#' age, epi-compartment, and economic group at each time (numeric). Numeric
#' values must be non-negative \eqn{> 0}, non-missing, and finite.
#'
#' 2. Compartments in `comp_infected`, `comp_non_working`, and `comp_dead` must
#' be found in `x$compartment`.
#'
#' 3. All compartments of `comp_infected` and `comp_dead` must be in
#' `comp_non_working`.
#'
#' ## Output
#'
#' The total cost in million dollars is returned as `total_cost`. This is
#' comprised of the following costs.
#'
#' ### Economic costs
#'
#' A three element list of `economic_cost_total`, the total costs from pandemic
#' impacts on economic sectors, including both costs of lost gross value added
#' (GVA) due to pandemic-control restrictions or closures
#' (`economic_cost_closures`), and pandemic-related absences due to illness and
#' death (`economic_cost_absences`).
#'
#' ### Educational costs
#'
#' A three element list of `education_cost_total`, the total costs from pandemic
#' impacts on education due to pandemic-control restrictions or closures
#' (`education_cost_closures`), and pandemic-related absences due to illness and
#' death (`education_cost_absences`).
#'
#' ### Life-value lost
#'
#' A four-element vector (for the number of age groups) giving the value of
#' life-years lost per age group. This is calculated as the life-expectancy of
#' each age group times the value of a statistical life, with all years assumed
#' to have the same value.
#'
#' ### Life-years lost
#'
#' A four-element vector (for the number of age groups) giving the value of
#' life-years lost per age group. This is calculated as the life-expectancy of
#' each age group times the number of deaths in that age group. No quality
#' adjustment is applied.
#'
#' @export
get_costs <- function(x, ...) {
  UseMethod("get_costs")
}
