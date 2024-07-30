#' @title Economic sector contacts data for DAEDALUS
#'
#' @description Data on the per-capita social contacts within and between each
#' economic sector of the DAEDALUS model.
#'
#' @format ## `economic_contacts`
#' A list with two elements:
#' \describe{
#'   \item{contacts_workplace}{A numeric vector with `N_ECON_SECTORS` (45)
#' elements, giving the per-capita contacts between workers within the same
#' economic sector.}
#'   \item{contacts_between_sectors}{A square matrix with values set to
#' \eqn{10^{-6}}, and with its diagonal set to zero, for the number of contacts
#' between workers across economic sectors.}
#' }
#' @source NOTE as yet unknown; see processing details in
#' `data-raw/economic_contacts.R
"economic_contacts"
