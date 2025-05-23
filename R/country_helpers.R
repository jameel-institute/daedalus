#' Get country name from arg
#' @description Function to get a supported country name given an arg which
#' may be a country name or may be an ISO2 or ISO3 code. In all cases,
#' the function checks if the country identifier is supported.
#'
#' @param country A string which could be a country name, or an ISO2 or ISO3
#' code.
#'
#' @return A supported country name
#' @keywords internal
country_name_from_arg <- function(country) {
  lookup_country_name_from_code <- function(code, code_list) {
    # check code is known, and lookup country name from idx
    code <- rlang::arg_match(code, code_list)
    idx <- match(code, code_list)
    daedalus.data::country_names[[idx]]
  }

  if (any(grep("^[A-Z]{2}$", country))) {
    # regex check for ISO2
    country <- lookup_country_name_from_code(
      country,
      daedalus.data::country_codes_iso2c
    )
  } else if (any(grep("^[A-Z]{3}$", country))) {
    # regex check for ISO3
    country <- lookup_country_name_from_code(
      country,
      daedalus.data::country_codes_iso3c
    )
  } else {
    # check country name is known
    country <- rlang::arg_match(country, daedalus.data::country_names)
  }
  country
}
