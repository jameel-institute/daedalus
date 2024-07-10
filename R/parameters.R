#' @title Default parameters for DAEDALUS
#' @param ... User-specified values to replace default parameter values.
#' @export
#' @examples
#' default_parameters(beta = 1.5 / 7.0)
default_parameters <- function(...) {
  user_params <- list(...)

  # check user input if any
  checkmate::assert_list(
    user_params, "numeric",
    null.ok = TRUE, all.missing = TRUE
  )

  # NOTE: these are arbitrary values roughly equivalent to
  # pandemic influenza, with R0 = 1.3, infectious period = 7 days
  # pre-infectious period = 3 days
  params <- list(
    beta = 1.3 / 7.0,
    sigma = 1.0 / 3.0,
    p_sigma = 0.66,
    epsilon = 0.2,
    gamma = 1.0 / 7.0,
    eta = 1.0 / 100.0,
    omega = 1.0 / 100.0,
    contact_matrix = matrix(1.0, 4L, 4L)
  )

  # check for user values that cannot be used and remove
  user_params[!names(user_params) %in% names(params)] <- NULL

  # replace defaults with user values
  params <- utils::modifyList(params, user_params)

  # return parameters
  params
}
