#' @title Make infection parameters list for DAEDALUS
#'
#' @name make_model_parameters
#' @keywords internal
make_infection_parameters <- function(epidemic, infect_params_manual) {
  # NOTE: epidemic is tested in top level function
  # get infection params from `infection_data`
  infection_params <- daedalus::infection_data[[epidemic]]

  is_good_subs <- checkmate::test_subset(
    names(infect_params_manual),
    names(infection_params),
    empty.ok = TRUE
  )

  if (!is_good_subs) {
    cli::cli_abort(
      "`infect_params_manual` found parameters that are not allowed:
      {.str {setdiff(names(infect_params_manual), names(infection_params))}}."
    )
  }

  is_empty_list <- checkmate::test_list(infect_params_manual, len = 0)
  # check all user inputs
  if (!is_empty_list) {
    # NOTE: some infection parameters must be numbers (numeric length 1)
    # while others are numeric vectors of length `N_AGE_GROUPS`
    allowed_numerics_names <- c("eta", "gamma_H", "omega")
    is_each_number <- all(
      vapply(
        infect_params_manual[!names(infect_params_manual) %in%
          allowed_numerics_names],
        checkmate::test_number,
        logical(1L),
        # NOTE: rate parameter limits allowed may need to be tweaked
        lower = 0.0,
        finite = TRUE
      )
    )

    is_numeric_good <- all(
      vapply(
        infect_params_manual[allowed_numerics_names], checkmate::test_numeric,
        logical(1L),
        len = N_AGE_GROUPS, lower = 0.0,
        finite = TRUE, null.ok = TRUE
      )
    )

    if (!is_each_number) {
      cli::cli_abort(
        c(
          "Expected the following parameters passed in `infect_params_manual`
          to be a single positive and finite number:
          {.str {setdiff(
            names(infect_params_manual), allowed_numerics_names
          )}}",
          i = "Only {.str {allowed_numerics_names}} may be numeric vectors.
          See the Help page for {.help daedalus::daedalus} for parameters that
          can be numerics."
        )
      )
    }

    if (!is_numeric_good) {
      cli::cli_abort(
        c(
          "Expected the following parameters passed in `infect_params_manual`
          to be numeric vectors of length {N_AGE_GROUPS} with positive and
          finite values:
          {.str
            {intersect(names(infect_params_manual), allowed_numerics_names)}
          }",
          i = "See the Help page for {.help daedalus::daedalus} for parameters
          that can be numerics."
        )
      )
    }
  }

  infection_params[names(infect_params_manual)] <- infect_params_manual

  infection_params
}
