#' Represent a time-limited intervention
#'
#' @name class_npi
#'
#' @description
#' `daedalus_timed_npi()` is a helper function to create a `<daedalus_npi>` that
#' is only trigged by time, and is not responsive to state variables.
#' Primarily intended for use in real time modelling.
#'
#' @export
#'
#' @examples
#'
#' # time-limited NPI with multiple phases
#' daedalus_timed_npi(
#'   start_time = c(10, 20, 30),
#'   end_time = c(15, 25, 40),
#'   openness = list(
#'     rep(1, 45),
#'     rep(0.5, 45),
#'     rep(0.2, 45)
#'   ),
#'   country = "GBR"
#' )
#'
daedalus_timed_npi <- function(
  start_time,
  end_time,
  openness,
  country
) {
  country <- validate_country_input(country)

  checkmate::assert_numeric(start_time, 0, finite = TRUE)
  checkmate::assert_numeric(
    end_time,
    0,
    finite = TRUE,
    len = length(start_time)
  )
  Map(
    start_time,
    end_time,
    seq_along(start_time),
    f = function(x, y, i) {
      if (y < x) {
        cli::cli_abort(
          "<daedalus_timed_npi>: `end_time` at index {i} is less than \
          `start_time`; must be greater!"
        )
      }
    }
  )

  are_times_increasing <- all(diff(start_time) >= 0)
  if (!are_times_increasing) {
    cli::cli_abort(
      "daedalus_timed_npi: `start_time` should be a vector of ascending values \
      but it is not."
    )
  }
  are_times_increasing <- all(diff(end_time) >= 0)
  if (!are_times_increasing) {
    cli::cli_abort(
      "daedalus_timed_npi: `end_time` should be a vector of ascending values \
      but it is not."
    )
  }

  # non-overlapping intervals
  are_exclusive_intervals <-
    all(
      diff(
        findInterval(
          unlist(
            Map(
              seq,
              start_time,
              end_time
            )
          ),
          end_time
        )
      ) >=
        0
    )
  if (!are_exclusive_intervals) {
    cli::cli_abort(
      "daedalus_timed_npi: intervals specified by `start_time` and `end_time`\
      must be non-overlapping, but overlaps were found."
    )
  }

  checkmate::assert_list(
    openness,
    "numeric",
    FALSE,
    len = length(start_time)
  )

  # add default or initial regime to openness list
  initial_openness <- rep(1.0, N_ECON_SECTORS)
  openness <- c(
    list(initial_openness),
    openness
  )

  x <- new_daedalus_npi(
    list(openness = openness),
    identifier = "custom_timed",
    id_flag = get_flag_index("npi_flag", country),
    time_on = start_time,
    time_off = end_time,
    id_time_log = get_flag_index("npi_start_time", country)
  )

  validate_daedalus_npi(x)

  x
}
