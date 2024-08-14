#' @title Pandemic response strategy data for DAEDALUS
#'
#' @description Coefficients of openness of economic sectors under different
#' pandemic response strategies.
#' There are four strategies (including no response), with two levels of
#' implementation ("heavy" and "light").
#'
#' @format ## `closure_data`
#' A list with 4 elements, each corresponding to a pandemic response strategy,
#' and each with 2 elements ("heavy" or "light") giving the coefficients of
#' sector openness under different levels of implementation.
#' \describe{
#'   \item{none}{All economic sectors are fully open and there is no pandemic
#' response.}
#'   \item{elimination}{Openness coefficients for an elimination strategy.}
#'   \item{economic_closures}{Openness coefficients for a strategy of mostly
#' economic closures.}
#'   \item{school_closures}{Openness coefficients for a strategy of mostly
#' school closures.}
#' }
#' @source Multiple sources; to be updated shortly. See processing details in
#' `data-raw/closure_data.R
"closure_data"
