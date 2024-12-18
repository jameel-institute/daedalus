#' @title Life expectancies and values
#'
#' @name life_value
#' @rdname life_value
#'
#' @description
#' Data on country- and age-group-specific life expectancy, and
#' values in dollars (scaled by purchasing power parity) of a
#' statistical life lost in each of the four DAEDALUS age groups, accounting
#' for life expectancy.
#'
#' @format ## `life_value`
#' A list with 67 elements, each corresponding to a country or territory, and
#' giving the age-group-specific value of a statistical life lost. Each list
#' element is a numeric vector of four values giving the value per age group,
#' in the following order: 0-4, 5-19, 20-65, 65+.
#' @source Multiple sources; see processing details in `data-raw/life_value.R.
"life_value"

#' @name life_value
#' @format ## `country_gni`
#' A list with 67 elements, each corresponding to a country or territory, and
#' giving the GNI (gross national income) in international dollars.
#' @source Multiple sources; see processing details in `data-raw/life_value.R.
"country_gni"

#' @name life_value
#' @format ## `country_gni`
#' A list with 67 elements, each corresponding to a country or territory, and
#' giving the life expectancy in years.
#' @source Multiple sources; see processing details in `data-raw/life_value.R.
"life_expectancy"
