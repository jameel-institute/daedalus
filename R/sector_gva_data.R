#' @title Country economic sector value-added data
#'
#' @description Country-wise data on gross value added (GVA) per economic sector
#' , taken from OECD reporting.
#'
#' @format ## `sector_gva_data`
#' A list with 69 elements, each corresponding to a recognised country or
#' territory; each element is a numeric vector of length 45 (for the number of
#' economic sectors) which gives the daily GVA per sector for that country.
#' @source Data collected from the OECD in 2018. See processing details in
#' `data-raw/sector_gva_data.R`
"sector_gva_data"
