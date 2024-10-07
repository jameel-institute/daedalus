#' @title DAEDALUS model constants
#' @description Frequently used values for the DAEDALUS model related
#' to the model population and structure. See also [epi_constants] for constants
#' specific to the epidemiological model.
#'
#' @name model_constants
#' @keywords model_constant
#' @rdname model_constants
#'
#' @return Values for the model constants.
#'
#' @details
#'
#' The DAEDALUS model requires the following values to be fixed.
#'
#' - Number of age groups: 4
#'
#' - Number of vaccination strata: 2
#'
#' - Age group bins: 0-4 years, 5-19 years, 20-65 years (working age), 65+ years
#'
#' - Index of the working-age age groups: 3; see `AGE_GROUPS`
#'
#' - Number of economic sectors: 45
#'
#' - Index of individuals not working as a layer in the 3D state tensor: 1
#'
#' - Number of economic strata: 46; note that this is always one more than the
#' number of economic sectors, with the additional stratum for those not in work
#' which includes all individuals not of working age, and a proportion of
#' working age individuals.
#'
#' - Array dimension of age groups: 1
#'
#' - Array dimension of epidemiological compartments: 2
#'
#' - Array dimension of economic sectors: 3
#'
#' - Array dimension of vaccination strata: 4
#'
#' - Indices and numbers of key groups.
#'
#' @keywords model_constant
N_AGE_GROUPS <- 4L

#' @name model_constants
#' @keywords model_constant
N_VACCINE_STRATA <- 2L

#' @name model_constants
#' @keywords model_constant
N_VACCINE_DATA_GROUPS <- 3L

#' @name model_constants
#' @keywords model_constant
AGE_GROUPS <- c("0-4", "5-19", "20-65", "65+")

#' @name model_constants
#' @keywords model_constant
i_WORKING_AGE <- 3L

#' @name model_constants
#' @keywords model_constant
N_ECON_SECTORS <- 45L

#' @name model_constants
#' @keywords model_constant
i_EDUCATION_SECTOR <- 41L

#' @name model_constants
#' @keywords model_constant
N_ECON_STRATA <- N_ECON_SECTORS + 1L

#' @name model_constants
#' @keywords model_constant
i_NOT_WORKING <- 1L

#' @name model_constants
#' @keywords model_constant
DIM_AGE_GROUPS <- 1L

#' @name model_constants
#' @keywords model_constant
DIM_EPI_COMPARTMENTS <- 2L

#' @name model_constants
#' @keywords model_constant
DIM_ECON_SECTORS <- 3L

#' @name model_constants
#' @keywords model_constant
DIM_VACCINE_STRATA <- 4L

#' @name model_constants
i_UNVACCINATED_STRATUM <- 1L

#' @name model_constants
i_VACCINATED_STRATUM <- 2L

#' @name model_constants
i_NEW_VAX_STRATUM <- 3L

#' @name model_constants
VACCINE_GROUPS <- c("unvaccinated", "vaccinated")

#' @name model_constants
#' @keywords model_constant
N_OUTPUT_COLS <- 6L

#' @title Epidemiological compartments and indices
#' @description Names and indices for the epidemiological compartments used in
#' DAEDALUS, for reuse in model code.
#'
#' @return
#'
#' `COMPARTMENTS` returns a character vector of the epidemiological
#' compartment names.
#'
#' All other constants return integer values.
#'
#' @details
#' DAEDALUS has 7 epidemiological compartments: susceptible, exposed, infectious
#' and symptomatic ("infect_symp"), infectious and asymptomatic ("infect_asymp")
#' , hospitalised, recovered, and dead.
#'
#' There are 3 additional compartments that track the number of new infections,
#' new hospitalisations, and new deaths (`i_dE`, `i_dH`, and `i_dD`).
#'
#' @name epi_constants
#' @rdname epi_constants
#'
#' @keywords epi_constant
COMPARTMENTS <- c(
  "susceptible", "exposed", "infect_symp", "infect_asymp",
  "hospitalised", "recovered", "dead",
  "new_infections", "new_hosp"
)

#' @name epi_constants
#' @keywords epi_constant
N_MODEL_COMPARTMENTS <- 9L

#' @name epi_constants
N_EPI_COMPARTMENTS <- 7L

#' @name epi_constants
N_DATA_COMPARTMENTS <- 2L

#' @name epi_constants
i_EPI_COMPARTMENTS <- seq.int(N_EPI_COMPARTMENTS)

#' @name epi_constants
i_DATA_COMPARTMENTS <- c(8L, 9L)

#' @name epi_constants
#' @keywords epi_constant
i_S <- 1L

#' @name epi_constants
i_E <- 2L

#' @name epi_constants
i_Is <- 3L

#' @name epi_constants
i_Ia <- 4L

#' @name epi_constants
i_H <- 5L

#' @name epi_constants
i_R <- 6L

#' @name epi_constants
i_D <- 7L

#' @name epi_constants
i_dE <- 8L # new infections

#' @name epi_constants
i_dH <- 9L # new hospitalisations

#' @name epi_constants
i_dD <- 10L # new deaths

#' Model output measures and groups for summaries and derived time-series
#'
#' @name summary_constants
#' @rdname summary_constants
#'
#' @keywords summary_constants
SUMMARY_MEASURES <- c(
  "infections", "hospitalisations", "deaths"
)

#' @name summary_constants
SUMMARY_GROUPS <- c("age_group", "vaccine_group", "econ_sector")
