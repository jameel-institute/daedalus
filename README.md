
<!-- README.md is generated from README.Rmd. Please edit that file -->

# daedalus: Optimise economic, social and health trade-offs in a pandemic

<!-- badges: start -->

[![Project Status: Concept – Minimal or no implementation has been done
yet, or the repository is only intended to be a limited example, demo,
or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build
status](https://github.com/j-idea/daedalus/workflows/R-CMD-check/badge.svg)](https://github.com/j-idea/daedalus/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/j-idea/daedalus/branch/main/graph/badge.svg)](https://app.codecov.io/gh/j-idea/daedalus?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/daedalus)](https://CRAN.R-project.org/package=daedalus)
<!-- badges: end -->

*daedalus* implements the integrated epidemiological and economic model
in Haw et al. ([2022](#ref-haw2022)).

## Installation

You can install the development version of daedalus from
[GitHub](https://github.com/) using the *remotes* package.

``` r
# install.packages("remotes")
remotes::install_github("j-idea/daedalus", upgrade = FALSE)
```

## Quick start

``` r
library(daedalus)
```

### Prepare contact matrix and initial conditions

``` r
polymod <- socialmixr::polymod
# suppress messages that clog up test output
suppressMessages(
  contact_data <- socialmixr::contact_matrix(
    polymod,
    countries = "United Kingdom",
    age.limits = c(0, 5, 20, 65),
    symmetric = TRUE
  )
)

# get demography vector
demography <- contact_data[["demography"]][["population"]]

# prepare contact matrix
contact_matrix <- t(contact_data[["matrix"]]) / demography

# initial state: one in every 1 million is infected
initial_i <- 1e-6
initial_state <- c(
  S = 1.0 - initial_i, E = 0.0,
  Is = initial_i, Ia = 0.0,
  H = 0.0, R = 0.0, D = 0.0
)

# build for all age groups
initial_state <- rbind(
  initial_state,
  initial_state,
  initial_state,
  initial_state,
  deparse.level = 0L
)

# multiply by demography vector for absolute values
initial_state <- initial_state * demography
```

Prepare the initial state container, a 3D tensor.

Constants `N_AGE_GROUPS`, `N_EPI_COMPARTMENTS`, `N_ECON_SECTORS`,
`i_NOT_WORKING` and `i_WORKING_AGE` are package constants.

``` r
# construct state array, all working age individuals are uniformly
# distributed into economic sectors including non-working
state <- array(
  0.0,
  dim = c(N_AGE_GROUPS, N_EPI_COMPARTMENTS, N_ECON_STRATA)
)
state[, , i_NOT_WORKING] <- initial_state
state[i_WORKING_AGE, , ] <- matrix(
  state[i_WORKING_AGE, , i_NOT_WORKING] * 1.0 / N_ECON_STRATA,
  nrow = N_EPI_COMPARTMENTS, ncol = N_ECON_STRATA
)

# prepare workplace contacts
cw <- c(0.0, rep(5.0, N_ECON_SECTORS - 1L)) # no work contacts for non-working

# prepare consumer worker contacts
consumer_contacts_per_sector <- withr::with_seed(
  0L,
  stats::runif(N_ECON_SECTORS, min = 5.0, max = 10.0)
)
consumer_contacts_per_sector[i_NOT_WORKING] <- 0.0
cmcw <- matrix(
  consumer_contacts_per_sector,
  nrow = N_ECON_SECTORS, ncol = N_AGE_GROUPS
) * demography / sum(demography)
```

### Run model

The model runs for 100 timesteps (assumed to be days) by default.

``` r
output <- daedalus(
  state,
  parameters = default_parameters(
    contact_matrix = contact_matrix,
    contacts_workplace = cw,
    contacts_consumer_worker = cmcw
  )
)
```

Get the data in long or ‘tidy’ format using `prepare_output()`.

``` r
data <- prepare_output(output)

head(data)
#>   time age_group compartment econ_sector   value
#> 1    1       0-4 susceptible    sector_0 3453667
#> 2    2       0-4 susceptible    sector_0 3453664
#> 3    3       0-4 susceptible    sector_0 3453661
#> 4    4       0-4 susceptible    sector_0 3453658
#> 5    5       0-4 susceptible    sector_0 3453654
#> 6    6       0-4 susceptible    sector_0 3453647
```

## Related projects

WIP.

## References

<div id="refs" class="references hanging-indent">

<div id="ref-haw2022">

Haw, David J., Giovanni Forchini, Patrick Doohan, Paula Christen, Matteo
Pianella, Robert Johnson, Sumali Bajaj, et al. 2022. “Optimizing Social
and Economic Activity While Containing SARS-CoV-2 Transmission Using
DAEDALUS.” *Nature Computational Science* 2 (4): 223–33.
<https://doi.org/10.1038/s43588-022-00233-0>.

</div>

</div>
