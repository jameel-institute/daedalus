
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

### Run model

The model runs for 100 timesteps (assumed to be days) by default.

``` r
output <- daedalus(
  initial_state,
  parameters = default_parameters(contact_matrix = contact_matrix)
)
```

Get the data in long or ‘tidy’ format using `prepare_output()`.

``` r
data <- prepare_output(output)

head(data)
#>   time age_group compartment   value
#> 1    1       0-4 susceptible 3453667
#> 2    2       0-4 susceptible 3453661
#> 3    3       0-4 susceptible 3453654
#> 4    4       0-4 susceptible 3453643
#> 5    5       0-4 susceptible 3453627
#> 6    6       0-4 susceptible 3453599
```

## Related projects

WIP.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-haw2022" class="csl-entry">

Haw, David J., Giovanni Forchini, Patrick Doohan, Paula Christen, Matteo
Pianella, Robert Johnson, Sumali Bajaj, et al. 2022. “Optimizing Social
and Economic Activity While Containing SARS-CoV-2 Transmission Using
DAEDALUS.” *Nature Computational Science* 2 (4): 223–33.
<https://doi.org/10.1038/s43588-022-00233-0>.

</div>

</div>
