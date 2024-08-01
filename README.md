
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

The model can be run for any country or territory in the `country_names`
list by passing the country name to the function. This automatically
pulls country-specific demographic and economic data, which is included
in the package, into the model (see the [‘Get started’
vignette](articles/daedalus.html) for more details).

The model runs for 300 timesteps (assumed to be days) by default.

``` r
library(daedalus)

# run model for Canada
output <- daedalus("Canada")
```

Users can specify pathogen parameter values as well as change the
initial conditions by passing arguments to `daedalus()` as `...`. The
function documentation details which parameters are accepted.

``` r
# not run
# e.g. change the transmission rate
daedalus("Canada", beta = 1.5 / 7.0) # assume R0 of 1.5, 7 days infectious period

# e.g. change the hospitalisation rate
daedalus("Canada", eta = 1.0 / 1000) # assume 1 in 1000 infectious need hospital
```

Get the data in long or ‘tidy’ format using `prepare_output()`.

``` r
data <- prepare_output(output)

head(data)
#>   time age_group compartment econ_sector   value
#> 1    1       0-4 susceptible    sector_0 1993130
#> 2    2       0-4 susceptible    sector_0 1993128
#> 3    3       0-4 susceptible    sector_0 1993127
#> 4    4       0-4 susceptible    sector_0 1993125
#> 5    5       0-4 susceptible    sector_0 1993123
#> 6    6       0-4 susceptible    sector_0 1993121
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
