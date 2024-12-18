
<!-- README.md is generated from README.Rmd. Please edit that file -->

# daedalus: Model health, social, and economic costs of a pandemic

<!-- badges: start -->

[![Project Status: Concept – Minimal or no implementation has been done
yet, or the repository is only intended to be a limited example, demo,
or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build
status](https://github.com/jameel-institute/daedalus/workflows/R-CMD-check/badge.svg)](https://github.com/jameel-institute/daedalus/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jameel-institute/daedalus/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jameel-institute/daedalus?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/daedalus)](https://CRAN.R-project.org/package=daedalus)
<!-- badges: end -->

*daedalus* implements the integrated epidemiological and economic model
in Haw et al. ([2022](#ref-haw2022)).

## Installation

You can install the development version of daedalus from this repository
using the *pak* package, or from the Jameel Institute R-universe.

``` r
# install.packages("pak")
pak::pak("jameel-institute/daedalus", upgrade = FALSE)

# installation from R-universe
install.packages(
  "daedalus", 
  repos = c(
    "https://jameel-institute.r-universe.dev", "https://cloud.r-project.org"
  )
)
```

## Quick start

The model can be run for any country or territory in the `country_names`
list by passing the country name to the function. This automatically
pulls country-specific demographic and economic data, which is included
in the package, into the model (see the [‘Get started’
vignette](https://jameel-institute.github.io/daedalus/articles/daedalus.html)
for more details).

``` r
library(daedalus)

# run model for Canada
data <- daedalus("Canada", "influenza_1918")

# get pandemic costs as a total in million dollars
get_costs(data, "total")
#> [1] 1621184

# disaggregate total for economic, education, and health costs
get_costs(data, "domain")
#>    economic   education  life_years 
#>   33680.950    2201.333 1585301.497
```

Users can select infection parameters from among seven epidemics caused
by directly-transmitted viral respiratory pathogens, which are stored in
the package as `daedalus::infection_data`, with epidemic identifiers are
stored as `daedalus::epidemic_names`.

Users can override default country contact data and epidemic-specific
infection arguments by passing custom classes to `daedalus()`; see the
package website for more details.

Users can also model the implementation of pandemic response measures:
for more on this see the documentation for the main model function
`daedalus()`, and the vignette on modelling interventions on the package
website.

## Related projects

*daedalus* is an R implementation of the scenario model from a [project
on the economics of pandemic
preparedness](https://github.com/robj411/p2_drivers).

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-haw2022" class="csl-entry">

Haw, David J., Giovanni Forchini, Patrick Doohan, Paula Christen, Matteo
Pianella, Robert Johnson, Sumali Bajaj, et al. 2022. “Optimizing Social
and Economic Activity While Containing SARS-CoV-2 Transmission Using
DAEDALUS.” *Nature Computational Science* 2 (4): 223–33.
<https://doi.org/10.1038/s43588-022-00233-0>.

</div>

</div>
