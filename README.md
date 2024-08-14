
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
vignette](https://jameel-institute.github.io/daedalus/articles/daedalus.html)
for more details).

``` r
library(daedalus)

# run model for Canada
output <- daedalus("Canada", "influenza_1918")
```

Users can select infection parameters from among seven epidemics caused
by directly-transmitted viral respiratory pathogens, which are stored in
the package as `daedalus::infection_data`: SARS 2004 (SARS-CoV-1),
influenza 2009 (influenza A H1N1), influenza 1957 (influenza A H2N2),
influenza 1918 (influenza A H1N1), Covid-19 wild type (SARS-Cov-2 wild
type), Covid-19 Omicron (SARS-CoV-2 omicron), and Covid-19 Delta
(SARS-CoV-2 delta).

Epidemic identifiers are stored as `daedalus::epidemic_names`.

Users can override default country contact data and epidemic-specific
infection arguments by passing named lists of parameters to
`daedalus()`; see the function documentation for more details.

A helper function prepares the data in long or ‘tidy’ format using
`prepare_output()`.

``` r
data <- prepare_output(output)

head(data)
#>   time age_group compartment econ_sector   value
#> 1    1       0-4 susceptible    sector_0 1993130
#> 2    2       0-4 susceptible    sector_0 1993128
#> 3    3       0-4 susceptible    sector_0 1993126
#> 4    4       0-4 susceptible    sector_0 1993123
#> 5    5       0-4 susceptible    sector_0 1993118
#> 6    6       0-4 susceptible    sector_0 1993109
```

## Related projects

*daedalus* is an R implementation of the scenario model from a [project
on the economics of pandemic
preparedness](https://github.com/robj411/p2_drivers).

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
