
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

![](vignettes/figures/daedalus.png)

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

### Installation notes

*daedalus* is under active development. To use a version of *daedalus*
associated with major milestones, install a specific version from the
list below.

- IDM Thailand 2024: `pak::pak("jameel-institute/daedalus@v0.1.0")`

- IfG workshop 2025 (with real time modelling):
  `pak::pak("jameel-institute/daedalus@v0.2.0")`

### Known issues

We have found the following issues with released versions:

1.  `v0.1.0`: Consumer-worker contacts are not correctly handled,
    leading to higher and faster epidemic peaks.

2.  `v0.2.0`: Consumer-worker contacts are not included in the model,
    leading to lower and later epidemic peaks. The ‘recovered’
    compartment does not include recoveries from hospitalisation. Please
    use the helper function `get_epidemic_summary()` to get epidemic
    sizes.

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
#> [1] 1049499

# disaggregate total for economic, education, and health costs
get_costs(data, "domain")
#>     economic    education   life_value   life_years 
#>    26819.051     1669.422  1021010.561 22171782.001
```

Users can select infection parameters from among seven epidemics caused
by directly-transmitted viral respiratory pathogens, which are stored in
the stand-alone helper package `daedalus.data`. These can be called as
`daedalus.data::infection_data`, while epidemic identifiers are stored
as `daedalus.data::epidemic_names`.

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

[*daedalus.data*](https://github.com/jameel-institute/daedalus.data) is
a stand-alone helper R package to handle input data for the *daedalus*
model. Its purpose is to allow users to flexibly manipulate or change
epidemiological and economic data, without needing to modify the model
itself.

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
