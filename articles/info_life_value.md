# Value of life-years lost

This vignette explains how the value of lives lost due to infection in a
pandemic scenario is calculated in *daedalus*.

The main focus is on explaining how the value of a statistical life
(VSL, also referred to hereafter as ‘life-value’) is calculated in
*daedalus*. The VSL is multiplied by the number of deaths to get the
total value of life-years lost due to the pandemic.

The methodology used here has been chosen so as to be generalisable for
the widest possible range of countries and territories, while using
publicly available data.

This approach balances the ability to run *daedalus* scenarios for a
large number of countries, and the effort required to obtain and update
official life-values from national governments.

**Note that** the VSL values used in a call to
[`daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.md)
for any country can always be substituted with values considered to be
more appropriate (e.g., from a national health ministry).

**Note also that** preparatory calculations of the value of a
statistical life are performed in the data preparation scripts in our
companion data package
[*daedalus.data*](https://github.com/jameel-institute/daedalus.data/).

## Value of a life year

The value of a life year for a country \\VLY\_\text{c}\\ is taken to be
the gross national income (GNI) per capita scaled for purchasing power
parity in international dollars.

Country GNI values are for the most recent year available, and are taken
from the [World Bank’s World Development
Indicators](https://databank.worldbank.org/source/world-development-indicators).

**Note that** there is no discounting applied to older age groups, that
is, all remaining years of life are valued the same, and are not
adjusted for disabilities or quality.

## Life expectancy and years of life lost

Life-expectancy data in terms of years for each country and territory is
initially accessed for age-bins and separated by sex, from the Global
Burden of Disease Collaborative Network ([2020](#ref-gbd_life)).

The life-expectancy data is then aggregated by the DAEDALUS age bins: 0
– 4, 5 – 19 (school age), 20 – 65 (working age), 65+ (retired or not
working).

**Note that** the aggregation of is a simple mean across all age-bins in
the raw data; a more appropriate approach which we aim to add in future
would be a weighted mean that takes into account the size of each
age-bin in the raw data.

This results in four life-expectancy values for each country, each
corresponding to one of the four DAEDALUS age groups.

## Value of a statistical life

Many policies aim to improve longevity, decreasing the risk of death in
each year. The value of these risk reductions is often expressed as the
value per statistical life (VSL); at times a value per statistical life
year (VSLY) may be used.

The VSL concept is widely misunderstood. It is not the value that the
analyst, the government, or the individual places on saving an
identified life with certainty. Instead, it reflects individuals’
willingness to exchange money for a small change in their own risk, such
as a 1 in 10,000 decrease in the chance of dying in a specific year
([Robinson et al. 2019](#ref-robinson2019)).

The value of a statistical life for an individual of country \\c\\ in
age group \\i\\ denoted as \\\text{VSL}\_{c_i}\\ is the product of the
country-specific VLY (\\\text{VLY}\_{c}\\) and the country- and
age-specific life expectancy (\\T\_{c_i}\\), and depends on the country
and age-group of the individual; this can be expressed as:

\\\text{VSL}\_{c_i} = \text{VLY}\_c \times T\_{c_i}\\

In *daedalus*, the country- and age-specifc VSL values can be accessed
from the package data object `daedalus::life_value`. These values are
also automatically accessed and included when a `<daedalus_country>`
object is created, under the name `vsl`.

``` r
# load library for examples
library(daedalus)

# create a <daedalus_country>
x <- daedalus_country("China")

# access VSL values using either a helper function or
# the subsetting operator
get_data(x, "vsl")
#> [1] 1322296.0 1166990.6  674010.6  177462.6

x$vsl
#> [1] 1322296.0 1166990.6  674010.6  177462.6
```

## Value of lives lost

In a
[`daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.md)
scenario, the total value of lives lost is the sum of the products of
the country- and age-specific VSL and the number of deaths in each
age-group in that country:

\\\Sigma\_{i = 1}^{N} \text{VSL}\_{c_i} \times D_i\\

where \\D_i\\ is the total number of deaths in age group \\i\\, with \\i
\in 1, 2, 3, 4\\ (as detailed above).

For users’ convenience, the total value of lives lost is reported in
million dollars, to bring it in line with the scaling used for other
pandemic costs.

## References

Global Burden of Disease Collaborative Network. 2020. “Global Burden of
Disease Study 2019 (GBD 2019) Life Tables 1950-2019.” Institute for
Health Metrics and Evaluation (IHME).
<https://doi.org/10.6069/1PF5-1M37>.

Robinson, Lisa A., James K. Hammitt, Michele Cecchini, Kalipso
Chalkidou, Karl Claxton, Maureen Cropper, Patrick Hoang-Vu Eozenou, et
al. 2019. “Reference Case Guidelines for Benefit-Cost Analysis in Global
Health and Development.” SSRN Scholarly Paper. Rochester, NY: Social
Science Research Network. <https://doi.org/10.2139/ssrn.4015886>.
