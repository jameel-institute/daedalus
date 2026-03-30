# Modelling multiple contact settings

This vignette shows how to pass community contact matrices disaggregated
into multiple settings to the Daedalus model.

The idea is — in future — to be able to apply closures or any arbitrary
scaling to each setting separately, rather than uniformly scaling
contacts across settings. An example of setting-specific scaling is
scaling school contacts for school-age children, while leaving contacts
in other settings intact.

**Note that** While the scaffolding for such scaling functionality is in
place on the C++ side, this functionality is still being developed on
the R side.

## Basic example using POLYMOD

This example using POLYMOD data ([Mossong et al.
2008](#ref-mossong2008)) shows how to assign contacts from two different
settings to a `<daedalus_country>` object.

Once this assignment is done, helper functions such as
[`prepare_parameters()`](https://jameel-institute.github.io/daedalus/reference/prepare_parameters.md)
take care of expanding and scaling the contact matrices when the country
object is passed to
[`daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.md).

**Note that** running `daedalus("XYZ", ...)` cannot be used to model
multiple settings, and the `country` argument must be a
`<daedalus_country>` if multiple settings are needed.

``` r
# load package
library(daedalus)
library(socialmixr) # for POLYMOD data
```

By default a country has a single setting, the ‘total’ community
contacts. This is displayed in the country object when printed to
screen.

``` r
daedalus_country("GBR")
#> <daedalus_country>
#> • Name: United Kingdom
#> • Demography: 3924490, 11762039, 39536463, and 12663012
#> • Default contact matrix:
#> • * setting name: "total"; found no more settings
#>             0-4      5-19    20-64       65+
#> 0-4   1.9157895 1.5379290 4.704999 0.2863619
#> 5-19  0.5131412 8.7339228 5.874591 0.7418483
#> 20-64 0.4670302 1.7476822 7.830182 1.0685802
#> 65+   0.1180517 0.7548304 3.531487 1.5212437
#> • GNI (PPP $): 45870
#> • Hospital capacity: 26219
```

We can get POLYMOD data ([Mossong et al. 2008](#ref-mossong2008)) for
the U.K. from the [R package
*socialmixr*](https://cran.r-project.org/package=socialmixr). Here in
this basic example, we consider only two settings, ‘home’ and ‘school’.

``` r
# load polymod data and get home and school contacts
data(polymod)

# get matrices with Daedalus age bins
cm_uk_home <- contact_matrix(
  polymod,
  "GB",
  age_limits = c(0, 5, 18, 65),
  filter = list(cnt_home = 1)
)$matrix

cm_uk_school <- contact_matrix(
  polymod,
  "GB",
  age_limits = c(0, 5, 18, 65),
  filter = list(cnt_school = 1)
)$matrix
```

Create a new country object, passing the new settings to the
`contact_matrix` argument as a named list. Having names helps when
examining the object later.

``` r
x <- daedalus_country(
  "GB",
  contact_matrix = list(
    home = cm_uk_home,
    school = cm_uk_school
  )
)

# print x to see output
x
#> <daedalus_country>
#> • Name: United Kingdom
#> • Demography: 3924490, 11762039, 39536463, and 12663012
#> • Default contact matrix:
#> • * setting name: "home"; found 1 more setting: "school"
#>          contact.age.group
#> age.group     [0,5)    [5,18)  [18,65)       65+
#>   [0,5)   0.4842105 1.0631579 2.694737 0.1157895
#>   [5,18)  0.3011152 1.7397770 2.516729 0.1449814
#>   [18,65) 0.2656514 0.9323181 2.218274 0.2081218
#>   65+     0.1250000 0.7142857 1.660714 0.7142857
#> • GNI (PPP $): 45870
#> • Hospital capacity: 26219
```

Pass the created country to \[daedalus()\] as usual to run the epi-econ
model taking into account contacts in each setting.

``` r
daedalus(x, "sars_cov_1", time_end = 100)
#> <daedalus_output>
#> • Country: United Kingdom
#> • Epidemic: sars_cov_1
#> • NPI response: none
#> • Vaccination: no vaccination
#> • Behaviour: no behaviour
```

## Future developments

A number of developments to this feature are being considered. These
include:

1.  Disaggregating workplace contacts into settings, so that scaling can
    be applied by setting rather than by economic sector, as this is
    much easier to work with from a policy perspective (e.g. closing
    pubs *versus* scaling hospitality contacts by some amount to
    represent pub closures).

2.  Applying age-specific setting scaling, to represent different policy
    recommendations or outcomes by age in different settings.

3.  Applying different behavioural modifiers to each setting to simulate
    endogenous risk-perception related modification in contacts across
    settings.

## References

Mossong, Joël, Niel Hens, Mark Jit, Philippe Beutels, Kari Auranen,
Rafael Mikolajczyk, Marco Massari, et al. 2008. “Social Contacts and
Mixing Patterns Relevant to the Spread of Infectious Diseases.” *PLOS
Medicine* 5 (3): e74. <https://doi.org/10.1371/journal.pmed.0050074>.
