# Modelling multiple contact settings

This vignette shows how to pass community contact matrices disaggregated
into multiple settings to the Daedalus model.

Daedalus allows users to apply closures or any arbitrary scaling to each
setting separately, rather than uniformly scaling contacts across
settings. An example of setting-specific scaling is scaling school
contacts for school-age children, while leaving contacts in other
settings intact.

``` r

# load package
library(daedalus)
library(socialmixr) # for POLYMOD data
```

## Default contact settings

Daedalus countries have two contact settings by default as of version
0.3.6:

1.  Community or ‘total’: This is the set of age-specific contacts that
    take place in the community, outside the workplace. In daedalus \<
    v0.3.5, this would have been the element referred to as
    `x$contact_matrix`;

2.  Workplace: This is a matrix giving the contacts within workplaces
    (so between workers in an economic sector; forming the diagonal of
    the matrix), and from non-working consumers to workers in each
    economic sector (considers only contacts from consumers to workers
    and not vice versa).

These are displayed in the country object when printed to screen.

``` r

daedalus_country("GBR")
#> <daedalus_country>
#> • Name: United Kingdom
#> • Demography: 3924490, 11762039, 39536463, and 12663012
#> • Default contact matrix:
#> • * setting name: "community"; found 1 more setting: "workplace"
#>             0-4      5-19    20-64       65+
#> 0-4   1.9157895 1.5379290 4.704999 0.2863619
#> 5-19  0.5131412 8.7339228 5.874591 0.7418483
#> 20-64 0.4670302 1.7476822 7.830182 1.0685802
#> 65+   0.1180517 0.7548304 3.531487 1.5212437
#> • GNI (PPP $): 45870
#> • Hospital capacity: 26219
```

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
#> • * setting name: "home"; found 2 more settings: "school" and "workplace"
#>           contact.age.group
#> age.group      [0,5)    [5,18)  [18,65)  [65,Inf)
#>   [0,5)    0.4842105 1.0631579 2.694737 0.1157895
#>   [5,18)   0.3011152 1.7397770 2.516729 0.1449814
#>   [18,65)  0.2656514 0.9323181 2.218274 0.2081218
#>   [65,Inf) 0.1250000 0.7142857 1.660714 0.7142857
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

## Passing setting-specific contact scaling

Users can pass setting-specific contact scaling by using the
`<daedalus_npi>` class.

Scaling must take the form of an \\M \times N\\ matrix, where \\M\\ is
the number of age and economic strata (49 for Daedalus), and \\N\\ is
the number of settings. For a default `<daedalus_country>`, this would
be a \\49 \times 2\\ matrix, due to the two contact settings,
‘community’ and ‘workplace’.

The example below shows a matrix that does not scale community contacts,
and which scales all workplace contacts to 30% of the total. The
underlying model will scale the within-sector contacts by the square of
the scaling coefficient (representing scaling of both incoming and
outgoing contacts), while the consumer-to-worker contacts will be scaled
by the coefficient alone, as we are not simulating a reduction in the
number of consumers visiting workplaces.

``` r

openness <- cbind(
  rep(1, 49), # no scaling on community
  c(rep(1, 4), rep(0.3, 45)) # scale 45 economic strata
)
```

The following construction is simpler because it assumes the same
scaling for age-specific contacts in workplaces as for worker contacts,
and would not cause issues as age-specific workplace contacts are 0.
Nonetheless, we advise using the construction above as it makes the
intention more explicit.

``` r

openness <- cbind(
  rep(1, 49), # no scaling on community
  rep(0.3, 49) # scale all 49 age-and-economic strata
)
```

A third alternative is to take advantage of the default behaviour of the
`<daedalus_npi>` class constructor, which is to assume that the scaling
of community contacts is the simple mean of the scaling of workplace
contacts, and pass a single 45-element workplace contacts scaling
vector. This also applies to \[daedalus_timed_npi()\].

``` r

openness <- rep(0.3, 45) # for 45 economic sectors
```

Create a `<daedalus_npi>`, passing the constructed matrix `openness`,
and use it with
[`daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.md)
as needed.

``` r

# a 90-day timed NPI operating on GB; note timed_npi needs a list of openness
daedalus_timed_npi(30, 90, list(openness), "GB")
#> <daedalus_npi/daedalus_response>
#> NPI strategy: custom_timed
#> • Start time (days): 30
#> • End time (days): 90
#> • Openness (mean prop.): 0.3
#> • Maximum duration (days): NA
```

Openness values for pre-defined NPIs are also stored in this way.

**Note that** pre-defined NPIs include community contacts scaling by
default, as shown in the code snippet below. The scaling of community
contacts is approximately 0.71, which is the mean of the scaling of
workplace contacts shown below.

``` r

x <- daedalus_npi("elimination", "GB", "sars_cov_1")
x
#> <daedalus_npi/daedalus_response>
#> NPI strategy: elimination
#> • Start time (days): 30
#> • End time (days): NA
#> • Openness (mean prop.): 0.713
#> • Maximum duration (days): 365

x$parameters$openness$openness
#>            [,1] [,2]
#>  [1,] 0.7128889 1.00
#>  [2,] 0.7128889 1.00
#>  [3,] 0.7128889 1.00
#>  [4,] 0.7128889 1.00
#>  [5,] 0.7128889 0.86
#>  [6,] 0.7128889 0.86
#>  [7,] 0.7128889 0.90
#>  [8,] 0.7128889 0.90
#>  [9,] 0.7128889 0.90
#> [10,] 0.7128889 0.70
#> [11,] 0.7128889 0.70
#> [12,] 0.7128889 0.70
#> [13,] 0.7128889 0.70
#> [14,] 0.7128889 0.70
#> [15,] 0.7128889 0.70
#> [16,] 0.7128889 0.70
#> [17,] 0.7128889 0.70
#> [18,] 0.7128889 0.70
#> [19,] 0.7128889 0.70
#> [20,] 0.7128889 0.70
#> [21,] 0.7128889 0.70
#> [22,] 0.7128889 0.70
#> [23,] 0.7128889 0.70
#> [24,] 0.7128889 0.70
#> [25,] 0.7128889 0.70
#> [26,] 0.7128889 0.70
#> [27,] 0.7128889 0.89
#> [28,] 0.7128889 0.92
#> [29,] 0.7128889 0.56
#> [30,] 0.7128889 0.64
#> [31,] 0.7128889 0.63
#> [32,] 0.7128889 0.63
#> [33,] 0.7128889 0.63
#> [34,] 0.7128889 0.63
#> [35,] 0.7128889 0.63
#> [36,] 0.7128889 0.10
#> [37,] 0.7128889 0.88
#> [38,] 0.7128889 0.88
#> [39,] 0.7128889 0.88
#> [40,] 0.7128889 0.94
#> [41,] 0.7128889 0.98
#> [42,] 0.7128889 0.85
#> [43,] 0.7128889 0.66
#> [44,] 0.7128889 1.00
#> [45,] 0.7128889 0.10
#> [46,] 0.7128889 0.75
#> [47,] 0.7128889 0.55
#> [48,] 0.7128889 0.54
#> [49,] 0.7128889 0.49
```

## Future developments

This functionality is still in development, and features being
considered include applying different behavioural modifiers to each
setting to simulate endogenous risk-perception related modification in
contacts across settings.

## References

Mossong, Joël, Niel Hens, Mark Jit, et al. 2008. “Social Contacts and
Mixing Patterns Relevant to the Spread of Infectious Diseases.” *PLOS
Medicine* 5 (3): e74. <https://doi.org/10.1371/journal.pmed.0050074>.
