# Represent vaccine investment scenarios for DAEDALUS

Helper functions to create and work with S3 class
`<daedalus_vaccination>` objects for use with
[`daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.md).
These objects store vaccination parameters for reuse and have methods
for easy parameter access and editing, as well as processing raw
vaccination characteristics for the DAEDALUS model.

## Usage

``` r
daedalus_vaccination(
  name,
  country,
  start_time = NULL,
  rate = NULL,
  uptake_limit = NULL,
  efficacy = 50,
  waning_period = 180
)

is_daedalus_vaccination(x)

# S3 method for class 'daedalus_vaccination'
print(x, ...)
```

## Arguments

- name:

  A vaccination investment scenario name from among
  [daedalus.data::vaccination_scenario_names](https://jameel-institute.github.io/daedalus.data/reference/vaccine_scenarios.html).
  Selecting an epidemic automatically pulls in vaccination parameters
  associated with the epidemic; these are stored as packaged data in
  [`daedalus.data::vaccination_scenario_data`](https://jameel-institute.github.io/daedalus.data/reference/vaccine_scenarios.html).

- country:

  A `<daedalus_country>` object or a 2- or 3-character string that can
  be coerced to a `<daedalus_country>` (e.g. `"GBR"` for the United
  Kingdom). Used to determine when vaccination should end.

- start_time:

  The number of days after the start of the epidemic that vaccination
  begins. Must be a single number. Defaults to `NULL` and the start time
  is taken from the vaccination scenarios specified by `name`. Passed to
  the `time_on` argument in
  [`new_daedalus_response()`](https://jameel-institute.github.io/daedalus/reference/class_response.md)
  via the class constructor `new_daedalus_vaccination()`.

- rate:

  A single number for the *percentage* of the total population that can
  be vaccinated each day. This is converted into a proportion
  automatically within
  [`daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.md).

- uptake_limit:

  A single number giving the upper limit for the *percentage* of the
  population that can be vaccinated. When this limit is reached,
  vaccination ends. Passed to the `value_state_off` argument in
  [`new_daedalus_response()`](https://jameel-institute.github.io/daedalus/reference/class_response.md)
  via the class constructor `new_daedalus_vaccination()`.

- efficacy:

  A single number in the range `[0, 100]` giving the efficacy of
  vaccination in preventing infection. A value of 0 indicates that
  vaccinated individuals are as susceptible to infection as unvaccinated
  ones, while 100 would indicate completely non-leaky vaccination that
  completely protects against infection.

- waning_period:

  A single number representing the number of days over which the average
  individual wanes out of the vaccinated stratum to the unvaccinated
  stratum. Only individuals in the susceptible and recovered
  compartments can wane out of being vaccinated.

- x:

  An object to be tested or printed as a `<daedalus_vaccination>`.

- ...:

  For `daedalus_vaccination()`, other parameters passed to
  [`new_daedalus_response()`](https://jameel-institute.github.io/daedalus/reference/class_response.md).
  For the `print` method, other parameters passed to
  [`print()`](https://rdrr.io/r/base/print.html).

## Details

Note that vaccination once ended by reaching the `uptake_limit` does not
restart once individuals wane out of the vaccinated compartment.

## Examples

``` r
# for no advance vaccine investment in the UK
daedalus_vaccination("none", "GBR")
#> <daedalus_vaccination/daedalus_response>
#> Vaccine investment scenario: none
#> • Start time (days): 365
#> • Rate (% per day): 0.143
#> • Uptake limit (%): 40
#> • Efficacy (%): 50
#> • Waning period (mean, days): 180

# modifying parameters during initialisation
# set daily vaccination rate to 1.5% of population
daedalus_vaccination("low", "GBR", rate = 1.5)
#> <daedalus_vaccination/daedalus_response>
#> Vaccine investment scenario: low
#> • Start time (days): 300
#> • Rate (% per day): 1.5
#> • Uptake limit (%): 50
#> • Efficacy (%): 50
#> • Waning period (mean, days): 180
```
