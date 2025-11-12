# Represent non-pharmaceutical intervention strategies for DAEDALUS

Helper functions to create and work with S3 class `<daedalus_npi>`
objects for use with
[`daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.md).
These objects store NPI parameters for reuse and have methods for easy
parameter access and editing, as well as processing raw NPI
characteristics for the DAEDALUS model.

Most NPIs must be initialised with an associated `country` and
`infection`; these are used to determine flag positions as well as the
state-values (hospital capacity) and incidence-prevalence ratio (IPR) at
which the NPI starts and ends reactively.

`daedalus_timed_npi()` is a helper function to create a `<daedalus_npi>`
that is only trigged by time, and is not responsive to state variables.
Primarily intended for use in real time modelling.

## Usage

``` r
daedalus_npi(
  name,
  country,
  infection,
  openness = NULL,
  start_time = 30,
  end_time = NULL,
  max_duration = 365
)

is_daedalus_npi(x)

# S3 method for class 'daedalus_npi'
print(x, ...)

daedalus_timed_npi(start_time, end_time, openness, country)
```

## Arguments

- name:

  An NPI strategy name from among
  [daedalus.data::closure_strategy_names](https://jameel-institute.github.io/daedalus.data/reference/closure_strategies.html),
  or `NA`. Passing a pre-defined strategy name automatically pulls in
  openness parameters for the associated response strategy; these are
  stored as packaged data in
  [daedalus.data::closure_strategy_data](https://jameel-institute.github.io/daedalus.data/reference/closure_strategies.html).

  Pass `NA` to define a custom response strategy by passing a vector to
  `openness`.

- country:

  A country or territory object of class `<daedalus_country>`, **or** a
  country or territory name from those included in the package; see
  [daedalus.data::country_names](https://jameel-institute.github.io/daedalus.data/reference/country_names_codes.html),
  **or** a country ISO2 or ISO3 code; see
  [daedalus.data::country_codes_iso2c](https://jameel-institute.github.io/daedalus.data/reference/country_names_codes.html)
  and
  [daedalus.data::country_codes_iso3c](https://jameel-institute.github.io/daedalus.data/reference/country_names_codes.html).
  Country-specific data such as the community and workplace contacts,
  the demography, and the distribution of the workforce into economic
  sectors is automatically accessed from package data for the relevant
  country name if it is passed as a string. To override package defaults
  for country characteristics, pass a `<daedalus_country>` object
  instead. See
  [`daedalus_country()`](https://jameel-institute.github.io/daedalus/reference/class_country.md)
  for more.

- infection:

  An infection parameter object of the class `<daedalus_infection>`,
  **or** an epidemic name for which data are provided in the package;
  see
  [daedalus.data::epidemic_names](https://jameel-institute.github.io/daedalus.data/reference/epidemic_data.html)
  for historical epidemics or epidemic waves for which parameters are
  available. Passing the name as a string automatically accesses the
  default parameters of an infection. Create and pass a
  `<daedalus_infection>` to tweak infection parameters using
  [`daedalus_infection()`](https://jameel-institute.github.io/daedalus/reference/class_infection.md).

- openness:

  For `daedalus_npi()`, an optional numeric vector giving the openness
  of each economic sector in the model when the NPI is in effect.

  For `daedalus_timed_npi()`, a list of numeric vectors giving the
  openness coefficients for each interval specified by corresponding
  elements of `start_time` and `end_time`.

  Expected to have a length of `N_ECON_SECTORS` (currently 45), with all
  values are in the range \\\[0, 1\]\\,

- start_time:

  For `daedalus_npi()`, a single number giving the number of days after
  the start of the model that the NPI response begins. Defaults to 30.

  For `daedalus_timed_npi()`: may be a vector of NPI start times, with
  no default values.

- end_time:

  For `daedalus_npi()`, a single number giving the number of days after
  the start of the model that the NPI response ends. Defaults to `NULL`,
  indicating that the response ends (if no other condition is met), at
  the maximum duration.

  For `daedalus_timed_npi()`: may be a vector of NPI end times, with no
  default values. Timed NPIs have no default duration.

- max_duration:

  The maximum number of days that an NPI response is active whether
  started by passing the `start_time` or when a state threshold is
  crossed. Defaults to 365 days. NPIs created using
  `daedalus_timed_npi()` have no default maximum duration and are
  encoded by pairs of start and end times.

- x:

  An object to be tested or printed as a `<daedalus_npi>`.

- ...:

  For `daedalus_npi()`, other parameters passed to
  [`new_daedalus_response()`](https://jameel-institute.github.io/daedalus/reference/class_response.md).
  For the `print` method, other parameters passed to
  [`print()`](https://rdrr.io/r/base/print.html).

## Value

`daedalus_npi()`: A `<daedalus_npi>` class object that specifies an NPI
that may be responsive to both model state and time.

`daedalus_timed_npi()`: A `<daedalus_npi>` class object which specifies
time-limited interventions only.

## Details

Note that NPIs created using `daedalus_npi()` are reactive to the model
state (i.e., the epidemic state), and can trigger multiple times when
state conditions are met.

## Examples

``` r
# for a school closure strategy
daedalus_npi("school_closures", "GBR", "sars_cov_1")
#> <daedalus_npi/daedalus_response>
#> NPI strategy: school_closures
#> • Start time (days): 30
#> • End time (days): NA
#> • Openness (mean prop.): 0.93
#> • Maximum duration (days): 365

# set custom openness
daedalus_npi(
  NA,
  "GBR", "sars_cov_1",
  openness = rep(0.1, 45)
)
#> <daedalus_npi/daedalus_response>
#> NPI strategy: custom
#> • Start time (days): 30
#> • End time (days): NA
#> • Openness (mean prop.): 0.1
#> • Maximum duration (days): 365

# time-limited NPI with multiple phases
daedalus_timed_npi(
  start_time = c(10, 20, 30),
  end_time = c(15, 25, 40),
  openness = list(
    rep(1, 45),
    rep(0.5, 45),
    rep(0.2, 45)
  ),
  country = "GBR"
)
#> <daedalus_npi/daedalus_response>
#> NPI strategy: custom_timed
#> • Start time (days): 10, 20, and 30
#> • End time (days): 15, 25, and 40
#> • Openness (mean prop.): 1, 0.5, and 0.2
#> • Maximum duration (days): NA
```
