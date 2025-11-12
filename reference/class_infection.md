# Represent infection parameters for DAEDALUS

Helper functions to create and work with S3 class `<daedalus_infection>`
objects for use with
[`daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.md).
These objects store infection parameters for reuse and have methods for
easy parameter access and editing, as well as processing raw infection
characteristics for the DAEDALUS model.

## Usage

``` r
daedalus_infection(name, ...)

is_daedalus_infection(x)

# S3 method for class 'daedalus_infection'
print(x, ...)
```

## Arguments

- name:

  An epidemic name from among
  [daedalus.data::epidemic_names](https://jameel-institute.github.io/daedalus.data/reference/epidemic_data.html).
  Selecting an epidemic automatically pulls in infection parameters
  associated with the epidemic; these are stored as packaged data in
  [`daedalus.data::infection_data`](https://jameel-institute.github.io/daedalus.data/reference/epidemic_data.html).
  Default infection parameters for epidemics can be over-ridden by
  passing them as a named list to `...`.

- ...:

  Other parameters passed to
  [`print()`](https://rdrr.io/r/base/print.html).

- x:

  An object of the `<daedalus_infection>` class.

## Value

- `daedalus_infection()` returns an object of the S3 class
  `<daedalus_infection>`.

- `is_daedalus_infection()` returns a logical for whether an object is a
  `<daedalus_infection>`.

- `print.daedalus_infection()` invisibly returns the
  `<daedalus_infection>` object `x`. Called for printing side-effects.

## Details

### Included epidemics

Epidemics for which data are available are given below (pathogen in
parentheses). The string indicates the name that must be passed to the
`name` argument.

- `"sars_cov_1"`: SARS 2004 (SARS-CoV-1),

- `"influenza_2009"`: influenza 2009 (influenza A H1N1),

- `"influenza_1957"`: influenza 1957 (influenza A H2N2),

- `"influenza_1918"`: influenza 1918 (influenza A H1N1),

- `"sars_cov_2_pre_alpha"`: Covid-19 wild type (SARS-Cov-2 wild type),

- `"sars_cov_2_omicron"`: Covid-19 Omicron (SARS-CoV-2 omicron),

- `"sars_cov_2_pre_delta"`: (SARS-CoV-2 delta).

### Infection parameters

All infections have the following parameters, which take default values
stored in the package under
[daedalus.data::infection_data](https://jameel-institute.github.io/daedalus.data/reference/epidemic_data.html).
Users can pass custom values for these parameters as arguments via
`...`.

- `r0`: A single numeric value for the basic reproduction value of the
  infection \\R_0\\.

- `sigma`: A single numeric value \> 0.0 for the rate of transition from
  the exposed compartment to one of two infectious compartments.

- `p_sigma`: A single numeric value in the range \\\[0.0, 1.0\]\\ for
  the proportion of infectious individuals who are also symptomatic.
  Asymptomatic individuals can have a different contribution to the
  force of infection from symptomatic individuals.

- `epsilon`: A single numeric value for the relative contribution of
  asymptomatic infectious individuals to the force of infection
  (compared to symptomatic individuals).

- `gamma_Is`: A single numeric value for the recovery rate of infectious
  individuals who are not hospitalised.

- `gamma_Ia`: A single numeric value for the recovery rate from
  asymptomatic infection.

- `ifr`: A numeric vector of length `N_AGE_GROUPS` (4) for the
  age-specific infection fatality risk.

- `eta`: A numeric vector of length `N_AGE_GROUPS` (4) for the
  age-specific hospitalisation rate for individuals who are infectious
  and symptomatic.

- `hfr`: A numeric vector of length `N_AGE_GROUPS` (4) for the
  age-specific probability of death conditional on hospitalisation.

- `gamma_H_recovery`: A single numeric value for the recovery rate of
  hospitalised individuals.

- `gamma_H_death`: A single numeric value for the death rate of
  hospitalised individuals.

- `rho`: A single numeric value for the rate at which infection-derived
  immunity wanes, returning individuals in the 'recovered' compartment
  to the 'susceptible' compartment.

## Examples

``` r
# make a <daedalus_infection> object with default parameter values
daedalus_infection("sars_cov_1")
#> <daedalus_infection>
#> • Epidemic name: sars_cov_1
#> • R0: 1.75
#> • sigma: 0.217
#> • p_sigma: 0.867
#> • epsilon: 0.58
#> • rho: 0.003
#> • eta: 0.018, 0.082, 0.018, and 0.246
#> • hfr: 0.255, 0.255, 0.255, and 0.255
#> • gamma_Ia: 0.476
#> • gamma_Is: 0.25
#> • gamma_H_recovery: 0.044
#> • gamma_H_death: 0.05

# modify infection parameters R0 and immunity waning rate
daedalus_infection("influenza_1918", r0 = 2.5, rho = 0.01)
#> <daedalus_infection>
#> • Epidemic name: influenza_1918
#> • R0: 2.5
#> • sigma: 0.909
#> • p_sigma: 0.669
#> • epsilon: 0.58
#> • rho: 0.01
#> • eta: 0.073, 0.064, 0.02, and 0.152
#> • hfr: 0.125, 0.125, 0.125, and 0.125
#> • gamma_Ia: 0.4
#> • gamma_Is: 0.4
#> • gamma_H_recovery: 0.2
#> • gamma_H_death: 0.2
```
