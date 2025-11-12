# Generate a default initial state for DAEDALUS

Function to prepare the model initial state. Assumes that 1 in every
million individuals is initially infected, and that 60% are asymptomatic
infections. This does not affect the actual probability of asymptomatic
infections in the simulation, which is a property of a
`<daedalus_infection>`.

## Usage

``` r
make_initial_state(country, initial_state_manual)
```

## Arguments

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

- initial_state_manual:

  An optional **named** list with the names `p_infectious`,
  `p_asymptomatic`, and `p_immune`. `p_infectious` and `p_asymptomatic`
  give the proportion of infectious and symptomatic individuals in each
  age group and economic sector. Defaults to `1e-6` and `0.0`
  respectively. `p_immune` may be a single number in the range
  `0.0 <= p_immune <= 1.0` or a 4-element vector in that range (the
  number of age groups in the model), for the proportion of individuals
  in the population or in each age group that have some pre-existing
  immunity to infection (reduced susceptibility). See **Details** for
  more.

## Value

An array with as many dimensions as `N_VACCINE_DATA_GROUPS` (currently,
3); rows specify the age and economic groups, columns specify the
epidemiological compartments (including new infections and
hospitalisations), and array layers hold information on vaccination
status (including new vaccinations).
