# Validate npi inputs

Validate npi inputs

## Usage

``` r
validate_npi_input(x, country, infection, response_time, max_duration = 365)
```

## Arguments

- x:

  An object to be validated as input to the `vaccine_investment`
  argument of
  [`daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.md).

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

- response_time:

  A single numeric value for the time in days at which the selected
  response is activated. This is ignored if the response has already
  been activated by the hospitalisation threshold being reached.
  Defaults to 30 days. Responses have a default maximum duration of 365
  days. This can be changed by passing a `<daedalus_npi>` object to
  `response_strategy`.

## Value

A `<daedalus_npi>` object.
