# Prepare susceptibility matrix for a vaccine-country pair

Helper function to prepare a susceptibility matrix to be used internally
to modulate the number of infections in vaccinated groups.

## Usage

``` r
make_susc_matrix(vaccination, country)
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

## Value

A matrix with dimensions as follows:

- Rows: number of age and economic groups in `country`;

- Cols: number of vaccination strata in the DAEDALUS model, given as
  `N_VAX_STRATA`.
