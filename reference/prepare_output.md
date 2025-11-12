# Prepare DAEDALUS data

Convert DAEDALUS data into a long-format `<data.frame>`.

## Usage

``` r
prepare_output(output, country, timesteps)
```

## Arguments

- output:

  Output from
  [`daedalus_internal()`](https://jameel-institute.github.io/daedalus/reference/daedalus_internal.md),
  expected to be a list.

- country:

  A `<daedalus_country>` object from which to get data on the number of
  demographic, economic, and vaccine groups.

- timesteps:

  A numeric vector of model timesteps.

## Value

A `<data.frame>` in long or 'tidy' format with the columns "time",
"age_group", "compartment", "econ_sector", and "value", for the
age-group specific value of the number of individuals in each economic
sector and epidemiological compartment in each timestep.

## Details

There are 45 economic sectors, given by `N_ECON_SECTORS`, with the first
'sector' representing non-working individuals. All age groups that are
considered non-working are in this first sector (children and retirees),
while working-age individuals may be distributed flexibly into the
various economic sectors (including non-working).
