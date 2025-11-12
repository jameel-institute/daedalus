# DAEDALUS model constants

Frequently used values for the DAEDALUS model related to the model
population and structure. See also
[epi_constants](https://jameel-institute.github.io/daedalus/reference/epi_constants.md)
for constants specific to the epidemiological model.

## Usage

``` r
N_AGE_GROUPS

i_AGE_GROUPS

N_VACCINE_STRATA

N_VACCINE_DATA_GROUPS

AGE_GROUPS

i_WORKING_AGE

i_SCHOOL_AGE

N_ECON_SECTORS

i_ECON_SECTORS

i_EDUCATION_SECTOR

N_ECON_STRATA

i_NOT_WORKING

DIM_AGE_GROUPS

DIM_EPI_COMPARTMENTS

DIM_ECON_SECTORS

DIM_VACCINE_STRATA

i_UNVACCINATED_STRATUM

i_VACCINATED_STRATUM

i_NEW_VAX_STRATUM

VACCINE_GROUPS

N_OUTPUT_COLS

N_FLAGS

FLAG_NAMES
```

## Format

An object of class `integer` of length 1.

An object of class `integer` of length 4.

An object of class `integer` of length 1.

An object of class `integer` of length 1.

An object of class `character` of length 4.

An object of class `integer` of length 1.

An object of class `integer` of length 1.

An object of class `integer` of length 1.

An object of class `integer` of length 45.

An object of class `integer` of length 1.

An object of class `integer` of length 1.

An object of class `integer` of length 1.

An object of class `integer` of length 1.

An object of class `integer` of length 1.

An object of class `integer` of length 1.

An object of class `integer` of length 1.

An object of class `integer` of length 1.

An object of class `integer` of length 1.

An object of class `integer` of length 1.

An object of class `character` of length 3.

An object of class `integer` of length 1.

An object of class `integer` of length 1.

An object of class `character` of length 9.

## Value

Values for the model constants.

## Details

The DAEDALUS model requires the following values to be fixed.

- Number of age groups: 4

- Number of vaccination strata: 2

- Age group bins: 0-4 years, 5-19 years, 20-65 years (working age), 65+
  years

- Index of the working-age age groups: 3; see `AGE_GROUPS`

- Number of economic sectors: 45

- Index of individuals not working as a layer in the 3D state tensor: 1

- Number of economic strata: 46; note that this is always one more than
  the number of economic sectors, with the additional stratum for those
  not in work which includes all individuals not of working age, and a
  proportion of working age individuals.

- Array dimension of age groups: 1

- Array dimension of epidemiological compartments: 2

- Array dimension of economic sectors: 3

- Array dimension of vaccination strata: 4

- Indices and numbers of key groups.

- Number of state variables that are flags, switches, or indicators.
