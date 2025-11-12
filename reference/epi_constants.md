# Epidemiological compartments and indices

Names and indices for the epidemiological compartments used in DAEDALUS,
for reuse in model code. The duplication is for ease of extracting
indices from a named list, and of extracting names without having to
call [`names()`](https://rdrr.io/r/base/names.html).

## Usage

``` r
COMPARTMENTS

idx_COMPARTMENTS

N_EPI_COMPARTMENTS

i_EPI_COMPARTMENTS

N_MODEL_COMPARTMENTS

N_INFECTION_SUBSYSTEM
```

## Format

An object of class `character` of length 10.

An object of class `list` of length 10.

An object of class `integer` of length 1.

An object of class `integer` of length 8.

An object of class `integer` of length 1.

An object of class `integer` of length 1.

## Value

`COMPARTMENTS` returns a character vector of the epidemiological
compartment names. `idx_COMPARTMENTS` returns a list with the
compartment index.

All other constants return integer values.

## Details

DAEDALUS has 8 epidemiological compartments: susceptible, exposed,
infectious and symptomatic ("infect_symp"), infectious and asymptomatic
("infect_asymp") , hospitalised leading to recovery
("hospitalised_recov"), hospitalised leading to death
("hospitalised_death"), recovered, and dead.

There are 2 additional compartments that track the number of new
infections and new hospitalisations.
