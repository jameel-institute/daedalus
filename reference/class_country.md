# Represent countries and territories for DAEDALUS

Helper functions to create and work with S3 class `<daedalus_country>`
objects for use with
[`daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.md).
These objects store country parameters for reuse and have methods for
easy parameter access and editing, as well as processing raw country
characteristics for the DAEDALUS model.

## Usage

``` r
daedalus_country(
  country,
  parameters = list(contact_matrix = NULL, contacts_workplace = NULL,
    contacts_consumer_worker = NULL),
  group_working_age = NULL
)

is_daedalus_country(x)

# S3 method for class 'daedalus_country'
print(x, ...)
```

## Arguments

- country:

  A string giving the country or territory name, or ISO2 or ISO3 code;
  must be from among
  [daedalus.data::country_codes_iso2c](https://jameel-institute.github.io/daedalus.data/reference/country_names_codes.html)
  or
  [daedalus.data::country_codes_iso3c](https://jameel-institute.github.io/daedalus.data/reference/country_names_codes.html)
  or
  [daedalus.data::country_names](https://jameel-institute.github.io/daedalus.data/reference/country_names_codes.html).

- parameters:

  An optional named list of country parameters that are allowed to be
  modified. Currently, users may only pass their own contact matrix,
  workplace contacts, and consumer-worker contact matrix. If these are
  not passed, default values are accessed from stored package data.

- group_working_age:

  An optional value for the age-group that is considered to be the
  working-age group. Defaults to `3`, which is taken from an internal
  constant.

- x:

  An object of the `<daedalus_country>` class.

- ...:

  Other parameters passed to
  [`print()`](https://rdrr.io/r/base/print.html).

## Value

- `daedalus_country()` returns an object of the S3 class
  `<daedalus_country>`

- `is_daedalus_country()` returns a logical for whether an object is a
  `<daedalus_country>`.

- `print.daedalus_country()` invisibly returns the `<daedalus_country>`
  object `x`. Called for printing side-effects.

## Examples

``` r
x <- daedalus_country("Canada")

x
#> <daedalus_country>
#> • Name: Canada
#> • Demography: 1993132, 5949109, 22966942, and 6832974
#> • Community contact matrix:
#>             0-4      5-19    20-64       65+
#> 0-4   1.9157895 1.5235823 5.014414 0.3169637
#> 5-19  0.5104463 8.7459756 6.322175 0.7948344
#> 20-64 0.4351641 1.6376280 7.821398 1.0350292
#> 65+   0.1187166 0.7488765 3.639207 1.5142917
#> • GNI (PPP $): 46050
#> • Hospital capacity: 7989

daedalus_country(
  "United Kingdom",
  parameters = list(contact_matrix = matrix(1, 4, 4))
)
#> <daedalus_country>
#> • Name: United Kingdom
#> • Demography: 3924490, 11762039, 39536463, and 12663012
#> • Community contact matrix:
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    1    1    1
#> [2,]    1    1    1    1
#> [3,]    1    1    1    1
#> [4,]    1    1    1    1
#> • GNI (PPP $): 45870
#> • Hospital capacity: 26219

# check whether `x` is a <country> object
is_daedalus_country(x)
#> [1] TRUE

# assign class members
# using set_data()
set_data(x, contact_matrix = matrix(99, 4, 4))
#> <daedalus_country>
#> • Name: Canada
#> • Demography: 1993132, 5949109, 22966942, and 6832974
#> • Community contact matrix:
#>      [,1] [,2] [,3] [,4]
#> [1,]   99   99   99   99
#> [2,]   99   99   99   99
#> [3,]   99   99   99   99
#> [4,]   99   99   99   99
#> • GNI (PPP $): 46050
#> • Hospital capacity: 7989

# using assignment operators
x$contact_matrix <- matrix(99, 4, 4)
x
#> <daedalus_country>
#> • Name: Canada
#> • Demography: 1993132, 5949109, 22966942, and 6832974
#> • Community contact matrix:
#>      [,1] [,2] [,3] [,4]
#> [1,]   99   99   99   99
#> [2,]   99   99   99   99
#> [3,]   99   99   99   99
#> [4,]   99   99   99   99
#> • GNI (PPP $): 46050
#> • Hospital capacity: 7989
```
