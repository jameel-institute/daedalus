# Get parameters from DAEDALUS classes

Generic and methods for S3 classes for safely getting class parameters.

## Usage

``` r
# S3 method for class 'daedalus_country'
get_data(x, to_get, ...)

# S3 method for class 'daedalus_infection'
get_data(x, to_get, ...)

# S3 method for class 'daedalus_npi'
get_data(x, to_get, ...)

# S3 method for class 'daedalus_npi'
get_data(x, to_get, ...)

# S3 method for class 'daedalus_output'
get_data(x, to_get = NULL, ...)

# S3 method for class 'daedalus_vaccination'
get_data(x, to_get, ...)

get_data(x, ...)
```

## Arguments

- x:

  An S3 class object from the daedalus package of the
  `<daedalus_country>` or `<infection>` class.

- to_get:

  A string giving the name of the element of `x` to return.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Other arguments to class methods. Class methods do not currently
  support any other arguments.

## Value

Returns a member of `x`, with the class preserved (e.g. numeric vector
for a country `"demography"`).

For `<daedalus_output>` objects, returns the model timeseries data when
no element is specified.

## Examples

``` r
# simple example of getting data
country_A <- daedalus_country("United Kingdom")
get_data(country_A, "demography")
#>      0-4     5-19    20-64      65+ 
#>  3924490 11762039 39536463 12663012 

get_data(country_A, "contact_matrix")
#>             0-4      5-19    20-64       65+
#> 0-4   1.9157895 1.5379290 4.704999 0.2863619
#> 5-19  0.5131412 8.7339228 5.874591 0.7418483
#> 20-64 0.4670302 1.7476822 7.830182 1.0685802
#> 65+   0.1180517 0.7548304 3.531487 1.5212437

disease_x <- daedalus_infection("sars_cov_1", r0 = 1.9)
get_data(disease_x, "r0")
#> [1] 1.9

# get model data
output <- daedalus("Canada", "influenza_1918")
head(
  get_data(output)
)
#>   time   value age_group econ_sector vaccine_group compartment
#> 1    0 1993132       0-4   sector_00  unvaccinated susceptible
#> 2    1 1993131       0-4   sector_00  unvaccinated susceptible
#> 3    2 1993130       0-4   sector_00  unvaccinated susceptible
#> 4    3 1993129       0-4   sector_00  unvaccinated susceptible
#> 5    4 1993127       0-4   sector_00  unvaccinated susceptible
#> 6    5 1993124       0-4   sector_00  unvaccinated susceptible
```
