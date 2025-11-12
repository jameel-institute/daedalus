# Set parameters in DAEDALUS classes

Generic and methods for S3 classes for safely setting class parameters.
Only parameters considered safe to change – mostly contact data in the
`<country>`, but all parameters in `<infection>` – can be changed in
this way.

## Usage

``` r
# S3 method for class 'daedalus_country'
set_data(x, ...)

# S3 method for class 'daedalus_infection'
set_data(x, ...)

# S3 method for class 'daedalus_vaccination'
set_data(x, ...)

set_data(x, ...)
```

## Arguments

- x:

  An S3 class object from the daedalus package of the
  `<daedalus_country>` or `<infection>` class.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Named optional arguments for parameters to be changed, with their new
  values. The only values allowed for `<daedalus_country>` objects are
  "contact_matrix", "contacts_workplace", and
  "contacts_consumer_worker".

## Value

An S3 object of the same class as input `x`.

## Examples

``` r
# simple example of setting all contacts to 1
country_A <- daedalus_country("United Kingdom")
country_A
#> <daedalus_country>
#> • Name: United Kingdom
#> • Demography: 3924490, 11762039, 39536463, and 12663012
#> • Community contact matrix:
#>             0-4      5-19    20-64       65+
#> 0-4   1.9157895 1.5379290 4.704999 0.2863619
#> 5-19  0.5131412 8.7339228 5.874591 0.7418483
#> 20-64 0.4670302 1.7476822 7.830182 1.0685802
#> 65+   0.1180517 0.7548304 3.531487 1.5212437
#> • GNI (PPP $): 45870
#> • Hospital capacity: 26219

country_A <- set_data(country_A, contact_matrix = matrix(1, 4, 4))
country_A
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

disease_x <- daedalus_infection("sars_cov_1")
disease_x <- set_data(disease_x, r0 = 3.0)
disease_x
#> <daedalus_infection>
#> • Epidemic name: sars_cov_1
#> • R0: 3
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
```
