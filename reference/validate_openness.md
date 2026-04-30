# Check openness matrix

Check openness matrix

## Usage

``` r
validate_openness(x, settings)
```

## Arguments

- x:

  A numeric vector or matrix to be validated as an accepted way of
  specifying contacts scaling.

- settings:

  A number for the number of settings

## Value

A numeric matrix of dimensions 49 x settings. Throws errors if `x` does
not conform to expectations.
