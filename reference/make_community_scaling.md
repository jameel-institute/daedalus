# Prepare community contacts scaling from workplace scaling

Helper function to determine the (uniform) community contacts scaling,
based on the scaling of workplace contacts. Higher mean workplace
contacts scaling leads to higher community contacts scaling. Workplace
contacts scaling mean is not weighted by worker counts.

## Usage

``` r
make_community_scaling(x)
```

## Arguments

- x:

  A numeric vector of workplace contacts scaling, with each element in
  the range \\\[0, 1\]\\. Expected to be length 45 (N_ECON_SECTORS), but
  this is not checked.

## Value

A numeric vector of length 49 (N_AGE_GROUPS + N_ECON_SECTORS).
