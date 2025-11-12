# Get a vector of state indices

A helper function to get a vector of state indices for use in the
response sub-class constructors.

## Usage

``` r
get_state_indices(state_name, country)
```

## Arguments

- state_name:

  The state name as a string.

- country:

  The country as a `<daedalus_country>`. Needed to correctly calculate
  the number of age and economic sector groups.

## Value

A vector of numbers representing indices.
