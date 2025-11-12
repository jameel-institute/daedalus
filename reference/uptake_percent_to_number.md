# Convert an uptake limit from a percentage to a number

Convert an uptake limit from a percentage to a number

## Usage

``` r
uptake_percent_to_number(uptake_limit, country)
```

## Arguments

- uptake_limit:

  A single number giving the upper limit for the *percentage* of the
  population that can be vaccinated. When this limit is reached,
  vaccination ends. Passed to the `value_state_off` argument in
  [`new_daedalus_response()`](https://jameel-institute.github.io/daedalus/reference/class_response.md)
  via the class constructor `new_daedalus_vaccination()`.

- country:

  A `<daedalus_country>` object or a 2- or 3-character string that can
  be coerced to a `<daedalus_country>` (e.g. `"GBR"` for the United
  Kingdom). Used to determine when vaccination should end.

## Value

A single number giving the total number of individuals expected to take
up vaccination. If `country` is `NULL`, the function returns `NULL`.
