# Get life-years lost by demographic group.

Get life-years lost by demographic group.

## Usage

``` r
get_life_years_lost(output, groups = c("none", "age_group"))
```

## Arguments

- output:

  A `<daedalus_output>` object.

- groups:

  Whether to get the life-years lost by age group. Selecting `"none"`
  gives the total life-years lost.

## Value

A `<data.frame>` with the number of life-years lost, optionally per
age-group.
