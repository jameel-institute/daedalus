# Reshape output data

An internal function to help
[`prepare_output()`](https://jameel-institute.github.io/daedalus/reference/prepare_output.md)
process lists of data.

## Usage

``` r
out_list_to_df(output, new_vaccinations, timesteps, labels)
```

## Arguments

- output:

  A list-like object originating from
  [`daedalus_internal()`](https://jameel-institute.github.io/daedalus/reference/daedalus_internal.md)
  and partially processed within
  [`prepare_output()`](https://jameel-institute.github.io/daedalus/reference/prepare_output.md).

- new_vaccinations:

  A list object of new vaccination data origination in
  [`daedalus_internal()`](https://jameel-institute.github.io/daedalus/reference/daedalus_internal.md).

- timesteps:

  A vector of timesteps, typically starting with 0.

- labels:

  A list of labels to be applied to the data.

## Value

A `<data.frame>` suitable for passing to a `<daedalus_output>` object.
