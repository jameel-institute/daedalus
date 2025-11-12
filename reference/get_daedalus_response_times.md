# Get model response times from dust2 output

Get model response times from dust2 output

## Usage

``` r
get_daedalus_response_times(output)
```

## Arguments

- output:

  dust2 output
  [`daedalus_internal()`](https://jameel-institute.github.io/daedalus/reference/daedalus_internal.md).

## Value

A list of event start and end times, closure periods, and the duration
of each closure event, suitable for a `<daedalus_output>` object.
