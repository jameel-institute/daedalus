# Dummy vaccination

The efficacy of a dummy vaccination object is set to 50% as a stop-gap
implementation of pre-existing population immunity. In scenarios where a
'true' vaccination scenario is to be passed, it doesn't matter. In
scenarios where no vaccination is intended, it allows any individuals
with pre-existing immunity to be only partially susceptible, while still
preventing any model-time vaccinations as the `rate` is set to 0.

## Usage

``` r
dummy_vaccination(country)
```

## Value

A `daedalus_vaccination` object intended to have no effect; vaccination
rate and efficacy are set to zero.
