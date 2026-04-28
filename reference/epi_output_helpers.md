# Calculate daily incidences and summarise epidemic measures

Functions to quickly summarise timeseries data from
[`daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.md)
to provide daily values for infections, hospitalisations, deaths, and
vaccinations, while allowing grouping by different strata.

## Usage

``` r
get_incidence(
  data,
  measures = c("infections", "hospitalisations", "deaths"),
  groups = NULL
)

get_epidemic_summary(
  data,
  measures = c("infections", "hospitalisations", "deaths"),
  groups = NULL
)

get_new_vaccinations(data, groups = NULL)
```

## Arguments

- data:

  Either a `<data.frame>` from a call to
  [`get_data()`](https://jameel-institute.github.io/daedalus/reference/get_data.md)
  on a `<daedalus_output>` object, or such an object directly.

- measures:

  A character vector of one or more of the following, passed to
  `get_incidence()` and `get_epidemic_summary()`: `"infections"`,
  `"hospitalisations"` or `"deaths"` for the measure to return. Defaults
  to returning all three in long format.

  `get_daily_vaccinations()` does not accept a `measures` argument and
  only provides the number of daily vaccinations.

- groups:

  An optional character vector of grouping variables that correspond to
  model strata. Defaults to `NULL` which gives incidence across the
  whole population. Allowed groups correspond to modelled strata:
  `"age_group"`, `"vaccine_group"`, and `"econ_sector"`.

  `get_daily_vaccinations()` only accepts "`age_group`" and
  `"econ_sector"`.

## Value

A `<data.frame>` in long format, with one entry per model timestep,
measure, and group chosen.

- `get_incidence()` returns a data frame with the number of daily new
  infections, new hospitalisations, and/or new deaths in each of the
  groups specified by `groups`.

- `get_epidemic_summary()` returns a data frame with the total number of
  the value specified in `measure` for each of the groups specified by
  `groups`.

- `get_daily_vaccinations()` returns a data frame with columns for the
  number of new daily vaccination in each combination of `groups` if
  provided. Columns for the `groups` are added when `groups` are
  specified.

## Examples

``` r
data <- daedalus("Canada", "sars_cov_1")

# new infections
new_infections <- get_incidence(data, "infections")

# epidemic summary
get_epidemic_summary(
  data,
  groups = "age_group"
)
#>    age_group       value                measure
#> 1        0-4    37839.95           total_deaths
#> 2       5-19   661729.16           total_deaths
#> 3      20-64   786336.90           total_deaths
#> 4        65+   647168.87           total_deaths
#> 5        0-4   664904.91          epidemic_size
#> 6       5-19  3121753.84          epidemic_size
#> 7      20-64 13832927.42          epidemic_size
#> 8        65+  1521060.22          epidemic_size
#> 9        0-4    38254.80 total_hospitalisations
#> 10      5-19   669149.83 total_hospitalisations
#> 11     20-64   795855.98 total_hospitalisations
#> 12       65+   654205.70 total_hospitalisations

# get daily vaccinations
daily_vaccinations <- get_new_vaccinations(data)
```
