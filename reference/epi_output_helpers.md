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

get_attack_rate(
  data,
  measures = c("infections", "hospitalisations", "deaths"),
  groups = c("econ_sector", "age_group")
)
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

  `get_attack_rate()` does not support `"vaccine_group"` due to a
  shifting baseline effect.

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

- `get_attack_rate()` returns a data frame similar to
  `get_epidemic_summary()`, but with a `p_affected` column giving the
  value as a proportion of the initial group size (capped at 1.0).

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
#> 1        0-4    75647.62           total_deaths
#> 2       5-19  1143409.25           total_deaths
#> 3      20-64  1291895.30           total_deaths
#> 4        65+  1369148.76           total_deaths
#> 5        0-4  1451572.73          epidemic_size
#> 6       5-19  5840351.78          epidemic_size
#> 7      20-64 25142970.11          epidemic_size
#> 8        65+  3458086.79          epidemic_size
#> 9        0-4    80712.81 total_hospitalisations
#> 10      5-19  1216688.98 total_hospitalisations
#> 11     20-64  1393901.44 total_hospitalisations
#> 12       65+  1451882.99 total_hospitalisations

# get daily vaccinations
daily_vaccinations <- get_new_vaccinations(data)

# get attack rate
get_attack_rate(data, "infections", groups = "age_group")
#>   age_group    value       measure p_affected
#> 1       0-4  1451573 epidemic_size  0.7282873
#> 2      5-19  5840352 epidemic_size  0.9817187
#> 3     20-64 25142970 epidemic_size  1.0000000
#> 4       65+  3458087 epidemic_size  0.5060881
```
