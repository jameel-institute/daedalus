# Run daedalus for multiple infection parameter sets

Run
[`daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.md)
for multiple parameter sets, with the intended use case of running the
model under uncertainty in infection parameters.

## Usage

``` r
daedalus_multi_infection(
  country,
  infection,
  response_strategy = NULL,
  vaccine_investment = NULL,
  behaviour = NULL,
  response_time = 30,
  initial_state_manual = NULL,
  time_end = 600,
  ...
)
```

## Arguments

- country:

  A country or territory object of class `<daedalus_country>`, **or** a
  country or territory name from those included in the package; see
  [daedalus.data::country_names](https://jameel-institute.github.io/daedalus.data/reference/country_names_codes.html),
  **or** a country ISO2 or ISO3 code; see
  [daedalus.data::country_codes_iso2c](https://jameel-institute.github.io/daedalus.data/reference/country_names_codes.html)
  and
  [daedalus.data::country_codes_iso3c](https://jameel-institute.github.io/daedalus.data/reference/country_names_codes.html).
  Country-specific data such as the community and workplace contacts,
  the demography, and the distribution of the workforce into economic
  sectors is automatically accessed from package data for the relevant
  country name if it is passed as a string. To override package defaults
  for country characteristics, pass a `<daedalus_country>` object
  instead. See
  [`daedalus_country()`](https://jameel-institute.github.io/daedalus/reference/class_country.md)
  for more.

- infection:

  A list of `<daedalus_infection>` objects. Must have a minimum length
  of 2.

- response_strategy:

  A string for the name of response strategy followed, a numeric of
  length 45 (number of economic sectors), or a `<daedalus_npi>` object.
  Defaults to "none". While the response strategy is active, economic
  contacts are scaled using the package data object
  [`daedalus.data::closure_strategy_data`](https://jameel-institute.github.io/daedalus.data/reference/closure_strategies.html).

- vaccine_investment:

  Either a single string or a `<daedalus_vaccination>` object specifying
  the vaccination parameters associated with an advance
  vaccine-investment scenario. Defaults to `NULL` for absolutely no
  vaccination in the model. A vaccination investment of `"none"`
  indicates no *prior* investment, but the model will include
  vaccination beginning after 1 year, at a low rate across all age
  groups. Other accepted values are `"low"`, `"medium"` and `"high"`.
  See
  [`daedalus_vaccination()`](https://jameel-institute.github.io/daedalus/reference/class_vaccination.md)
  for more information.

- behaviour:

  An optional object of class `<daedalus_behaviour>` which determines
  how population-level perception of epidemic signals affects infection
  transmission. May be `NULL` for no behavioural modification of
  infection transmission. See the `<daedalus_behaviour>` [class
  documentation](https://jameel-institute.github.io/daedalus/reference/class_behaviour.md)
  for more details on the available behavioural mechanisms

- response_time:

  A single numeric value for the time in days at which the selected
  response is activated. This is ignored if the response has already
  been activated by the hospitalisation threshold being reached.
  Defaults to 30 days. Responses have a default maximum duration of 365
  days. This can be changed by passing a `<daedalus_npi>` object to
  `response_strategy`.

- initial_state_manual:

  An optional **named** list with the names `p_infectious`,
  `p_asymptomatic`, and `p_immune`. `p_infectious` and `p_asymptomatic`
  give the proportion of infectious and symptomatic individuals in each
  age group and economic sector. Defaults to `1e-6` and `0.0`
  respectively. `p_immune` may be a single number in the range
  `0.0 <= p_immune <= 1.0` or a 4-element vector in that range (the
  number of age groups in the model), for the proportion of individuals
  in the population or in each age group that have some pre-existing
  immunity to infection (reduced susceptibility). See **Details** for
  more.

- time_end:

  An integer-like value for the number of timesteps at which to return
  data. This is treated as the number of days with data returned for
  each day. Defaults to 300 days.

- ...:

  Optional arguments that are passed to
  [`dust2::dust_ode_control()`](https://mrc-ide.github.io/dust2/reference/dust_ode_control.html).

## Value

A list of `<daedalus_output>` objects of the same length as `infection`.
