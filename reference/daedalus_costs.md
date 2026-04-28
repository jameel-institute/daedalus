# Get epidemic losses from a DAEDALUS model run

Calculate the costs of an epidemic and any mitigation measures, either
from a Daedalus model run, or from a timeseries of epidemic data and
relevant demographic and economic parameters.

## Usage

``` r
# S3 method for class 'daedalus_output'
get_costs(
  x,
  summarise_as = c("none", "total", "domain"),
  productivity_loss_infection = 1,
  ...
)

# S3 method for class 'data.frame'
get_costs(
  x,
  comp_non_working,
  comp_infected,
  comp_dead,
  workforce,
  daily_gva,
  productivity_loss_infection,
  vsl_by_age,
  life_expectancy,
  value_school_year,
  n_students,
  edu_effectiveness_remote,
  npi_data = NULL,
  summarise_as = c("none", "total", "domain"),
  ...
)

get_costs(x, ...)
```

## Arguments

- x:

  A `<daedalus_output>` object from a call to
  [`daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.md),
  or a `<data.frame>` of an epidemic timeseries.

  If `x` is a `<data.frame>`, it must have the columns `"time"`,
  `"compartment"`, `"age_group"`, `"econ_sector"` and `"value"`, giving
  the model time, the epidemiological compartment, and the number of
  individuals of each age group and economic sector in each compartment
  at each time. See **Details** for more on the expectations around how
  this dataset is organised.

- summarise_as:

  A string from among "none", "total", or "domain", for how the costs
  should be returned. Select "none", the default, for the raw costs
  along with overall and domain-specific totals; "total" for the overall
  cost, and "domain" for the total costs per domain; the domains are
  'economic', 'education', and 'life years'.

- productivity_loss_infection:

  A single number in the range \\\[0, 1\]\\ giving the loss in
  productivity associated with symptomatic infection. Currently defaults
  to 1.0 for compatibility with earlier function versions.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Other arguments to class methods.

- comp_non_working:

  A character vector giving the names of compartments in which
  individuals are assumed to have a reduced ability to work. This may
  (and should) include compartments where individuals are infected and
  symptomatic, as well as hospitalised or dead.

- comp_infected:

  A character vector giving the name of compartments in which
  individuals are assumed to be infected with reduced ability to work.
  This should **not** include compartments where individuals are assumed
  to have **no** ability to work (hospitalised or dead).

- comp_dead:

  A string giving the name of the compartment holding the count of
  deaths. Note that only a single deaths compartment is supported, so
  models with multiple death states should pass the **final state**.
  Used to calculate the value of lives lost.

- workforce:

  A numeric vector of the number of workers in each economic sector of
  the population. Must be absolute numbers (i.e., not scaled to
  thousands or millions). Must have the same length as `daily_gva`.

- daily_gva:

  A numeric vector of the daily gross-value added (GVA) in million
  dollars, by each economic sector of the population (or territory or
  state) in which the epidemic occurs.

- vsl_by_age:

  A numeric vector of the value of a statistical life (VSL) for each age
  age group in `x`.

- life_expectancy:

  A numeric vector of the life-expectancy per age group. Expected to be
  the same length as `vsl_by_age`.

- value_school_year:

  A single number giving the value of a school year. Users can use the
  helper function
  [`get_value_school_year()`](https://jameel-institute.github.io/daedalus/reference/get_value_school_year.md)
  if a population's gross income per-capita is known to apply the
  internal Daedalus model for valuing a school year, or a value obtained
  in some other way.

- n_students:

  A single number for the number of students in the population, whose
  education is expected to be impacted by the epidemic.

- edu_effectiveness_remote:

  A single number for the effectiveness of remote education, in the
  range \\\[0, 1\]\\.

- npi_data:

  A list with numeric elements that mirrors the elements of a
  `<daedalus_npi>`, with the following names:

  - `npi_times_start`: A numeric vector of the start times of any NPIs
    modelled.

  - `npi_times_end`: A numeric vector of the end times of modelled NPIs.

  - `npi_durations`: A numeric vector of the durations of NPIs.

  - `npi_periods`: A numeric vector giving integer-ish elements in the
    ranges specified by the start and end times.

  - `openness`: Either a numeric vector (if modelling a single NPI), or
    a list of numeric vectors (if modelling multiple NPIs), giving the
    openness coefficients of NPI regimes. Vector lengths must be the
    same as `workforce`.

## Value

A list of different cost values, including the total cost. See
**Details** for more information.

## Details

### Expectations for non-Daedalus model data

When a `<data.frame> x` is passed, it must have at least one row, at
least some non-missing values, and:

1.  Columns `"time"`, `"age_group"`, `"compartment"`, `"econ_sector"`,
    and `"value"`, giving the model time (numeric), age group identifier
    (character), epidemiological compartment identifier (character),
    economic sector identifier (character), and the value of the number
    of individuals in each age, epi-compartment, and economic group at
    each time (numeric). Numeric values must be non-negative \\\> 0\\,
    non-missing, and finite.

2.  Compartments in `comp_infected`, `comp_non_working`, and `comp_dead`
    must be found in `x$compartment`.

3.  All compartments of `comp_infected` and `comp_dead` must be in
    `comp_non_working`.

### Output

The total cost in million dollars is returned as `total_cost`. This is
comprised of the following costs.

#### Economic costs

A three element list of `economic_cost_total`, the total costs from
pandemic impacts on economic sectors, including both costs of lost gross
value added (GVA) due to pandemic-control restrictions or closures
(`economic_cost_closures`), and pandemic-related absences due to illness
and death (`economic_cost_absences`).

#### Educational costs

A three element list of `education_cost_total`, the total costs from
pandemic impacts on education due to pandemic-control restrictions or
closures (`education_cost_closures`), and pandemic-related absences due
to illness and death (`education_cost_absences`).

#### Life-value lost

A four-element vector (for the number of age groups) giving the value of
life-years lost per age group. This is calculated as the life-expectancy
of each age group times the value of a statistical life, with all years
assumed to have the same value.

#### Life-years lost

A four-element vector (for the number of age groups) giving the value of
life-years lost per age group. This is calculated as the life-expectancy
of each age group times the number of deaths in that age group. No
quality adjustment is applied.

## Examples

``` r
# for <daedalus_output> objects from `daedalus()`
o <- daedalus("CAN", "sars_cov_1", time_end = 100)
get_costs(o, "domain")
#>     economic    education   life_value   life_years 
#>   0.71370354   0.06463232  36.94926465 802.37273941 

# for a data.frame of epi compartment timeseries, as from any epi model
output <- daedalus("Canada", "influenza_1918", time_end = 100)

data <- get_data(output)
comp_non_working <- c(
  "infect_symp",
  "hospitalised_recov",
  "hospitalised_death",
  "dead"
)
comp_infected <- "infect_symp"
comp_dead <- "dead"

daily_gva <- output$country$gva
workforce <- output$country$workers
vsl_by_age <- output$country$vsl
life_expectancy <- output$country$life_expectancy

value_school_year <- 1e6 # 1 million dollars
n_students <- output$country$demography[3L]

edu_effectiveness_remote <- 0.33

productivity_loss_infection <- 1.0

get_costs(
  data,
  comp_non_working,
  comp_infected,
  comp_dead,
  daily_gva,
  workforce,
  vsl_by_age,
  life_expectancy,
  value_school_year,
  n_students,
  productivity_loss_infection = productivity_loss_infection,
  edu_effectiveness_remote = edu_effectiveness_remote,
  summarise_as = "domain"
)
#>     economic    education   life_value   life_years 
#> 239704139047  21246144137      2209410     47978495 
```
