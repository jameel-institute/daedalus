# Get epidemic costs from a DAEDALUS model run

Get epidemic costs from a DAEDALUS model run

## Usage

``` r
get_costs(
  x,
  summarise_as = c("none", "total", "domain"),
  productivity_loss_infection = 1
)
```

## Arguments

- x:

  A `<daedalus_output>` object from a call to
  [`daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.md).

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

## Value

A list of different cost values, including the total cost. See
**Details** for more information.

## Details

The total cost in million dollars is returned as `total_cost`. This is
comprised of the following costs.

### Economic costs

A three element list of `economic_cost_total`, the total costs from
pandemic impacts on economic sectors, including both costs of lost gross
value added (GVA) due to pandemic-control restrictions or closures
(`economic_cost_closures`), and pandemic-related absences due to illness
and death (`economic_cost_absences`).

### Educational costs

A three element list of `education_cost_total`, the total costs from
pandemic impacts on education due to pandemic-control restrictions or
closures (`education_cost_closures`), and pandemic-related absences due
to illness and death (`education_cost_absences`).

### Life-value lost

A four-element vector (for the number of age groups) giving the value of
life-years lost per age group. This is calculated as the life-expectancy
of each age group times the value of a statistical life, with all years
assumed to have the same value.

### Life-years lost

A four-element vector (for the number of age groups) giving the value of
life-years lost per age group. This is calculated as the life-expectancy
of each age group times the number of deaths in that age group. No
quality adjustment is applied.

## Examples

``` r
output <- daedalus("Canada", "influenza_1918")

get_costs(output)
#> $total_cost
#> [1] 1469908
#> 
#> $economic_costs
#> $economic_costs$economic_cost_total
#> [1] 29756.42
#> 
#> $economic_costs$economic_cost_closures
#> [1] 0
#> 
#> $economic_costs$economic_cost_absences
#> [1] 29756.42
#> 
#> $economic_costs$sector_cost_closures
#>  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#> [39] 0 0 0 0 0 0 0
#> 
#> $economic_costs$sector_cost_absences
#>  [1]  507.31406   45.92487  897.03024  365.65928  197.76757  533.77293
#>  [7]   38.32229  159.72376  197.01777  170.55792  236.51218   82.10726
#> [13]  156.96928  119.57756  191.16098  227.73294   98.41081   64.25801
#> [19]  247.65010  290.94969  181.08177  263.77509  636.12711  161.68246
#> [25] 2082.19999 3711.38190  814.85766   25.90400  157.59367  360.15513
#> [31]  141.91605  787.15257  299.19807  566.51980  586.37225 2286.13777
#> [37] 3941.55682 1417.93072 1043.81615 2288.65292 1897.38174 2522.06199
#> [43]  270.38851  334.04600   47.49147
#> 
#> 
#> $education_costs
#> $education_costs$education_cost_total
#> [1] 1897.382
#> 
#> $education_costs$education_cost_closures
#> [1] 0
#> 
#> $education_costs$education_cost_absences
#> [1] 1897.382
#> 
#> 
#> $life_value_lost
#> $life_value_lost$life_value_lost_total
#> [1] 1438254
#> 
#> $life_value_lost$life_value_lost_age
#>      0-4     5-19    20-64      65+ 
#> 274625.5 597520.1 375965.7 190142.4 
#> 
#> 
#> $life_years_lost
#> $life_years_lost$life_years_lost_total
#> [1] 31232438
#> 
#> $life_years_lost$life_years_lost_age
#>      0-4     5-19    20-64      65+ 
#>  5963637 12975463  8164294  4129043 
#> 
#> 
```
