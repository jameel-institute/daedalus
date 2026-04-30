# Get pandemic fiscal costs from a model run

A helper function that post-processes a `<daedalus_output>` object to
calculate the costs to a national government (fiscal costs) of
responding to a pandemic. Includes costs of economic support,
vaccinations given, and NPIs administered or implemented.

## Usage

``` r
get_fiscal_costs(
  x,
  support_level = 0.2,
  price_vax = 1,
  price_npi = 1,
  uptake_npi = 1,
  interest_rate = 4,
  tax_rate = 35,
  spending_rate = 45,
  starting_debt = 0,
  productivity_loss_infection = 1
)
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

- support_level:

  The proportion of pandemic-related economic losses that a government
  compensates, as a proportion.

- price_vax:

  The per-dose price of vaccination.

- price_npi:

  The per-day and per-person price of implementing any pandemic
  response. May include costs such as testing or masks.

- uptake_npi:

  The extent to which NPIs are taken up by the population; essentially a
  number that modifies (reduces) the total cost of implementing an NPI.

- interest_rate:

  The annual interest rate on government borrowing for pandemic
  response.

- tax_rate:

  The annual mean tax rate on economic output; used to calculate
  government revenue.

- spending_rate:

  The annual mean rate of public spending as a percentage of GDP.

- starting_debt:

  The value of national starting debt. Currently assumed to be zero
  while country-specific data are collected.

- productivity_loss_infection:

  A single number in the range \\\[0, 1\]\\ giving the loss in
  productivity associated with symptomatic infection. Currently defaults
  to 1.0 for compatibility with earlier function versions.

## Value

A two-element list giving:

- A list of `fiscal_costs` with elements giving the fiscal costs
  \\\text{TCG}\_t\\ and a breakdown of these costs, as well as interest;

- A list of `public_debt` of the public debt \\b_t\\, which is the net
  of baseline public spending, pandemic response costs, and
  pandemic-impacted revenue.

## Details

Default argument values are intended to be representative. This function
is intended to be called after
[`daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.md)
and parameters required to compute fiscal costs may need to be bundled
along with model outputs. Note that all rates (interest rate, spending
rate, and tax rate) are given as annual percentages. Only the interest
rate is converted to an daily value from an annual one for use in
compounding.

### Public spending

Spending is calculated as:

\$\$G_t = \sigma(\text{GVA}^\* - \text{GVA}\_t) + Cv_t + Cp_t\$\$

where \\Cv_t\\ is the time-specific cost of vaccination, and is
calculated as the cost of new vaccinations in each timestep: \\P_v
\times \delta V_t\\ .

\\Cp_t\\ is the time-specific cost of implementing pandemic response,
and is calculated as \\\psi (\bar N - D_t) P_p\\, where \\\bar N - D_t\\
is the remaining number of individuals in the population, and \\\psi\\
is the proportion taking up any protection offered by the response.

### Interest on spending and fiscal cost

We assume that the government borrows to spend on pandemic mitigation
measures outlined above such that the total cost to the public is then

\$\$\text{TCG}\_t = G_t + (1 + R_t^T) \text{TCG}\_{t - 1}\$\$

where \\R_t^T\\ is the daily rate of interest to be paid on the borrowed
amount.

The interest rate is modelled as being constant over time. Users pass
the annual rate of interest as a percentage, and this is converted to a
daily rate using the internal function
[`annual_rate_daily()`](https://jameel-institute.github.io/daedalus/reference/tools.md)
as \\(1 + R_t^T)^{1 / 365} - 1\\.

### Total public debt

The total public debt at the end of the pandemic \\b_t\\ is then the sum
of :

- total public spending on the pandemic \\\text{TCG}\_t\\,

- existing day-to-day public spending \\\bar G\\ which is assumed to be
  a fraction of daily GDP \\\nu \text{GDP}\\,

- existing debt owed due to past daily spending (including on pandemic
  mitigation), and interest to be paid on the debt,

- less the revenues collected from taxation, \\\mu \text{GVA}\_{t-1}\\,
  where \\\mu\\ is the mean rate of taxation.

The daily GVA is the pre-pandemic GVA scaled by the available labour
supply during the pandemic, taking into account labour restrictions due
to illness-related absences and deaths, and response-related
restrictions.

GDP is calculated as the sum of sector-specific daily GVA, and existing
debt is currently assumed to be zero and is not included in the
equation.

\$\$b_t = \bar G + \text{TCG}\_{t-1} + (1 + R\_{t-1}^T) b\_{t-1} - \mu
\text{GVA}\_{t-1} \$\$

## Examples

``` r
# get fiscal costs for UK with SARS-CoV-2
# and both closures and vaccinations
o <- daedalus(
  "GBR", "sars_cov_2_pre_alpha",
  "economic_closures", "high",
  time_end = 100
)
fc <- get_fiscal_costs(o)

# also works when no closures are applied
o <- daedalus(
  "CAN", "influenza_2009",
  time_end = 30
)

# Compare public debt added estimates with other estimates from Covid-19:
# https://www.ctf.ca/EN/EN/Newsletters/Perspectives/2020/3/200302.aspx
get_fiscal_costs(o)
#> $fiscal_costs
#> $fiscal_costs$fiscal_cost
#>  [1] 0.000000e+00 8.774001e-05 1.752632e-04 2.936561e-04 4.556386e-04
#>  [6] 6.731565e-04 9.615989e-04 1.341548e-03 1.840368e-03 2.494193e-03
#> [11] 3.350539e-03 4.471758e-03 5.939575e-03 7.861045e-03 1.037636e-02
#> [16] 1.366907e-02 1.797950e-02 2.362227e-02 3.100924e-02 4.067963e-02
#> [21] 5.333927e-02 6.991219e-02 9.160797e-02 1.200100e-01 1.571907e-01
#> [26] 2.058631e-01 2.695777e-01 3.529814e-01 4.621554e-01 6.050562e-01
#> [31] 7.920933e-01
#> 
#> $fiscal_costs$gva_support
#>  [1] 8.774001e-05 8.751378e-05 1.183740e-04 1.619509e-04 2.174690e-04
#>  [6] 2.883700e-04 3.798457e-04 4.986761e-04 6.536268e-04 8.560780e-04
#> [11] 1.120859e-03 1.467337e-03 1.920831e-03 2.514468e-03 3.291598e-03
#> [16] 4.308962e-03 5.640834e-03 7.384439e-03 9.667051e-03 1.265527e-02
#> [21] 1.656719e-02 2.168826e-02 2.839214e-02 3.716787e-02 4.865547e-02
#> [26] 6.369249e-02 8.337478e-02 1.091360e-01 1.428512e-01 1.869721e-01
#> [31] 2.447033e-01
#> 
#> $fiscal_costs$vax_support
#>  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#> 
#> $fiscal_costs$npi_support
#> [1] 0
#> 
#> $fiscal_costs$interest_value
#> [1] 2.310547
#> 
#> 
#> $public_debt
#> $public_debt$public_debt
#>  [1]     0.0000   438.7002   877.4476  1316.2422  1755.0841  2193.9732
#>  [7]  2632.9097  3071.8936  3510.9249  3950.0038  4389.1303  4828.3047
#> [13]  5267.5271  5706.7978  6146.1171  6585.4855  7024.9035  7464.3720
#> [19]  7903.8919  8343.4644  8783.0914  9222.7751  9662.5182 10102.3247
#> [25] 10542.1995 10982.1492 11422.1822 11862.3098 12302.5465 12742.9114
#> [31] 13183.4295
#> 
#> $public_debt$added_public_debt
#> [1] 13183.43
#> 
#> 
```
