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
  [`daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.md).

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
#>  [1] 0.000000e+00 8.774001e-05 1.634749e-04 2.483235e-04 3.499055e-04
#>  [6] 4.733630e-04 6.239538e-04 8.078276e-04 1.032432e-03 1.306873e-03
#> [11] 1.642306e-03 2.052410e-03 2.553944e-03 3.167439e-03 3.918038e-03
#> [16] 4.836528e-03 5.960597e-03 7.336384e-03 9.020371e-03 1.108170e-02
#> [21] 1.360502e-02 1.669393e-02 2.047529e-02 2.510436e-02 3.077124e-02
#> [26] 3.770862e-02 4.620136e-02 5.659815e-02 6.932586e-02 8.490701e-02
#> [31] 1.039812e-01
#> 
#> $fiscal_costs$gva_support
#>  [1] 8.774001e-05 7.572551e-05 8.483100e-05 1.015553e-04 1.234199e-04
#>  [6] 1.505399e-04 1.838068e-04 2.245179e-04 2.743296e-04 3.352932e-04
#> [11] 4.099271e-04 5.013132e-04 6.132206e-04 7.502593e-04 9.180685e-04
#> [16] 1.123549e-03 1.375146e-03 1.683199e-03 2.060361e-03 2.522124e-03
#> [21] 3.087453e-03 3.779563e-03 4.626876e-03 5.664182e-03 6.934072e-03
#> [26] 8.488681e-03 1.039183e-02 1.272163e-02 1.557370e-02 1.906508e-02
#> [31] 2.333901e-02
#> 
#> $fiscal_costs$vax_support
#>  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#> 
#> $fiscal_costs$npi_support
#> [1] 0
#> 
#> $fiscal_costs$interest_value
#> [1] 0.4347646
#> 
#> 
#> $public_debt
#> $public_debt$public_debt
#>  [1]     0.0000   438.7002   877.4476  1316.2421  1755.0838  2193.9727
#>  [7]  2632.9089  3071.8923  3510.9230  3950.0009  4389.1262  4828.2989
#> [13]  5267.5190  5706.7865  6146.1016  6585.4643  7024.8747  7464.3329
#> [19]  7903.8391  8343.3934  8782.9961  9222.6473  9662.3475 10102.0969
#> [25] 10541.8961 10981.7456 11421.6461 11861.5985 12301.6037 12741.6630
#> [31] 13181.7780
#> 
#> $public_debt$added_public_debt
#> [1] 13181.78
#> 
#> 
```
