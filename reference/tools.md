# Helper functions

`weighted_rowsums()` is used to get the row-wise sums of a matrix after
column-wise multiplication by a vector of weights.

`interest_accumulation()` is used to calculate the new principal after
assuming an interest `rate` and with some external `contribution`. Used
in
[`get_fiscal_costs()`](https://jameel-institute.github.io/daedalus/reference/get_fiscal_costs.md)
when the borrowed principal to fund pandemic response both compounds
over time, and increases due to fresh borrowing in each timestep.

`annual_rate_daily()` is used to convert an annual rate of interest to a
daily rate, for use in `interest_accumulation()` and eventually in
[`get_fiscal_costs()`](https://jameel-institute.github.io/daedalus/reference/get_fiscal_costs.md).

## Usage

``` r
drop_null(x)

first(x)

last(x)

weighted_rowsums(x, weights)

interest_accumulation(principal, contribution, rate)

annual_rate_daily(x)
```
