# Calculate the effective R

A simple calculation for the next-generation matrix, allowing for
heterogeneity in social contacts and optionally, a reduction in the
proportion of each age group remaining susceptible.

This assumes a single susceptible stratum with full susceptibility, and
will be updated in future versions to account for the addition of
vaccination strata with reduced susceptibility.

## Usage

``` r
get_ngm(country, infection, p_susc = NULL)
```

## Arguments

- country:

  A `<daedalus_country>` object.

- infection:

  A `<daedalus_infection>` object.

- p_susc:

  A sequence of length 4 (the number of demographic groups) giving the
  proportion of susceptibles in each demographic group.

## Value

A numeric matrix of dimensions \\\[4, 4\]\\ where the number of rows and
columns is the number of demographic groups.

## Details

Follows the methods in Diekmann and Hesterbeek (2010) J. Roy. Soc.
Interface, \<doi.org/10.1098/rsif.2009.0386\>.
