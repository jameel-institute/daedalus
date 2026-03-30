# Make large contact matrix for Cpp model.

Make large contact matrix for Cpp model.

## Usage

``` r
make_conmat_large(country, scaling = c("demography", "none"))

make_work_contacts(country)

make_consumer_contacts(country)

make_full_contacts(country)
```

## Arguments

- country:

  A `<daedalus_country>`.

- scaling:

  A string, either `"demography"` (default) or `"none"`, for whether the
  matrix should be scaled by the relevant demographic (age or economic
  sector) group. Scaling by demography is used as a substitute for
  division by the relevant population size \\N\\ in the force of
  infection calculation in the ODE RHS, to save computation.

## Value

1.  `make_conmat_large()` returns an array of 49x49xN contact matrices
    scaled by the size of demography and economic sector groups, while
    splitting up community contacts from working age individuals to
    other age groups, among the different age groups in proportion to
    workers in those sectors. Each index along the third dimension
    represents a contact setting.

2.  `make_work_contacts()` returns a 45-element vector (for the number
    of economic sectors) scaled by the number of workers per sector.

3.  `make_consumer_contacts()` returns a 45x4 contact matrix with each
    row scaled by the number of workers per sector. Dimensions are the
    number of economic sectors and the number of age groups.
