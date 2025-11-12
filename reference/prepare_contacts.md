# Make large contact matrix for Cpp model.

Make large contact matrix for Cpp model.

## Usage

``` r
make_conmat_large(country)

make_work_contacts(country)

make_consumer_contacts(country)

make_full_contacts(country)
```

## Arguments

- country:

  A `<daedalus_country>`.

## Value

1.  `make_conmat_large()` returns a 49x49 contact matrix scaled by the
    size of demography groups.

2.  `make_work_contacts()` returns a 45-element vector (for the number
    of economic sectors) scaled by the number of workers per sector.

3.  `make_consumer_contacts()` returns a 45x4 contact matrix with each
    row scaled by the number of workers per sector. Dimensions are the
    number of economic sectors and the number of age groups.
