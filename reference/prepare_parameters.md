# Prepare infection parameters for model

Generic for the prepare_parameters methods associated with daedalus
classes.

## Usage

``` r
# S3 method for class 'daedalus_behaviour'
prepare_parameters(x)

# S3 method for class 'daedalus_country'
prepare_parameters(x, ...)

# S3 method for class 'daedalus_infection'
prepare_parameters(x, ...)

# S3 method for class 'daedalus_vaccination'
prepare_parameters(x, ...)

prepare_parameters(x, ...)
```

## Arguments

- x:

  An S3 object with an appropriate method.

- ...:

  Not used; included for compatibility with methods.

## Value

A list of parameters suitable for the DAEDALUS model.
`prepare_parameters.daedalus_country()` returns the country parameters,
while `prepare_parameters.daedalus_infection()` returns infection
parameters.

## Details

### Country parameters

Country contact data is processed as follows:

- `contact_matrix`: scaled by its leading eigenvalue, and with each
  column `j` scaled by the `j`-th element of the country demography
  vector (i.e., scaling contacts from each age group by the size of that
  group).

The returned parameter list consists of:

- `demography`: the demography vector;

- `contact_matrix`: the contact matrix;

- `contacts_workplace`: the contacts in workplaces scaled by the number
  of workers in each sector;

- `contacts_consumer_worker`: contacts in workplaces distributed in
  proportion to the demography distribution, and scaled by the largest
  singular value (similar to eigenvalue for non-square matrices).

### Infection parameters

Infection parameters are returned from `<daedalus_infection>` objects
without modification and only the name removed.
