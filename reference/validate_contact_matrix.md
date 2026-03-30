# Validate contact matrix

Validate contact matrix

## Usage

``` r
validate_contact_matrix(x, name)
```

## Arguments

- x:

  An object to be validated as a contact matrix or list of matrices.

- name:

  A country name

## Value

If `x` is a list of numeric matrices, or a numeric matrix meeting
expectations for Daedalus contact matrices, or `NULL`, returns a list of
contact matrices. If a single matrix was passed, returns a
single-element list with the name `"total"`.

If `x` is `NULL`, returns the country default contact matrix from
daedalus.data as a single-element list.

Returns errors if expectations fail.
