# Create and work with `<daedalus_output>` objects

Create and work with `<daedalus_output>` objects

Validate a potential `<daedalus_output>` class object

Check if an object is of the `<daedalus_output>` class

Print `<daedalus_output>` class objects

## Usage

``` r
as_daedalus_output(x)

validate_daedalus_output(x)

is_daedalus_output(x)

# S3 method for class 'daedalus_output'
print(x, ...)
```

## Arguments

- x:

  An object of the `<daedalus_output>` class.

- ...:

  Not used; added for compatibility with the generic.

## Value

An object of the `<daedalus_output>` class.

Invisibly returns `x`; called primarily for its side effects of erroring
when the object does not satisfy the `<daedalus_output>` class
requirements.

A logical for whether `x` is of the `<daedalus_output>` class.

None; called for its printing side effects.
