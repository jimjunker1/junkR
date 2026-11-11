# %ni%

This function is the opposite of `%in%` in that it finds items not in a
vector

## Usage

``` r
x %ni% table
```

## Arguments

- x:

  vector or NULL: the values to exclude. Long vectors are supported.

- table:

  vector or NULL: the values to be excluded against. Long vectors are
  not supported. \#' @returns A logical vector, indicating if a match
  was not located for each element of x: thus the values are TRUE or
  FALSE and never NA.
