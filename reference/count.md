# A more versatile form of the T-SQL `count()` function.

Implementation of T-SQL `count` and Excel `COUNTIF` functions. Shows the
total number of elements in any number of data objects altogether or
that match a condition.

## Usage

``` r
count(..., condition = (function(x) TRUE))
```

## Arguments

- ...:

  an arbitrary number of `R` objects

- condition:

  a 1 argument condition

## Examples

``` r
count(c(NA,1,2))
#> [1] 3
count(c(NA,1,2),is.na)
#> [1] 4
count(c(NA,1,2),list('A',4),cbind(1,2,3))
#> [1] 8
count(c(NA,1,2),list('A',4),cbind(1,2,3),condition=is.character)
#> [1] 1
```
