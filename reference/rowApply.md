# Applies a function row-wise on any data object.

Essentially functions as a `MARGIN=1`
[`apply`](https://rdrr.io/r/base/apply.html) apply but also works on
data objects without 2 dimensions such as lists and vectors.

## Usage

``` r
rowApply(data, fun, ...)
```

## Arguments

- data:

  any `R` object

- fun:

  the function to evaluate

- ...:

  additional arguments to pass to `fun`

## Examples

``` r
rowApply(list(1,2,3),function (x) sum(unlist(x)))
#> [1] 1 2 3
df<-data.frame(a=c(1,2,3),b=c(1,2,3))
rowApply(df,sum)
#> [1] 2 4 6
```
