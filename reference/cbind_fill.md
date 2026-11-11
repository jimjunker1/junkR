# Combine arbitrary data types, filling in missing rows.

Robust alternative to [`cbind`](https://rdrr.io/r/base/cbind.html) that
fills missing values and works on arbitrary data types. Combines any
number of R objects into a single matrix, with each input corresponding
to the greater of 1 or ncol. `cbind` has counterintuitive results when
working with lists, cannot handle certain inputs of differing length,
and does not allow the fill to be specified.

## Usage

``` r
cbind_fill(..., fill = NULL)
```

## Arguments

- ...:

  any number of R data objects

- fill:

  R object to fill empty rows in columns below the max size. If
  unspecified, repeats input rows in the same way as `cbind`. Passed to
  [`buffer`](https://jimjunker1.github.io/junkR/reference/buffer.md).

## Examples

``` r
cbind_fill(c(1,2,3),list(1,2,3),cbind(c(1,2,3)))
#>   object object object
#> 1      1      1      1
#> 2      2      2      2
#> 3      3      3      3
cbind_fill(rbind(1:2),rbind(3:4))
#>   X1 X2 X1 X2
#> 1  1  2  3  4
df<-data.frame(a=c(1,2,3),b=c(1,2,3))
cbind_fill(c(1,2,3),list(1,2,3),cbind(c('a','b')),'a',df)
#>   object object object object a b
#> 1      1      1      a      a 1 1
#> 2      2      2      b      a 2 2
#> 3      3      3      a      a 3 3
cbind_fill(a=c(1,2,3),list(1,2,3),cbind(c('a','b')),'a',df,fill=NA)
#>   object object object object a b
#> 1      1      1      a      a 1 1
#> 2      2      2      b   <NA> 2 2
#> 3      3      3   <NA>   <NA> 3 3
```
