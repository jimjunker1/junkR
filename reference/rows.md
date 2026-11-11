# Allows row indexing without knowledge of dimensionality or class.

Allows row indexing without knowledge of dimensionality or class.

## Usage

``` r
rows(data, rownums)
```

## Arguments

- data:

  any `R` object

- rownums:

  indices of target rows

## Examples

``` r
rows(c('A','B','C'),c(1,3))
#> [1] "A" "C"
rows(list('A','B','C'),c(1,3))
#> [[1]]
#> [1] "A"
#> 
#> [[2]]
#> [1] "C"
#> 
df<-data.frame(a=c(1,2,3),b=c(1,2,3))
rows(df,3)
#>   a b
#> 3 3 3
```
