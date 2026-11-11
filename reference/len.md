# Allows finding the 'length' without knowledge of dimensionality.

Allows finding the 'length' without knowledge of dimensionality.

## Usage

``` r
len(data)
```

## Arguments

- data:

  any `R` object

## Examples

``` r
len(list(1,2,3))
#> [1] 3
len(c(1,2,3,4))
#> [1] 4
df<-data.frame(a=c(1,2,3),b=c(1,2,3))
len(df)
#> [1] 3
```
