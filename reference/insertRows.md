# Inserts a matrix into another matrix.

Inserts a matrix or data frame into another matrix or data frame. The
new rows are placed together at the row index specified.

## Usage

``` r
insertRows(existing, insert, r)
```

## Arguments

- existing:

  table to insert into

- insert:

  rows to insert

- r:

  index at which to insert

## Examples

``` r
df1<-data.frame(a=c(1,2,3),b=c(1,2,3),c=c(1,2,3))
insertRows(df1,data.frame(list('a','a','a')),5)
#>   a b c
#> 1 1 1 1
#> 2 2 2 2
#> 3 3 3 3
#> 4 a a a
insertRows(df1,data.frame(list('a','a','a')),4)
#>   a b c
#> 1 1 1 1
#> 2 2 2 2
#> 3 3 3 3
#> 4 a a a
insertRows(df1,data.frame(list('a','a','a')),3)
#>   a b c
#> 1 1 1 1
#> 2 2 2 2
#> 4 a a a
#> 3 3 3 3
insertRows(df1,data.frame(list('a','a','a')),2)
#>   a b c
#> 1 1 1 1
#> 4 a a a
#> 2 2 2 2
#> 3 3 3 3
insertRows(df1,data.frame(list('a','a','a')),1)
#>   a b c
#> 4 a a a
#> 1 1 1 1
#> 2 2 2 2
#> 3 3 3 3
insertRows(df1,df1,3)
#>   a b c
#> 1 1 1 1
#> 2 2 2 2
#> 4 1 1 1
#> 5 2 2 2
#> 6 3 3 3
#> 3 3 3 3
```
