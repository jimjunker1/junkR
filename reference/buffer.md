# Pads an object to a desired length, either with replicates of itself or another repeated object.

Pads an object to a desired length, either with replicates of itself or
another repeated object.

## Usage

``` r
buffer(x, length.out = len(x), fill = NULL, preserveClass = TRUE)
```

## Arguments

- x:

  an R object

- length.out:

  the desired length of the final output

- fill:

  R object to fill empty rows in columns below the max size. If
  unspecified, repeats input rows in the same way as `cbind`.

- preserveClass:

  determines whether to return an object of the same class as the
  original argument. Otherwise, returns a matrix.

## Examples

``` r
buffer(c(1,2,3),20)
#>  [1] 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2
buffer(matrix(c(1,2,3,4),nrow=2),20)
#> Error in if (class == "factor") return(as.factor(as.character(object))): the condition has length > 1
buffer(list(1,2,3),20)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
#> 
#> [[4]]
#> [1] 1
#> 
#> [[5]]
#> [1] 2
#> 
#> [[6]]
#> [1] 3
#> 
#> [[7]]
#> [1] 1
#> 
#> [[8]]
#> [1] 2
#> 
#> [[9]]
#> [1] 3
#> 
#> [[10]]
#> [1] 1
#> 
#> [[11]]
#> [1] 2
#> 
#> [[12]]
#> [1] 3
#> 
#> [[13]]
#> [1] 1
#> 
#> [[14]]
#> [1] 2
#> 
#> [[15]]
#> [1] 3
#> 
#> [[16]]
#> [1] 1
#> 
#> [[17]]
#> [1] 2
#> 
#> [[18]]
#> [1] 3
#> 
#> [[19]]
#> [1] 1
#> 
#> [[20]]
#> [1] 2
#> 
df<-data.frame(as.factor(c('Hello','Goodbye')),c(1,2))
buffer(df,5)
#>   as.factor.c..Hello....Goodbye... c.1..2.
#> 1                            Hello       1
#> 2                          Goodbye       2
#> 3                            Hello       1
#> 4                          Goodbye       2
#> 5                            Hello       1
buffer((factor(x=c('Hello'))),5)
#> [1] Hello Hello Hello Hello Hello
#> Levels: Hello
```
