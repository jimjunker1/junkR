# Applies a function over a rolling window on any data object.

Simple generalized alternative to `rollapply` in package `zoo` with the
advantage that it works on any type of data structure (vector, list,
matrix, etc) instead of requiring a `zoo` object.

## Usage

``` r
rollApply(data, fun, window = len(data), minimum = 1, align = "left", ...)
```

## Arguments

- data:

  any `R` object

- fun:

  the function to evaluate

- window:

  window width defining the size of the subset available to the fun at
  any given point

- minimum:

  minimum width of the window. Will not return results if the window is
  truncated below this value at the end of the data set

- align:

  whether to align the window right or left

- ...:

  additional arguments to pass to `fun`

## Examples

``` r
rollApply(1:100,sum,minimum=2,window=2)
#>  [1]   3   5   7   9  11  13  15  17  19  21  23  25  27  29  31  33  35  37  39
#> [20]  41  43  45  47  49  51  53  55  57  59  61  63  65  67  69  71  73  75  77
#> [39]  79  81  83  85  87  89  91  93  95  97  99 101 103 105 107 109 111 113 115
#> [58] 117 119 121 123 125 127 129 131 133 135 137 139 141 143 145 147 149 151 153
#> [77] 155 157 159 161 163 165 167 169 171 173 175 177 179 181 183 185 187 189 191
#> [96] 193 195 197 199
rollApply(c(1,2,3),sum)
#> [1] 6 5 3
##6 5 3
rollApply(c(1,2,3,4,5,6,7,8,9),sum)
#> [1] 45 44 42 39 35 30 24 17  9
##45 44 42 39 35 30 24 17  9
rollApply(c(1,2,3,4,5,6,7,8,9),sum,window=2)
#> [1]  3  5  7  9 11 13 15 17  9
##3  5  7  9 11 13 15 17  9
rollApply(list(1,2,3,4,5,6,7,8,9),function(x) sum(unlist(x)),window=2,minimum=2)
#> [1]  3  5  7  9 11 13 15 17
##3  5  7  9 11 13 15 17
cbind(women,Rolling3=rollApply(women,fun=function(x) mean(x$weight),window=3,align='right'))
#>    height weight Rolling3
#> 1      58    115 115.0000
#> 2      59    117 116.0000
#> 3      60    120 117.3333
#> 4      61    123 120.0000
#> 5      62    126 123.0000
#> 6      63    129 126.0000
#> 7      64    132 129.0000
#> 8      65    135 132.0000
#> 9      66    139 135.3333
#> 10     67    142 138.6667
#> 11     68    146 142.3333
#> 12     69    150 146.0000
#> 13     70    154 150.0000
#> 14     71    159 154.3333
#> 15     72    164 159.0000
```
