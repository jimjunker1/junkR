# named_group_split

This function returns a named list split on a grouping variable. It is
based on the comments here:
https://github.com/tidyverse/dplyr/issues/4223#issuecomment-469269857

## Usage

``` r
named_group_split(.tbl, ...)
```

## Arguments

- .tbl:

  A tibble or tibble-convertable object

- ...:

  further arguments, including the variable/s by which to group_split
  and set names

## Examples

``` r
named_group_split()
#> Error in named_group_split(): argument ".tbl" is missing, with no default
```
