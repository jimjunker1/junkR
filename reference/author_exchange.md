# This function exchanges full author list formated with First and Middle initials and full family name. This allows the creation of bib entries with full author lists

This function exchanges full author list formated with First and Middle
initials and full family name. This allows the creation of bib entries
with full author lists

## Usage

``` r
author_exchange(x, y)
```

## Arguments

- x:

  bib entry created from RefManageR::ReadGS() (i.e., list) with
  incomplete author list

- y:

  personList created from convert_complete_author(). Same length as x

## Examples

``` r
author_exchange()
#> Error in author_exchange(): argument "x" is missing, with no default
#mapply(author_exchange, x = jrj.bib, y = full_authors_list)
```
