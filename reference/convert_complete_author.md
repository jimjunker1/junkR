# This function converts full author lists to person objects.

It can be used to import full author person objects into bib objects
returned from RefManageR::ReadGS. This further compresses the full
author names into just the first character for given and middle names.

## Usage

``` r
convert_complete_author(complete_coauthor_string)
```

## Arguments

- complete_coauthor_string:

  A character string of the full author list separated by ,s of the form
  e.g., James Robert Junker, John Jacob Jingle Heimer-Schimdt, etc.
