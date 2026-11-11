# This function scrapes the full coauthor list for a publication from Google Scholar. It is a modified version from the scholar package that includes a sleep option to allow a time between http requests.

This function scrapes the full coauthor list for a publication from
Google Scholar. It is a modified version from the scholar package that
includes a sleep option to allow a time between http requests.

## Usage

``` r
get_complete_coauthors(id, pubid, sleep = 3, ...)
```

## Arguments

- id:

  A Google scholar id from a public id page

- pubid:

  A publication id can be returned from get_publications()

- sleep:

  A number in seconds to wait in between requests passes to Sys.sleep()

- ...:

  further argments to pass such as agent identification, encoding, etc.

## Value

A character string with a full author list of full author names
