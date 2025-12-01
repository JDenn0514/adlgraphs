# Make pretty HTML tables

Takes a data frame object and makes it into a pretty table. This is
designed to work within the `adlgraphs` package but it should work with
most objects of class `tbl_df`, `tbl`, or `data.frame`. Very important
note, at the moment, this only works with frequency tables that have
columns labelled `pct` and `n`. I do not see this changing for frequency
tables. Nevertheless, I am working on adding in more functionality,
including for mean tables, tables of factor loadings, linear regression
coefficients, and others.

## Usage

``` r
prettytable(x, show_genpop = FALSE)
```

## Arguments

- x:

  An object to turn into a pretty table.

- show_genpop:

  Logical. If the data is grouped, determines if data should should be
  shown for the general population as well. `FALSE`, the default, does
  not show the results for the general population. `TRUE` shows the
  results for the general population in a new column.
