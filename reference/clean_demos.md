# Clean up demographic variables

This function makes it easier to clean demographic data but it really
only works when the data is set up in a very specific way. This is
designed to work with how ADL programs its surveys and to work in case
any of the variables listed below are not found in the data.

## Usage

``` r
clean_demos(df)
```

## Arguments

- df:

  A dataframe or tibble. This can be piped in like with a normal
  [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
  function.

## Value

A data.frame object
