# Calculate individual weighted correlations

This is one of the main worker functions behind
[`get_corr()`](https://jdenn0514.github.io/adlgraphs/reference/get_corr.md).
It calculates weighted correlations and outputs the data as a one row
tibble.

## Usage

``` r
wtd_corr(data, x, y, wt, decimals = 3)
```

## Arguments

- data:

  An object of type data.frame or tibble. If piping the data into the
  function, this is not required.

- x, y:

  Can be either character strings or symbols. Name of two variables in
  the data you want to calculate the correlation between.

- wt:

  Can be either character strings or symbols. Weights. Add if you have a
  weighting variable and want to get weighted correlations

- decimals:

  Number of decimals each number should be rounded to. Default is 3.
