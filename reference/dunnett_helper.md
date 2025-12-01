# Perform Dunnett's test (mostly internal function)

This function was created to mostly serve as an internal function for
[`dunnett()`](https://jdenn0514.github.io/adlgraphs/reference/dunnett.md).
It does all the same things as
[`dunnett()`](https://jdenn0514.github.io/adlgraphs/reference/dunnett.md),
however, you can't group the data and then do the calculation.

## Usage

``` r
dunnett_helper(
  data,
  x,
  treats,
  wt = NULL,
  control = NULL,
  conf.level = 0.95,
  show_means = FALSE,
  show_diffs = TRUE,
  decimals = 3
)
```

## Arguments

- data:

  A data frame or tibble.

- x:

  A numeric vector that will be used to calculate the means. This can be
  a string or symbol.

- treats:

  A variable whose values are used to determine if the means are
  statistically significantly different from each other. Should be a
  factor or character vector. This can be a string or symbol.

- wt:

  Weights. Add if you have a weighting variable and want to perform
  Dunnett's test with weighted means,

- control:

  A string that specifies the level of the reference group through which
  the others will be tested.

- conf.level:

  A number between 0 and 1 that signifies the width of the desired
  confidence interval. Default is 0.95, which corresponds to a 95%
  confidence interval.

- show_means:

  Logical. Determines if the output should contain the means of each
  level. Default is `FALSE`

- show_diffs:

  Logical. Determines if the output should contain the difference in
  means

- decimals:

  Number of decimals each number should be rounded to. Default is 2.
