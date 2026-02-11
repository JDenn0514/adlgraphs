# Calculate difference in means

This function calculates the difference in means using a bivariate
regression, as well the p-value indicating how significant each
difference is. The main function doing the calculations
[`lm()`](https://rdrr.io/r/stats/lm.html).

NOTE: This function does not perform an actual Dunnet Test as it does
not calculate the quantile of the multivariate t-distribution when
determining the confidence intervals and p-values. If you need to
perform an actual Dunnett Test use the
[`dunnett()`](https://jdenn0514.github.io/adlgraphs/reference/dunnett.md)
function instead. Please be aware that that function is far slower when
there are many comparison groups due to the nature of
[`mvtnorm::qmvt()`](https://rdrr.io/pkg/mvtnorm/man/qmvt.html) and high
dimensional data.

## Usage

``` r
get_diffs(
  data,
  x,
  treats,
  group,
  wt,
  ref_level = NULL,
  pval_adj = NULL,
  conf_level = 0.95,
  conf_method = c("wald", "profile"),
  show_means = TRUE,
  show_pct_change = FALSE,
  decimals = 3,
  na.rm = TRUE
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

- group:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  A selection of columns to group the data by in addition to `treats`.
  This operates very similarly to `.by` from dplyr (for more info on
  that see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html)). See
  examples to see how it operates.

- wt:

  Weights. Add if you have a weighting variable and want to perform
  Dunnett's test with weighted means.

- ref_level:

  A string that specifies the level of the reference group through which
  the others will be tested.

- pval_adj:

  Method for adjusting p-values for multiple comparisons. Passed
  directly to
  [`stats::p.adjust`](https://rdrr.io/r/stats/p.adjust.html). Options
  include "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr",
  "none". Default is `NULL` (no adjustment).

- conf_level:

  A number between 0 and 1 that signifies the width of the desired
  confidence interval. Default is 0.95, which corresponds to a 95%
  confidence interval.

- conf_method:

  Determines whether the confidence intervals are calculated using the
  profile likelihood or the Wald method. Obviously has two options,
  "profile" and "wald". Wald is between 3 to 25 times as fast but not as
  reliable for small sample sizes (n \< 50). For larger sample sizes, (n
  \> 100), they will be very similar. **The default is Wald.**

- show_means:

  Logical. Default is `FALSE` which does not show the mean values for
  the levels. If `TRUE`, will add a column called `mean` that contains
  the means.

- show_pct_change:

  Logical. Default is `FALSE` which does not show the percent change
  from the reference category to the other categories. If `TRUE`, will
  show the percent change.

- decimals:

  Number of decimals each number should be rounded to. Default is 3.

- na.rm:

  Logical. Default is `TRUE` which removes NAs prior to calculation.

## Value

A tibble with one row if no `group` is provided and `data` is not of
class `"grouped_df"`. If data is of class `"grouped_df"` or `group` is
provided, it will return one row for each unique observation if one
group is provides and one row per unique combination of observations if
multiple groups are used.
