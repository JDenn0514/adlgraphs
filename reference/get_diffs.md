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
  group = NULL,
  wt = NULL,
  ref_level,
  show_means = FALSE,
  show_pct_change = FALSE,
  decimals = 3,
  conf.level = 0.95,
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

- conf.level:

  A number between 0 and 1 that signifies the width of the desired
  confidence interval. Default is 0.95, which corresponds to a 95%
  confidence interval.

- na.rm:

  Logical. Default is `TRUE` which removes NAs prior to calculation.

## Value

A tibble with one row if no `group` is provided and `data` is not of
class `"grouped_df"`. If data is of class `"grouped_df"` or `group` is
provided, it will return one row for each unique observation if one
group is provides and one row per unique combination of observations if
multiple groups are used.

## Examples

``` r
# load dplyr for the pipe: %>% 
library(dplyr)
library(adlgraphs)

# Check to see if any of the partisan groups are significantly different
# from the control group (in this case "Democrat") for conspiracy
# theory belief
get_diffs(test_data, "acts_avg", "pid_f3")
#> Error in UseMethod("get_diffs"): no applicable method for 'get_diffs' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"

# now do the same as above but make "Independent" the control group
get_diffs(test_data, "acts_avg", "pid_f3", ref_level = "Independent")
#> Error in UseMethod("get_diffs"): no applicable method for 'get_diffs' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"

# now let's add in education (`edu_f2`) as the `group` variable. This let's us
# compare education levels within each level of `edu_f2`. Note how the arguments
# don't have to be strings
get_diffs(test_data, acts_avg, pid_f3, edu_f2)
#> Error in UseMethod("get_diffs"): no applicable method for 'get_diffs' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"

# we can also group by multiple variables. Due to a small n, I'm going to use 
# `edu_f2` instead of `edu_f`. 
test_data %>% 
  dplyr::mutate(values_f2 = make_dicho(values)) %>% 
  get_diffs(acts_avg, treats = pid_f3, group = c(edu_f2, values_f2))
#> Error in UseMethod("get_diffs"): no applicable method for 'get_diffs' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"

# now let's do those previous two calculations but using `dplyr::group_by()`
test_data %>% 
  dplyr::group_by(pid_f3) %>% 
  get_diffs(acts_avg, edu_f)
#> Error in UseMethod("get_diffs"): no applicable method for 'get_diffs' applied to an object of class "c('grouped_df', 'tbl_df', 'tbl', 'data.frame')"

# we can also group by multiple variables
test_data %>% 
  dplyr::mutate(values_f2 = make_dicho(values)) %>% 
  dplyr::group_by(pid_f3, values_f2) %>% 
  get_diffs(acts_avg, edu_f2)
#> Error in UseMethod("get_diffs"): no applicable method for 'get_diffs' applied to an object of class "c('grouped_df', 'tbl_df', 'tbl', 'data.frame')"
```
