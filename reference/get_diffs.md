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
#> # A tibble: 2 × 7
#>   pid_f3      diffs     n conf.low conf.high p_value stars
#> * <fct>       <dbl> <dbl>    <dbl>     <dbl>   <dbl> <chr>
#> 1 Independent 0.262    57    0.015     0.509   0.037 "*"  
#> 2 Republican  0.175    95   -0.038     0.388   0.107 ""   

# now do the same as above but make "Independent" the control group
get_diffs(test_data, "acts_avg", "pid_f3", ref_level = "Independent")
#> # A tibble: 2 × 7
#>   pid_f3      diffs     n conf.low conf.high p_value stars
#> * <fct>       <dbl> <dbl>    <dbl>     <dbl>   <dbl> <chr>
#> 1 Democrat   -0.262    98   -0.509    -0.015   0.037 "*"  
#> 2 Republican -0.087    95   -0.335     0.161   0.491 ""   

# now let's add in education (`edu_f2`) as the `group` variable. This let's us
# compare education levels within each level of `edu_f2`. Note how the arguments
# don't have to be strings
get_diffs(test_data, acts_avg, pid_f3, edu_f2)
#> # A tibble: 4 × 8
#>   edu_f2                     pid_f3 diffs     n conf.low conf.high p_value stars
#> * <fct>                      <fct>  <dbl> <dbl>    <dbl>     <dbl>   <dbl> <chr>
#> 1 No College Degree          Indep… 0.168    40   -0.134     0.471   0.273 ""   
#> 2 No College Degree          Repub… 0.156    50   -0.129     0.441   0.281 ""   
#> 3 At Least a Bachelor's Deg… Indep… 0.316    17   -0.118     0.749   0.152 ""   
#> 4 At Least a Bachelor's Deg… Repub… 0.2      45   -0.121     0.52    0.22  ""   

# we can also group by multiple variables. Due to a small n, I'm going to use 
# `edu_f2` instead of `edu_f`. 
test_data %>% 
  dplyr::mutate(values_f2 = make_dicho(values)) %>% 
  get_diffs(acts_avg, treats = pid_f3, group = c(edu_f2, values_f2))
#> # A tibble: 8 × 9
#>   edu_f2          values_f2 pid_f3  diffs     n conf.low conf.high p_value stars
#> * <fct>           <fct>     <fct>   <dbl> <dbl>    <dbl>     <dbl>   <dbl> <chr>
#> 1 No College Deg… True      Indep…  0.163    18   -0.249     0.575   0.432 ""   
#> 2 No College Deg… True      Repub…  0.08     35   -0.26      0.421   0.639 ""   
#> 3 No College Deg… False     Indep…  0.081    22   -0.379     0.541   0.726 ""   
#> 4 No College Deg… False     Repub…  0.369    15   -0.137     0.876   0.149 ""   
#> 5 At Least a Bac… True      Indep…  0.235    11   -0.359     0.829   0.433 ""   
#> 6 At Least a Bac… True      Repub… -0.015    28   -0.454     0.423   0.945 ""   
#> 7 At Least a Bac… False     Indep…  0.49      6   -0.042     1.02    0.07  "."  
#> 8 At Least a Bac… False     Repub…  0.586    17    0.188     0.983   0.005 "**" 

# now let's do those previous two calculations but using `dplyr::group_by()`
test_data %>% 
  dplyr::group_by(pid_f3) %>% 
  get_diffs(acts_avg, edu_f)
#> # A tibble: 9 × 8
#>   pid_f3      edu_f              diffs     n conf.low conf.high p_value stars
#> * <fct>       <fct>              <dbl> <dbl>    <dbl>     <dbl>   <dbl> <chr>
#> 1 Democrat    Some College      -0.111    32   -0.533     0.311   0.603 ""   
#> 2 Democrat    Bachelor's Degree -0.346    28   -0.779     0.087   0.116 ""   
#> 3 Democrat    Graduate Degree   -0.379    18   -0.86      0.101   0.121 ""   
#> 4 Independent Some College      -0.096    14   -0.645     0.453   0.727 ""   
#> 5 Independent Bachelor's Degree -0.054    12   -0.633     0.524   0.851 ""   
#> 6 Independent Graduate Degree   -0.471     5   -1.28      0.338   0.248 ""   
#> 7 Republican  Some College      -0.074    32   -0.49      0.342   0.725 ""   
#> 8 Republican  Bachelor's Degree -0.204    28   -0.631     0.222   0.344 ""   
#> 9 Republican  Graduate Degree   -0.443    17   -0.92      0.035   0.069 "."  

# we can also group by multiple variables
test_data %>% 
  dplyr::mutate(values_f2 = make_dicho(values)) %>% 
  dplyr::group_by(pid_f3, values_f2) %>% 
  get_diffs(acts_avg, edu_f2)
#> # A tibble: 6 × 9
#>   pid_f3      values_f2 edu_f2      diffs     n conf.low conf.high p_value stars
#> * <fct>       <fct>     <fct>       <dbl> <dbl>    <dbl>     <dbl>   <dbl> <chr>
#> 1 Democrat    True      At Least … -0.169    33   -0.546     0.209   0.376 ""   
#> 2 Democrat    False     At Least … -0.546    13   -1.04     -0.056   0.03  "*"  
#> 3 Independent True      At Least … -0.097    11   -0.828     0.634   0.787 ""   
#> 4 Independent False     At Least … -0.136     6   -0.81      0.537   0.681 ""   
#> 5 Republican  True      At Least … -0.264    28   -0.623     0.095   0.146 ""   
#> 6 Republican  False     At Least … -0.329    17   -0.76      0.101   0.129 ""   
```
