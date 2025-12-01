# Calculate weighted correlations

This function calculates weighted Pearson correlations between two
variables. It also allows you to group the data and calculate
correlations along each level of the grouping variable. If data is not
grouped and no group is specified, then it will return the same output
as
[`wtd_corr()`](https://jdenn0514.github.io/adlgraphs/reference/wtd_corr.md).

## Usage

``` r
get_corr(data, x, y, group = NULL, wt = NULL, decimals = 3)
```

## Arguments

- data:

  An object of type data.frame or tibble. If piping the data into the
  function, this is not required.

- x, y:

  Can be either character strings or symbols. Name of two variables in
  the data you want to calculate the correlation between.

- group:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  A selection of columns to group the data by in addition to `treats`.
  This operates very similarly to `.by` from dplyr (for more info on
  that see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html)). See
  examples to see how it operates.

- wt:

  Can be either character strings or symbols. Weights. Add if you have a
  weighting variable and want to get weighted correlations

- decimals:

  Number of decimals each number should be rounded to. Default is 3.

## Value

A tibble showing correlations (`correlation`), number of observations
(`n`), low and high confidence intervals (`conf.low`, `conf.high`), the
p-value (`p_value`), and stars indicating it's statistical significance.
If data is of class `"grouped_df"` or the `group` argument is specified,
it will return one row for each unique observation if one group is
provided and one row per unique combination of observations if multiple
groups are used.

## Examples

``` r
# load the dplyr for piping and grouping
library(dplyr)

# Let's first do a simple correlation where we pipe in the data
test_data %>% get_corr(x = top, y = sdo_sum)
#> # A tibble: 1 × 8
#>   x                y          correlation     n conf.low conf.high p_value stars
#>   <chr+lbl>        <chr+lbl>        <dbl> <int>    <dbl>     <dbl>   <dbl> <chr>
#> 1 top [An ideal s… sdo… [Soc…      -0.736   250   -0.821    -0.651       0 ***  

# Repeat but with weights
test_data %>% get_corr(x = top, y = sdo_sum, wt = wts)
#> # A tibble: 1 × 8
#>   x                y          correlation     n conf.low conf.high p_value stars
#>   <chr+lbl>        <chr+lbl>        <dbl> <int>    <dbl>     <dbl>   <dbl> <chr>
#> 1 top [An ideal s… sdo… [Soc…      -0.721   250   -0.808    -0.634       0 ***  

# Now let's get the correlatoin among only people with a bachelor's degree
test_data %>% 
  filter(edu_f2 == "At Least a Bachelor's Degree") %>% 
  get_corr(x = top, y = sdo_sum, wt = wts)
#> # A tibble: 1 × 8
#>   x                y          correlation     n conf.low conf.high p_value stars
#>   <chr+lbl>        <chr+lbl>        <dbl> <int>    <dbl>     <dbl>   <dbl> <chr>
#> 1 top [An ideal s… sdo… [Soc…      -0.712   108   -0.847    -0.577       0 ***  

# Now let's get it for each education level. Two ways of doing this:
# The first is to group the data ahead of time
test_data %>% 
  group_by(edu_f) %>% 
  get_corr(x = top, y = sdo_sum, wt = wts)
#> # A tibble: 4 × 9
#>   edu_f  x         y          correlation     n conf.low conf.high p_value stars
#>   <fct>  <chr+lbl> <chr+lbl>        <dbl> <int>    <dbl>     <dbl>   <dbl> <chr>
#> 1 High … top [An … sdo… [Soc…      -0.728    64   -0.902    -0.555       0 ***  
#> 2 Some … top [An … sdo… [Soc…      -0.729    78   -0.885    -0.572       0 ***  
#> 3 Bache… top [An … sdo… [Soc…      -0.603    68   -0.799    -0.407       0 ***  
#> 4 Gradu… top [An … sdo… [Soc…      -0.814    40   -1.00     -0.623       0 ***  

# The second is to use the group argument
test_data %>% get_corr(x = top, y = sdo_sum, group = edu_f, wt = wts)
#> # A tibble: 4 × 9
#>   edu_f  x         y          correlation     n conf.low conf.high p_value stars
#>   <fct>  <chr+lbl> <chr+lbl>        <dbl> <int>    <dbl>     <dbl>   <dbl> <chr>
#> 1 High … top [An … sdo… [Soc…      -0.728    64   -0.902    -0.555       0 ***  
#> 2 Some … top [An … sdo… [Soc…      -0.729    78   -0.885    -0.572       0 ***  
#> 3 Bache… top [An … sdo… [Soc…      -0.603    68   -0.799    -0.407       0 ***  
#> 4 Gradu… top [An … sdo… [Soc…      -0.814    40   -1.00     -0.623       0 ***  
```
