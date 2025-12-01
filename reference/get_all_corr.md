# Get correlations for a combination of variables

`get_all_corr` makes it easy to calculate correlations across every
variable in a data frame or a select set of variables. It also works
with grouped data frames so you can check correlations among the levels
of several grouping variables.

## Usage

``` r
get_all_corr(data, cols, group, wt, remove_redundant = TRUE, decimals = 3)
```

## Arguments

- data:

  A data frame or tibble object

- cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  The variables you want to get the correlations for.

- group:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  A selection of columns to group the data by in addition to `treats`.
  This operates very similarly to `.by` from dplyr (for more info on
  that see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html)). See
  examples to see how it operates.

- wt:

  A variable to use as the weights for weighted correlations

- remove_redundant:

  Should rows where the two variables are the same be kept or removed?
  If `TRUE`, the default, they are removed.

- decimals:

  Number of decimals each number should be rounded to. Default is 3.

## Value

A data.frame with the correlations between every combination of columns
in `data`.

## Examples

``` r
# load dplyr and adlgraphs
library(dplyr)
library(adlgraphs)

# To get correlations with three variables you can do it three ways
# 1. Create a new data frame with only the columns you want
new_data <- test_data %>% dplyr::select(top:dominate)
get_all_corr(new_data)
#> # A tibble: 6 × 8
#>   x                y          correlation     n conf.low conf.high p_value stars
#>   <chr+lbl>        <chr+lbl>        <dbl> <int>    <dbl>     <dbl>   <dbl> <chr>
#> 1 inferior [Some … top [An i…       0.498   250    0.389     0.606   0     ***  
#> 2 dominate [No on… top [An i…      -0.147   250   -0.271    -0.023   0.02  *    
#> 3 top [An ideal s… inf… [Som…       0.498   250    0.389     0.606   0     ***  
#> 4 dominate [No on… inf… [Som…      -0.138   250   -0.262    -0.015   0.029 *    
#> 5 top [An ideal s… dom… [No …      -0.147   250   -0.271    -0.023   0.02  *    
#> 6 inferior [Some … dom… [No …      -0.138   250   -0.262    -0.015   0.029 *    

# 2. Using dplyr::select() and pipes
test_data %>% 
  dplyr::select(c(top:dominate)) %>% 
  get_all_corr()
#> # A tibble: 6 × 8
#>   x                y          correlation     n conf.low conf.high p_value stars
#>   <chr+lbl>        <chr+lbl>        <dbl> <int>    <dbl>     <dbl>   <dbl> <chr>
#> 1 inferior [Some … top [An i…       0.498   250    0.389     0.606   0     ***  
#> 2 dominate [No on… top [An i…      -0.147   250   -0.271    -0.023   0.02  *    
#> 3 top [An ideal s… inf… [Som…       0.498   250    0.389     0.606   0     ***  
#> 4 dominate [No on… inf… [Som…      -0.138   250   -0.262    -0.015   0.029 *    
#> 5 top [An ideal s… dom… [No …      -0.147   250   -0.271    -0.023   0.02  *    
#> 6 inferior [Some … dom… [No …      -0.138   250   -0.262    -0.015   0.029 *    

# 3. Use the `cols` argument
get_all_corr(test_data, cols = c(top:dominate))
#> # A tibble: 6 × 8
#>   x                y          correlation     n conf.low conf.high p_value stars
#>   <chr+lbl>        <chr+lbl>        <dbl> <int>    <dbl>     <dbl>   <dbl> <chr>
#> 1 inferior [Some … top [An i…       0.498   250    0.389     0.606   0     ***  
#> 2 dominate [No on… top [An i…      -0.147   250   -0.271    -0.023   0.02  *    
#> 3 top [An ideal s… inf… [Som…       0.498   250    0.389     0.606   0     ***  
#> 4 dominate [No on… inf… [Som…      -0.138   250   -0.262    -0.015   0.029 *    
#> 5 top [An ideal s… dom… [No …      -0.147   250   -0.271    -0.023   0.02  *    
#> 6 inferior [Some … dom… [No …      -0.138   250   -0.262    -0.015   0.029 *    
# or 
test_data %>% get_all_corr(c(top:dominate))
#> # A tibble: 6 × 8
#>   x                y          correlation     n conf.low conf.high p_value stars
#>   <chr+lbl>        <chr+lbl>        <dbl> <int>    <dbl>     <dbl>   <dbl> <chr>
#> 1 inferior [Some … top [An i…       0.498   250    0.389     0.606   0     ***  
#> 2 dominate [No on… top [An i…      -0.147   250   -0.271    -0.023   0.02  *    
#> 3 top [An ideal s… inf… [Som…       0.498   250    0.389     0.606   0     ***  
#> 4 dominate [No on… inf… [Som…      -0.138   250   -0.262    -0.015   0.029 *    
#> 5 top [An ideal s… dom… [No …      -0.147   250   -0.271    -0.023   0.02  *    
#> 6 inferior [Some … dom… [No …      -0.138   250   -0.262    -0.015   0.029 *    

# To get weighted correlations just specify the `wt` argument
test_data %>% get_all_corr(c(top:dominate), wt = wts)
#> # A tibble: 6 × 8
#>   x                y          correlation     n conf.low conf.high p_value stars
#>   <chr+lbl>        <chr+lbl>        <dbl> <int>    <dbl>     <dbl>   <dbl> <chr>
#> 1 inferior [Some … top [An i…       0.509   250    0.401     0.617   0     ***  
#> 2 dominate [No on… top [An i…      -0.112   250   -0.237     0.012   0.076 .    
#> 3 top [An ideal s… inf… [Som…       0.509   250    0.401     0.617   0     ***  
#> 4 dominate [No on… inf… [Som…      -0.143   250   -0.267    -0.019   0.024 *    
#> 5 top [An ideal s… dom… [No …      -0.112   250   -0.237     0.012   0.076 .    
#> 6 inferior [Some … dom… [No …      -0.143   250   -0.267    -0.019   0.024 *    

# You can also calculate grouped correlations. For example, if
# you were interested in comparing the weighted correlations 
# among people with a college degree vs those without one, you 
# would do it like this:
test_data %>% 
  dplyr::group_by(edu_f2) %>% 
  get_all_corr(c(top:dominate), wt = wts)
#> # A tibble: 12 × 9
#>    edu_f2     x          y          correlation     n conf.low conf.high p_value
#>    <fct>      <chr+lbl>  <chr+lbl>        <dbl> <int>    <dbl>     <dbl>   <dbl>
#>  1 No Colleg… inf… [Som… top [An i…       0.526   142    0.384     0.668   0    
#>  2 At Least … inf… [Som… top [An i…       0.479   108    0.31      0.648   0    
#>  3 No Colleg… dom… [No … top [An i…      -0.058   142   -0.225     0.108   0.49 
#>  4 At Least … dom… [No … top [An i…      -0.217   108   -0.405    -0.029   0.024
#>  5 No Colleg… top [An i… inf… [Som…       0.526   142    0.384     0.668   0    
#>  6 At Least … top [An i… inf… [Som…       0.479   108    0.31      0.648   0    
#>  7 No Colleg… dom… [No … inf… [Som…      -0.121   142   -0.286     0.045   0.153
#>  8 At Least … dom… [No … inf… [Som…      -0.18    108   -0.37      0.009   0.062
#>  9 No Colleg… top [An i… dom… [No …      -0.058   142   -0.225     0.108   0.49 
#> 10 At Least … top [An i… dom… [No …      -0.217   108   -0.405    -0.029   0.024
#> 11 No Colleg… inf… [Som… dom… [No …      -0.121   142   -0.286     0.045   0.153
#> 12 At Least … inf… [Som… dom… [No …      -0.18    108   -0.37      0.009   0.062
#> # ℹ 1 more variable: stars <chr>

# Another way to calculate grouped correlations is to 
# specify the group argument inside the function call:
get_all_corr(test_data, c(top:dominate), edu_f2, wts)
#> # A tibble: 12 × 9
#>    edu_f2     x          y          correlation     n conf.low conf.high p_value
#>    <fct>      <chr+lbl>  <chr+lbl>        <dbl> <int>    <dbl>     <dbl>   <dbl>
#>  1 No Colleg… inf… [Som… top [An i…       0.526   142    0.384     0.668   0    
#>  2 At Least … inf… [Som… top [An i…       0.479   108    0.31      0.648   0    
#>  3 No Colleg… dom… [No … top [An i…      -0.058   142   -0.225     0.108   0.49 
#>  4 At Least … dom… [No … top [An i…      -0.217   108   -0.405    -0.029   0.024
#>  5 No Colleg… top [An i… inf… [Som…       0.526   142    0.384     0.668   0    
#>  6 At Least … top [An i… inf… [Som…       0.479   108    0.31      0.648   0    
#>  7 No Colleg… dom… [No … inf… [Som…      -0.121   142   -0.286     0.045   0.153
#>  8 At Least … dom… [No … inf… [Som…      -0.18    108   -0.37      0.009   0.062
#>  9 No Colleg… top [An i… dom… [No …      -0.058   142   -0.225     0.108   0.49 
#> 10 At Least … top [An i… dom… [No …      -0.217   108   -0.405    -0.029   0.024
#> 11 No Colleg… inf… [Som… dom… [No …      -0.121   142   -0.286     0.045   0.153
#> 12 At Least … inf… [Som… dom… [No …      -0.18    108   -0.37      0.009   0.062
#> # ℹ 1 more variable: stars <chr>

# You can also use multiple grouping variables
get_all_corr(test_data, c(top:dominate), c(edu_f2, pid_f3), wts)
#> # A tibble: 36 × 10
#>    edu_f2       pid_f3 x          y         correlation     n conf.low conf.high
#>    <fct>        <fct>  <chr+lbl>  <chr+lbl>       <dbl> <int>    <dbl>     <dbl>
#>  1 No College … Democ… inf… [Som… top [An …       0.514    52    0.27      0.757
#>  2 No College … Indep… inf… [Som… top [An …       0.446    40    0.152     0.74 
#>  3 No College … Repub… inf… [Som… top [An …       0.521    50    0.274     0.769
#>  4 At Least a … Democ… inf… [Som… top [An …       0.333    46    0.046     0.619
#>  5 At Least a … Indep… inf… [Som… top [An …       0.729    17    0.352     1.11 
#>  6 At Least a … Repub… inf… [Som… top [An …       0.571    45    0.318     0.823
#>  7 No College … Democ… dom… [No … top [An …      -0.05     52   -0.334     0.233
#>  8 No College … Indep… dom… [No … top [An …      -0.301    40   -0.614     0.013
#>  9 No College … Repub… dom… [No … top [An …       0.044    50   -0.246     0.334
#> 10 At Least a … Democ… dom… [No … top [An …      -0.227    46   -0.523     0.069
#> # ℹ 26 more rows
#> # ℹ 2 more variables: p_value <dbl>, stars <chr>

```
