# Run Dunnett's multiple comparisons test with one control.

`dunnett()` calculates Dunnett's post hoc pairwise multiple comparisons
procedure. More simply, it calculates the mean of a variable (`x`) along
the different levels of a grouping variable (`treats`) and then
determines if the difference between the control/reference group and
each level is statistically significant.

## Usage

``` r
dunnett(
  data,
  x,
  treats = NULL,
  group = NULL,
  wt = NULL,
  control = NULL,
  conf.level = 0.95,
  show_means = FALSE,
  show_diffs = TRUE,
  decimals = 2,
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

- na.rm:

  Logical. Determines if NAs should be removed

## Details

While there are other functions that also perform Dunnett's Test, like
`PMCMRplus::dunnettTest()` and `DescTools::DunnettTest()` to name a few,
there are a few key differences between this function and those.
Firstly, this function takes in a data frame or tibble. This was done so
that it can either be piped in or specified in the argument. See more in
the example section below.

Another important difference, is the addition of the `group` variable
and the ability to pipe in a grouped data frame. When `group` is
specified or a grouped data frame is piped in, the function still
performs the Dunnett test between the variable in `x` and the variable
in `treats`, but it does so along each level of whatever variable(s)
are/is specified in `group` or that the data is grouped by. You can see
this in action in the examples section below.

Lastly, this function allows you to add weights to calculate weighted
means.

## Examples

``` r
# load dply for the pipe: %>% 
library(dplyr)
library(adlgraphs)

# Check to see if any of the education groups are significantly different
# from the control group (in this case "High School or Less") for conspiracy
# theory belief
dunnett(test_data, "acts_avg", "pid_f3")
#> # A tibble: 2 × 7
#>   pid_f3       diff     n conf.low conf.high p.value stars
#> * <fct>       <dbl> <dbl>    <dbl>     <dbl>   <dbl> <chr>
#> 1 Independent 0.262    57   -0.017     0.542   0.07  "."  
#> 2 Republican  0.175    95   -0.066     0.417   0.191 ""   
# now let's do the same but have it show the means
dunnett(test_data, "acts_avg", "pid_f3", show_means = TRUE)
#> # A tibble: 3 × 9
#>   pid_f3       mean  diff    sd     n conf.low conf.high p.value stars
#> * <fct>       <dbl> <dbl> <dbl> <dbl>    <dbl>     <dbl>   <dbl> <chr>
#> 1 Democrat     2.63 0     0.745    98     2.48      2.78  NA      NA  
#> 2 Independent  2.90 0.262 0.807    57     2.68      3.11   0.07  "."  
#> 3 Republican   2.81 0.175 0.711    95     2.66      2.95   0.191 ""   

# now do the same as above but make "Independent" the control group
dunnett(test_data, "acts_avg", "pid_f3", control = "Independent", show_means = TRUE)
#> # A tibble: 3 × 9
#>   pid_f3       mean   diff    sd     n conf.low conf.high p.value stars
#> * <fct>       <dbl>  <dbl> <dbl> <dbl>    <dbl>     <dbl>   <dbl> <chr>
#> 1 Democrat     2.63  0     0.745    98     2.48      2.78  NA      NA  
#> 2 Independent  2.90 -0.262 0.807    57     2.68      3.11   0.065 "."  
#> 3 Republican   2.81 -0.087 0.711    95     2.66      2.95   0.694 ""   

# now let's add in education (`edu_f2`) as the `group` variable. This let's us
# compare education levels within each level of `edu_f2`. Note how the arguments
# don't have to be strings
dunnett(test_data, acts_avg, pid_f3, edu_f2)
#> # A tibble: 4 × 8
#>   edu_f2                     pid_f3  diff     n conf.low conf.high p.value stars
#> * <fct>                      <fct>  <dbl> <dbl>    <dbl>     <dbl>   <dbl> <chr>
#> 1 No College Degree          Indep… 0.168    40   -0.174     0.511   0.445 ""   
#> 2 No College Degree          Repub… 0.156    50   -0.167     0.478   0.457 ""   
#> 3 At Least a Bachelor's Deg… Indep… 0.316    17   -0.177     0.809   0.268 ""   
#> 4 At Least a Bachelor's Deg… Repub… 0.2      45   -0.165     0.564   0.376 ""   

# we can also group by multiple variables. Due to a small n, I'm going to use 
# `edu_f2` instead of `edu_f`. 
test_data %>% 
  dplyr::mutate(values_f2 = make_dicho(values)) %>% 
  dunnett(acts_avg, treats = pid_f3, group = c(edu_f2, values_f2))
#> # A tibble: 8 × 9
#> # Groups:   edu_f2, values_f2 [4]
#>   edu_f2          values_f2 pid_f3   diff     n conf.low conf.high p.value stars
#> * <fct>           <fct>     <fct>   <dbl> <dbl>    <dbl>     <dbl>   <dbl> <chr>
#> 1 No College Deg… True      Indep…  0.163    18   -0.304     0.631   0.656 ""   
#> 2 No College Deg… True      Repub…  0.08     35   -0.306     0.467   0.858 ""   
#> 3 No College Deg… False     Indep…  0.081    22   -0.44      0.602   0.914 ""   
#> 4 No College Deg… False     Repub…  0.369    15   -0.204     0.943   0.254 ""   
#> 5 At Least a Bac… True      Indep…  0.235    11   -0.442     0.912   0.664 ""   
#> 6 At Least a Bac… True      Repub… -0.015    28   -0.515     0.485   0.997 ""   
#> 7 At Least a Bac… False     Indep…  0.49      6   -0.117     1.10    0.127 ""   
#> 8 At Least a Bac… False     Repub…  0.586    17    0.133     1.04    0.01  "**" 

# now let's do those previous two calculations but using `dplyr::group_by()`
test_data %>% 
  dplyr::group_by(pid_f3) %>% 
  dunnett(acts_avg, edu_f)
#> # A tibble: 9 × 8
#>   pid_f3      edu_f               diff     n conf.low conf.high p.value stars
#> * <fct>       <chr>              <dbl> <dbl>    <dbl>     <dbl>   <dbl> <chr>
#> 1 Independent Some College      -0.096    14   -0.767     0.575   0.977 ""   
#> 2 Independent Bachelor's Degree -0.054    12   -0.761     0.652   0.996 ""   
#> 3 Independent Graduate Degree   -0.471     5   -1.46      0.518   0.553 ""   
#> 4 Democrat    Some College      -0.111    32   -0.615     0.394   0.909 ""   
#> 5 Democrat    Bachelor's Degree -0.346    28   -0.865     0.172   0.259 ""   
#> 6 Democrat    Graduate Degree   -0.379    18   -0.954     0.196   0.269 ""   
#> 7 Republican  Some College      -0.074    32   -0.57      0.423   0.967 ""   
#> 8 Republican  Bachelor's Degree -0.204    28   -0.714     0.305   0.639 ""   
#> 9 Republican  Graduate Degree   -0.443    17   -1.01      0.127   0.16  ""   

# we can also group by multiple
test_data %>% 
  dplyr::mutate(values_f2 = make_dicho(values)) %>% 
  dplyr::group_by(pid_f3, values_f2) %>% 
  dunnett(acts_avg, edu_f2)
#> # A tibble: 6 × 9
#> # Groups:   pid_f3, values_f2 [6]
#>   pid_f3      values_f2 edu_f2       diff     n conf.low conf.high p.value stars
#> * <fct>       <fct>     <chr>       <dbl> <dbl>    <dbl>     <dbl>   <dbl> <chr>
#> 1 Democrat    True      At Least … -0.169    33   -0.546     0.209   0.376 ""   
#> 2 Democrat    False     At Least … -0.546    13   -1.04     -0.056   0.03  "*"  
#> 3 Independent True      At Least … -0.097    11   -0.828     0.634   0.787 ""   
#> 4 Independent False     At Least … -0.136     6   -0.81      0.537   0.681 ""   
#> 5 Republican  True      At Least … -0.264    28   -0.623     0.095   0.146 ""   
#> 6 Republican  False     At Least … -0.329    17   -0.76      0.101   0.129 ""   

# If we want to show means and differences, set show_means to TRUE
# we don't need to set show_diffs to TRUE since that is the default
dunnett(test_data, acts_avg, edu_f, show_means = TRUE)
#> # A tibble: 4 × 9
#>   edu_f                mean   diff    sd     n conf.low conf.high p.value stars
#> * <fct>               <dbl>  <dbl> <dbl> <dbl>    <dbl>     <dbl>   <dbl> <chr>
#> 1 High School or Less  2.93  0     0.697    64     2.76      3.10  NA      NA  
#> 2 Some College         2.82 -0.106 0.742    78     2.66      2.99   0.738 ""   
#> 3 Bachelor's Degree    2.68 -0.25  0.789    68     2.49      2.87   0.14  ""   
#> 4 Graduate Degree      2.49 -0.436 0.719    40     2.26      2.72   0.012 "*"  

# if we want to show means without differences, set `show_diffs = FALSE`
dunnett(test_data, acts_avg, edu_f, show_means = TRUE, show_diffs = FALSE)
#> # A tibble: 4 × 8
#>   edu_f                mean    sd     n conf.low conf.high p.value stars
#> * <fct>               <dbl> <dbl> <dbl>    <dbl>     <dbl>   <dbl> <chr>
#> 1 High School or Less  2.93 0.697    64     2.76      3.10  NA      NA  
#> 2 Some College         2.82 0.742    78     2.66      2.99   0.738 ""   
#> 3 Bachelor's Degree    2.68 0.789    68     2.49      2.87   0.14  ""   
#> 4 Graduate Degree      2.49 0.719    40     2.26      2.72   0.011 "*"  

```
