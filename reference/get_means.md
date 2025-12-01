# Calculate means with confidence intervals

Use this function to calculate simple weighted means with 95% confidence
intervals or weighted grouped means.

The `x`, `group`, and `wt` arguments can either be strings or symbols
(meaning they can have quotes or no quotes). The benefit of this is that
it makes it easy to iterate this function over a list or vector of
variables with other functions like
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html)
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) or
[`purrr::walk()`](https://purrr.tidyverse.org/reference/map.html)
[`purrr::walk()`](https://purrr.tidyverse.org/reference/map.html) that
are found in the `purrr` package.

## Usage

``` r
get_means(
  data,
  x,
  group = NULL,
  wt = NULL,
  decimals = 3,
  na.rm = TRUE,
  conf_level = 0.95
)
```

## Arguments

- data:

  An object of type data.frame or tibble. If piping the data into the
  function, this is not required.

- x:

  Either a character string or symbol. The variable with which you want
  to get the mean.

- group:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  A selection of columns to group the data by in addition to `treats`.
  This operates very similarly to `.by` from dplyr (for more info on
  that see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html)). It
  can also be a character vector, but it can't be an external vector.

- wt:

  Weights. Add if you have a weighting variable and want to get weighted
  means.

- decimals:

  Number of decimals to round the results to. Default is 3.

- na.rm:

  Logical. Determines if NAs should be removed from the grouping
  variables prior to analysis. Default is TRUE.

- conf_level:

  What should the confidence level be when calculating confidence
  intervals. Defaults to 0.95

## Value

A tibble with one row if no `group` is provided and `data` is not of
class `"grouped_df"`. If data is of class `"grouped_df"` or `group` is
provided, it will return a row for each unique observation or
combination of observations.

## Examples

``` r
# load the package
library(dplyr)

# Let's calculate the overall average score for trad_n
get_means(test_data, trad_n)
#> # A tibble: 1 × 5
#>    mean    sd     n conf.low conf.high
#>   <dbl> <dbl> <dbl>    <dbl>     <dbl>
#> 1  4.22  3.87   250     3.74      4.70

# it also works if x is a string
get_means(test_data, "trad_n")
#> # A tibble: 1 × 5
#>    mean    sd     n conf.low conf.high
#>   <dbl> <dbl> <dbl>    <dbl>     <dbl>
#> 1  4.22  3.87   250     3.74      4.70

# Let's do that again but add weights
get_means(test_data, trad_n, wt = wts)
#> # A tibble: 1 × 5
#>    mean    sd     n conf.low conf.high
#>   <dbl> <dbl> <dbl>    <dbl>     <dbl>
#> 1  3.97  3.76  245.     3.50      4.45

# the wt argument can also be in quotes like this
get_means(test_data, "trad_n", wt = "wts")
#> # A tibble: 1 × 5
#>    mean    sd     n conf.low conf.high
#>   <dbl> <dbl> <dbl>    <dbl>     <dbl>
#> 1  3.97  3.76  245.     3.50      4.45

# Now let's do the average score for different education levels
get_means(test_data, trad_n, edu_f, wts)
#> # A tibble: 4 × 6
#>   edu_f                mean    sd     n conf.low conf.high
#>   <fct>               <dbl> <dbl> <dbl>    <dbl>     <dbl>
#> 1 High School or Less  4.14  3.59  69.8     3.29      5.00
#> 2 Some College         3.93  3.96  86.3     3.08      4.78
#> 3 Bachelor's Degree    4.23  3.83  49.7     3.14      5.32
#> 4 Graduate Degree      3.45  3.41  39.4     2.35      4.55

# it also works with quotes
get_means(test_data, "trad_n", "edu_f", "wts")
#> # A tibble: 4 × 6
#>   edu_f                mean    sd     n conf.low conf.high
#>   <fct>               <dbl> <dbl> <dbl>    <dbl>     <dbl>
#> 1 High School or Less  4.14  3.59  69.8     3.29      5.00
#> 2 Some College         3.93  3.96  86.3     3.08      4.78
#> 3 Bachelor's Degree    4.23  3.83  49.7     3.14      5.32
#> 4 Graduate Degree      3.45  3.41  39.4     2.35      4.55

# you can also pipe in the `data` argument if you want to do some data
# transformations before you calculate the means. For example, say you want
# to compare the means of `trad_n` among people who agreed vs disagreed with
# the variable `top`:
test_data %>%
  mutate(top_f2 = make_dicho(top)) %>%
  get_means(trad_n, top_f2, wts)
#> # A tibble: 2 × 6
#>   top_f2    mean    sd     n conf.low conf.high
#>   <fct>    <dbl> <dbl> <dbl>    <dbl>     <dbl>
#> 1 Agree     5.08  3.83  111.     4.36      5.80
#> 2 Disagree  3.05  3.43  134.     2.46      3.64
```
