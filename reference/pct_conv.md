# Make proper percent labels

This function is a wrapper around a very common data transformation that
is done every time we make a frequency plot. It changes the `x` variable
by multiplying it 100 and creates a new variable called `pct_lab` which
is a string of the `x` variable but with a "%" symbol added.

## Usage

``` r
pct_conv(data, x = pct, decimals = 1)
```

## Arguments

- data:

  A data frame or vector. Can be left blank if used during piping

- x:

  a variable we want to convert to a percentage. The value is `pct` by
  default and should always be pct.

- decimals:

  Number of decimal places the percent should be rounded to

## Value

The original data.frame found in `data` with two changes. The column
called "pct" is multiplied by 100 and a new column is created called
"pct_lab" that is the same as pct but with a "%" symbol at the end of
it.

## Examples

``` r
library(dplyr)
# get the frequencies of top andupdate 
test_data %>%
  get_freqs(top) %>% 
  pct_conv()
#> # A tibble: 4 × 4
#>   top                   n   pct pct_lab
#>   <fct>             <dbl> <dbl> <chr>  
#> 1 Strongly agree       25    10 10%    
#> 2 Somewhat agree       85    34 34%    
#> 3 Somewhat disagree    75    30 30%    
#> 4 Strongly disagree    65    26 26%    
```
