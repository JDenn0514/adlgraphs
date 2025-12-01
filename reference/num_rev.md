# Reverse a numeric function

Reverse a numeric variable while maintaining variable and value labels
if available. Also adds an attribute describing the transformation the
original variable underwent. Please check the vignette to have a better
understanding of exactly what this function does.

## Usage

``` r
num_rev(x)
```

## Arguments

- x:

  A vector of class `haven_labelled` or `numeric`

## Value

A numeric vector of the same length as `x`

## Examples

``` r
library(dplyr)

test_data %>%
  # reverse the variable accept_isr
  mutate(accept_isr_rev = num_rev(accept_isr)) %>%
  select(starts_with("accept"))
#> # A tibble: 250 × 3
#>    accept_hamas              accept_isr                accept_isr_rev
#>    <dbl+lbl>                 <dbl+lbl>                          <dbl>
#>  1 3 [Somewhat unacceptable] 3 [Somewhat unacceptable]              2
#>  2 3 [Somewhat unacceptable] 3 [Somewhat unacceptable]              2
#>  3 4 [Totally unacceptable]  3 [Somewhat unacceptable]              2
#>  4 4 [Totally unacceptable]  1 [Totally acceptable]                 4
#>  5 4 [Totally unacceptable]  2 [Somewhat acceptable]                3
#>  6 3 [Somewhat unacceptable] 1 [Totally acceptable]                 4
#>  7 3 [Somewhat unacceptable] 2 [Somewhat acceptable]                3
#>  8 3 [Somewhat unacceptable] 3 [Somewhat unacceptable]              2
#>  9 4 [Totally unacceptable]  2 [Somewhat acceptable]                3
#> 10 4 [Totally unacceptable]  3 [Somewhat unacceptable]              2
#> # ℹ 240 more rows
```
