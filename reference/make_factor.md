# Convert a labelled vector into a factor

`make_factor()` takes a labelled vector and converts it to a factor
variable using the value labels. This works with numeric, character, and
factor vectors.

This function is very similar to
[[`haven::as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html)](https://haven.tidyverse.org/reference/as_factor.html)
and
[`sjlabelled::as_label()`](https://strengejacke.github.io/sjlabelled/reference/as_label.html)
and is heavily based on both. However, it has some key differences. The
main difference compared to both functions is that `make_factor()` adds
a "transformation" attribute to the new variable indicating how it was
created. You can see this in the examples.

Compared to
[`sjlabelled::as_label()`](https://strengejacke.github.io/sjlabelled/reference/as_label.html)
it is not as extensive. For example, while
[`sjlabelled::as_label()`](https://strengejacke.github.io/sjlabelled/reference/as_label.html)
works with data.frames and vectors, `make_factor()` only works with
vectors. In addition,
[`sjlabelled::as_label()`](https://strengejacke.github.io/sjlabelled/reference/as_label.html)
has many different arguments that enable you to control the appearance
of the labels, NAs, and other things. `make_factor()` on the other hand
is much simpler. Similarly,
[[`haven::as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html)](https://haven.tidyverse.org/reference/as_factor.html)
also enables more customization over the output of the labels. Another
key difference between this function and those is that if there are
values without labels, this function returns an error.

## Usage

``` r
make_factor(
  x,
  ordered = FALSE,
  drop_levels = TRUE,
  force = TRUE,
  na.rm = FALSE
)
```

## Arguments

- x:

  A vector with value labels. Can be numeric, character, or a factor

- ordered:

  Logical. Determines if the factor be ordered. Defaults to `TRUE.`

- drop_levels:

  Logical. Determines if unused factor levels should be dropped.
  Defaults to `TRUE.`

- force:

  Logical. Determines if `x` should be forced to a vector even if there
  are no value labels. Defaults to `TRUE.`

- na.rm:

  Logical. Determines if tags should be removed from NAs. Defaults to
  `FALSE`.

## Value

A factor vector of same length as `x`.

## Examples

``` r
library(adlgraphs)
library(dplyr)

# let's make a new variable and data set
new_df <- test_data %>%
  # convert top into a factor
  mutate(top_f = make_factor(top))

# compare the "top_f" to "top"
new_df %>% select(top, top_f)
#> # A tibble: 250 × 2
#>    top                   top_f            
#>    <dbl+lbl>             <fct>            
#>  1 1 [Strongly agree]    Strongly agree   
#>  2 2 [Somewhat agree]    Somewhat agree   
#>  3 2 [Somewhat agree]    Somewhat agree   
#>  4 3 [Somewhat disagree] Somewhat disagree
#>  5 2 [Somewhat agree]    Somewhat agree   
#>  6 4 [Strongly disagree] Strongly disagree
#>  7 2 [Somewhat agree]    Somewhat agree   
#>  8 2 [Somewhat agree]    Somewhat agree   
#>  9 2 [Somewhat agree]    Somewhat agree   
#> 10 4 [Strongly disagree] Strongly disagree
#> # ℹ 240 more rows

# check the attributes to see the label and transformation
attributes(new_df$top_f)
#> $levels
#> [1] "Strongly agree"    "Somewhat agree"    "Somewhat disagree"
#> [4] "Strongly disagree"
#> 
#> $class
#> [1] "factor"
#> 
#> $label
#> [1] "An ideal society requires some groups to be on top and others to be on the bottom"
#> 
#> $transformation
#> [1] "Converted 'top' into a factor based on its value labels"
#> 
```
