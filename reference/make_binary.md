# Make binary variables

Convert a vector of class `factor` or
[`haven_labelled`](https://haven.tidyverse.org/reference/labelled.html)
to a "binary vector". When I refer to a "binary vector", I am referring
to a vector of class `numeric` with two values: 0 or 1. Another way of
thinking about this is by turning a variable into a dummy variable.

## Usage

``` r
make_binary(x, flip_values = FALSE)
```

## Arguments

- x:

  A vector of class `haven_labelled` or `factor`.

- flip_values:

  Logical. If `FALSE`, the default, the values are kept the same. If
  `TRUE`, the values associated with 1 and 0 are flipped. See third
  example for more information.

## Value

A numeric vector of same length as `x`.

## Details

`make_binary()` builds off
[`make_dicho()`](https://jdenn0514.github.io/adlgraphs/reference/make_dicho.md)
and therefore was designed to work on any vector that is of class
`factor`,
[`haven_labelled`](https://haven.tidyverse.org/reference/labelled.html),
or `numeric` with value labels. Because this was built off of
[make_dicho](https://jdenn0514.github.io/adlgraphs/reference/make_dicho.md),
if the vector is numeric with no value labels, the function will return
an error.

Similar to how
[`make_dicho()`](https://jdenn0514.github.io/adlgraphs/reference/make_dicho.md)
provides the opportunity to flip the factor levels, `make_binary()`
allows you to flip which values should be recoded as 0 and which should
be recoded as 1. To do so, just set `flip_values = TRUE`.

In addition, this function adds three new attributes. The first
attribute, `transformation`, indicates the data transformation that the
original vector underwent to create this new vector. The second
attribute, `label`, contains the variable label that was found in the
original variable. However, if the original vector did not have a
variable label, then this attribute will not show up. The third
attribute, `labels`, adds value labels so you can see what the 1 and 0
mean.

## Examples

``` r
library(dplyr)
library(adlgraphs)

df <- tibble::tribble(
  ~x, ~y, ~z,
  3, 2, 3,
  4, 4, 2,
  2, 6, 1,
  1, 1, 4,
  5, 4, 3,
  6, 5, 6
) 

labs <- c(
  "Strongly agree" = 1,
  "Agree" = 2,
  "Somewhat agree" = 3,
  "Somewhat disagree" = 4,
  "Disagree" = 5,
  "Strongly disagree" = 6
 )

attr(df$x, "labels") <- labs
attr(df$y, "labels") <- labs
attr(df$z, "labels") <- labs

# show the data transformation with a haven_labelled vector
binary_df <- dplyr::mutate(df, binary_x = make_binary(x))
# check the updated dataset
binary_df
#> # A tibble: 6 × 4
#>       x     y     z binary_x
#>   <dbl> <dbl> <dbl>    <dbl>
#> 1     3     2     3        1
#> 2     4     4     2        0
#> 3     2     6     1       NA
#> 4     1     1     4        1
#> 5     5     4     3       NA
#> 6     6     5     6        0

# Check the attributes
attributes(binary_df$binary_x)
#> $transformation
#> [1] "Converting 'x' to a binary variable with 'Agree' = 1 and 'Disagree' = 0."
#> 
#> $labels
#>    Agree Disagree 
#>        1        0 
#> 
#> $label
#> [1] "x"
#> 
# another way of checking the attributes
str(binary_df$binary_x)
#>  num [1:6] 1 0 NA 1 NA 0
#>  - attr(*, "transformation")= chr "Converting 'x' to a binary variable with 'Agree' = 1 and 'Disagree' = 0."
#>  - attr(*, "labels")= Named num [1:2] 1 0
#>   ..- attr(*, "names")= chr [1:2] "Agree" "Disagree"
#>  - attr(*, "label")= chr "x"

# check the factor levels
unique(binary_df$binary_x)
#> [1]  1  0 NA

# ----------------------------------------------------------------------------

# function also works with factors
binary_df <- df %>%
  dplyr::mutate(
    # convert variable to a factor
    factor_x = make_factor(x),
    # convert the factor to a binary variable
    binary_x = make_binary(factor_x)
  )

# check the updated dataset
binary_df
#> # A tibble: 6 × 5
#>       x     y     z factor_x          binary_x
#>   <dbl> <dbl> <dbl> <fct>                <dbl>
#> 1     3     2     3 Somewhat agree           1
#> 2     4     4     2 Somewhat disagree        0
#> 3     2     6     1 Agree                   NA
#> 4     1     1     4 Strongly agree           1
#> 5     5     4     3 Disagree                NA
#> 6     6     5     6 Strongly disagree        0

# Check the attributes
attributes(binary_df$binary_x)
#> $transformation
#> [1] "Converting 'factor_x' to a binary variable with 'Agree' = 1 and 'Disagree' = 0."
#> 
#> $label
#> [1] "x"
#> 
#> $labels
#>    Agree Disagree 
#>        1        0 
#> 
# another way of checking the attributes
str(binary_df$binary_x)
#>  num [1:6] 1 0 NA 1 NA 0
#>  - attr(*, "transformation")= chr "Converting 'factor_x' to a binary variable with 'Agree' = 1 and 'Disagree' = 0."
#>  - attr(*, "label")= chr "x"
#>  - attr(*, "labels")= Named num [1:2] 1 0
#>   ..- attr(*, "names")= chr [1:2] "Agree" "Disagree"

# check the factor levels
unique(binary_df$binary_x)
#> [1]  1  0 NA

# ----------------------------------------------------------------------------

# function also works inside dplyr::across()

# Create new columns using `across()`
binary_df <- df %>%
  dplyr::mutate(
    # use this example if you don't want to flip the factor levels
    dplyr::across(
      x:z,
      make_binary,
      .names = "binary_{col}"
    ),
    # if you want to flip the factor levels, follow this example
    dplyr::across(
      c(x:z),
      # the . placeholder is important to remember
      ~ make_binary(.x, flip_values = TRUE),
      .names = "binary_flipped_{col}"
    )
  )
# show that the function worked properly by creating two new sets of variables
binary_df
#> # A tibble: 6 × 9
#>       x     y     z binary_x binary_y binary_z binary_flipped_x binary_flipped_y
#>   <dbl> <dbl> <dbl>    <dbl>    <dbl>    <dbl>            <dbl>            <dbl>
#> 1     3     2     3        1       NA        1                0               NA
#> 2     4     4     2        0        0       NA                1                1
#> 3     2     6     1       NA        0        1               NA                1
#> 4     1     1     4        1        1        0                0                0
#> 5     5     4     3       NA        0        1               NA                1
#> 6     6     5     6        0       NA        0                1               NA
#> # ℹ 1 more variable: binary_flipped_z <dbl>

# show the underlying structure of the entire df
str(binary_df)
#> tibble [6 × 9] (S3: tbl_df/tbl/data.frame)
#>  $ x               : num [1:6] 3 4 2 1 5 6
#>   ..- attr(*, "labels")= Named num [1:6] 1 2 3 4 5 6
#>   .. ..- attr(*, "names")= chr [1:6] "Strongly agree" "Agree" "Somewhat agree" "Somewhat disagree" ...
#>  $ y               : num [1:6] 2 4 6 1 4 5
#>   ..- attr(*, "labels")= Named num [1:6] 1 2 3 4 5 6
#>   .. ..- attr(*, "names")= chr [1:6] "Strongly agree" "Agree" "Somewhat agree" "Somewhat disagree" ...
#>  $ z               : num [1:6] 3 2 1 4 3 6
#>   ..- attr(*, "labels")= Named num [1:6] 1 2 3 4 5 6
#>   .. ..- attr(*, "names")= chr [1:6] "Strongly agree" "Agree" "Somewhat agree" "Somewhat disagree" ...
#>  $ binary_x        : num [1:6] 1 0 NA 1 NA 0
#>   ..- attr(*, "transformation")= chr "Converting 'x' to a binary variable with 'Agree' = 1 and 'Disagree' = 0."
#>   ..- attr(*, "labels")= Named num [1:2] 1 0
#>   .. ..- attr(*, "names")= chr [1:2] "Agree" "Disagree"
#>   ..- attr(*, "label")= chr "x"
#>  $ binary_y        : num [1:6] NA 0 0 1 0 NA
#>   ..- attr(*, "transformation")= chr "Converting 'y' to a binary variable with 'Agree' = 1 and 'Disagree' = 0."
#>   ..- attr(*, "labels")= Named num [1:2] 1 0
#>   .. ..- attr(*, "names")= chr [1:2] "Agree" "Disagree"
#>   ..- attr(*, "label")= chr "y"
#>  $ binary_z        : num [1:6] 1 NA 1 0 1 0
#>   ..- attr(*, "transformation")= chr "Converting 'z' to a binary variable with 'Agree' = 1 and 'Disagree' = 0."
#>   ..- attr(*, "labels")= Named num [1:2] 1 0
#>   .. ..- attr(*, "names")= chr [1:2] "Agree" "Disagree"
#>   ..- attr(*, "label")= chr "z"
#>  $ binary_flipped_x: num [1:6] 0 1 NA 0 NA 1
#>   ..- attr(*, "transformation")= chr "Converting 'x' to a binary variable with 'Disagree' = 1 and 'Agree' = 0."
#>   ..- attr(*, "labels")= Named num [1:2] 1 0
#>   .. ..- attr(*, "names")= chr [1:2] "Disagree" "Agree"
#>   ..- attr(*, "label")= chr "x"
#>  $ binary_flipped_y: num [1:6] NA 1 1 0 1 NA
#>   ..- attr(*, "transformation")= chr "Converting 'y' to a binary variable with 'Disagree' = 1 and 'Agree' = 0."
#>   ..- attr(*, "labels")= Named num [1:2] 1 0
#>   .. ..- attr(*, "names")= chr [1:2] "Disagree" "Agree"
#>   ..- attr(*, "label")= chr "y"
#>  $ binary_flipped_z: num [1:6] 0 NA 0 1 0 1
#>   ..- attr(*, "transformation")= chr "Converting 'z' to a binary variable with 'Disagree' = 1 and 'Agree' = 0."
#>   ..- attr(*, "labels")= Named num [1:2] 1 0
#>   .. ..- attr(*, "names")= chr [1:2] "Disagree" "Agree"
#>   ..- attr(*, "label")= chr "z"

# show how the levels are flipped when "flip_levels = TRUE"
levels(binary_df$binary_x)
#> NULL
levels(binary_df$binary_flipped_x)
#> NULL
```
