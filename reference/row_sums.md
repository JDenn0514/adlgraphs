# Calculate row sums

This function makes it easy to calculate row sums for multiple
variables. It uses
\<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
syntax to determine which variables to include in the operation.

## Usage

``` r
row_sums(cols, label = NULL, na.rm = TRUE)
```

## Arguments

- cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  The variables you want to use when calculating row sums

- label:

  A string specifying the variable label. If not specified, defaults to
  NULL

- na.rm:

  Determines if NAs should be removed. Default is TRUE.

## Details

This function also has the option of adding a new variable label
attribute. Furthermore, it automatically adds two more attributes:
`transformation` and `variables`. The `trasnformation` attribute
basically explains how the variable was created by saying "Took the
average of..." and then lists the variables included. `variables` just
lists the variables included in the operation.

## Examples

``` r
# load the dplyr package
library(dplyr)
# make a new df with the new column
new <- test_data %>% 
  mutate(
    sdo_sum_new = row_sums(
      # specify the variables involved in the row means
      cols = c(top_rev:deserving_flip),
      # specify the variable label
      label = "Social Dominance Orientation Sum",
      # remove NAs
      na.rm = TRUE
    )
  )

# Show that the attributes
attributes(new$sdo_sum_new)
#> $label
#> [1] "Social Dominance Orientation Sum"
#> 
#> $transformation
#> [1] "Took the sum of top_rev, inferior_rev, dominate_flip, deserving_flip"
#> 
#> $variables
#> [1] "top_rev"        "inferior_rev"   "dominate_flip"  "deserving_flip"
#> 

# show the output
new$sdo_sum_new
#>   [1]  7  7 10  6 10  4  8  8  9  4  4  8  8  8  7  9 11  4  9  5 12  8  7 13  5
#>  [26]  8  4  4 10  4 10  6  6  9 12  4  9  7  7  8  5  7  7  4  6  4 14  9  4  9
#>  [51]  9  8  8  7  7  8  4  9  8  9  6  8  9  6  5  9  6  5  6  6  9  5  4  9  7
#>  [76] 13  4  7  9  7  9  4  4  4  9  4  8  8  5  8  9  9 10  9  4  6  6 10 11  8
#> [101] 10  6 10  6  7  9 10  9  4  4  9  7  6  9  8  8 11  6  8  9  8  5  5  4 12
#> [126]  4 10 10 11  9  8 12  9 12 12 11 12  5 11 11 10  4  8  8  7  9  7  9  7  9
#> [151] 13  8  8  6  7 10  6 12  5  5 10 11  6  6 11  5  5  7  5  5  4  6 10  8  8
#> [176]  4  5  9 10  7 10  6  7 10  4 11  7  6  4  9  8  5  7  7 12  8  4  4  5  6
#> [201] 10 10  9  6  5  4  4  7  5  8  5  9  7  8 10  4 10 10  7  8  8  4  8  6  8
#> [226]  4  4 10  4  9  7 10  7  4  4  8 11  9 11  7 10  5 11  6  7  6 10  9  8  6
#> attr(,"label")
#> [1] "Social Dominance Orientation Sum"
#> attr(,"transformation")
#> [1] "Took the sum of top_rev, inferior_rev, dominate_flip, deserving_flip"
#> attr(,"variables")
#> [1] "top_rev"        "inferior_rev"   "dominate_flip"  "deserving_flip"
```
