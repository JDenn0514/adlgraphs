# Calculate row means

This function makes it easy to calculate row means for multiple
variables. It uses
\<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
syntax to determine which variables to include in the operation.

## Usage

``` r
row_means(cols, label = NULL, na.rm = TRUE)
```

## Arguments

- cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  The variables you want to use when calculating row means

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
    sdo_avg_new = row_means(
      # specify the variables involved in the row means
      cols = c(top_rev:deserving_flip),
      # specify the variable label
      label = "Social Dominance Orientation Average",
      # remove NAs
      na.rm = TRUE
    )
  )

# Show that the attributes
attributes(new$sdo_avg_new)
#> $label
#> [1] "Social Dominance Orientation Average"
#> 
#> $transformation
#> [1] "Took the average of top_rev, inferior_rev, dominate_flip, deserving_flip"
#> 
#> $variables
#> [1] "top_rev"        "inferior_rev"   "dominate_flip"  "deserving_flip"
#> 

# show the output
new$sdo_avg_new
#>   [1] 1.75 1.75 2.50 1.50 2.50 1.00 2.00 2.00 2.25 1.00 1.00 2.00 2.00 2.00 1.75
#>  [16] 2.25 2.75 1.00 2.25 1.25 3.00 2.00 1.75 3.25 1.25 2.00 1.00 1.00 2.50 1.00
#>  [31] 2.50 1.50 1.50 2.25 3.00 1.00 2.25 1.75 1.75 2.00 1.25 1.75 1.75 1.00 1.50
#>  [46] 1.00 3.50 2.25 1.00 2.25 2.25 2.00 2.00 1.75 1.75 2.00 1.00 2.25 2.00 2.25
#>  [61] 1.50 2.00 2.25 1.50 1.25 2.25 1.50 1.25 1.50 1.50 2.25 1.25 1.00 2.25 1.75
#>  [76] 3.25 1.00 1.75 2.25 1.75 2.25 1.00 1.00 1.00 2.25 1.00 2.00 2.00 1.25 2.00
#>  [91] 2.25 2.25 2.50 2.25 1.00 1.50 1.50 2.50 2.75 2.00 2.50 1.50 2.50 1.50 1.75
#> [106] 2.25 2.50 2.25 1.00 1.00 2.25 1.75 1.50 2.25 2.00 2.00 2.75 1.50 2.00 2.25
#> [121] 2.00 1.25 1.25 1.00 3.00 1.00 2.50 2.50 2.75 2.25 2.00 3.00 2.25 3.00 3.00
#> [136] 2.75 3.00 1.25 2.75 2.75 2.50 1.00 2.00 2.00 1.75 2.25 1.75 2.25 1.75 2.25
#> [151] 3.25 2.00 2.00 1.50 1.75 2.50 1.50 3.00 1.25 1.25 2.50 2.75 1.50 1.50 2.75
#> [166] 1.25 1.25 1.75 1.25 1.25 1.00 1.50 2.50 2.00 2.00 1.00 1.25 2.25 2.50 1.75
#> [181] 2.50 1.50 1.75 2.50 1.00 2.75 1.75 1.50 1.00 2.25 2.00 1.25 1.75 1.75 3.00
#> [196] 2.00 1.00 1.00 1.25 1.50 2.50 2.50 2.25 1.50 1.25 1.00 1.00 1.75 1.25 2.00
#> [211] 1.25 2.25 1.75 2.00 2.50 1.00 2.50 2.50 1.75 2.00 2.00 1.00 2.00 1.50 2.00
#> [226] 1.00 1.00 2.50 1.00 2.25 1.75 2.50 1.75 1.00 1.00 2.00 2.75 2.25 2.75 1.75
#> [241] 2.50 1.25 2.75 1.50 1.75 1.50 2.50 2.25 2.00 1.50
#> attr(,"label")
#> [1] "Social Dominance Orientation Average"
#> attr(,"transformation")
#> [1] "Took the average of top_rev, inferior_rev, dominate_flip, deserving_flip"
#> attr(,"variables")
#> [1] "top_rev"        "inferior_rev"   "dominate_flip"  "deserving_flip"
```
