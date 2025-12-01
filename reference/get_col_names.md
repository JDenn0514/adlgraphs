# Get the column names

This function is essentially a wrapper around
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
and [`colnames()`](https://rdrr.io/r/base/colnames.html).
`get_col_names(data, cols)` is the the same thing as
`colnames(dplyr::select(data, cols))` just with slightly less typing.

## Usage

``` r
get_col_names(data, cols)
```

## Arguments

- data:

  a `data.frame` or `tibble`

- cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  One or more unquoted expressions separated by commas. Variable names
  can be used as if they were positions in the data frame, so
  expressions like x:y can be used to select a range of variables.

## Value

a vector of columns in data

## Examples

``` r
library(adlgraphs)
# let's get all of the column names between "top" and "run"
get_col_names(test_data, c(top:run))
#> [1] "top"        "inferior"   "dominate"   "deserving"  "special"   
#> [6] "harder"     "controlled" "small"      "run"       
# can also use tidyselect syntax
get_col_names(test_data, tidyselect::starts_with("pol_part"))
#> [1] "pol_part_rally"    "pol_part_worked"   "pol_part_contact" 
#> [4] "pol_part_money"    "pol_part_social"   "pol_part_attended"
#> [7] "pol_part_none"     "pol_part_sum"     
```
