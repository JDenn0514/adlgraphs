# Create a nested data frame

This function is very similar to
[`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html)in
that it creates a data frame where one of the columns is comprised of a
list of data frames.

## Usage

``` r
make_nested(data, group, na.rm = TRUE, sep = "_")
```

## Arguments

- data:

  A data frame

- group:

  Columns to nest by; these will remain in the outer data frame. If
  `data` is not a grouped data frame, then this must be supplied. If
  supplying multiple must be in a vector. Can be either a string or
  symbol.

- na.rm:

  Determines if rows with NA should be kept or dropped. Defaults to
  TRUE.

- sep:

  A character string to separate the values. This is the `sep` argument
  in [`paste()`](https://rdrr.io/r/base/paste.html). Check examples to
  to see it in action.

## Details

While similar to
[`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html) there
are a few key differences.

The first is that this function uses the vctrs package under the hood,
which makes it about twice as fast as
[`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html).

Second, it is more limited in its scope and functionality. Where
[`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html)
allows you to determine which columns are in the inner data frames (the
ones in the list column), this function does not. Instead, you are only
able to specify the variables that remain in the outer data frame.

Third, this function creates an extra column called "name" that
concatenates the values from the columns in the outer rows together
using [`paste()`](https://rdrr.io/r/base/paste.html).

Fourth, unlike in
[`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html) where
variables supplied to the `.by` argument supersede any grouping
variables specified through
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
`nest_data()` combines the two to nest the data.

## Examples

``` r
library(dplyr)
# nest by one variable
make_nested(test_data, pid_f3)
#> Error in UseMethod("make_nested"): no applicable method for 'make_nested' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"

# nest by multiple variables
make_nested(test_data, c(pid_f3, edu_f2))
#> Error in UseMethod("make_nested"): no applicable method for 'make_nested' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"

# group data to create nested data frame
test_data %>% 
  dplyr::group_by(pid_f3, edu_f2) %>% 
  make_nested()
#> Error in UseMethod("make_nested"): no applicable method for 'make_nested' applied to an object of class "c('grouped_df', 'tbl_df', 'tbl', 'data.frame')"

# use group_by and nest_data 
test_data %>% 
  dplyr::group_by(pid_f3) %>% 
  make_nested(edu_f2)
#> Error in UseMethod("make_nested"): no applicable method for 'make_nested' applied to an object of class "c('grouped_df', 'tbl_df', 'tbl', 'data.frame')"

# use different sep argument
make_nested(test_data, c(pid_f3, edu_f2), sep = ":")
#> Error in UseMethod("make_nested"): no applicable method for 'make_nested' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"
  

```
