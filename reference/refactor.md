# Assign or reorder factor levels manually

This is a low level function that allows you to convert a character
vector into a factor and manually assign the levels, or to manually
reassign the levels of a factor vector. While very similar to
[`factor()`](https://rdrr.io/r/base/factor.html) a key difference is
that this keeps the original attributes of `x`.

## Usage

``` r
refactor(x, new_levels, ordered = NA)
```

## Arguments

- x:

  A factor or character vector

- new_levels:

  A list of the new levels of the factor

- ordered:

  Logical. Specifies if the factor is ordered. Default is NA, which
  checks to see if the factor is ordered and then uses that to determine
  if it should be ordered

## Value

A factor variable of the same length as `x`

## Examples

``` r
# load the dplyr library so we can use `mutate()`
library(dplyr)

# let's manually reorder the factor levels of `edu_f` from the `test_data`
# data set so it's in a random order that I specify
test_data <- test_data %>%
  mutate(
    # make the new reordered variable
    edu_f_reordered = refactor(
      # specify we are reordering the `edu_f` variable
      x = edu_f,
      new_levels = c(
        "Bachelor's Degree",
        "Graduate Degree",
        "High School or Less",
        "Some College"
      )
    )
  )

# let's check the new levels
levels(test_data$edu_f_reordered)
#> [1] "Bachelor's Degree"   "Graduate Degree"     "High School or Less"
#> [4] "Some College"       
# and compare them to the original levels
levels(test_data$edu_f)
#> [1] "High School or Less" "Some College"        "Bachelor's Degree"  
#> [4] "Graduate Degree"    
```
