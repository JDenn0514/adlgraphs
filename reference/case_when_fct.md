# `case_when` with factor levels

Recode a variable using the
[`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case_when.html)
syntax

## Usage

``` r
case_when_fct(..., .default = NULL)
```

## Arguments

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  A sequence of two-sided formulas. The left hand side (LHS) determines
  which values match this case. The right hand side (RHS) provides the
  replacement value.

  The LHS inputs must evaluate to logical vectors.

  The RHS inputs will be coerced to their common type.

  All inputs will be recycled to their common size. That said, we
  encourage all LHS inputs to be the same size. Recycling is mainly
  useful for RHS inputs, where you might supply a size 1 input that will
  be recycled to the size of the LHS inputs.

  `NULL` inputs are ignored.

- .default:

  A string. The value used when all of the LHS inputs return either
  `FALSE` or `NA`. If `NULL`, the default, a `NA` will be used.

## Value

A vector with the same size as the common size computed from the inputs
in `...` and the same type as the common type of the RHS inputs in
`...`.

## See also

[`case_match_fct()`](https://jdenn0514.github.io/adlgraphs/reference/case_match_fct.md)

## Examples

``` r
# load the dplyr library so we can use `mutate()`
library(dplyr)

# let's use the `test_data` data set and convert `edu` to a dichotomous factor
# of college degree vs no degree. I'll demonstrate this a few different ways
new_data <- test_data %>%
  mutate(
    edu_f2 = case_when_fct(
      edu %in% c(1:3) ~ "No degree",
      # we can specify the other values explicitly like this
      edu %in% c(4:5) ~ "College degree"
    )
  )

# we can see that the `edu_f2` has two levels and they are in the correct
# and it attached the transformation metadata
str(new_data$edu_f2)
#>  Factor w/ 2 levels "No degree","College degree": 1 1 2 1 1 2 2 2 1 1 ...

# another way of doing this is to indicate that when edu is less than 4 it
# means no degree and when edu is above 3  it means college degree, like this
new_data <- test_data %>%
  mutate(
    edu_f2 = case_when_fct(
      edu < 4 ~ "No degree",
      # we can specify the other values explicitly like this
      edu > 3 ~ "College degree"
    )
  )

# let's check it again
str(new_data$edu_f2)
#>  Factor w/ 2 levels "No degree","College degree": 1 1 2 1 1 2 2 2 1 1 ...

# yet another way to do it, is to use the `.default =` argument to specify
# ALL other values not listed in the prior arguments
new_data <- test_data %>%
  mutate(
    edu_f2 = case_when_fct(
      edu < 4 ~ "No degree",
      # we can specify the other values explicitly like this
      .default = "College degree"
    )
  )

# let's check it again
str(new_data$edu_f2)
#>  Factor w/ 2 levels "No degree","College degree": 1 1 2 1 1 2 2 2 1 1 ...
```
