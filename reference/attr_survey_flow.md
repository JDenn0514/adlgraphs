# Get the survey_flow attribute

This function makes it easy to get the survey_flow from either an
individual vector or a data frame. NOTE: it is not possible to set or
modify the question preface attribute with this function.

## Usage

``` r
attr_survey_flow(x, data)
```

## Arguments

- x:

  A vector object, the name of a column in a `data.frame`, or an an
  actual `data.frame` object.

- data:

  A `data.frame` or `tibble` object. This should only be specified when
  `x` is only the name of a column in a `data.frame`.

## Value

If `x` is a variable or vector, a string containing the "survey_flow"
attribute, if one is present, is returned. If `x` is a `data.frame` then
a named vector with the "survey_flow" attribute from each variable is
returned.
