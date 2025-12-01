# Get variable label

This function makes it easy to get the variable labels from either an
individual vector or a data frame. NOTE: it is not possible to set or
modify the variable labels with this function.

## Usage

``` r
attr_levels(x, data)
```

## Arguments

- x:

  A vector object, the name of a column in a `data.frame`, or an an
  actual `data.frame` object.

- data:

  A `data.frame` or `tibble` object. This should only be specified when
  `x` is only the name of a column in a `data.frame`.
