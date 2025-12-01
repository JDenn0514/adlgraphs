# Get value labels

This function makes it easy to get the value labels from either an
individual vector or a data frame. NOTE: it is not possible to set or
modify the variable labels with this function.

## Usage

``` r
attr_val_labels(x, data)
```

## Arguments

- x:

  A vector object, the name of a column in a `data.frame`, or an an
  actual `data.frame` object.

- data:

  A `data.frame` or `tibble` object. This should only be specified when
  `x` is only the name of a column in a `data.frame`.

## Value

If `x` is a variable or vector, a named vector containing the "labels"
attribute, if one is present, is returned. If `x` is a `data.frame` then
a named list comprised of the "labels" attribute from each variable is
returned.
