# Get the note attribute

This function makes it easy to get the note attribute from either an
individual vector or a data frame. NOTE: it is not possible to set or
modify the note attribute with this function.

## Usage

``` r
attr_note(x, data)
```

## Arguments

- x:

  A vector object, the name of a column in a `data.frame`, or an an
  actual `data.frame` object.

- data:

  A `data.frame` or `tibble` object. This should only be specified when
  `x` is only the name of a column in a `data.frame`.

## Value

If `x` is a variable or vector, a string containing the "note"
attribute, if one is present, is returned. If `x` is a `data.frame` then
a named vector with the "note" attribute from each variable is returned.
