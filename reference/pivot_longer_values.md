# Pivot data from wide to long with value labels

This function is a wrapper around
[`pivot_longer`](https://tidyr.tidyverse.org/reference/pivot_longer.html).
This function operates in pretty much the exact same way but uses the
variable labels from the variables specified in `cols` to make new value
labels in the new variable created in the `names_to` variable.

## Usage

``` r
pivot_longer_values(
  data,
  cols,
  names_to = "names",
  values_to = "values",
  name_label,
  ...
)
```

## Arguments

- data:

  A data frame to pivot.

- cols:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Columns to pivot into longer format.

- names_to:

  A character vector specifying the new column or columns to create from
  the information stored in the column names of `data` specified by
  `cols`.

  - If length 0, or if `NULL` is supplied, no columns will be created.

  - If length 1, a single column will be created which will contain the
    column names specified by `cols`.

  - If length \>1, multiple columns will be created. In this case, one
    of `names_sep` or `names_pattern` must be supplied to specify how
    the column names should be split. There are also two additional
    character values you can take advantage of:

    - `NA` will discard the corresponding component of the column name.

    - `".value"` indicates that the corresponding component of the
      column name defines the name of the output column containing the
      cell values, overriding `values_to` entirely.

- values_to:

  A string specifying the name of the column to create from the data
  stored in cell values. If `names_to` is a character containing the
  special `.value` sentinel, this value will be ignored, and the name of
  the value column will be derived from part of the existing column
  names.

- name_label:

  Add a variable label to the new column with the names of the columns

- ...:

  Additional arguments passed to
  [`pivot_longer`](https://tidyr.tidyverse.org/reference/pivot_longer.html).

## Value

A "long" data.frame.

## Details

An additional note is that this function also works with survey objects
created with either
[`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
or `sryvr::as_survey_design()`. The function first pivots the data, then
re-creates the survey object using the same variables used
