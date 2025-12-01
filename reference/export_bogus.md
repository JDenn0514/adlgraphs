# Export data frame with only bots and duplicates

This function creates a file containing all responses suspected of being
bots, duplicates, or speedsters. Can create a .xlsx, .sav, or .csv file.
It uses
[`get_bogus()`](https://jdenn0514.github.io/adlgraphs/reference/get_bogus.md)
under the hood.

## Usage

``` r
export_bogus(
  data,
  filename,
  export_raw_data = TRUE,
  id = NULL,
  duration,
  cut_off = 0.3
)
```

## Arguments

- data:

  A data.frame object you want to operate on

- filename:

  The name of the file you want to create

- export_raw_data:

  Logical. If TRUE, the default, exports the raw data. If FALSE, exports
  only three columns: the ID, duplicate, and bot

- id:

  The unique ID variable. Only relevant when `export_raw_data` is FALSE.

- duration:

  The name of the time duration variable. Must be a string.

- cut_off:

  Specify what percentage of the median time should be used to remove
  speedsters. Default is 0.3, which means people who's time to complete
  is 0.3 that of the median completion time are removed.
