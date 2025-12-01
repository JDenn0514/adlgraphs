# Export data frame with only bots and duplicates

**\[superseded\]**

## Usage

``` r
export_bot_dupe(data, filename, export_raw_data = TRUE, id = NULL)
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

## Details

`export_bot_dupe()` is superseded because the function
[`export_bogus()`](https://jdenn0514.github.io/adlgraphs/reference/export_bogus.md)
does the same thing but also removes speedsters.

This function creates a file containing all responses suspected of being
bots and/or duplicates. Can create a .xlsx, .sav, or .csv file
