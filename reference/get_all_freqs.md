# Export frequencies for a set of variables to a word doc.

This function makes it easy to export frequencies and cross-tabs of a
set of variables to a Word Document. It uses the

## Usage

``` r
get_all_freqs(
  data,
  cols,
  group = NULL,
  wt = NULL,
  drop_zero = FALSE,
  decimals = 1,
  na.rm = TRUE,
  show_genpop = FALSE,
  file_name
)
```

## Arguments

- data:

  A data frame or tibble object

- cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  The variables you want to get the frequencies for.

- group:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  A selection of columns to group the data by in addition to `treats`.
  This operates very similarly to `.by` from dplyr (for more info on
  that see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html)). See
  examples to see how it operates.

- wt:

  A character string. Add if you have a weighting variable and want to
  get weighted frequencies

- drop_zero:

  Logical. Determines if rows with 0 should be removed. Default is
  `FALSE`.

- decimals:

  Number of decimals each number should be rounded to. Default is 1.

- na.rm:

  Logical. Determines if NAs should be kept or removed. Default is
  `TRUE`.

- show_genpop:

  Logical. Determines if there is a column showing the frequencies for
  the general population. Default is `FALSE` which does not include
  columns for the full sample. If `TRUE`, includeds two columns at the
  end for the full sample.

- file_name:

  A character string specifying the name of the file to be created with
  the frequencies and where the file will be located. File must end in
  .docx
