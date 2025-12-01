# Get the frequencies as a GT table

This is essentially a wrapper around
[`get_freqs()`](https://jdenn0514.github.io/adlgraphs/reference/get_freqs.md)
and
[`prettytable()`](https://jdenn0514.github.io/adlgraphs/reference/prettytable.md)
so you only have to call this rather than calling both functions. It
takes the
[`get_freqs()`](https://jdenn0514.github.io/adlgraphs/reference/get_freqs.md)
output and then makes it pretty using
[`prettytable()`](https://jdenn0514.github.io/adlgraphs/reference/prettytable.md).

## Usage

``` r
get_freq_table(
  data,
  x,
  group = NULL,
  wt = NULL,
  drop_zero = FALSE,
  decimals = 3,
  na.rm = TRUE,
  show_genpop = FALSE
)
```

## Arguments

- data:

  An object of type data.frame or tibble. If piping the data into the
  function, this is not required.

- x:

  Either a character string or symbol. The variable with which want to
  get the frequencies.

- group:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  A selection of columns to group the data by in addition to `treats`.
  This operates very similarly to `.by` from dplyr (for more info on
  that see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html)).

- wt:

  Weights. Add if you have a weighting variable and want to get weighted
  frequencies.

- drop_zero:

  Logical. Determines if rows with 0 should be removed Default is
  `FALSE`.

- decimals:

  Number of decimals each number should be rounded to. Default is 3.

- na.rm:

  Logical. Determines if NAs should be kept or removed Default is
  `TRUE`.

- show_genpop:

  Logical. If the data is grouped, determines if data should should be
  shown for the general population as well. `FALSE`, the default, does
  not show the results for the general population. `TRUE` shows the
  results for the general population in a new column.
