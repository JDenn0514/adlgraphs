# Calculate weighted frequencies

**\[experimental\]**

Use this function to calculate simple weighted frequencies. You can also
specify a grouping variable by which you want to calculate the
frequencies.

The `x`, `group`, and `wt` arguments can either be strings or symbols
(meaning they can have quotes or no quotes). The benefit of this is that
it makes it really easy to iterate this function over a list or vector
of variables with other functions like
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html)
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) or
[`purrr::walk()`](https://purrr.tidyverse.org/reference/map.html)
[`purrr::walk()`](https://purrr.tidyverse.org/reference/map.html) that
are found in the `purrr` package.

## Usage

``` r
funky_freqs(data, x, group, wt, drop_zero = FALSE, decimals = 3, na.rm = TRUE)
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
