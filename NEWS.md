# adlgraphs 0.2.2

## New functions

#### `make_factor`

- This function takes a labelled vector and uses the value labels to turn it into a factor
- contains two arguments: `x` and `ordered`

## Bug fixes and small additions

#### `pivot_longer_values`

- Added a new argument that enables you to add a variable label to the variable that contains the names (names_to)
- Using internal functions to reduce external dependencies

#### attr_var_label

- Updated the internal code so that it only gives you the attribute under "label"
- Previously if "label" was NULL, it would give you the values in "labels"

# adlgraphs 0.2.1

#### This update features a lot of new changes:

## New functions

#### `codebook`

- This function allows you to create a new data.frame object where each column is a different attribute and each row is a different variable
- Contains one argument: `df`

#### `prettytable`

- Add this function after using `get_freqs()` to make a pretty gt table
- Contains one argument: `x`
- Hoping to continue building this out for means, coefficients, and possibly more

#### `refactor`

- This allows you to reorder the levels of a factor variable
- Contains one argument: `x`

#### `stat_density_quant`

- Create a density plot with quantile lines
- Has a bunch of arguments: `mapping`, `data`, `geom`, `position`, `...`,  `bw`, `adjust`, `kernel`, `n`, `na.rm`, `bounds`, `show.legend`, `inherit.aes`, `quantile_lines`, `calc_ecdf`, `quantiles`
- This may still be buggy and I still need to create the accompanying geom


# adlgraphs 0.2.0

#### This update includes a bunch of new features and bug fixes. Some of the underlying code has also been updated to be more modular by replacing repetitive code with internal helper functions.

## New functions

#### `get_means`

-   This function makes it easy to calculate means with confidence intervals.
-   It contains four arguments: `df`, `x`, `group`, and `wt`
-   The most important argument is `group` as it allows you to calculate means by a grouping variable. It operates similar to the `.by` argument in some `dplyr` functions.

#### `get_freqs`

-   This makes it easy to calculate weighted frequencies.
-   It contains the same first four arguments as `get_means` but also contains a fifth argument, `cross_tab`. By setting `cross_tab = TRUE` you pivot the table so that it appears like most cross tabs.

#### `make_percent`

-   Add the % symbol to a numeric vector and as a result convert to a character vector
-   Contains three arguments: x, digits, and scale
    -   Digits specifies how many decimal places the percentage should be rounded to
    -   Scale determines by what scale the numeric vector values should be multiplied

#### make_df_oxy

-   This is a simple function that makes it easier to create documentation for data sets by leveraging the underlying variable labels in the dataset

## Bug fixes and new features

#### `get_freq_table`

-   Now all arguments accept either strings or symbols, making it easier to use with functional programming
-   It also contains a new argument `show_genpop` which enables you to determine whether you want a column showing the frequencies for the general population or not.

#### `adl_bar_plots`

-   Fixed a bug where mean plots didn't work in a dodged vertical charts.
