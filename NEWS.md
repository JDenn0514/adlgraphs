# adlgraphs 

# adlgraphs 0.2.0

#### This update includes a bunch of new features and bug fixes. Some of the underlying code has also been updated to be more modular by replacing repetitive code with internal helper functions.

## New functions

#### `get_means`

* This function makes it easy to calculate means with confidence intervals.
* It contains four arguments: `df`, `x`, `group`, and `wt`
* The most important argument is `group` as it allows you to calculate means by a grouping variable. It operates similar to the `.by` argument in some `dplyr` functions. 

#### `get_freqs` 

* This makes it easy to calculate weighted frequencies.
* It contains the same first four arguments as `get_means` but also contains a fifth argument, `cross_tab`. By setting `cross_tab = TRUE` you pivot the table so that it appears like most cross tabs.

#### `make_percent`

* Add the % symbol to a numeric vector and as a result convert to a character vector
* Contains three arguments: x, digits, and scale
  * Digits specifies how many decimal places the percentage should be rounded to
  * Scale determines by what scale the numeric vector values should be multiplied

#### make_df_oxy
* This is a simple function that makes it easier to create documentation for data sets by leveraging the underlying variable labels in the dataset

## Bug fixes and new features

#### `get_freq_table` 

* Now all arguments accept either strings or symbols, making it easier to use with functional programming
* It also contains a new argument `show_genpop` which enables you to determine whether you want a column showing the frequencies for the general population or not.

#### `adl_bar_plots` 
* Fixed a bug where mean plots didn't work in a dodged vertical charts.




