# adlgraphs 0.3.62

## Changes to get_loadings

- Changed the underlying methods so that it works on an object created used `psych::fa()` and `factanal()`

- Redeveloped the way it calculates the grouped variables. It now operates the same way as most of the other functions in the package.

# adlgraphs 0.3.61

## Deprecated the `bot_dupe` functions in favor of `bogus` functions

#### `bogus`

- These functions operate virtually the same as bot_dupe, but also remove speedsters.

# adlgraphs 0.3.6

## added two new functions `row_means()` and `make_nested()`

#### `row_means`

- This allows you to calculate row means

#### `make_nested` 

- This produces a nested data frame

#### Fixing some bugs in `get_corr()`

- Previously `get_corr()` had some bugs that have been fixed.

- Also added the ability to group by multiple variables

# adlgraphs 0.3.53

## added a new function `flip_val()` and updated a vignette

#### `flip_val`

- This allows you to flip the valence of a variable

#### updated the vignette for `data_transformations`

- Simplified the vignette and added information about `flip_val()` 

# adlgraphs 0.3.52
## Added a new function `set_question_preface()`

- Allows you to set the question preface and update the variable label

# adlgraphs 0.3.51

## Updated an existing function

#### get_diffs

- Added two new arguments, show_means, and show_pct_change.

## Updated the documentation for most function

- Added a return value for most functions

# adlgraphs 0.3.5

## Create a new function

#### get_diffs

- This function calculates the difference in means. 

# adlgraphs 0.3.42

## added an additional argument to `make_factor()`

- I added a new argument that allows you to specify the levels. This is not fully implemented yet, however.

# adlgraphs 0.3.41

## created an experimental function and updated another

#### funky_freqs

- This is an experimental function that will hopefully become the basis for get_freqs as it is three times as fast.

#### get_loadings

- Adding an argument that sorts the data

# adlgraphs 0.3.4

## created a new function and updated another

#### get_all corr

- This function makes it easy to get the correlations for all variables in a data set.

#### Decently big changes to `dunnet`

- Added new functionality so that data can be grouped before performing `dunnet()` and improved the group argument to include functionality for grouping by multiple variables.

# adlgraphs 0.3.3

## created two new functions

#### flatten_labelled_vec

- Flatten a labelled vector. or example if a vector has values 1 and 0 where 1 is "Yes" and 0 is "No", it will output a string that reads: `1 = "Yes", 0 = "No"`.

#### remove_bot_dupe, get_bot_dupe, and export_bot_dupe

- These three functions make it easy to remove bots and duplicates, remove all non-bots and duplicates, and export all bots and duplicates.

# adlgraphs 0.3.3

## Created a new function

#### get_corr

- Calculate weighted correlations among each level of a group

# adlgraphs 0.3.2

## Created a new function

#### dunnett

- Perform Dunnett's multiple comparisons test

# adlgraphs 0.3.1

## A bunch of new functions
#### attr_var_label

- Get variable label for vectors and dataframes

#### attr_val_label

- Get value labels for vectors and dataframes

#### attr_levels

- Get factor levels for vectors and dataframes

#### attr_note

- Get note attribute for vectors and dataframes

#### attr_survey_flow

- Get survey_flow attribute for vectors and dataframes

#### attr_question_preface

- Get question_preface attribute for vectors and dataframes

#### attr_transformation

- get transformation attribute for vectors and dataframes


# adlgraphs 0.2.4

## Minor addition

#### `adl_bar_plots`
- Added new argument that enables wrapping facet labels.

# adlgraphs 0.2.3

## This update mainly consisted of bug fixes

#### `make_factor` 
- Did not work with factor or character vectors so updated
- With factor vectors now just returns the factor vector
- With character vectors converts it to a factor

#### `prettytable` 
- Fixed a bug that returned the variable name instead of variable labels




# adlgraphs 0.2.2

## New functions

#### `make_factor`

- This function takes a labelled vector and uses the value labels to turn it into a factor
- contains two arguments: `x` and `ordered`

## Bug fixes and small additions

#### `pivot_longer_values`

- Added a new argument that enables you to add a variable label to the variable that contains the names (names_to)
- Using internal functions to reduce external dependencies

#### `attr_var_label`

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
