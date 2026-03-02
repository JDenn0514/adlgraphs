# Compute survey means with CIs, SD, and N for grouped complex designs

`get_means()` computes design-based means and confidence intervals for a
single numeric variable from complex survey objects, optionally grouped
by one or more variables. It supports `survey.design`, `svyrep.design`,
[`srvyr::tbl_svy`](http://gdfe.co/srvyr/reference/tbl_svy.md), and plain
`data.frame` inputs (with a simpler weighted/unweighted implementation).

## Usage

``` r
get_means(
  data,
  x,
  group = NULL,
  wt = NULL,
  decimals = 3,
  na.rm = TRUE,
  conf_level = 0.95,
  df = Inf
)
```

## Arguments

- data:

  A survey object (survey.design, svyrep.design, or srvyr::tbl_svy), or
  a plain data.frame.

- x:

  The variable with which you wany to get the mean. Variable must be
  numeric but can be input as a string or symbol (e.g., "var" or var).

- group:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  A selection of columns to group the data. This operates very similarly
  to `.by` from dplyr (for more info on that see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html)). It
  can also be a character vector. If using an external character vector
  it must be wrapped in curly brackets (`{{}}`).

  In addition, grouped data can be piped in via
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  or
  [`srvyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  If data is a `grouped_df` and `group` is provided, `get_means()` will
  combine the variable(s) used in either `group_by` function and the
  variable(s) supplied in `group` to calculate means.

- wt:

  Optional weights column name (for data.frame method only). Ignored for
  survey objects, since weights/replicates are stored in the design.

- decimals:

  Number of decimals to round the results to. Default is 3.

- na.rm:

  Logical; whether to remove rows with missing values in `x` and `group`
  before computing frequencies. Default is `TRUE`.

- conf_level:

  Determine the confidence level for Wald CIs. Default is 0.95.

- df:

  Degrees of freedom for t critical values. Defaults to `Inf` (z-based).
  For designs, you may prefer survey::degf(design); for replicate
  designs, Inf is conventional.

## Value

A tibble with one row per group (or one row if ungrouped). With the
following columns:

- grouping columns (preserving factor/label semantics for display)

- `mean`: design-based mean of x

- `sd`: weighted population standard deviation within each group

- `n`: sum of weights within each group (weighted N)

- `conf_low`, `conf_high`: Wald confidence interval bounds

The result has class `"adlgraphs_means"` and common attributes:

- `attr(., "dataset")`: the original dataset

- `attr(., "variable_label")`, `attr(., "variable_name")`

- For grouped outputs: `attr(., "group_names")` and
  `attr(., "group_labels")`

- For multi-variable: `attr(., "item_names")`, `attr(., "item_labels")`,
  `attr(., "x_expr")`

## Details

- Survey objects are subset to remove NAs in x and grouping variables
  before grouping, preserving design alignment
  (ids/strata/weights/replicates).

- Means and standard errors are computed via survey::svymean(). CIs are
  Wald intervals using either z (df = Inf) or t (finite df) critical
  values.

- sd is the weighted population SD and n is the sum of weights. Both are
  used in the SE and CI calculations.

- Group output order follows factor levels for factors, alphabetical for
  characters, and ascending for numerics.

- Value labels for grouping variables are preserved in the output by
  factorizing keys against original labels; variable-level labels are
  copied to output columns.

## Methods

- `get_means.survey.design()`: Complex survey designs. For more
  information visit
  [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
  and
  [`srvyr::as_survey_design()`](http://gdfe.co/srvyr/reference/as_survey_design.md).

- `get_means.svyrep.design()`: Replicate-weight survey designs. For more
  information visit
  [`survey::as.svrepdesign()`](https://rdrr.io/pkg/survey/man/as.svrepdesign.html)
  and
  [`srvyr::as_survey_rep()`](http://gdfe.co/srvyr/reference/as_survey_rep.md)

- ``` get_means.tbl_svy()``: Unwraps srvyr::tbl_svy and dispatches to the appropriate survey method. For more info, visit  ```srvyr::as_survey_design()`and`srvyr::as_survey_rep()\`.

- `get_means.data.frame()`: Non-design summary with optional weights.

## Examples

``` r
# Setup example data
library(dplyr)

# Let's calculate the overall average score for trad_n
get_means(test_data, trad_n)
#> # A tibble: 1 × 5
#>    mean    sd     n conf_low conf_high
#> * <dbl> <dbl> <dbl>    <dbl>     <dbl>
#> 1  4.22  3.87   250     3.74      4.70

# it also works if x is a string
get_means(test_data, "trad_n")
#> # A tibble: 1 × 5
#>    mean    sd     n conf_low conf_high
#> * <dbl> <dbl> <dbl>    <dbl>     <dbl>
#> 1  4.22  3.87   250     3.74      4.70

# Let's do that again but add weights
get_means(test_data, trad_n, wt = wts)
#> # A tibble: 1 × 5
#>    mean    sd     n conf_low conf_high
#> * <dbl> <dbl> <dbl>    <dbl>     <dbl>
#> 1  3.97  3.76  245.     3.50      4.45

# the wt argument can also be in quotes like this
get_means(test_data, "trad_n", wt = "wts")
#> # A tibble: 1 × 5
#>    mean    sd     n conf_low conf_high
#> * <dbl> <dbl> <dbl>    <dbl>     <dbl>
#> 1  3.97  3.76  245.     3.50      4.45

# Now let's do the average score for different education levels
get_means(test_data, trad_n, edu_f, wts)
#> # A tibble: 4 × 6
#>   edu_f                mean    sd     n conf_low conf_high
#> * <fct>               <dbl> <dbl> <dbl>    <dbl>     <dbl>
#> 1 High School or Less  4.14  3.59  69.8     3.29      5.00
#> 2 Some College         3.93  3.96  86.3     3.08      4.78
#> 3 Bachelor's Degree    4.23  3.83  49.7     3.14      5.32
#> 4 Graduate Degree      3.45  3.41  39.4     2.35      4.55

# it also works with quotes
get_means(test_data, "trad_n", "edu_f", "wts")
#> # A tibble: 4 × 6
#>   edu_f                mean    sd     n conf_low conf_high
#> * <fct>               <dbl> <dbl> <dbl>    <dbl>     <dbl>
#> 1 High School or Less  4.14  3.59  69.8     3.29      5.00
#> 2 Some College         3.93  3.96  86.3     3.08      4.78
#> 3 Bachelor's Degree    4.23  3.83  49.7     3.14      5.32
#> 4 Graduate Degree      3.45  3.41  39.4     2.35      4.55

# you can also pipe in the `data` argument if you want to do some data
# transformations before you calculate the means. For example, say you want
# to compare the means of `trad_n` among people who agreed vs disagreed with
# the variable `top`:
test_data %>%
  mutate(top_f2 = make_dicho(top)) %>%
  get_means(trad_n, top_f2, wts)
#> # A tibble: 2 × 6
#>   top_f2    mean    sd     n conf_low conf_high
#> * <fct>    <dbl> <dbl> <dbl>    <dbl>     <dbl>
#> 1 Agree     5.08  3.83  111.     4.36      5.80
#> 2 Disagree  3.05  3.43  134.     2.46      3.64
```
