# Compute weighted frequencies, optionally grouped and/or across multiple variables

`get_freqs()` computes weighted frequency tables for survey-style data.
It supports:

- Plain data frames with an optional weight column

- `survey.design` and `svyrep.design` objects (from the survey/srvyr
  ecosystem)

- Single-variable or multi-variable inputs

- Optional grouping variables

- Optional inclusion/exclusion of zero-count levels (see Limitations for
  survey multi-variable)

For single-variable inputs, the response column in the output retains
the original variable name. For multi-variable inputs, responses are
pivoted to long format using `names_to` and `values_to`.

## Usage

``` r
get_freqs(
  data,
  x,
  group = NULL,
  wt = NULL,
  names_to = "name",
  values_to = "value",
  name_label = NULL,
  keep = NULL,
  drop_zero = FALSE,
  decimals = 1,
  na.rm = TRUE
)
```

## Arguments

- data:

  A data frame/tibble, `survey.design`, or `svyrep.design` object.

- x:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Columns selecting one or more variables. You can pass:

  - A bare column name (e.g., `x = q1`)

  - A tidyselect expression (e.g., `x = tidyselect::starts_with("q")`)

  - A vector of strings or symbols `c("q1", "q2")` or `c(q1, q2)`.

  - An object containing a vector `tidyselect::all_of(variables)`.

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
  If data is a `grouped_df` and `group` is provided,
  [`get_means()`](https://jdenn0514.github.io/adlgraphs/reference/get_means.md)
  will combine the variable(s) used in either `group_by` function and
  the variable(s) supplied in `group` to calculate frequencies.

- wt:

  Optional weight column (numeric). Ignored for `survey.design` or
  `svyrep.design` inputs, where weights come from the design. If omitted
  for data frames, will output unweighted frequencies.

- names_to:

  A string specifying the name of the column to create to hold the
  variable names (or labels) when `x` selects multiple variables.
  Default is `"name"`.

- values_to:

  A string specifying the name of the column to create to hold the
  responses when `x` selects multiple variables. Default is `"value"`.

- name_label:

  Optional label to attach to the `names_to` column in multi-variable
  outputs (e.g., a question preface). If missing, will check the data
  for a `question_preface` attribute.

- keep:

  Optional post-aggregation filter applied only to multi-variable
  outputs:

  - character vector: keep only rows where `values_to` is in this set

  - function: predicate on the `values_to` vector; returns logical mask
    (length `nrow` or scalar `TRUE`)

  - tidy expression: a dplyr-style filter expression evaluated in the
    result context

- drop_zero:

  Logical; whether to drop zero-count rows from the output. If `FALSE`,
  zero-count levels will be included by using
  [`tidyr::complete()`](https://tidyr.tidyverse.org/reference/complete.html).
  Default is `FALSE`.

- decimals:

  Number of decimal places for rounding counts (`n`). Percent (`pct`) is
  rounded to `decimals + 2` so that it contains the right number of
  decimals when multiplying by 100.

- na.rm:

  Logical; whether to remove rows with missing values in `x` and `group`
  before computing frequencies. Default is `TRUE`.

## Value

A tibble with columns:

- For single-variable inputs: `[x variable]`, `n`, `pct`, and any
  grouping columns.

- For multi-variable inputs: `[grouping columns if any]`, `[names_to]`,
  `[values_to]`, `n`, `pct`.

The result has class `"adlgraphs_freqs"` and common attributes:

- `attr(., "dataset")`: the original dataset

- `attr(., "variable_label")`, `attr(., "variable_name")`

- For grouped outputs: `attr(., "group_names")` and
  `attr(., "group_labels")`

- For multi-variable: `attr(., "item_names")`, `attr(., "item_labels")`,
  `attr(., "x_expr")`

## Details

The `keep` argument is applied after aggregation in multi-variable
outputs to filter rows of the result based on the response column
(`values_to`). It is ignored for single-variable calls. Accepted forms:

- Character vector: `keep = c("yes", "no")`

- Function: `keep = \(v) v %in% c("yes", "no")` or any predicate that
  returns a logical vector of length `nrow(result)` or a single `TRUE`
  (no filtering). `NA` entries are dropped.

- Tidy expression: `keep = .data[[values_to]] %in% c("yes","no")` or
  simply `resp != "skip"` when `values_to = "resp"`.

Note: For tidy expressions, the expression is evaluated in the context
of the result tibble.

## Methods

- `get_freqs.default()`: Operates on data frames/tibbles. If `wt` is
  omitted, simple unweighted frequencies are reported. Calculation is
  from
  [`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)
  with `.drop = drop_zero`; zero-count levels can be included when
  `drop_zero = FALSE` and the variables are factors with unused levels.

- `get_freqs.survey.design()`: Operates on `survey.design` objects.
  Weights are taken from the design. Grouping is honored inside
  low-level survey computations.

- `get_freqs.svyrep.design()`: Operates on `svyrep.design` objects.

## See also

- [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html),
  [`srvyr::as_survey()`](http://gdfe.co/srvyr/reference/as_survey.md)

- [`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html),
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)

## Examples

``` r
# here's a basic unweighted frequency for satisfaction_service
get_freqs(basic_df, x = satisfaction_service)
#> # A tibble: 5 × 3
#>   satisfaction_service     n   pct
#>   <fct>                <dbl> <dbl>
#> 1 Very Dissatisfied        0 0    
#> 2 Dissatisfied             2 0.167
#> 3 Neutral                  3 0.25 
#> 4 Satisfied                4 0.333
#> 5 Very Satisfied           3 0.25 

# now check it with weights
get_freqs(basic_df, x = satisfaction_service, wt = wts)
#> # A tibble: 5 × 3
#>   satisfaction_service     n   pct
#>   <fct>                <dbl> <dbl>
#> 1 Very Dissatisfied        0 0    
#> 2 Dissatisfied             2 0.105
#> 3 Neutral                  3 0.158
#> 4 Satisfied                9 0.474
#> 5 Very Satisfied           5 0.263

# now check grouped
get_freqs(basic_df, x = satisfaction_service, group = grp, wt = wts)
#> # A tibble: 15 × 4
#>    grp   satisfaction_service     n   pct
#>    <fct> <fct>                <dbl> <dbl>
#>  1 A     Very Dissatisfied        0 0    
#>  2 A     Dissatisfied             1 0.2  
#>  3 A     Neutral                  1 0.2  
#>  4 A     Satisfied                1 0.2  
#>  5 A     Very Satisfied           2 0.4  
#>  6 B     Very Dissatisfied        0 0    
#>  7 B     Dissatisfied             0 0    
#>  8 B     Neutral                  1 0.143
#>  9 B     Satisfied                5 0.714
#> 10 B     Very Satisfied           1 0.143
#> 11 C     Very Dissatisfied        0 0    
#> 12 C     Dissatisfied             1 0.143
#> 13 C     Neutral                  1 0.143
#> 14 C     Satisfied                3 0.429
#> 15 C     Very Satisfied           2 0.286

# groups can also be added by using `group_by()` ahead of time
basic_df |>
  dplyr::group_by(grp) |>
  get_freqs(x = satisfaction_service, wt = wts)
#> # A tibble: 15 × 4
#>    grp   satisfaction_service     n   pct
#>    <fct> <fct>                <dbl> <dbl>
#>  1 A     Very Dissatisfied        0 0    
#>  2 A     Dissatisfied             1 0.2  
#>  3 A     Neutral                  1 0.2  
#>  4 A     Satisfied                1 0.2  
#>  5 A     Very Satisfied           2 0.4  
#>  6 B     Very Dissatisfied        0 0    
#>  7 B     Dissatisfied             0 0    
#>  8 B     Neutral                  1 0.143
#>  9 B     Satisfied                5 0.714
#> 10 B     Very Satisfied           1 0.143
#> 11 C     Very Dissatisfied        0 0    
#> 12 C     Dissatisfied             1 0.143
#> 13 C     Neutral                  1 0.143
#> 14 C     Satisfied                3 0.429
#> 15 C     Very Satisfied           2 0.286

# Now check with multiple x variables
get_freqs(
  basic_df,
  x = c("x1", "x2"),
  wt = wts,
  keep = "Yes",
  na.rm = TRUE
)
#> # A tibble: 2 × 4
#>   name     value     n   pct
#>   <fct>    <fct> <dbl> <dbl>
#> 1 Q1. Blue Yes       8 0.444
#> 2 Q2. Red  Yes       7 0.412

# rename the outputs
get_freqs(
  basic_df,
  x = c("x1", "x2"),
  wt = wts,
  names_to = "item",
  values_to = "resp",
  na.rm = TRUE
)
#> # A tibble: 4 × 4
#>   item     resp      n   pct
#>   <fct>    <fct> <dbl> <dbl>
#> 1 Q1. Blue Yes       8 0.444
#> 2 Q1. Blue No       10 0.556
#> 3 Q2. Red  Yes       7 0.412
#> 4 Q2. Red  No       10 0.588

# now check multiple x variables with a grouping variable
get_freqs(
  basic_df,
  x = c("x1", "x2"),
  group = "grp",
  wt = wts,
  na.rm = TRUE
)
#> # A tibble: 12 × 5
#>    grp   name     value     n   pct
#>    <fct> <fct>    <fct> <dbl> <dbl>
#>  1 A     Q1. Blue Yes       2 0.5  
#>  2 A     Q1. Blue No        2 0.5  
#>  3 A     Q2. Red  Yes       3 0.6  
#>  4 A     Q2. Red  No        2 0.4  
#>  5 B     Q1. Blue Yes       1 0.143
#>  6 B     Q1. Blue No        6 0.857
#>  7 B     Q2. Red  Yes       1 0.2  
#>  8 B     Q2. Red  No        4 0.8  
#>  9 C     Q1. Blue Yes       5 0.714
#> 10 C     Q1. Blue No        2 0.286
#> 11 C     Q2. Red  Yes       3 0.429
#> 12 C     Q2. Red  No        4 0.571


# ---- keep examples (multi-variable, data frame) ----

# 1) keep as a character vector: retain only "yes" responses across items
get_freqs(
  basic_df,
  x = c("x1", "x2"),
  wt = wts,
  keep = c("Yes"),
  na.rm = TRUE
)
#> # A tibble: 2 × 4
#>   name     value     n   pct
#>   <fct>    <fct> <dbl> <dbl>
#> 1 Q1. Blue Yes       8 0.444
#> 2 Q2. Red  Yes       7 0.412

# Survey design  ------------
basic_df_svy <- srvyr::as_survey_design(
   ids = id,
   strata = strata,
   weights = "wts",
   .data = basic_df
 )

# basic example
get_freqs(basic_df_svy, x = satisfaction_service)
#> # A tibble: 5 × 3
#>   satisfaction_service     n   pct
#>   <fct>                <dbl> <dbl>
#> 1 Dissatisfied             2 0.105
#> 2 Neutral                  3 0.158
#> 3 Satisfied                9 0.474
#> 4 Very Dissatisfied        0 0    
#> 5 Very Satisfied           5 0.263

# multi-variable example
get_freqs(basic_df_svy, c(x1, x2))
#> # A tibble: 4 × 4
#>   name     value     n   pct
#>   <fct>    <fct> <dbl> <dbl>
#> 1 Q1. Blue Yes       8 0.444
#> 2 Q1. Blue No       10 0.556
#> 3 Q2. Red  Yes       7 0.412
#> 4 Q2. Red  No       10 0.588

# grouped example
get_freqs(basic_df_svy, satisfaction_service, grp)
#> # A tibble: 15 × 4
#>    grp   satisfaction_service     n   pct
#>    <chr> <fct>                <dbl> <dbl>
#>  1 A     Dissatisfied             1 0.2  
#>  2 A     Neutral                  1 0.2  
#>  3 A     Satisfied                1 0.2  
#>  4 A     Very Dissatisfied        0 0    
#>  5 A     Very Satisfied           2 0.4  
#>  6 B     Dissatisfied             0 0    
#>  7 B     Neutral                  1 0.143
#>  8 B     Satisfied                5 0.714
#>  9 B     Very Dissatisfied        0 0    
#> 10 B     Very Satisfied           1 0.143
#> 11 C     Dissatisfied             1 0.143
#> 12 C     Neutral                  1 0.143
#> 13 C     Satisfied                3 0.429
#> 14 C     Very Dissatisfied        0 0    
#> 15 C     Very Satisfied           2 0.286

# grouped example with multi-variable
get_freqs(basic_df_svy, c(x1, x2), grp)
#> # A tibble: 12 × 5
#>    grp   name     value     n   pct
#>    <chr> <fct>    <fct> <dbl> <dbl>
#>  1 A     Q1. Blue Yes       2 0.5  
#>  2 A     Q1. Blue No        2 0.5  
#>  3 A     Q2. Red  Yes       3 0.6  
#>  4 A     Q2. Red  No        2 0.4  
#>  5 B     Q1. Blue Yes       1 0.143
#>  6 B     Q1. Blue No        6 0.857
#>  7 B     Q2. Red  Yes       1 0.2  
#>  8 B     Q2. Red  No        4 0.8  
#>  9 C     Q1. Blue Yes       5 0.714
#> 10 C     Q1. Blue No        2 0.286
#> 11 C     Q2. Red  Yes       3 0.429
#> 12 C     Q2. Red  No        4 0.571
```
