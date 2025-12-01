# Compute weighted frequencies, optionally grouped and/or across multiple variables

`get_freqs()` computes weighted frequency tables for survey-style data.
It supports:

- Plain data frames with an optional weight column

- `survey.design` objects (from the survey/srvyr ecosystem)

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
  group,
  wt,
  names_to = "names",
  values_to = "values",
  name_label,
  keep,
  drop_zero = FALSE,
  decimals = 3,
  na.rm = TRUE
)
```

## Arguments

- data:

  A data frame/tibble or a `survey.design` object.

- x:

  Columns selecting one or more variables to tabulate. You can pass:

  - A bare column name (e.g., `x = q1`)

  - A tidyselect expression (e.g., `x = dplyr::starts_with("q")`)

  - For programmatic selection, use `tidyselect::all_of(c("q1", "q2"))`.

- group:

  Optional columns to group by. Accepts a tidyselect expression. If
  `data` is already a `grouped_df`, those groups are honored in addition
  to `group`.

- wt:

  Optional weight column (numeric). Ignored for `survey.design` inputs,
  where weights come from the design. If omitted for data frames, unit
  weights are added internally.

- names_to:

  Character scalar used only when `x` selects multiple variables; names
  the “item” column in the long output. Default is `"names"`.

- values_to:

  Character scalar used only when `x` selects multiple variables; names
  the response column in the long output. Default is `"values"`.

- name_label:

  Optional label to attach to the `names_to` column in multi-variable
  outputs (e.g., a question preface).

- keep:

  Optional post-aggregation filter applied only to multi-variable
  outputs:

  - character vector: keep only rows where `values_to` is in this set

  - function: predicate on the `values_to` vector; returns logical mask
    (length `nrow` or scalar `TRUE`)

  - tidy expression: a dplyr-style filter expression evaluated in the
    result context

- drop_zero:

  Logical; whether to drop zero-count rows from the output.

  - Default path (data.frame): combined with
    `dplyr::count(.drop = drop_zero)` to control inclusion of zero
    levels.

  - Survey path (`survey.design`):

    - Single-variable: zero-count response levels can be included when
      `drop_zero = FALSE`.

    - Multi-variable: zero-count levels are not materialized at this
      time; see “Limitations”.

- decimals:

  Integer number of decimal places for rounding counts (`n`). Percent
  (`pct`) is rounded to `decimals + 2`.

- na.rm:

  Logical; whether to remove rows with missing values in `x` and `group`
  before computing frequencies. If removing NAs would leave zero rows,
  an informative error is raised.

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
  omitted, unit weights are used. Uses
  [`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)
  with `.drop = drop_zero`; zero-count levels can be included when
  `drop_zero = FALSE` and the variables are factors with unused levels.

- `get_freqs.survey.design()`: Operates on `survey.design` objects.
  Weights are taken from the design. Grouping is honored inside
  low-level survey computations.

## Limitations (survey.design with multiple variables)

For multi-variable `survey.design` inputs (`x` selects multiple
variables), zero-count response levels are not currently expanded.
Results include only observed levels per item, regardless of
`drop_zero`. This differs from the default (non-survey) path. For
single-variable `survey.design` inputs, zero-count levels can be
included when `drop_zero = FALSE`.

## Errors and edge cases

- If `x` selects no columns, an error is raised.

- If `na.rm = TRUE` and removing NAs would leave zero rows for the
  selected `x`/`group` variables, an error is raised.

- If `wt` is provided (default path) but the column does not exist or is
  non-numeric, an error is raised.

- Survey path includes checks for required variables and emptiness after
  NA removal.

## See also

- [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html),
  [`srvyr::as_survey()`](http://gdfe.co/srvyr/reference/as_survey.md)

- [`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html),
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)

## Examples

``` r
# Basic example (data frame)
df <- tibble::tibble(
  grp = rep(c("A", "B"), each = 4),
  q1  = c("yes", "no", "yes", NA, "no", "no", "yes", "no"),
  wts = c(1, 2, 1, 1, 1, 3, 1, 2)
)
get_freqs(df, x = q1, wt = wts, na.rm = TRUE)
#> # A tibble: 2 × 3
#>   q1        n   pct
#>   <chr> <dbl> <dbl>
#> 1 no        8 0.727
#> 2 yes       3 0.273

# Grouped
get_freqs(df, x = q1, group = grp, wt = wts, na.rm = TRUE)
#> # A tibble: 4 × 4
#>   grp   q1        n   pct
#>   <fct> <chr> <dbl> <dbl>
#> 1 A     no        2 0.5  
#> 2 A     yes       2 0.5  
#> 3 B     no        6 0.857
#> 4 B     yes       1 0.143

# Multi-variable (data frame)
df$q2 <- c("red", "red", "blue", "blue", "red", "blue", "blue", NA)
res_multi <- get_freqs(
  df,
  x = tidyselect::all_of(c("q1", "q2")),
  wt = wts,
  names_to = "item",
  values_to = "resp",
  na.rm = TRUE
)
res_multi
#> # A tibble: 4 × 4
#>   item  resp      n   pct
#>   <fct> <chr> <dbl> <dbl>
#> 1 q1    no        8 0.727
#> 2 q1    yes       3 0.273
#> 3 q2    blue      6 0.6  
#> 4 q2    red       4 0.4  

# ---- keep examples (multi-variable, data frame) ----

# 1) keep as a character vector: retain only "yes" responses across items
get_freqs(
  df,
  x = tidyselect::all_of(c("q1", "q2")),
  wt = wts,
  names_to = "item",
  values_to = "resp",
  keep = c("yes"),
  na.rm = TRUE
)
#> # A tibble: 1 × 4
#>   item  resp      n   pct
#>   <fct> <chr> <dbl> <dbl>
#> 1 q1    yes       3 0.273

# 2) keep as a function: retain values ending with 'e' (e.g., "blue")
get_freqs(
  df,
  x = tidyselect::all_of(c("q1", "q2")),
  wt = wts,
  names_to = "item",
  values_to = "resp",
  keep = function(v) grepl("e$", v),
  na.rm = TRUE
)
#> # A tibble: 1 × 4
#>   item  resp      n   pct
#>   <fct> <chr> <dbl> <dbl>
#> 1 q2    blue      6   0.6

# 3) keep as a tidy expression: drop a specific response level
# Here we keep everything except "no"
get_freqs(
  df,
  x = tidyselect::all_of(c("q1", "q2")),
  wt = wts,
  names_to = "item",
  values_to = "resp",
  keep = resp != "no",
  na.rm = TRUE
)
#> # A tibble: 3 × 4
#>   item  resp      n   pct
#>   <fct> <chr> <dbl> <dbl>
#> 1 q1    yes       3 0.273
#> 2 q2    blue      6 0.6  
#> 3 q2    red       4 0.4  

# 4) keep with grouping: filter within groups after aggregation
get_freqs(
  df,
  x = tidyselect::all_of(c("q1", "q2")),
  group = grp,
  wt = wts,
  names_to = "item",
  values_to = "resp",
  keep = .data$resp %in% c("yes", "red"),
  na.rm = TRUE
)
#> # A tibble: 4 × 5
#>   grp   item  resp      n   pct
#>   <fct> <fct> <chr> <dbl> <dbl>
#> 1 A     q1    yes       2 0.5  
#> 2 A     q2    red       3 0.6  
#> 3 B     q1    yes       1 0.143
#> 4 B     q2    red       1 0.2  

# 5) keep function returning TRUE (no-op): leaves result unchanged
get_freqs(
  df,
  x = tidyselect::all_of(c("q1", "q2")),
  wt = wts,
  names_to = "item",
  values_to = "resp",
  keep = function(v) TRUE,
  na.rm = TRUE
)
#> # A tibble: 4 × 4
#>   item  resp      n   pct
#>   <fct> <chr> <dbl> <dbl>
#> 1 q1    no        8 0.727
#> 2 q1    yes       3 0.273
#> 3 q2    blue      6 0.6  
#> 4 q2    red       4 0.4  

# Survey design (single variable)
dff <- tibble::tibble(
  grp = rep(c("A", "B"), each = 4),
  q1  = c("yes", "no", "yes", NA, "no", "no", "yes", "no"),
  q2  = c("red", "red", "blue", "blue", "red", "blue", "blue", NA),
  wts = c(1, 2, 1, 1, 1, 3, 1, 2)
)
dsn <- survey::svydesign(ids = ~1, weights = ~wts, data = dff)
get_freqs(dsn, x = q1, na.rm = TRUE)
#> # A tibble: 2 × 3
#>   q1        n   pct
#>   <chr> <dbl> <dbl>
#> 1 no        8 0.727
#> 2 yes       3 0.273

# Survey design (multi-variable) — limitation:
# Zero-count levels are not expanded for multi-variable survey inputs.
get_freqs(
  dsn,
  x = tidyselect::all_of(c("q1", "q2")),
  names_to = "item",
  values_to = "resp",
  na.rm = TRUE
)
#> # A tibble: 4 × 4
#>   item  resp      n   pct
#>   <fct> <chr> <dbl> <dbl>
#> 1 q1    no        4 0.571
#> 2 q1    yes       3 0.429
#> 3 q2    blue      4 0.571
#> 4 q2    red       3 0.429

# Note: keep is also supported for survey multi-variable outputs
get_freqs(
  dsn,
  x = tidyselect::all_of(c("q1", "q2")),
  names_to = "item",
  values_to = "resp",
  keep = resp %in% c("yes", "red"),
  na.rm = TRUE
)
#> # A tibble: 2 × 4
#>   item  resp      n   pct
#>   <fct> <chr> <dbl> <dbl>
#> 1 q1    yes       3 0.429
#> 2 q2    red       3 0.429
```
