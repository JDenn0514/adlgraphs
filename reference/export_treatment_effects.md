# Export Treatment Effects to Excel

Iterates through a list of dependent variables, calculates treatment
effects using `get_diffs`, and exports the results to a formatted Excel
workbook. It handles statistical testing, conditional formatting (color-
coding significance), and workbook management (appending to existing
files).

## Usage

``` r
export_treatment_effects(
  data,
  dv_list,
  treats,
  group = NULL,
  pos_vars = NULL,
  neg_vars = NULL,
  wt = "wts",
  pval_adj = NULL,
  conf_level = 0.95,
  conf_method = c("wald", "profile"),
  show_means = TRUE,
  show_pct_change = FALSE,
  decimals = 3,
  na.rm = TRUE,
  filename,
  sheet_name = "Treatment Effects",
  append = TRUE
)
```

## Arguments

- data:

  A data frame containing the survey data.

- dv_list:

  A character vector of column names representing the dependent
  variables to analyze.

- treats:

  The column name (unquoted or string) identifying the treatment
  variable.

- group:

  Optional. The column name (unquoted or string) identifying a subgroup
  variable. Defaults to `NULL`.

- pos_vars:

  A character vector of dependent variable names where a positive
  difference is considered "good" (colored green).

- neg_vars:

  A character vector of dependent variable names where a positive
  difference is considered "bad" (colored red).

- wt:

  The column name of the weights variable. Defaults to "wts".

- pval_adj:

  Character string indicating the p-value adjustment method (e.g.,
  "holm", "bonferroni", "fdr"). Passed to
  [`stats::p.adjust`](https://rdrr.io/r/stats/p.adjust.html). Defaults
  to `NULL` (no adjustment).

- conf_level:

  Numeric value between 0 and 1 indicating the confidence level.
  Defaults to 0.95.

- conf_method:

  Character string indicating the method for confidence intervals
  ("wald" or "profile"). Defaults to "wald".

- show_means:

  Logical. Whether to include mean values in the output. Defaults to
  `TRUE`.

- show_pct_change:

  Logical. Whether to include percent change in the output. Defaults to
  `FALSE`.

- decimals:

  Integer. The number of decimal places for numeric outputs. Defaults to
  3.

- na.rm:

  Logical. Whether to remove NA values during calculation. Defaults to
  `TRUE`.

- filename:

  String. The file path for the Excel workbook (e.g.,
  "output/results.xlsx").

- sheet_name:

  String. The name of the worksheet to create or append to. Defaults to
  "Treatment Effects".

- append:

  Logical. If `TRUE`, adds the sheet to an existing workbook if the file
  exists. If `FALSE`, overwrites the file. Defaults to `TRUE`.

## Value

None. The function calculates statistics and writes them directly to an
.xlsx file.

## Examples

``` r
if (FALSE) { # \dontrun{
export_treatment_effects(
  data = survey_data,
  dv_list = c("satisfaction", "likelihood_to_recommend"),
  treats = treatment_group,
  group = segment,
  pos_vars = c("satisfaction"),
  filename = "results.xlsx",
  pval_adj = "holm"
)
} # }
```
