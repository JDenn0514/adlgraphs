# Create Polling Crosstabs and Export to Excel

This function takes survey data and creates formatted crosstabs showing
weighted percentages of row variables across different subgroups. The
results are exported to an Excel file with proper formatting.

## Usage

``` r
create_polling_crosstabs(
  data,
  row_vars,
  subgroup_vars,
  wt_var,
  min_n = 75,
  file_name,
  sheet_name,
  overwrite_existing_sheet = TRUE,
  summary_string = ""
)
```

## Arguments

- data:

  A data frame or survey design object containing survey responses

- row_vars:

  Character vector of variable names to display as rows

- subgroup_vars:

  Character vector of variable names to use as column subgroups

- wt_var:

  Character string specifying the name of the weight variable. Ignored
  when `data` is a `survey.design` or `svyrep.design` object, in which
  case weights are taken from the design.

- min_n:

  Integer specifying minimum unweighted sample size for a subgroup to be
  included (default: 75)

- file_name:

  Character string specifying the Excel file path/name

- sheet_name:

  Character string specifying the sheet name

- overwrite_existing_sheet:

  Logical indicating whether to overwrite existing sheet with same name
  (default: TRUE)

- summary_string:

  Character string to display in the top-left merged cell (default: "")

## Value

Data frame with columns: subgroup_var, subgroup_var_label, subgroup_val,
n, row_var, row_var_label, row_val, pct (as decimal), moe (as decimal).
All categorical columns are factors with levels in the order passed to
the function. Sorted by subgroup variables then row variables.

## Details

Weighted percentages are calculated via
[`get_freqs()`](https://jdenn0514.github.io/adlgraphs/reference/get_freqs.md),
which supports plain data frames with a weight column as well as
`survey.design` and `svyrep.design` objects.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- create_polling_crosstabs(
  data = survey_data,
  row_vars = c("job_approval", "direction"),
  subgroup_vars = c("gender", "race"),
  wt_var = "weight",
  min_n = 75,
  file_name = "crosstabs.xlsx",
  sheet_name = "Results",
  summary_string = "January 2026 National Poll"
)
} # }
```
