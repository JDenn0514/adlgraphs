# Export Frequency Tables to Excel with Professional Formatting

Creates professionally formatted frequency tables in Excel with support
for hierarchical headers, multiple grouping variables, and multi-sheet
workbooks. Tables include proper percentage formatting, borders,
footnotes, and optional comments explaining grouping variables.

## Usage

``` r
export_freqs_excel(
  data,
  cols,
  group = NULL,
  wt = NULL,
  drop_zero = FALSE,
  decimals = 1,
  na.rm = TRUE,
  show_genpop = FALSE,
  file_name,
  wb_subject = "",
  wb_category,
  add_comments = TRUE,
  sheet = "Frequencies",
  append_to_existing = TRUE
)
```

## Arguments

- data:

  A data frame or tibble containing the variables to analyze

- cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Column selection for variables to create frequency tables for.

- group:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optional grouping variables for cross-tabulation. Can be a single
  variable name or vector of variable names. Supports multiple levels of
  grouping which will create hierarchical column headers

- wt:

  Optional weight variable name for weighted frequencies. If not
  provided, equal weights are applied to all observations

- drop_zero:

  Logical. Whether to drop categories with zero frequencies. Default is
  `FALSE`

- decimals:

  Integer. Number of decimal places to display for percentages. Default
  is `1`

- na.rm:

  Logical. Whether to remove missing values before calculating
  frequencies. Default is `TRUE`

- show_genpop:

  Logical. Whether to include a "General Population" column showing
  overall frequencies. Default is `FALSE`

- file_name:

  Character string specifying the output Excel file path

- wb_subject:

  Character string for workbook subject metadata. Default is `""`

- wb_category:

  Character string for workbook category metadata

- add_comments:

  Logical. Whether to add hover comments to group headers explaining
  what each grouping variable represents. Default is `TRUE`

- sheet:

  Character string specifying the worksheet name. Default is
  `"Frequencies"`

- append_to_existing:

  Logical. Whether to add to an existing Excel file (if it exists) or
  create a new one. Default is `TRUE`

## Value

Invisibly returns `NULL`. The function is called for its side effect of
creating an Excel file

## Details

The function creates professional frequency tables with the following
features:

- **Hierarchical headers**: Multiple grouping variables create nested
  column headers with proper spanning

- **Smart formatting**: Percentages are formatted as actual Excel
  percentages (not text) to avoid warnings and enable calculations

- **Professional styling**: Tables include borders, centered headers,
  and color-coded sections

- **Informative footnotes**: Automatically generated notes explain what
  each level of grouping represents

- **Multi-sheet support**: Can append new sheets to existing files with
  automatic sheet name incrementing to avoid conflicts

- **Optional comments**: Hover comments on headers provide additional
  context about grouping variables

When using multiple grouping variables, column names are created by
joining group values with underscores (e.g., "Gen_Z_Male_pct"). The
function automatically creates hierarchical headers that make these
relationships clear.

## Multi-sheet Usage

To create multiple sheets in the same Excel file:

    # First call creates the file
    export_freqs_excel(data, vars, group = "generation",
                       file_name = "analysis.xlsx", sheet = "By Generation")

    # Subsequent calls add new sheets
    export_freqs_excel(data, vars, group = c("generation", "gender"),
                       file_name = "analysis.xlsx", sheet = "By Gen and Gender")

## See also

[`get_freqs`](https://jdenn0514.github.io/adlgraphs/reference/get_freqs.md)
for the underlying frequency calculation function

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic frequency table
export_freqs_excel(survey_data, 
                   cols = c(political_party, ideology),
                   file_name = "frequencies.xlsx")

# Grouped by single variable
export_freqs_excel(survey_data,
                   cols = c(political_party, ideology),
                   group = generation,
                   file_name = "by_generation.xlsx")

# Multiple grouping variables with hierarchical headers
export_freqs_excel(survey_data,
                   cols = political_party,
                   group = c(generation, has_children),
                   show_genpop = TRUE,
                   file_name = "detailed_analysis.xlsx")

# Add to existing file with new sheet
export_freqs_excel(survey_data,
                   cols = political_party,
                   group = education,
                   file_name = "detailed_analysis.xlsx",
                   sheet = "By Education")
} # }
```
