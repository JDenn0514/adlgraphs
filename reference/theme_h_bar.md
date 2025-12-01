# Theme for non-stacked horizontal bar plots

This function creates a theme for non-stacked horizontal bar plots.

## Usage

``` r
theme_h_bar(
  base_size = 12,
  legend_position = "none",
  axis_text_x = FALSE,
  grid = FALSE,
  ...
)
```

## Arguments

- base_size:

  Base font size, given in pts. Also controls the spacing in the graph.

- legend_position:

  The position of the legend. Options are: "left", "right", "top",
  "bottom", or "none". "none" removes the legend. "none" is the default.

- axis_text_x:

  Logical. Determines if the x-axis has labels. If `FALSE`, the default,
  the x-axis labels are removed. If `TRUE`, the x-axis labels are shown.

- grid:

  Logical. Determines if ALL grid lines should appear. If `FALSE`, the
  default, all grid lines disappear. If `TRUE`, all grid lines appear.

- ...:

  Other arguments passed onto
  [`theme_default()`](https://jdenn0514.github.io/adlgraphs/reference/theme_default.md).
