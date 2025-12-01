# Theme for vertical non-stacked bar plots

This function creates the theme for non-stacked vertical bar plots

## Usage

``` r
theme_v_bar(
  base_size = 12,
  legend_position = "none",
  axis_text_y = FALSE,
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

- axis_text_y:

  Logical. Determines if the y-axis has labels. If `FALSE`, the default,
  the y-axis labels are removed. If `TRUE`, the y-axis labels are shown.

- grid:

  Logical. Determines if ALL grid lines should appear. If `FALSE`, the
  default, all grid lines disappear. If `TRUE`, all grid lines appear.

- ...:

  Other arguments passed onto
  [`theme_default()`](https://jdenn0514.github.io/adlgraphs/reference/theme_default.md).
