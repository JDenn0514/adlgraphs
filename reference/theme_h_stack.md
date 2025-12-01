# Theme for horizontal stacked bar plots

This function creates the theme for horizontally stacked bar plots.

## Usage

``` r
theme_h_stack(
  base_size = 12,
  legend_position = "top",
  axis_text = FALSE,
  grid = FALSE,
  ...
)
```

## Arguments

- base_size:

  Base font size, given in pts. Also controls the spacing in the graph.

- legend_position:

  The position of the legend. Options are: "left", "right", "top",
  "bottom", or "none". "none" removes the legend. "top" is the default.

- axis_text:

  Logical. Determines if BOTH axes have labels. If `FALSE`, the default,
  neither axis are labelled. If `TRUE`, both axes is labelled.

- grid:

  Logical. Determines if any grid lines appear. If `FALSE`, the default,
  all grid lines disappear. If `TRUE`, all grid lines appear.

- ...:

  Other arguments passed onto
  [`theme_default()`](https://jdenn0514.github.io/adlgraphs/reference/theme_default.md).
