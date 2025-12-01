# Theme for coefficient plots

This function creates a theme for coefficient plots.

## Usage

``` r
theme_coef(
  base_size = 12,
  grid_x_only = TRUE,
  facet_title_margin_top = base_size,
  facet_title_margin_bottom = half_line,
  facet_title_margin_right = 0.8 * half_line,
  facet_title_margin_left = 0.8 * half_line,
  ...
)
```

## Arguments

- base_size:

  Base font size, given in pts. Also controls the spacing in the graph.

- grid_x_only:

  Logical. Determines if only x-axis grid lines (vertical lines) should
  appear. If `FALSE`, the default, all grid lines appear. If `TRUE`,
  only the x-axis grid lines appear and the y-axis grid lines will
  disappear.

- facet_title_margin_top:

  The margin above the facet title, specified in pts. Default is
  `0.8 * half_line`.

- facet_title_margin_bottom:

  The margin beneath the facet title, specified in pts. Default is
  `0.8 * half_line`.

- facet_title_margin_right:

  The margin to the right of the facet title, specified in pts. Default
  is `0.8 * half_line`.

- facet_title_margin_left:

  The margin to the left of the facet title, specified in pts. Default
  is `0.8 * half_line`.

- ...:

  Other arguments passed onto
  [`theme_default()`](https://jdenn0514.github.io/adlgraphs/reference/theme_default.md).
