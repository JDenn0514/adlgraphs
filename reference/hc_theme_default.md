# The default theme for hc charts

This function creates the default theme that all ADL highcharter themes
are built off of. It's ggplot2 equivalent is `theme_default`.

## Usage

``` r
hc_theme_default(
  hc,
  base_size = 16,
  legend_position = "right",
  axis_text = TRUE,
  axis_text_x = TRUE,
  axis_text_y = TRUE,
  grid = TRUE,
  grid_x_only = FALSE,
  grid_y_only = FALSE,
  ...
)
```

## Arguments

- hc:

  A highcharter object

- base_size:

  Base font size, given in px Also controls the spacing in the graph.

- legend_position:

  The position of the legend. Options are: "right", "top", "bottom", or
  "none". "none" removes the legend. "right" is the default.

- axis_text:

  Logical. Determines if BOTH axes should have labels. If `TRUE`, the
  default, both axes are labelled. If `FALSE`, neither axis is labelled.
  Note, this controls both axes. If you want to remove only one axis,
  use the `axis_text_x` or `axis_text_y`.

- axis_text_x:

  Logical. Determines if the x-axis has labels. If `TRUE`, the default,
  the x-axis labels are shown. If `FALSE`, the x-axis labels are removed
  from the plot.

- axis_text_y:

  Logical. Determines if the y-axis has labels. If `TRUE`, the default,
  the y-axis labels are shown. If `FALSE`, the y-axis labels are removed
  from the plot.

- grid:

  Logical. Determines if ALL grid lines should appear. If `TRUE`, the
  default, all grid lines appear. If `FALSE`, all grid lines disappear,
  regardless of what `grid_x_only` or `grid_y_only` are set to.

- grid_x_only:

  Logical. Determines if only x-axis grid lines (vertical lines) should
  appear. If `FALSE`, the default, all grid lines appear. If `TRUE`,
  only the x-axis grid lines appear and the y-axis grid lines will
  disappear.

- grid_y_only:

  Logical. Determines if only y-axis grid lines (horizontal lines)
  should appear. If `FALSE`, the default, all grid lines appear. If
  `TRUE`, only the y-axis grid lines appear and the x-axis grid lines
  will disappear.

- ...:

  Additional arguments passed

## Details

Due to certain limitations of Highcharter, this is not quite as
extensive as `theme_default`. For example, there are no minor grid lines
right now. If I am able to figure it out then I might add them in. In
addition, the faceting arguments from `theme_default` are not present
here
