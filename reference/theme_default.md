# Default theme

This function creates the default theme that all ADL theme functions are
built off of. It functions similarly to
[`theme_gray`](https://ggplot2.tidyverse.org/reference/ggtheme.html), in
that all of the default ggplot2 themes are built off
[`theme_gray`](https://ggplot2.tidyverse.org/reference/ggtheme.html). It
should be noted that this works best when dpi is set to 400 either in a
Quarto or Rmarkdown doc or in
[`ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html).

This function creates the default theme that all ADL theme functions are
built off of. It functions similarly to
[`theme_gray`](https://ggplot2.tidyverse.org/reference/ggtheme.html), in
that all of the default ggplot2 themes are built off
[`theme_gray`](https://ggplot2.tidyverse.org/reference/ggtheme.html). It
should be noted that this works best when dpi is set to 400 either in a
Quarto or Rmarkdown doc or in
[`ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## Usage

``` r
theme_default(
  base_size = 12,
  base_line_size = base_size/24,
  base_rect_size = base_size/24,
  base_lineheight = 1.1,
  legend_position = "right",
  axis_text = TRUE,
  axis_text_x = TRUE,
  axis_text_y = TRUE,
  grid = TRUE,
  grid_x_only = FALSE,
  grid_y_only = FALSE,
  grid_major = TRUE,
  grid_minor = TRUE,
  grid_major_x = TRUE,
  grid_major_y = TRUE,
  grid_minor_x = TRUE,
  grid_minor_y = TRUE,
  facet_title_bold = FALSE,
  facet_title_size = base_size * 0.8,
  facet_title_margin_top = 0.8 * half_line,
  facet_title_margin_bottom = 0.8 * half_line,
  facet_title_margin_right = 0.8 * half_line,
  facet_title_margin_left = 0.8 * half_line,
  panel_spacing_x = 0,
  panel_spacing_y = 0
)

theme_default(
  base_size = 12,
  base_line_size = base_size/24,
  base_rect_size = base_size/24,
  base_lineheight = 1.1,
  legend_position = "right",
  axis_text = TRUE,
  axis_text_x = TRUE,
  axis_text_y = TRUE,
  grid = TRUE,
  grid_x_only = FALSE,
  grid_y_only = FALSE,
  grid_major = TRUE,
  grid_minor = TRUE,
  grid_major_x = TRUE,
  grid_major_y = TRUE,
  grid_minor_x = TRUE,
  grid_minor_y = TRUE,
  facet_title_bold = FALSE,
  facet_title_size = base_size * 0.8,
  facet_title_margin_top = 0.8 * half_line,
  facet_title_margin_bottom = 0.8 * half_line,
  facet_title_margin_right = 0.8 * half_line,
  facet_title_margin_left = 0.8 * half_line,
  panel_spacing_x = 0,
  panel_spacing_y = 0
)
```

## Arguments

- base_size:

  Base font size, given in pts. Also controls the spacing in the graph.

- base_line_size:

  Base size for line elements.

- base_rect_size:

  Base size for rect elements.

- base_lineheight:

  Base line height for all text

- legend_position:

  The position of the legend. Options are: "left", "right", "top",
  "bottom", or "none". "none" removes the legend. "right" is the
  default.

- axis_text:

  Logical. Determines if BOTH axes have labels. If `TRUE`, the default,
  both axes are labelled. If `FALSE`, neither axis is labelled. Note,
  this controls both axes. If you want to remove only one axis, use the
  `axis_text_x` or `axis_text_y`.

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
  default, all grid lines appear. If `FALSE`, all grid lines disappear.

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

- grid_major:

  Logical. Determines if the major grid lines should appear. If `TRUE`,
  the default, the major grid lines will appear. If `FALSE`, the major
  grid lines will disappear.

- grid_minor:

  Logical. Determines if the minor grid lines should appear. If `TRUE`,
  the default, the minor grid lines will appear. If `FALSE`, the minor
  grid lines will disappear.

- grid_major_x:

  Logical. Determines if the major x-axis grid lines will appear. If
  `TRUE`, the default, the major x-axis grid lines will appear. If
  `FALSE`, the major x-axis grid lines will disappear.

- grid_major_y:

  Logical. Determines if the major y-axis grid lines will appear. If
  `TRUE`, the default, the major y-axis grid lines will appear. If
  `FALSE`, the major y-axis grid lines will disappear.

- grid_minor_x:

  Logical. Determines if the minor x-axis grid lines will appear. If
  `TRUE`, the default, the minor x-axis grid lines will appear. If
  `FALSE`, the minor x-axis grid lines will disappear.

- grid_minor_y:

  Logical. Determines if the minor y-axis grid lines will appear. If
  `TRUE`, the default, the minor y-axis grid lines will appear. If
  `FALSE`, the minor y-axis grid lines will disappear.

- facet_title_bold:

  Logical. Determines if the facet labels should be bold or not. Default
  is `FALSE`.

- facet_title_size:

  Size of the facet titles, specified in pts. Default is
  `base_size * 0.8`.

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

- panel_spacing_x:

  Horizontal spacing between the different panels when faceting a graph,
  given in pts. Default is 0.

- panel_spacing_y:

  Vertical spacing between the different panels when faceting a graph,
  given in pts. Default is 0.
