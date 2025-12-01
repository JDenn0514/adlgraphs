# Lollipop plots with optional dodging and auto label offsets

Create vertical or horizontal lollipop plots for frequency-style data
(bars replaced with stems and points). Supports grouped/dodged layouts,
automatic label offsets based on axis span, and optional mapping of
label color to the plotted color or group.

## Usage

``` r
adl_lollipop_plots(
  data,
  x,
  y,
  col_label,
  group = NULL,
  color = NULL,
  direction = "vertical",
  col_text_size = 3.25,
  distance_from_col = "auto",
  auto_offset_prop = 0.03,
  position = NULL,
  dodge_width = 0.8,
  dodge_reverse = TRUE,
  wrap_facet_labels = 100,
  point_size = 3.5,
  line_width = 0.9,
  text_color = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame or tibble.

- x:

  Variable that goes in the x-axis. This is required.

- y:

  Variable that goes in the y-axis. This is required.

- col_label:

  Variable used for text labels (e.g., the frequency value).

- group:

  Optional grouping variable (used for dodging and color mapping when
  `color = NULL`).

- color:

  Optional variable to map color of stems/points (and labels when
  `text_color = TRUE`).

- direction:

  A character string indicating the direction of the lines. There are
  two options:

  1.  "vertical", the default, the bars are vertical

  2.  "horizontal" the bars are horizontal This must be set explicitly
      as it affects the location of the text, labels, and lines

- col_text_size:

  Numeric, text size for labels (default `3.25`).

- distance_from_col:

  How far the labels are from the points in the plot. There are two
  options:

  1.  "auto", this checks the limits of the scales to determine how much
      space to add between the point and the text label (this is the
      default)

  2.  Numeric, a number referring to the size based on the scale

- auto_offset_prop:

  Numeric scalar (\>= 0). Proportion of axis span used for the automatic
  label offset when `distance_from_col = "auto"`. Default is `0.03`
  (i.e., 3% of span).

- position:

  Either `NULL` (no dodging) or `"dodge"` for grouped layouts. `"stack"`
  is not supported.

- dodge_width:

  Numeric width passed to `position_dodge2()` (default `0.8`).

- dodge_reverse:

  Logical, whether to reverse the dodging order (default `TRUE`).

- wrap_facet_labels:

  Integer passed to your faceting helpers if you facet elsewhere (kept
  for interface compatibility).

- point_size:

  Numeric, size of the lollipop head (default `3.5`).

- line_width:

  Numeric, thickness of the lollipop stem (default `0.9`).

- text_color:

  Logical; if `TRUE`, `geom_text()` maps its color aesthetic to `color`
  (or `group` if `color` is `NULL`). If both are `NULL`, text uses
  "#14A2FCFF". If `FALSE` (default), text is drawn with a fixed color
  (`#2c2e35`).

- ...:

  Additional arguments passed to
  [`theme_default()`](https://jdenn0514.github.io/adlgraphs/reference/theme_default.md).

## Value

A `ggplot` object.

## Details

Lollipop stems are drawn with
[`ggplot2::geom_linerange()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)
to ensure proper behavior under `position = "dodge"`. Labels can be
offset by a fixed amount or automatically by a proportion of the
rendered axis span (`auto_offset_prop`), computed after scales are
applied.

When both `color` and `group` are `NULL`, stems and points are drawn
with `adl_palettes$primary`, and labels default to `#2c2e35` unless
`text_color = TRUE`, in which case labels inherit the plotted color (or
the primary if unmapped).

For more information on how the arguments point_size and line_width
check out vignette from the ggplot2 website:

## Auto label offset

- For `direction = "horizontal"`, the label offset is computed from the
  x-axis span.

- For `direction = "vertical"`, the label offset is computed from the
  y-axis span.

- If the span cannot be determined at build time, the offset falls back
  to `0.25`.

## See also

- [`ggplot2::geom_linerange()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html),
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html),
  [`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)

- [`ggplot2::position_dodge2()`](https://ggplot2.tidyverse.org/reference/position_dodge.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Minimal vertical lollipop with auto label offset
adl_lollipop_plots(
  data = df,
  x = category,
  y = count,
  col_label = count,
  direction = "vertical",
  distance_from_col = "auto",
  auto_offset_prop = 0.03
)

# Horizontal grouped with dodging; labels inherit color mapping
adl_lollipop_plots(
  data = df,
  x = count,
  y = category,
  col_label = count,
  group = grp,
  color = grp,
  direction = "horizontal",
  position = "dodge",
  distance_from_col = "auto",
  auto_offset_prop = 0.02,
  text_color = TRUE
)
} # }
```
