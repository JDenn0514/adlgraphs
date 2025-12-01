# Create coefficient plots in ADL's style

This function allows users to create a coefficient plot with ADL's style
and it was designed to help reduce the time it takes to make
publication-ready graphs.

## Usage

``` r
adl_coef_plots(
  data,
  x = estimate,
  y = value_label,
  color = NULL,
  facet = var_label,
  facet_order = "original",
  wrap_facet_labels = 50,
  wrap_y_labels = 20,
  x_intercept = 0,
  point_size = 3.5,
  line_width = 1,
  position = NULL,
  dodge_width = 0.6,
  dodge_reverse = FALSE,
  ...
)
```

## Arguments

- data:

  A dataframe or tibble. This can be piped in like with a normal
  [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
  function.

- x:

  Variable that goes in the x-axis.

- y:

  Variable that goes in the y-axis.

- color:

  Set the grouping variable for which the color of the dots and the
  confidence intervals are colored. If NULL, the default, no grouping
  variable is used. Note: No need to set this if the data is not grouped
  at all

- facet:

  Set the variable for which you want the plot faceted. This is
  typically used instead of the `color` argument to distinguish the
  different variables and their values. Default is `var_label` but can
  be set to any other column in the object set in `data`. Must be set to
  `NULL` to remove faceting from the plot.

- facet_order:

  A character string indicating if the order of variables with which the
  graph is faceted should be reversed or not. There are two options:

  1.  "original" keep the original order faceting variables

  2.  "reverse" flips the order of faceting variables Note: this does
      not change the ordering of the values within each facet on the
      y-axis, it just reorders the different facets.

- wrap_facet_labels:

  Determine number of characters per line in the facet labels. Uses
  [label_wrap](https://scales.r-lib.org/reference/label_wrap.html) to
  wrap the text across multiple lines. If left blank, defaults to 50.

- wrap_y_labels:

  Determine number of characters per line in the y-axis labels. Uses
  [label_wrap](https://scales.r-lib.org/reference/label_wrap.html) to
  wrap the text across multiple lines. If left blank, defaults to 20.

- x_intercept:

  The value for which the x_intercept should be set. Default is 0 but
  for exponentiated models should be set to 1. If set to `NULL` then no
  x_intercept is shown.

- point_size:

  The size of the dots in the plot. Default is 3.5.

- line_width:

  The thickness of the lines. Default is

- position:

  A character string determining how the plot handles a grouped graph.
  By default it is `NULL` which assumes there is no grouping variable.
  If set to "dodge", it will create a dodged plot based on the variable
  supplied in color.

- dodge_width:

  This adjusts the width in the dodge plot. Default is 0.6. For more
  info check out
  [`position_dodge`](https://ggplot2.tidyverse.org/reference/position_dodge.html).

- dodge_reverse:

  Logical. If set to TRUE, reverses the order of the dots and confidence
  intervals. Default is FALSE. For more info check out
  [`position_dodge`](https://ggplot2.tidyverse.org/reference/position_dodge.html).

- ...:

  Additional arguments passed on to
  [`theme_coef()`](https://jdenn0514.github.io/adlgraphs/reference/theme_coef.md)

## Details

This function works best when used in conjunction with
[`get_coefficients()`](https://jdenn0514.github.io/adlgraphs/reference/get_coefficients.md).

`adl_coef_plots()` is a wrapper around
[`geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html),
[`geom_linerange`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)/
and
[`geom_vline`](https://ggplot2.tidyverse.org/reference/geom_abline.html).
As a result, it is not possible to customize every aspect of this graph.
If you would like to do so, I recommend using the actual geoms from
`{ggplot2}`.

To differentiate between different variables and different models, use
the `facet` and `color` arguments like you would normally. However, when
just differentiating between variables, using the `facet` argument is
recommended as it makes it easier to distinguish the different variables
and to compare the estimates their values have.
