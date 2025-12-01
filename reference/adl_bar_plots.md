# Create bar plots in ADL's style

This function allows users to create different types of bar plots with
ADL's style. This a wrapper around
[`geom_col`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
[`geom_text`](https://ggplot2.tidyverse.org/reference/geom_text.html)/,[`geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html),
and
[`geom_errorbar`](https://ggplot2.tidyverse.org/reference/geom_linerange.html).
This function was created to standardize the graphs produced by CAR's
team and to cut down on the amount of time it takes to make these
graphs.

## Usage

``` r
adl_bar_plots(
  data,
  x,
  y,
  col_label,
  group = NULL,
  fill = NULL,
  direction = "vertical",
  col_text_size = 3.25,
  distance_from_col = 0.25,
  freq_plot = TRUE,
  position = NULL,
  dodge_width = 0.8,
  dodge_reverse = TRUE,
  wrap_facet_labels = 100,
  ...
)
```

## Arguments

- data:

  A dataframe or tibble. This can be piped in like with a normal
  [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
  function.

- x:

  Variable that goes in the x-axis. This is required.

- y:

  Variable that goes in the y-axis. This is required.

- col_label:

  Variable that provides the values to be used to label each column or
  stack.

- group:

  Explicitly set the overall grouping variable. This is used in stacked
  graphs and dodged graphs. If NULL, the default, no grouping variable
  is used. Note: No need to set this if the data is not grouped at all.

- fill:

  Set the grouping variable for which the inside of the bars are
  colored. This is used in stacked graphs and dodged graphs. If NULL,
  the default, no grouping variable is used. Note: No need to set this
  if the data is not grouped at all.

- direction:

  A character string indicating the direction of the bars. There are two
  options:

  1.  "vertical", the default, the bars are vertical

  2.  "horizontal" the bars are horizontal This must be set explicitly
      as it affects the location of the text, labels, and error bars.

- col_text_size:

  The size of the text inside/on top of the columns. Default is 3.25.

- distance_from_col:

  How far the labels are from the bars in freq plots and how far they
  are from the bottom of the bar in the mean plots.

- freq_plot:

  Logical. Determines if this is a frequency plot. If `TRUE`, default,
  then the graph will be styled as a frequency plot with the bar labels
  appearing outside the bars. `FALSE`, the graph will be styled as a
  mean plot with the labels appearing.

- position:

  A character string determining how the plot handles a grouped graph.
  By default it is `NULL` which assumes there is no grouping variable.
  If you set to "dodge" then you will get a dodged plot. This is best
  used when comparing two groups, especially when they do not add up
  to 100. If you set it to "stacked" then you get a stacked plot. This
  should be used when comparing multiple statements with likert scales
  and other things that add up to 100.

- dodge_width:

  This adjusts the width in the dodge plot. For more info check out
  [`position_dodge`](https://ggplot2.tidyverse.org/reference/position_dodge.html).

- dodge_reverse:

  Reverses the order of the bars and text in a dodge plot. For more info
  check out
  [`position_dodge`](https://ggplot2.tidyverse.org/reference/position_dodge.html).

- wrap_facet_labels:

  Determine number of characters per line in the labels. Uses
  [label_wrap_gen](https://ggplot2.tidyverse.org/reference/labellers.html)
  to wrap the text across multiple lines. If left blank, defaults to 200
  so that it in essence won't wrap the text. text

- ...:

  Additional arguments passed on to `theme_default`

## Details

As mentioned previously, this function is a wrapper around various
`{ggplot2}` functions in order to save time when making simple bar
plots, dodged bar plots, and stacked bar plots. As a result, it is not
possible to combine every element of the graph. If you would like to do
so, we recommend using the actual geoms from `{ggplot2}`.

Each of the elements included serve a purpose to allow you to customize
the graphs so that they look nice. Moreover, the arguments were created
for the types of graphs that CAR produces, namely mean plots and
frequency plots.
