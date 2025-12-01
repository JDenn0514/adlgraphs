# Stat for density ridgeline plots

This stat is the default stat that will be used in `geom_density_quant`
when I get around to making it. Nevertheless, it still works with
[`geom_density`](https://ggplot2.tidyverse.org/reference/geom_density.html).
It is very similar to
[`stat_density`](https://ggplot2.tidyverse.org/reference/geom_density.html)
and `stat_density_ridges` as it was built as a sort of combination of
the two. One of the key differences between this function and those two
is that this one uses the Sheather & Jones ("sj") as the default
bandwidth selector. This is done because this is a better bandwidth
selector than Silverman's ("nrd0") which is the default for the other
two functions. In addition, this function allows you to add quantile
lines similar to `stat_density_ridges`.

## Usage

``` r
stat_density_quant(
  mapping = NULL,
  data = NULL,
  geom = geom,
  position = "stack",
  ...,
  bw = "sj",
  adjust = 1,
  kernel = "gaussian",
  n = 512,
  na.rm = FALSE,
  bounds = c(-Inf, Inf),
  show.legend = NA,
  inherit.aes = TRUE,
  quantile_lines = FALSE,
  calc_ecdf = FALSE,
  quantiles = 4
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by `aes()`. If specified and
  `inherit.aes = TRUE` (the default), it is combined with the default
  mapping at the top level of the plot. You must supply `mapping` if
  there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`ggplot2::fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- geom:

  The geometric object to use to display the data.
  [`ggplot2::geom_density`](https://ggplot2.tidyverse.org/reference/geom_density.html)
  is the default.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    `position_jitter()`. This method allows for passing extra arguments
    to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use `position_jitter()`, give the position as
    `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- ...:

  Other arguments passed on to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
    may also be passed on through `...`. This can be one of the
    functions described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- bw:

  The smoothing bandwidth to be used. If numeric, the standard deviation
  of the smoothing kernel. If character, a rule to choose the bandwidth,
  as listed in
  [`stats::bw.nrd()`](https://rdrr.io/r/stats/bandwidth.html). Note that
  automatic calculation of the bandwidth does not take weights into
  account. Default is `sj`.

- adjust:

  A multiplicate bandwidth adjustment. This makes it possible to adjust
  the bandwidth while still using the a bandwidth estimator. For
  example, `adjust = 1/2` means use half of the default bandwidth.

- kernel:

  Kernel. See list of available kernels in
  [`density()`](https://rdrr.io/r/stats/density.html).

- n:

  number of equally spaced points at which the density is to be
  estimated, should be a power of two, see
  [`density()`](https://rdrr.io/r/stats/density.html) for details

- na.rm:

  If `FALSE` (the default), removes missing values with a warning. If
  `TRUE` silently removes missing values.

- bounds:

  Known lower and upper bounds for estimated data. Default
  `c(-Inf, Inf)` means that there are no (finite) bounds. If any bound
  is finite, boundary effect of default density estimation will be
  corrected by reflecting tails outside `bounds` around their closest
  edge. Data points outside of bounds are removed with a warning.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`ggplot2::borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- quantile_lines:

  Logical. Determines if quantile lines should be drawn or not. FALSE is
  default.

- calc_ecdf:

  If `TRUE`, `stat_density_ridges` calculates an empirical cumulative
  distribution function (ecdf) and returns a variable `ecdf` and a
  variable `quantile`. Both can be mapped onto aesthetics via
  `stat(ecdf)` and `stat(quantile)`, respectively.

- quantiles:

  Sets the number of quantiles the data should be broken into. Used if
  either `calc_ecdf = TRUE` or `quantile_lines = TRUE`. If `quantiles`
  is an integer then the data will be cut into that many equal
  quantiles. If it is a vector of probabilities then the data will cut
  by them.
