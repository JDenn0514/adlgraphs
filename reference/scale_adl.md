# ADL color scale

Create sequential, diverging, and categorical color scales using
official ADL colors.

## Usage

``` r
scale_adl(
  type = "categorical",
  palette = "base",
  aesthetic = c("fill", "color"),
  n,
  direction = "original",
  legend_order = "original",
  legend_title = NULL,
  wrap_legend_labels = NULL,
  ...
)
```

## Arguments

- type:

  A character string indicating the type of color palette. There are two
  options:

  1.  Categorical is used for both diverging and discrete palettes.

  2.  Sequential is used for sequential palettes.

- palette:

  A character string indicating the name of the palette (e.g.,
  "likert_6"). For more info and to see all the different color palettes
  available, check out `adl_palettes`.

- aesthetic:

  Character string or vector of character strings listing the name(s) of
  the aesthetic(s) that this scale works with. This can be useful, for
  example, to apply colour settings to the `colour` and `fill`
  aesthetics at the same time, via `aesthetic = c("colour", "fill")`

- n:

  [`lifecycle::deprecated()`](https://lifecycle.r-lib.org/reference/deprecated.html)

- direction:

  A character string indicating if the order of the colors should be
  reversed. There are two values:

  1.  "original" keep the original order of colors

  2.  "reverse" flips the order of colors

- legend_order:

  A character string indicating if the order of the colors and labels in
  the legend should be reversed. Currently there are two options:

  1.  "original" keeps the original order of the legend

  2.  "reverse" flips the order of the legend

- legend_title:

  A character string indicating what the title of the legend should be.
  There are three options for this:

  1.  `legend_title` is left blank. If this happens the title of the
      scale is taken from the first mapping used for that aesthetic

  2.  `legend_title = "none"` If this happens then the title is removed
      from the legend

  3.  `legend_title = "some string"` If this happens the legend title
      becomes whatever is in the string (in this case the title would be
      "some string")

- wrap_legend_labels:

  Determine number of characters per line in the labels. Uses
  [label_wrap](https://scales.r-lib.org/reference/label_wrap.html) to
  wrap the text across multiple lines. If left blank, defaults to `NULL`
  which does not wrap the labels at all.

- ...:

  additional arguments to pass to
  [`discrete_scale`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)

## Details

The function `scale_adl()` was created for survey data.
