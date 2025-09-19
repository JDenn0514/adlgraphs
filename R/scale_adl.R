#' ADL color scale
#'
#' Create sequential, diverging, and categorical color scales using official ADL
#' colors.
#'
#' The function `scale_adl()` was created for survey data.
#'
#' @param type A character string indicating the type of color palette.
#'   There are two options:
#'   1. Categorical is used for both diverging and discrete palettes.
#'   2. Sequential is used for sequential palettes.
#' @param palette A character string indicating the name of the palette
#'   (e.g., "likert_6"). For more info and to see all the different color
#'   palettes available, check out `adl_palettes`.
#' @param aesthetic Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful,
#'   for example, to apply colour settings to the `colour` and `fill` aesthetics
#'   at the same time, via `aesthetic = c("colour", "fill")`
#' @param n `lifecycle::deprecated()`
#' @param direction A character string indicating if the order of the colors
#'   should be reversed. There are two values:
#'   1. "original" keep the original order of colors
#'   2. "reverse" flips the order of colors
#' @param legend_title A character string indicating what the title of the
#'   legend should be. There are three options for this:
#'   1. `legend_title` is left blank. If this happens the title of the scale is
#'      taken from the first mapping used for that aesthetic
#'   2. `legend_title = "none"` If this happens then the title is removed from
#'     the legend
#'   3. `legend_title = "some string"` If this happens the legend title becomes
#'     whatever is in the string (in this case the title would be "some string")
#' @param legend_order A character string indicating if the order of the colors
#'   and labels in the legend should be reversed. Currently there are two options:
#'   1. "original" keeps the original order of the legend
#'   2. "reverse" flips the order of the legend
#' @param wrap_legend_labels Determine number of characters per line in the
#'   labels. Uses \link[scales]{label_wrap} to wrap the text across
#'   multiple lines. If left blank, defaults to `NULL` which does not wrap the
#'   labels at all.
#' @param ... additional arguments to pass to \code{\link[ggplot2]{discrete_scale}}
#'
#'
#' @export
scale_adl <- function(
  type = "categorical",
  palette = "base",
  aesthetic = c("fill", "color"),
  n,
  direction = "original",
  legend_order = "original",
  legend_title = NULL,
  wrap_legend_labels = NULL,
  ...
) {
  warning(
    "`n` has been deprecated as the function now automatically detects number of colors needed."
  )

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('ggplot2 is required for this functionality', call. = FALSE)
  }

  if (!direction %in% c("original", "reverse")) {
    stop('direction must be "original" or "reverse"')
  }

  # set the legend_title to waiver() if legend_title is missing
  if (is.null(legend_title)) {
    legend_title <- ggplot2::waiver()
  } else if (legend_title == "none") {
    legend_title <- NULL
  } else {
    legend_title
  }

  # set the labels
  if (is.null(wrap_legend_labels)) {
    # if wrap_legend_labels = NULL then just leave as is
    wrap_legend_labels <- ggplot2::waiver()
  } else {
    # wrap the legend labels
    wrap_legend_labels <- label_wrap(wrap_legend_labels)
  }

  # reverse the legend o
  if (legend_order == "reverse") {
    return(
      ggplot2::discrete_scale(
        aesthetics = aesthetic,
        palette = palette_gen(type, palette, direction),
        labels = wrap_legend_labels,
        name = legend_title,
        guide = ggplot2::guide_legend(reverse = TRUE),
        ...
      )
    )
  } else {
    return(
      ggplot2::discrete_scale(
        aesthetics = aesthetic,
        palette = palette_gen(type, palette, direction),
        labels = wrap_legend_labels,
        name = legend_title,
        ...
      )
    )
  }
}


palette_gen <- function(
  type = "categorical",
  palette = "base",
  direction = "original"
) {
  if (palette == "base" && type == "sequential") {
    palette <- "bluescale"
  } else if (palette == "base" && type == "categorical") {
    palette <- "categorical"
  } else if (palette == "pid_f3" && type == "categorical") {
    palette <- "pid3"
  }

  function(n) {
    # get the list of colors from the palette
    all_colors <- adl_palettes[[palette]]
    # convert it to a character vector
    all_colors <- as.character(all_colors)

    if (palette == "pid3" && n > 3) {
      # if the palette is pid3 and n is greater than 3

      # interpolate the colors until needed
      color_list <- grDevices::colorRampPalette(all_colors)(n)
    } else if (palette == "bluescale") {
      # if the palette is bluescale,

      # interpolate the colors until needed
      color_list <- grDevices::colorRampPalette(all_colors)(n)
    } else {
      # for all other colors, just do it from 1 to n
      color_list <- all_colors[1:n]
    }

    # get the list of colors
    # color_list <- all_colors[1:n]
    color_list <- if (direction == "original") color_list else rev(color_list)
  }
}
