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
#' @param n The number of colors.
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
#' @param ... additional arguments to pass to \code{\link[ggplot2]{scale_fill_manual}}
#'
#'
#' @export

scale_adl <- function(type = "categorical",
                      palette = "base",
                      aesthetic = c("fill", "color"),
                      n,
                      direction = "original",
                      legend_order = "original",
                      legend_title = NULL,
                      wrap_legend_labels = NULL,
                      ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('ggplot2 is required for this functionality', call. = FALSE)
  }

  if (!direction %in% c("original", "reverse")) {
    stop('direction must be "original" or "reverse"')
  }


  # create the "sequential" palettes
  if (type == "sequential") {

    if (missing(n)) {
      stop("n must be supplied when calculating a sequential color palette")
    }

    ### get the palettes
    # interpolate the palettes using colorRampPalette()
    # get the bluescale colors
    if (palette == "base" || palette == "bluescale") {
      bluescale <- adl_palettes[["bluescale"]]
      pal <- colorRampPalette(bluescale)(n)
    }
    # get the grayscale colors
    else if(palette == "grayscale") {
      grayscale <- adl_palettes[["grayscale"]]
      pal <- colorRampPalette(grayscale)(n)
    }

  }

  # create the discrete palettes
  else if (type == "categorical") {
    # calculate the n if it is not supplied
    if (missing(n)) {
      n <- length(adl_palettes[[palette]])
    } else {
      n <- n
    }

    # get the palettes
    if (palette == "base") {
      pal <- adl_palettes[["categorical"]][1:n]
    } else if (palette == "pid3") {
      # this should give us three colors if n is provided, if it is provided
      # it will interpolate the other colors
      pid3 <- adl_palettes[["pid3"]]
      pal <- colorRampPalette(pid3)(n)
    } else {
      # need the
      pal <- adl_palettes[[palette]][1:n]
    }

  }

  # if order = "reverse" reverse the color palette order
  if (direction == "reverse") {
    pal <- rev(pal)
  }

  # set the legend_title to waiver() if legend_title is missing
  if (is.null(legend_title)) {
    legend_title <- waiver()
  } else if (legend_title == "none") {
    legend_title <- NULL
  } else {
    legend_title
  }

  # set the labels
  if (is.null(wrap_legend_labels)) {
    # if wrap_legend_labels = NULL then just leave as is
    wrap_legend_labels <- waiver()
  } else {
    # wrap the legend labels
    wrap_legend_labels <- label_wrap(wrap_legend_labels)
  }

  # reverse the legend o
  if (legend_order == "reverse") {
    return(
      ggplot2::scale_fill_manual(
        values = pal,
        labels = wrap_legend_labels,
        aesthetics = aesthetic,
        name = legend_title,
        guide = guide_legend(reverse = TRUE),
        ...
      )
    )
  } else {
    return(
      ggplot2::scale_fill_manual(
        values = pal,
        labels = wrap_legend_labels,
        aesthetics = aesthetic,
        name = legend_title,
        ...
      )
    )
  }


}


