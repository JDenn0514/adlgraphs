library(rlang)
library(tidyverse)




#' @export

scale_adl <- function(type = "categorical",
                      palette = "base",
                      aesthetic = c("fill", "color"),
                      n,
                      direction = "original",
                      legend_title = NULL,
                      legend_order = "original") {

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
    if (palette == "base") {
      bluescale <- adl_palettes[["bluescale"]]
      pal <- colorRampPalette(bluescale)(n)
    }
    # get the grayscale colors
    else if(palette == "gray") {
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
  }


  # reverse the legend o
  if (legend_order == "reverse") {
    return(
      ggplot2::scale_fill_manual(
        values = pal,
        aesthetics = aesthetic,
        name = legend_title,
        guide = guide_legend(reverse = TRUE)
      )
    )
  } else {
    return(
      ggplot2::scale_fill_manual(
        values = pal,
        aesthetics = aesthetic,
        name = legend_title
      )
    )
  }


}








