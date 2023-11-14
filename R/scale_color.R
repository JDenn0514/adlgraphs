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

    ### get the palettes
    # get the grayscale colors
    if (palette == "base") {
      pal <- monochromeR::generate_palette(
        # set the original color
        colour = "#093c71",
        # set the color to blend to
        blend_colour = "#97d0dc",
        # set the number of colors
        n_colours = n,
        # blend the colors together
        modification = "blend"
      )
      # get the blue color
    } else if(palette == "gray") {
      pal <- monochromeR::generate_palette(
        # set the original color
        colour = "#2c2e35",
        # set the color to blend to
        blend_colour = "#d8d9da",
        # set the number of colors
        n_colours = n,
        # blend the colors together
        modification = "blend"
      )
    }
    # create the discrete palettes
  } else if (type == "categorical") {

    if (palette == "base") {
      pal <- adl_palettes["categorical"]
    } else {
      pal <- adl_palettes[[palette]]
    }

    if (missing(n)) {
      n <- length(pal)
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








