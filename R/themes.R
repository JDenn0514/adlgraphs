#' Default theme
#'
#' This function creates the default theme that all ADL theme functions are
#' built off of. It functions similarly to \code{\link[ggplot2]{theme_gray}},
#' in that all of the default ggplot2 themes are built off
#' \code{\link[ggplot2]{theme_gray}}.
#'
#' @param base_size Base font size, given in pts. Also controls
#'   the spacing in the graph.
#' @param base_family Base font family. Default is Lato.
#' @param base_line_size Base size for line elements.
#' @param base_rect_size Base size for rect elements.
#' @param axis_text Logical. Determines if BOTH axes have labels. If `TRUE`, the
#'   default, both axes are labelled. If `FALSE`, neither axis is labelled.
#'   Note, this controls both axes. If you want to remove only one axis, use the
#'   `axis_text_x` or `axis_text_y`.
#' @param axis_text_x Logical. Determines if the x-axis has labels. If `TRUE`,
#'   the default, the x-axis labels are shown. If `FALSE`, the x-axis labels are
#'   removed from the plot.
#' @param axis_text_y Logical. Determines if the y-axis has labels. If `TRUE`,
#'   the default, the y-axis labels are shown. If `FALSE`, the y-axis labels are
#'   removed from the plot.
#' @param grid Logical. Determines if ALL grid lines should appear. If `TRUE`,
#'   the default, all grid lines appear. If `FALSE`, all grid lines disappear.
#' @param grid_x_only Logical. Determines if only x-axis grid lines (vertical
#'   lines) should appear. If `FALSE`, the default, all grid lines appear. If
#'   `TRUE`, only the x-axis grid lines appear and the y-axis grid lines will
#'   disappear.
#' @param grid_y_only Logical. Determines if only y-axis grid lines (horizontal
#'   lines) should appear. If `FALSE`, the default, all grid lines appear. If
#'   `TRUE`, only the y-axis grid lines appear and the  x-axis grid lines will
#'   disppear.
#' @param grid_major Logical. Determines if the major grid lines should appear.
#'   If `TRUE`, the default, the major grid lines will appear. If `FALSE`, the
#'   major grid lines will disappear.
#' @param grid_minor Logical. Determines if the minor grid lines should appear.
#'   If `TRUE`, the default, the minor grid lines will appear. If `FALSE`, the
#'   minor grid lines will disappear.
#' @param grid_major_x Logical. Determines if the major x-axis grid lines will
#'   appear. If `TRUE`, the default, the major x-axis grid lines will appear.
#'   If `FALSE`, the major x-axis grid lines will disappear.
#' @param grid_major_y Logical. Determines if the major y-axis grid lines will
#'   appear. If `TRUE`, the default, the major y-axis grid lines will appear.
#'   If `FALSE`, the major t-axis grid lines will disappear.
#' @param grid_minor_x Logical. Determines if the minor x-axis grid lines will
#'   appear. If `TRUE`, the default, the minor x-axis grid lines will appear.
#'   If `FALSE`, the minor x-axis grid lines will disappear.
#' @param grid_minor_y Logical. Determines if the minor y-axis grid lines will
#'   appear. If `TRUE`, the default, the minor y-axis grid lines will appear.
#'   If `FALSE`, the minor t-axis grid lines will disappear.
#'
#' @examples
#' # This is designed to make it easy to control various aspects of the graph.
#' # I will demonstrate a variety of different things that this is capable of.
#'
#' # First let's create the plot object. We are going to use code from Alison
#' # Horst's palmerpenguins package:
#'
#' install.packages("palmerpenguins")
#' library(palmerpenguins)
#' library(ggplot2)
#' library(adlgraphs)
#'
#' mass_flipper <- ggplot(data = penguins,
#'   aes(x = flipper_length_mm,
#'       y = body_mass_g)) +
#'   geom_point(aes(color = species,
#'                  shape = species),
#'              size = 3,
#'              alpha = 0.8) +
#'   scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
#'   labs(title = "Penguin size, Palmer Station LTER",
#'        subtitle = "Flipper length and body mass for Adelie, Chinstrap and Gentoo Penguins",
#'        x = "Flipper length (mm)",
#'        y = "Body mass (g)",
#'        color = "Penguin species",
#'        shape = "Penguin species")
#'
#' # The basic graph with no customizations:
#' mass_flipper + theme_default()
#'
#'
#' @export
#'
#'


theme_default <- function(
  # control the base font size, this also determines spacing
  base_size = 12,
  # control the base font
  base_family = "L",
  # control the base line_width
  base_line_size = base_size / 24,
  # control the base rect_width
  base_rect_size = base_size / 24,
  # determine if the axis labels are shown (controls both axes)
  axis_text = TRUE,
  # determine if the x-axis labels are shown, only controls x-axis
  axis_text_x = TRUE,
  # determine if the y-axis labels are shown, only controls y-axis
  axis_text_y = TRUE,
  # determine if grid lines should be shown (controls major and minor, x and y grid lines)
  grid = TRUE,
  # only have x-axis grid lines
  grid_x_only = FALSE,
  # only have y-axis grid_lines
  grid_y_only = FALSE,
  # determine if the major grid lines are shown (x and y)
  grid_major = TRUE,
  # determine if minor grid lines are shown (x and y)
  grid_minor = TRUE,
  # determine if major x axis grid lines are shown (vertical bars)
  grid_major_x = TRUE,
  # determine if major y axis grid lines are shown (horizontal bars)
  grid_major_y = TRUE,
  # determine if minor x axis grid lines are shown (vertical bars)
  grid_minor_x = TRUE,
  # determine if minor y axis grid lines are shown (horizontal bars)
  grid_minor_y = TRUE
) {

  half_line <- base_size / 2

  # set the axis text
  if (axis_text == FALSE) {
    axis_text_labels <- element_blank()
  } else {
    axis_text_labels <- element_text(
      size = rel(0.8),
      colour = "#595b60",
    )
  }

  # set the x axis labels
  if (axis_text_x == FALSE || axis_text == FALSE) {
    axis_text_x_labels <- element_blank()
  } else {
    axis_text_x_labels <- element_text(
      size = rel(0.8),
      colour = "#595b60",
      margin = margin(t = 0.8 * half_line / 2),
      vjust = 1
    )
  }

  # set the y axis labels
  if (axis_text_y == FALSE || axis_text == FALSE) {
    axis_text_y_labels <- element_blank()
  } else {
    axis_text_y_labels <- element_text(
      size = rel(0.8),
      colour = "#595b60",
      margin = margin(r = 0.8 * half_line / 2),
      vjust = 1
    )
  }

  # set the major grid lines
  if (grid_major == FALSE || grid == FALSE) {
    grid_major_line <- element_blank()
  } else {
    grid_major_line <- element_line(
      color = "gray75",
      linetype = "dashed",
      linewidth = base_line_size
    )
  }

  # set the minor grid lines
  if (grid_minor == FALSE || grid == FALSE) {
    grid_minor_line <- element_blank()
  } else {
    grid_minor_line <- element_line(
      color = "gray90",
      linetype = "dashed",
      linewidth = base_line_size / 2
    )
  }


  # set the major x axis grid lines
  if (grid_major_x == FALSE || grid_major == FALSE || grid == FALSE || grid_y_only == TRUE) {
    grid_major_x_line <- element_blank()
  } else {
    grid_major_x_line <- element_line(
      color = "gray75",
      linetype = "dashed",
      linewidth = base_line_size
    )
  }

  # set the major y axis grid lines
  if (grid_major_y == FALSE || grid_major == FALSE || grid == FALSE || grid_x_only == TRUE) {
    grid_major_y_line <- element_blank()
  } else {
    grid_major_y_line <- element_line(
      color = "gray75",
      linetype = "dashed",
      linewidth = base_line_size
    )
  }

  # set the minor x axis grid lines
  if (grid_minor_x == FALSE || grid_minor == FALSE || grid == FALSE || grid_y_only == TRUE) {
    grid_minor_x_line <- element_blank()
  } else {
    grid_minor_x_line <- element_line(
      color = "gray90",
      linetype = "dashed",
      linewidth = base_line_size / 2
    )
  }

  # set the major y axis grid lines
  if (grid_minor_y == FALSE || grid_minor == FALSE || grid == FALSE || grid_x_only == TRUE) {
    grid_minor_y_line <- element_blank()
  } else {
    grid_minor_y_line <- element_line(
      color = "gray90",
      linetype = "dashed",
      linewidth = base_line_size / 2
    )
  }


  theme(
    line                             = element_line(
      colour = "black",
      linewidth = base_line_size,
      linetype = 1,
      lineend = "butt"
    ),
    rect                             = element_rect(
      fill = "white",
      colour = "black",
      linewidth = base_rect_size,
      linetype = 1
    ),
    text                             = element_text(
      family = base_family,
      face = "plain",
      colour = "black",
      size = 12,
      lineheight = 1.1,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = margin(),
      debug = FALSE
    ),
    title                            = NULL,
    aspect.ratio                     = NULL,
    axis.title                       = NULL,
    axis.title.x                     = element_text(
      margin = margin(t = half_line),
      vjust = 1
    ),
    axis.title.x.top                 = NULL,
    axis.title.x.bottom              = NULL,
    axis.title.y                     = element_text(
      margin = margin(r = half_line),
      vjust = 1
    ),
    axis.title.y.left                = NULL,
    axis.title.y.right               = NULL,
    axis.text                        = axis_text_labels,
    axis.text.x                      = axis_text_x_labels,
    axis.text.x.top                  = NULL,
    axis.text.x.bottom               = NULL,
    axis.text.y                      = axis_text_y_labels,
    axis.text.y.left                 = NULL,
    axis.text.y.right                = NULL,
    axis.ticks                       = element_blank(),
    axis.ticks.x                     = NULL,
    axis.ticks.x.top                 = NULL,
    axis.ticks.x.bottom              = NULL,
    axis.ticks.y                     = NULL,
    axis.ticks.y.left                = NULL,
    axis.ticks.y.right               = NULL,
    axis.minor.ticks.x.top           = NULL,
    axis.minor.ticks.x.bottom        = NULL,
    axis.minor.ticks.y.left          = NULL,
    axis.minor.ticks.y.right         = NULL,
    axis.ticks.length                = unit(half_line / 2, "pt"),
    axis.ticks.length.x              = NULL,
    axis.ticks.length.x.top          = NULL,
    axis.ticks.length.x.bottom       = NULL,
    axis.ticks.length.y              = NULL,
    axis.ticks.length.y.left         = NULL,
    axis.ticks.length.y.right        = NULL,
    axis.minor.ticks.length          = NULL,
    axis.minor.ticks.length.x        = NULL,
    axis.minor.ticks.length.x.top    = NULL,
    axis.minor.ticks.length.x.bottom = NULL,
    axis.minor.ticks.length.y        = NULL,
    axis.minor.ticks.length.y.left   = NULL,
    axis.minor.ticks.length.y.right  = NULL,
    axis.line                        = element_blank(),
    axis.line.x                      = NULL,
    axis.line.x.top                  = NULL,
    axis.line.x.bottom               = NULL,
    axis.line.y                      = NULL,
    axis.line.y.left                 = NULL,
    axis.line.y.right                = NULL,
    legend.background                = element_blank(),
    legend.margin                    = margin(half_line, half_line, half_line, half_line),
    legend.spacing                   = unit(base_size, "pt"),
    legend.spacing.x                 = NULL,
    legend.spacing.y                 = NULL,
    legend.key                       = element_blank(),
    legend.key.size                  = unit(1.2, "lines"),
    legend.key.height                = NULL,
    legend.key.width                 = NULL,
    legend.text                      = element_text(size = ggplot2::rel(0.8)),
    legend.text.align                = NULL,
    legend.title                     = element_text(hjust = 0),
    legend.title.align                = NULL,
    legend.position                  = "right",
    legend.direction                 = NULL,
    legend.justification             = "center",
    legend.box                       = NULL,
    legend.box.just                  = NULL,
    legend.box.margin                = margin(0, 0, 0, 0, "cm"),
    legend.box.background            = element_blank(),
    legend.box.spacing               = unit(base_size, "pt"),
    panel.background                 = element_rect(fill = "white", colour = NA),
    panel.border                     = element_blank(),
    panel.spacing                    = unit(half_line, "pt"),
    panel.spacing.x                  = NULL,
    panel.spacing.y                  = NULL,
    panel.grid                       = element_blank(),
    panel.grid.major                 = grid_major_line,
    panel.grid.minor                 = grid_minor_line,
    panel.grid.major.x               = grid_major_x_line,
    panel.grid.major.y               = grid_major_y_line,
    panel.grid.minor.x               = grid_minor_x_line,
    panel.grid.minor.y               = grid_minor_y_line,
    panel.ontop                      = FALSE,
    plot.background                  = element_rect(colour = "white"),
    plot.title                       = element_text(
      size = rel(1.2),
      family = "TW",
      face = "bold",
      hjust = 0.5,
      vjust = 1,
      margin = margin(b = 10)
    ),
    plot.title.position              = "plot",
    plot.subtitle                    = element_text(
      family = "TW",
      hjust = 0,
      vjust = 1,
      margin = margin(b = 15)
    ),
    plot.caption                     = element_text(
      size = rel(0.66),
      hjust = 1,
      vjust = 1,
      margin = margin(t = base_size * 0.8)
    ),
    plot.caption.position            = "panel",
    plot.tag                         = element_text(
      size = rel(1.2),
      hjust = 0.5,
      vjust = 0.5
    ),
    plot.tag.position                = "topleft",
    plot.tag.location                = NULL,
    plot.margin                      = margin(base_size, base_size, base_size, base_size),
    strip.background                 = element_blank(),
    strip.background.x               = NULL,
    strip.background.y               = NULL,
    strip.clip                       = "inherit",
    strip.placement                  = "inside",
    strip.text                       = element_text(
      colour = "grey10",
      size = rel(0.8),
      margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
    ),
    strip.text.x                     = NULL,
    strip.text.x.bottom              = NULL,
    strip.text.x.top                 = NULL,
    strip.text.y                     = element_text(angle = -90),
    strip.text.y.left                = element_text(angle = -90),
    strip.text.y.right               = NULL,
    strip.switch.pad.grid            = unit(half_line / 2, "pt"),
    strip.switch.pad.wrap            = unit(half_line / 2, "pt"),
    complete = FALSE,
    validate = TRUE
  )
}









#' A function for coefficient plots
#'
#' This function creates a theme for coefficient plots.
#' @importFrom ggplot2 rel
#'@export
#'

theme_coef <- function() {
  ggplot::theme_minimal() +
    ggplot::theme(
      # adjust the overall text font family, size and lineheight
      text = element_text(family = "L", size = 12, lineheight = 1.1),
      # make the plot title TW font, bold, centered horizontally, and add bottom margin
      plot.title = element_text(family = "TW", face = "bold", hjust = 0.5,
                                margin = margin(b = 15)),
      # make plot subtitle TW font, left adjusted, add bottom margin
      plot.subtitle = element_text(family = "TW", hjust = 0,
                                   margin = margin(b = 15)),
      # add
      strip.text = element_text(face = "bold", size = 12, hjust = 0.5,
                                margin = margin(t = 10, b = 5)),
      plot.margin = margin(15, 15, 15, 15),
      panel.spacing.x = unit(2, "lines"),
      legend.position = "right",
      legend.text = element_text(lineheight = 1.1,
                                 margin = margin(b = 10, t = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.5, color = "gray75"),
      panel.grid.minor.x = element_line(linewidth = 0.25, color = "gray90")
    )
}


#' A function for horizontal stacked bar plots
#'
#' This function creates the theme for horizontal stacked bar plots
#' @inheritParams theme_default
#' @export
theme_h_stack <- function(

  base_size = 12,
  # determine if the axis labels are shown (controls both axes)
  axis_text = FALSE,
  # determine if grid lines should be shown (controls major and minor, x and y grid lines)
  grid = FALSE,
  ...
) {

  half_line <- base_size / 2

  theme_default(
    grid = grid,
    axis_text = axis_text,
    ...
  ) +
    ggplot2::theme(
      # adjust space margin around legend labels
      legend.text = element_text(margin = margin(r = 0, l = - half_line)),
      # adjust space around entire legend
      legend.margin = margin(b = 0),
      # legend position at the top
      legend.position = "top"
    )
}



#' A function for simple horizontal bar plots
#'
#' This function creates the theme for simple horizontal bar plots
#'
#' @export

# ggplot::theme for horizontal bar plots
theme_h_bar <- function() {
  ggplot::theme_minimal() +
    ggplot::theme(
      text = element_text(family = "L", size = 12, lineheight = 1.1),
      plot.title = element_text(family = "TW", face = "bold", hjust = 0.5,
                                margin = margin(b = 10)),
      # center title over entire plot
      plot.title.position = "plot",
      plot.subtitle = element_text(family = "TW", hjust = 0,
                                   margin = margin(b = 15)),
      plot.caption = element_text(hjust = 1, size = 8, lineheight = 1.05,
                                  # add space above caption
                                  margin = margin(10)),
      panel.grid = element_blank(),
      legend.position = "none",
      axis.text.x = element_blank(),
      plot.margin = margin(15, 15, 15, 15)
    )
}

#' A function for simple vertical bar plots
#'
#' This function creates the theme for simple vertical bar plots
#'
#' @export

# ggplot::theme for vertical bar charts
theme_v_bar <- function() {
  ggplot::theme_minimal() +
    ggplot::theme(
      text = element_text(family = "L", size = 12, lineheight = 1.1),
      plot.title = element_text(family = "TW", face = "bold", hjust = 0.5,
                                margin = margin(b = 10)),
      # center title over entire plot
      plot.title.position = "plot",
      plot.subtitle = element_text(family = "TW", hjust = 0,
                                   margin = margin(b = 15)),
      plot.caption = element_text(hjust = 1, size = 8, margin = margin(5), lineheight = 1.05),
      # make sure there is space around the graph
      plot.margin = margin(15, 15, 15, 15),
      # make the background white
      #plot.background = element_rect(fill = "white"),
      axis.text.x = element_text(margin = margin(b = 10), lineheight = 1.1),
      axis.text.y = element_blank(),
      axis.title.x = element_text(margin = margin(b = 10),
                                  lineheight = 1.1),
      #axis.title.y = element_text(margin = margin(r = 10)),
      legend.text = element_text(margin = margin(r = 10, l = -3)),
      # legend position
      legend.position = "top",
      panel.grid = element_blank(),

    )
}







