
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
#' @param legend_position The position of the legend. Options are: "left",
#'   "right", "top", "bottom", or "none". "none" removes the legend. "right"
#'   is the default.
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
  # determine where the legend is located
  legend_position = "right",
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
      inherit.blank = TRUE
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
      vjust = 1,
      inherit.blank = TRUE
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
      vjust = 1,
      inherit.blank = TRUE
    )
  }

  # set the major grid lines
  if (grid_major == FALSE || grid == FALSE) {
    grid_major_line <- element_blank()
  } else {
    grid_major_line <- element_line(
      color = "gray75",
      linetype = "dashed",
      linewidth = base_line_size,
      inherit.blank = TRUE
    )
  }

  # set the minor grid lines
  if (grid_minor == FALSE || grid == FALSE) {
    grid_minor_line <- element_blank()
  } else {
    grid_minor_line <- element_line(
      color = "gray90",
      linetype = "dashed",
      linewidth = base_line_size / 2,
      inherit.blank = TRUE
    )
  }


  # set the major x axis grid lines
  if (grid_major_x == FALSE || grid_major == FALSE || grid == FALSE || grid_y_only == TRUE) {
    grid_major_x_line <- element_blank()
  } else {
    grid_major_x_line <- element_line(
      color = "gray75",
      linetype = "dashed",
      linewidth = base_line_size,
      inherit.blank = TRUE
    )
  }

  # set the major y axis grid lines
  if (grid_major_y == FALSE || grid_major == FALSE || grid == FALSE || grid_x_only == TRUE) {
    grid_major_y_line <- element_blank()
  } else {
    grid_major_y_line <- element_line(
      color = "gray75",
      linetype = "dashed",
      linewidth = base_line_size,
      inherit.blank = TRUE
    )
  }

  # set the minor x axis grid lines
  if (grid_minor_x == FALSE || grid_minor == FALSE || grid == FALSE || grid_y_only == TRUE) {
    grid_minor_x_line <- element_blank()
  } else {
    grid_minor_x_line <- element_line(
      color = "gray90",
      linetype = "dashed",
      linewidth = base_line_size / 2,
      inherit.blank = TRUE
    )
  }

  # set the major y axis grid lines
  if (grid_minor_y == FALSE || grid_minor == FALSE || grid == FALSE || grid_x_only == TRUE) {
    grid_minor_y_line <- element_blank()
  } else {
    grid_minor_y_line <- element_line(
      color = "gray90",
      linetype = "dashed",
      linewidth = base_line_size / 2,
      inherit.blank = TRUE
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
      vjust = 1,
      inherit.blank = TRUE
    ),
    axis.title.x.top                 = NULL,
    axis.title.x.bottom              = NULL,
    axis.title.y                     = element_text(
      margin = margin(r = half_line),
      vjust = 1,
      inherit.blank = TRUE
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
    legend.text                      = element_text(
      size = ggplot2::rel(0.8),
      inherit.blank = TRUE
    ),
    legend.text.align                = NULL,
    legend.title                     = element_text(
      hjust = 0,
      inherit.blank = TRUE
    ),
    legend.title.align                = NULL,
    legend.position                  = legend_position,
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
    panel.grid                       = NULL,
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
      margin = margin(b = 10),
      inherit.blank = TRUE
    ),
    plot.title.position              = "plot",
    plot.subtitle                    = element_text(
      family = "TW",
      hjust = 0,
      vjust = 1,
      margin = margin(b = 15),
      inherit.blank = TRUE
    ),
    plot.caption                     = element_text(
      size = rel(0.66),
      hjust = 1,
      vjust = 1,
      margin = margin(t = base_size * 0.8),
      inherit.blank = TRUE
    ),
    plot.caption.position            = "panel",
    plot.tag                         = element_text(
      size = rel(1.2),
      hjust = 0.5,
      vjust = 0.5,
      inherit.blank = TRUE
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
      margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line),
      inherit.blank = TRUE
    ),
    strip.text.x                     = NULL,
    strip.text.x.bottom              = NULL,
    strip.text.x.top                 = NULL,
    strip.text.y                     = element_text(
      angle = -90,
      inherit.blank = TRUE
    ),
    strip.text.y.left                = element_text(
      angle = -90,
      inherit.blank = TRUE
    ),
    strip.text.y.right               = NULL,
    strip.switch.pad.grid            = unit(half_line / 2, "pt"),
    strip.switch.pad.wrap            = unit(half_line / 2, "pt"),
    complete = FALSE,
    validate = TRUE
  )
}


#' Theme for coefficient plots
#'
#' This function creates a theme for coefficient plots.
#'
#' @param base_size Base font size, given in pts. Also controls the spacing in
#'   the graph.
#' @param grid_x_only Logical. Determines if only x-axis grid lines (vertical
#'   lines) should appear. If `FALSE`, the default, all grid lines appear. If
#'   `TRUE`, only the x-axis grid lines appear and the y-axis grid lines will
#'   disappear.
#' @param ... Other arguments passed onto `theme_default()`.
#'
#' @export
theme_coef <- function(
    # set the base font size
  base_size = 12,
  # determine if grid lines should be shown (controls major and minor, x and y grid lines)
  grid_x_only = TRUE,
  ...
) {

  half_line <- base_size / 2

  theme_default(
    base_size = base_size,
    grid_x_only = grid_x_only,
    ...
  ) +
    ggplot2::theme(
      strip.text = element_text(
        face = "bold",
        size = 12,
        hjust = 0.5,
        margin = margin(t = 10, b = 5)
      ),
      panel.spacing.x = unit(2, "lines"),
      axis.title.x = element_text(margin = margin(t = base_size)),
    )
}


#' Theme for horizontal stacked bar plots
#'
#' This function creates the theme for horizontally stacked bar plots.
#'
#' @param base_size Base font size, given in pts. Also controls the spacing in
#'   the graph.
#' @param legend_position The position of the legend. Options are: "left",
#'   "right", "top", "bottom", or "none". "none" removes the legend. "top"
#'   is the default.
#' @param axis_text Logical. Determines if BOTH axes have labels. If `FALSE`,
#'   the  default, neither axis are labelled. If `TRUE`, both axes is labelled.
#' @param grid Logical. Determines if any grid lines appear. If `FALSE`, the
#'   default, all grid lines disappear. If `TRUE`, all grid lines appear.
#' @param ... Other arguments passed onto `theme_default()`.
#'
#' @export
theme_h_stack <- function(
  # set the base font size
  base_size = 12,
  # put the legend at the top of the graph
  legend_position = "top",
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
      legend.margin = margin(b = 0)
    )
}





#' Theme for non-stacked horizontal bar plots
#'
#' This function creates a theme for non-stacked horizontal bar plots.
#'
#' @param base_size Base font size, given in pts. Also controls the spacing in
#'   the graph.
#' @param legend_position The position of the legend. Options are: "left",
#'   "right", "top", "bottom", or "none". "none" removes the legend. "right"
#'   is the default.
#' @param axis_text_x Logical. Determines if the x-axis has labels. If `FALSE`,
#'   the default, the x-axis labels are removed. If `TRUE`, the x-axis labels
#'   are shown.
#' @param grid Logical. Determines if ALL grid lines should appear. If `FALSE`,
#'   the default, all grid lines disappear. If `TRUE`, all grid lines appear.
#' @param ... Other arguments passed onto `theme_default()`.
#'
#' @export
#'
theme_h_bar <- function(
  # set the base font size
  base_size = 12,
  # adjust the position of the legend
  legend_position = "none",
  # turn off the x-axis labels so only y-axis are shown
  axis_text_x = FALSE,
  # determine if grid lines should be shown (controls major and minor, x and y grid lines)
  grid = FALSE,
  ...
) {

  half_line <- base_size / 2

  theme_default(
    base_size = base_size,
    legend_position = legend_position,
    axis_text_x = axis_text_x,
    grid = grid,
    ...
  )
}


#' Theme for vertical non-stacked bar plots
#'
#' This function creates the theme for non-stacked vertical bar plots
#'
#' #' @param base_size Base font size, given in pts. Also controls the spacing in
#'   the graph.
#' @param legend_position The position of the legend. Options are: "left",
#'   "right", "top", "bottom", or "none". "none" removes the legend. "right"
#'   is the default.
#' @param axis_text_y Logical. Determines if the y-axis has labels. If `FALSE`,
#'   the default, the y-axis labels are removed. If `TRUE`, the y-axis labels
#'   are shown.
#' @param grid Logical. Determines if ALL grid lines should appear. If `FALSE`,
#'   the default, all grid lines disappear. If `TRUE`, all grid lines appear.
#' @param ... Other arguments passed onto `theme_default()`.

#' @export

theme_v_bar <- function(
    # set the base font size
  base_size = 12,
  # adjust the position of the legend
  legend_position = "none",
  # turn off the y-axis labels so only x-axis are shown
  axis_text_y = FALSE,
  # determine if grid lines should be shown (controls major and minor, x and y grid lines)
  grid = FALSE,
  ...
) {

  half_line <- base_size / 2

  theme_default(
    base_size = base_size,
    legend_position = legend_position,
    axis_text_y = axis_text_y,
    grid = grid,
    ...
  )
}








