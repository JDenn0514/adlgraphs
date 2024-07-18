#' #' Theme for plots used on the website
#' #'
#' #' This function is the same as `theme_default()` with one key difference: the
#' #' `base_size` is set to 42 instead of 12. This is so that it looks properly
#' #' when the `dpi` is set to 72 when saving a file in `ggsave()`.
#' #'
#' #' @param base_size Base font size, given in pts. Also controls the spacing in
#' #'   the graph.
#' #' @param ... Other arguments passed onto `theme_default()`.
#'
#' #' @export
#'
#' theme_default_web <- function(
#'     # set the base font size
#'   base_size = 42,
#'   ...
#' ) {
#'
#'   half_line <- base_size / 2
#'
#'   theme_default(
#'     base_size = base_size,
#'     ...
#'   )
#' }
#'
#'
#' #' Theme for coefficient plots
#' #'
#' #' This function creates a theme for coefficient plots that will be used on the
#' #' web.
#' #'
#' #' @param base_size Base font size, given in pts. Also controls the spacing in
#' #'   the graph.
#' #' @param grid_x_only Logical. Determines if only x-axis grid lines (vertical
#' #'   lines) should appear. If `FALSE`, the default, all grid lines appear. If
#' #'   `TRUE`, only the x-axis grid lines appear and the y-axis grid lines will
#' #'   disappear.
#' #' @param facet_title_margin_top The margin above the facet title, specified in
#' #'   pts. Default is `0.8 * half_line`.
#' #' @param facet_title_margin_bottom The margin beneath the facet title,
#' #'   specified in pts. Default is `0.8 * half_line`.
#' #' @param facet_title_margin_right The margin to the right of the facet title,
#' #'   specified in pts. Default is `0.8 * half_line`.
#' #' @param facet_title_margin_left The margin to the left of the facet title,
#' #'   specified in pts. Default is `0.8 * half_line`.
#' #' @param ... Other arguments passed onto `theme_default()`.
#' #'
#' #' @export
#' theme_coef_web <- function(
#'     # set the base font size
#'   base_size = 42,
#'   # determine if grid lines should be shown (controls major and minor, x and y grid lines)
#'   grid_x_only = TRUE,
#'   facet_title_margin_top = base_size,
#'   facet_title_margin_bottom = half_line,
#'   facet_title_margin_right = 0.8 * half_line,
#'   facet_title_margin_left = 0.8 * half_line,
#'   ...
#' ) {
#'
#'   half_line <- base_size / 2
#'
#'   theme_default(
#'     base_size = base_size,
#'     grid_x_only = grid_x_only,
#'     facet_title_margin_top = facet_title_margin_top,
#'     facet_title_margin_bottom = facet_title_margin_bottom,
#'     facet_title_margin_right = facet_title_margin_right,
#'     facet_title_margin_left = facet_title_margin_left,
#'     ...
#'   ) +
#'     ggplot2::theme(axis.title.x = element_text(margin = margin(t = base_size)))
#' }
#'
#'
#' #' Theme for horizontal stacked bar plots
#' #'
#' #' This function creates the theme for horizontally stacked bar plots that will
#' #' be used on the web.
#' #'
#' #' @param base_size Base font size, given in pts. Also controls the spacing in
#' #'   the graph.
#' #' @param legend_position The position of the legend. Options are: "left",
#' #'   "right", "top", "bottom", or "none". "none" removes the legend. "top"
#' #'   is the default.
#' #' @param axis_text Logical. Determines if BOTH axes have labels. If `FALSE`,
#' #'   the  default, neither axis are labelled. If `TRUE`, both axes is labelled.
#' #' @param grid Logical. Determines if any grid lines appear. If `FALSE`, the
#' #'   default, all grid lines disappear. If `TRUE`, all grid lines appear.
#' #' @param ... Other arguments passed onto `theme_default()`.
#' #'
#' #' @export
#' theme_h_stack <- function(
#'     # set the base font size
#'   base_size = 42,
#'   # put the legend at the top of the graph
#'   legend_position = "top",
#'   # determine if the axis labels are shown (controls both axes)
#'   axis_text = FALSE,
#'   # determine if grid lines should be shown (controls major and minor, x and y grid lines)
#'   grid = FALSE,
#'   ...
#' ) {
#'
#'   half_line <- base_size / 2
#'
#'   theme_default(
#'     base_size = base_size,
#'     grid = grid,
#'     legend_position = legend_position,
#'     axis_text = axis_text,
#'     ...
#'   )
#' }
#'
#'
#'
#'
#'
#' #' Theme for non-stacked horizontal bar plots
#' #'
#' #' This function creates a theme for non-stacked horizontal bar plots.
#' #'
#' #' @param base_size Base font size, given in pts. Also controls the spacing in
#' #'   the graph.
#' #' @param legend_position The position of the legend. Options are: "left",
#' #'   "right", "top", "bottom", or "none". "none" removes the legend. "none"
#' #'   is the default.
#' #' @param axis_text_x Logical. Determines if the x-axis has labels. If `FALSE`,
#' #'   the default, the x-axis labels are removed. If `TRUE`, the x-axis labels
#' #'   are shown.
#' #' @param grid Logical. Determines if ALL grid lines should appear. If `FALSE`,
#' #'   the default, all grid lines disappear. If `TRUE`, all grid lines appear.
#' #' @param ... Other arguments passed onto `theme_default()`.
#' #'
#' #' @export
#' #'
#' theme_h_bar <- function(
#'     # set the base font size
#'   base_size = 42,
#'   # adjust the position of the legend
#'   legend_position = "none",
#'   # turn off the x-axis labels so only y-axis are shown
#'   axis_text_x = FALSE,
#'   # determine if grid lines should be shown (controls major and minor, x and y grid lines)
#'   grid = FALSE,
#'   ...
#' ) {
#'
#'   half_line <- base_size / 2
#'
#'   theme_default(
#'     base_size = base_size,
#'     legend_position = legend_position,
#'     axis_text_x = axis_text_x,
#'     grid = grid,
#'     ...
#'   )
#' }
#'
#'
#' #' Theme for vertical non-stacked bar plots
#' #'
#' #' This function creates the theme for non-stacked vertical bar plots
#' #'
#' #' @param base_size Base font size, given in pts. Also controls the spacing in
#' #'   the graph.
#' #' @param legend_position The position of the legend. Options are: "left",
#' #'   "right", "top", "bottom", or "none". "none" removes the legend. "none"
#' #'   is the default.
#' #' @param axis_text_y Logical. Determines if the y-axis has labels. If `FALSE`,
#' #'   the default, the y-axis labels are removed. If `TRUE`, the y-axis labels
#' #'   are shown.
#' #' @param grid Logical. Determines if ALL grid lines should appear. If `FALSE`,
#' #'   the default, all grid lines disappear. If `TRUE`, all grid lines appear.
#' #' @param ... Other arguments passed onto `theme_default()`.
#'
#' #' @export
#'
#' theme_v_bar_web <- function(
#'     # set the base font size
#'   base_size = 42,
#'   # adjust the position of the legend
#'   legend_position = "none",
#'   # turn off the y-axis labels so only x-axis are shown
#'   axis_text_y = FALSE,
#'   # determine if grid lines should be shown (controls major and minor, x and y grid lines)
#'   grid = FALSE,
#'   ...
#' ) {
#'
#'   half_line <- base_size / 2
#'
#'   theme_default(
#'     base_size = base_size,
#'     legend_position = legend_position,
#'     axis_text_y = axis_text_y,
#'     grid = grid,
#'     ...
#'   )
#' }
