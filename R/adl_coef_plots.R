#' Create coefficient plots in ADL's style
#'
#' This function allows users to create a coefficient plot with ADL's style.
#' It is a wrapper around \code{\link[ggplot2]{geom_point}},
#' \code{\link[ggplot2]{geom_text}}/,\code{\link[ggplot2]{geom_label}}, and
#' \code{\link[ggplot2]{geom_errorbar}}. This function was created to
#' standardize the graphs produced by CAR's team and to cut down on the amount
#' of time it takes to make these graphs.
#'
#' As mentioned previously, this function is a wrapper around various `{ggplot2}`
#' functions in order to save time when making simple bar plots, dodged bar
#' plots, and stacked bar plots. As a result, it is not possible to combine
#' every element of the graph. If you would like to do so, we recommend using the
#' actual geoms from `{ggplot2}`.
#'
#' Each of the elements included serve a purpose to allow you to customize the
#' graphs so that they look nice. Moreover, the arguments were created for the
#' types of graphs that CAR produces, namely mean plots and frequency plots.
#'
#'
#' @param df A dataframe or tibble. This can be piped in like with a normal
#'   \code{\link[ggplot2]{ggplot}} function.
#'
#' @param x Variable that goes in the x-axis. This is required.
#' @param y Variable that goes in the y-axis. This is required.
#' @param col_label Variable that provides the values to be used as the labels
#'   in the plot. This is what is used in `geom_text` and `geom_label`
#' @param group Explicitly set the overall grouping variable. This is used in
#'   stacked graphs and dodged graphs. If NULL, the default, no grouping
#'   variable is used. Note: No need to set this if the  data is not grouped
#'   at all.
#' @param color Set the grouping variable for which the color of the dots and
#'   the confidence intervals are colored. If NULL, the default, no grouping
#'   variable is used. Note: No need to set this if the
#'   data is not grouped at all.
#' @param direction A character string indicating the direction of the bars.
#'   There are two options:
#'   1. "vertical", the default, the bars are vertical
#'   2. "horizontal" the bars are horizontal
#'   This must be set explicitly as it affects the location of the text, labels,
#'   and error bars.
#' @param col_text_size The size of the text inside/on top of the columns.
#'   Default is 3.25.
#' @param distance_from_col How far the labels are from the bars in freq plots
#'   and how far they are from the bottom of the bar in the mean plots.
#' @param freq_plot Logical. Determines if this is a frequency plot. If `TRUE`,
#'   default, then the graph will be styled as a frequency plot with the bar
#'   labels appearing outside the bars. `FALSE`, the graph will be styled as a
#'   mean plot with the labels appearing.
#' @param position A character string determining how the plot handles a grouped
#'   graph. By default it is `NULL` which assumes there is no grouping variable.
#'   If you set to "dodge" then you will get a dodged plot. This is best used
#'   when comparing two groups, especially when they do not add up to 100. If
#'   you set it to "stacked" then you get a stacked plot. This should be used
#'   when comparing multiple statements with likert scales and other things that
#'   add up to 100.
#' @param dodge_width This adjusts the width in the dodge plot. For more info
#'   check out \code{\link[ggplot2]{position_dodge}}.
#' @param dodge_reverse Reverses the order of the bars and text in a dodge plot.
#'   For more info check out \code{\link[ggplot2]{position_dodge}}.
#' @param ... Additional arguments passed on to `theme_default`
#'
#' @export
#'
#'

# adl_coef_plots <- function(
#     df,
#     x,
#     y,
#     point_size = 3.5,
#     color = NULL,
#     facet = var_label
# ) {
#   plot <- df %>%
#     ggplot2::ggplot(., ggplot2::aes(x = {{ x }}, y = {{ y }}, color = {{ color }}))
#
#
#   # create the coefficient plot
#   if (!is.null(color)) {
#     # if color is not null
#     plot <- plot +
#       ggplot2::geom_point(size = point_size) +
#       ggplot2::geom_linerange(ggplot2::aes(xmin = conf.low, xmax = conf.high)) +
#       ggplot2::geom_vline(xintercept = 0) +
#       #ggplot2::facet_wrap(~group, ncol = 1, scales = "free_y") +
#       theme_coef()
#
#
#   } else {
#     # if color is null
#     plot <- plot +
#       ggplot2::geom_point(size = point_size) +
#       ggplot2::geom_linerange(ggplot2::aes(xmin = conf.low, xmax = conf.high)) +
#       ggplot2::geom_vline(xintercept = 1) +
#       ggplot2::facet_wrap(~var_label, ncol = 1, scales = "free_y") +
#       theme_coef()
#   }
#
#   return(plot)
#
# }
#










