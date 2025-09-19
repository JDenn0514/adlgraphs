#' Create bar plots in ADL's style
#'
#' This function allows users to create different types of bar plots with
#' ADL's style. This a wrapper around \code{\link[ggplot2]{geom_col}},
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
#' @param data A dataframe or tibble. This can be piped in like with a normal
#'   \code{\link[ggplot2]{ggplot}} function.
#'
#' @param x Variable that goes in the x-axis. This is required.
#' @param y Variable that goes in the y-axis. This is required.
#' @param col_label Variable that provides the values to be used to label each
#'   column or stack.
#' @param group Explicitly set the overall grouping variable. This is used in
#'   stacked graphs and dodged graphs. If NULL, the default, no grouping
#'   variable is used. Note: No need to set this if the  data is not grouped
#'   at all.
#' @param fill Set the grouping variable for which the inside of the bars are
#'   colored. This is used in stacked graphs and dodged graphs. If NULL, the
#'   default, no grouping variable is used. Note: No need to set this if the
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
#' @param wrap_facet_labels Determine number of characters per line in the
#'   labels. Uses \link[ggplot2]{label_wrap_gen} to wrap the text across
#'   multiple lines. If left blank, defaults to 200 so that it in essence
#'   won't wrap the text.
#'   text
#' @param ... Additional arguments passed on to `theme_default`
#'
#' @export
#'
#'

adl_bar_plots <- function(
  data,
  x,
  y,
  col_label,
  group = NULL,
  fill = NULL,
  direction = "vertical",
  col_text_size = 3.25,
  distance_from_col = 0.25,
  freq_plot = TRUE,
  position = NULL,
  dodge_width = 0.8,
  dodge_reverse = TRUE,
  wrap_facet_labels = 100,
  ...
) {
  # check to see if the user is missing some variables
  if (missing(data)) {
    cli::cli_abort(c(
      "{.var data} is missing",
      "i" = "An object of type `data.frame` or `tibble` must be supplied to {.var data}"
    ))
  } else if (missing(x)) {
    cli::cli_abort(c(
      "{.var x} is missing",
      "i" = "A vector must be supplied to {.var x}"
    ))
  } else if (missing(y)) {
    cli::cli_abort(c(
      "{.var y} is missing",
      "i" = "A vector must be supplied to {.var y}"
    ))
  } else if (missing(col_label)) {
    cli::cli_abort(c(
      "{.var col_label} is missing",
      "i" = "A vector must be supplied to {.var col_label}"
    ))
  }

  # create the bar plot
  plot <- data %>%
    ggplot2::ggplot(
      .,
      ggplot2::aes(
        x = {{ x }},
        y = {{ y }},
        group = {{ group }},
        fill = {{ fill }}
      )
    )

  if (is.null(position)) {
    # position is NULL

    if (freq_plot == TRUE) {
      # if freq_plot is set to TRUE

      if (direction == "horizontal") {
        # if direction is set to horizontal

        if (is.null(group) && is.null(fill)) {
          # if the group is set to NULL and fill is set to NULL

          plot <- plot +
            ggplot2::geom_col(
              fill = adl_palettes$primary,
              width = 0.8
            ) +
            ggplot2::geom_text(
              ggplot2::aes(
                label = {{ col_label }},
                x = ({{ x }} + distance_from_col)
              ),
              family = adlgraphs_global$font$regular$family,
              size = col_text_size,
              color = "#2c2e35",
              hjust = 0
            ) +
            theme_h_bar(...)
        } else {
          # if fill and group are not set to NULL

          plot <- plot +
            ggplot2::geom_col(
              width = 0.8
            ) +
            ggplot2::geom_text(
              ggplot2::aes(
                label = {{ col_label }},
                x = ({{ x }} + distance_from_col)
              ),
              family = adlgraphs_global$font$regular$family,
              size = col_text_size,
              color = "#2c2e35",
              hjust = 0
            ) +
            theme_h_bar(...)
        }
      } else if (direction == "vertical") {
        # if the direction is set to vertical

        if (is.null(group) && is.null(fill)) {
          # if group and fill are NULL

          plot <- plot +
            ggplot2::geom_col(
              fill = adl_palettes$primary,
              width = 0.8,
            ) +
            ggplot2::geom_text(
              ggplot2::aes(
                label = {{ col_label }},
                y = ({{ y }} + distance_from_col)
              ),
              family = adlgraphs_global$font$regular$family,
              size = col_text_size,
              color = "#2c2e35",
              vjust = 0
            ) +
            theme_v_bar(...)
        } else {
          # if neither group nor fill are NULL

          plot <- plot +
            ggplot2::geom_col(
              width = 0.8
            ) +
            ggplot2::geom_text(
              ggplot2::aes(
                label = {{ col_label }},
                y = ({{ y }} + distance_from_col)
              ),
              family = adlgraphs_global$font$regular$family,
              size = col_text_size,
              color = "#2c2e35",
              vjust = 0
            ) +
            theme_v_bar(...)
        }
      }
    } else if (freq_plot == FALSE) {
      # freq_plot is set to false (i.e., it is a mean plot)

      if (direction == "horizontal") {
        # if direction is set to horizontal

        plot <- plot +
          ggplot2::geom_col(
            fill = adl_palettes$primary,
            width = 0.8
          ) +
          ggplot2::geom_errorbar(
            ggplot2::aes(xmin = conf.low, xmax = conf.high),
            width = 0.2,
            color = "#2c2e35"
          ) +
          ggplot2::geom_label(
            ggplot2::aes(label = {{ col_label }}, x = distance_from_col),
            family = adlgraphs_global$font$regular$family,
            size = col_text_size,
            hjust = 0,
            fill = "white",
            color = "#2c2e35",
            label.padding = ggplot2::unit(2.5, "pt")
          ) +
          theme_h_bar(...)
      } else if (direction == "vertical") {
        # if direction is to vertical

        plot <- plot +
          ggplot2::geom_col(
            fill = adl_palettes$primary,
            width = 0.8
          ) +
          ggplot2::geom_errorbar(
            ggplot2::aes(ymin = conf.low, ymax = conf.high),
            width = 0.2,
            color = "#2c2e35"
          ) +
          ggplot2::geom_label(
            ggplot2::aes(label = {{ col_label }}, y = distance_from_col),
            family = adlgraphs_global$font$regular$family,
            size = col_text_size,
            vjust = 0,
            fill = "white",
            color = "#2c2e35",
            label.padding = ggplot2::unit(2.5, "pt")
          ) +
          theme_v_bar(...)
      }
    }
  } else if (position == "dodge") {
    # if position is set to dodge

    if (freq_plot == TRUE) {
      # if frequency plot is set to true

      if (direction == "horizontal") {
        # normal horizontal freq plot
        plot <- plot +
          ggplot2::geom_col(
            ggplot2::aes(fill = {{ fill }}),
            position = ggplot2::position_dodge2(
              width = dodge_width,
              reverse = dodge_reverse
            ),
            width = 0.8
          ) +
          ggplot2::geom_text(
            ggplot2::aes(
              label = {{ col_label }},
              x = ({{ x }} + distance_from_col)
            ),
            position = ggplot2::position_dodge2(
              width = dodge_width,
              reverse = dodge_reverse
            ),
            family = adlgraphs_global$font$regular$family,
            size = col_text_size,
            color = "#2c2e35",
            hjust = 0
          ) +
          theme_h_bar(...)
      } else if (direction == "vertical") {
        # normal vertical freak plot
        plot <- plot +
          ggplot2::geom_col(
            ggplot2::aes(fill = {{ fill }}),
            position = ggplot2::position_dodge2(
              width = dodge_width,
              reverse = dodge_reverse
            ),
            width = 0.8
          ) +
          ggplot2::geom_text(
            ggplot2::aes(
              label = {{ col_label }},
              y = ({{ y }} + distance_from_col)
            ),
            position = ggplot2::position_dodge2(
              width = dodge_width,
              reverse = dodge_reverse
            ),
            family = adlgraphs_global$font$regular$family,
            size = col_text_size,
            color = "#2c2e35",
            vjust = 0
          ) +
          theme_v_bar(...)
      }
    } else if (freq_plot == FALSE) {
      # if freq_plot is set to false

      if (direction == "horizontal") {
        # horizontal dodged mean plot
        plot <- plot +
          ggplot2::geom_col(
            ggplot2::aes(fill = {{ fill }}),
            position = position_dodge(width = dodge_width),
            width = 0.8
          ) +
          ggplot2::geom_errorbar(
            ggplot2::aes(xmin = conf.low, xmax = conf.high),
            position = position_dodge(width = dodge_width),
            width = 0.2,
            color = "#2c2e35"
          ) +
          ggplot2::geom_label(
            ggplot2::aes(label = {{ col_label }}, x = distance_from_col),
            position = position_dodge(width = dodge_width),
            hjust = 0,
            family = adlgraphs_global$font$regular$family,
            size = col_text_size,
            fill = "white",
            color = "#2c2e35",
            label.padding = ggplot2::unit(2.5, "pt")
          ) +
          theme_h_bar(...)
      } else {
        # vertical dodged mean plot
        plot <- plot +
          ggplot2::geom_col(
            ggplot2::aes(fill = {{ fill }}),
            position = position_dodge(width = dodge_width),
            width = 0.8
          ) +
          ggplot2::geom_errorbar(
            ggplot2::aes(ymin = conf.low, ymax = conf.high),
            position = position_dodge(width = dodge_width),
            width = 0.2,
            color = "#2c2e35"
          ) +
          ggplot2::geom_label(
            ggplot2::aes(label = {{ col_label }}, y = distance_from_col),
            position = ggplot2::position_dodge(width = dodge_width),
            vjust = 0,
            family = adlgraphs_global$font$regular$family,
            size = col_text_size,
            fill = "white",
            color = "#2c2e35",
            label.padding = ggplot2::unit(2.5, "pt")
          ) +
          theme_v_bar(...)
      }
    }
  } else if (position == "stack") {
    # id position is set to "stack

    if (direction == "horizontal") {
      plot <- plot +
        ggplot2::geom_col(
          position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE)
        ) +
        ggplot2::geom_label(
          aes(label = {{ col_label }}),
          position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
          family = adlgraphs_global$font$regular$family,
          size = col_text_size,
          hjust = 0.5,
          fill = "white",
          color = "#2c2e35",
          label.padding = ggplot2::unit(2.5, "pt")
        ) +
        ggplot2::facet_wrap(
          vars({{ y }}),
          ncol = 1,
          labeller = ggplot2::label_wrap_gen(wrap_facet_labels),
          scales = "free_y"
        ) +
        theme_h_stack(...)
    }
  }
  return(plot)
}
