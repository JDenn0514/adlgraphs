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
#' @param ... Additional arguments passed on to `theme_default`
#'
#' @export
#'
#'


# Add the ability to add groups to a non-dodged or stacked bar

adl_bar_plots <- function(
    df,
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
    ...
) {

  # check to see if the user is missing some variables
  if (missing(df)) {
    cli::cli_abort(c(
      "{.var df} is missing",
      "i" = "An object of type data.frame or tibble must be supplied to {.var df}"
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
  plot <- df %>%
    ggplot(., aes(x = {{ x }}, y = {{ y }}, group = {{ group }}, fill = {{ fill }}))


  if (is.null(position)) {
    if (freq_plot == TRUE) {
      if (direction == "horizontal") {
        if (is.null(group) && is.null(fill)) {
          plot <- plot +
            geom_col(
              fill = adl_palettes$primary,
              width = 0.8
            ) +
            geom_text(
              aes(label = {{ col_label }}, x = ({{ x }} + distance_from_col)),
              family = "L",
              size = col_text_size,
              color = "#2c2e35",
              hjust = 0
            ) +
            theme_h_bar(...)
        } else {
          plot <- plot +
            geom_col(
              width = 0.8
            ) +
            geom_text(
              aes(label = {{ col_label }}, x = ({{ x }} + distance_from_col)),
              family = "L",
              size = col_text_size,
              color = "#2c2e35",
              hjust = 0
            ) +
            theme_h_bar(...)
        }
      }
      else if (direction == "vertical") {
        if (is.null(group) && is.null(fill)) {
          plot <- plot +
            geom_col(
              fill = adl_palettes$primary,
              width = 0.8,
            ) +
            geom_text(
              aes(label = {{ col_label }}, y = ({{ y }} + distance_from_col)),
              family = "L",
              size = col_text_size,
              color = "#2c2e35",
              vjust = 0
            ) +
            theme_v_bar(...)

        } else {
          plot <- plot +
            geom_col(
              width = 0.8
            ) +
            geom_text(
              aes(label = {{ col_label }}, y = ({{ y }} + distance_from_col)),
              family = "L",
              size = col_text_size,
              color = "#2c2e35",
              vjust = 0
            ) +
            theme_v_bar(...)

        }
      }
    } else if (freq_plot == FALSE) {
      if (direction == "horizontal") {
        plot <- plot +
          geom_col(
            fill = adl_palettes$primary,
            width = 0.8
          ) +
          geom_errorbar(
            aes(xmin = lower, xmax = upper),
            width = 0.2,
            color = "#2c2e35"
          ) +
          geom_label(
            aes(label = mean, x = distance_from_col),
            family = "L",
            size = col_text_size,
            hjust = 0,
            fill = "white",
            color = "#2c2e35",
            label.padding = unit(2.5, "pt")
          ) +
          theme_h_bar(...)
      } else if (direction == "vertical") {
        plot <- plot +
          geom_col(
            fill = adl_palettes$primary,
            width = 0.8
          ) +
          geom_errorbar(
            aes(ymin = lower, ymax = upper),
            width = 0.2,
            color = "#2c2e35"
          ) +
          geom_label(
            aes(label = {{ col_label }}, y = distance_from_col),
            family = "L",
            size = col_text_size,
            vjust = 0,
            fill = "white",
            color = "#2c2e35",
            label.padding = unit(2.5, "pt")
          ) +
          theme_v_bar(...)
      }
    }
  } else if (position == "dodge") {
    if (freq_plot == TRUE) {
      if (direction == "horizontal") {
        # normal horizontal freq plot
        plot <- plot +
          geom_col(
            aes(fill = {{ fill }}),
            position = position_dodge2(width = dodge_width, reverse = dodge_reverse),
            width = 0.8
          ) +
          geom_text(
            aes(label = {{ col_label }}, x = ({{ x }} + distance_from_col)),
            position = position_dodge2(width = dodge_width, reverse = dodge_reverse),
            family = "L",
            size = col_text_size,
            color = "#2c2e35",
            hjust = 0
          ) +
          theme_h_bar(...)
      }
      else if (direction == "vertical") {
        # normal vertical freak plot
        plot <- plot +
          geom_col(
            aes(fill = {{ fill }}),
            position = position_dodge2(width = dodge_width, reverse = dodge_reverse),
            width = 0.8
          ) +
          geom_text(
            aes(label = {{ col_label }}, y = ({{ y }} + distance_from_col)),
            position = position_dodge2(width = dodge_width, reverse = dodge_reverse),
            family = "L",
            size = col_text_size,
            color = "#2c2e35",
            vjust = 0
          ) +
          theme_v_bar(...)

      }
    } else if (freq_plot == FALSE) {
      if (direction == "horizontal") {
        # horizontal dodged mean plot
        plot <- plot +
          geom_col(
            aes(fill = {{ fill }}),
            position = position_dodge(width = dodge_width),
            width = 0.8
          ) +
          geom_errorbar(
            aes(xmin = lower, xmax = upper),
            position = position_dodge(width = dodge_width),
            width = 0.2,
            color = "#2c2e35"
          ) +
          geom_label(
            aes(label = {{ col_label }}, x = distance_from_col),
            position = position_dodge(width = dodge_width),
            hjust = 0,
            family = "L",
            size = col_text_size,
            fill = "white",
            color = "#2c2e35",
            label.padding = unit(2.5, "pt")
          ) +
          theme_h_bar(...)

      } else if (direction == "vertical") {
        # vertical dodged mean plot
        plot <- plot +
          geom_col(
            aes(fill = {{ fill }}),
            position = position_dodge(width = dodge_width),
            width = 0.8
          ) +
          geom_errorbar(
            aes(ymin = lower, ymax = upper),
            position = position_dodge(width = dodge_width),
            width = 0.2,
            color = "#2c2e35"
          ) +
          geom_label(
            aes(label = {{ col_label }}, y = distance_from_col),
            position = position_dodge(width = dodge_width),
            vjust = 0,
            family = "L",
            size = col_text_size,
            fill = "white",
            color = "#2c2e35",
            label.padding = unit(2.5, "pt")
          ) +
          theme_v_bar(...)
      }
    }

  }
  else if (position == "stack") {
    if (direction == "horizontal") {
      plot <- plot +
        geom_col(
          position = position_stack(vjust = 0.5, reverse = TRUE)
        ) +
        geom_label(
          aes(label = {{ col_label }}),
          position = position_stack(vjust = 0.5, reverse = TRUE),
          family = "L",
          size = col_text_size,
          hjust = 0.5,
          fill = "white",
          color = "#2c2e35",
          label.padding = unit(2.5, "pt")
        ) +
        facet_wrap(vars({{ y }}), ncol = 1, scales = "free_y") +
        theme_h_stack(...)
    }

  }
  return(plot)
}













