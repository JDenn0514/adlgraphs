#' Create bar plots in ADL's style
#'
#' This function allows users to create different types of bar plots with
#' ADL's style. This a wrapper around \code{\link[ggplot2]{geom_col}},
#' \code{\link[ggplot2]{geom_text}}/\code{\link[ggplot2]{geom_label}}, and
#' \code{\link[ggplot2]{geom_errorbar}}. The purpose of this function is to save
#' the Center for Antisemitism Research's team time when making graphs.
#'
#' @param df A dataframe or tibble.

adl_bar_plots <- function(
    df,
    x,
    y,
    group = NULL,
    fill = NULL,
    color = NULL,
    orientation = "vertical",
    col_text_size = 3.25,
    freq_plot = TRUE,
    tropes = FALSE,
    position = "stack",
    dodge_width = 0.8,
    dodge_reverse = TRUE,
    distance_from_bar = 0.25,
    ...
) {
  # create the bar plot
  plot <- df %>%
    ggplot(., aes(x = {{ x }}, y = {{ y }}, group = {{ group }}, fill = {{ fill }}, color = {{ fill }}))


  if (position == "stack") {
    if (freq_plot == TRUE) {
      if (orientation == "horizontal") {
        plot <- plot +
          geom_col(
            fill = adl_palettes$primary,
            width = 0.8
          ) +
          geom_text(
            aes(label = pct_lab, x = ({{ x }} + distance_from_bar)),
            family = "L",
            size = col_text_size,
            color = "#2c2e35",
            hjust = 0
          ) +
          theme_h_bar(...)
      }
      else if (orientation == "vertical") {
        plot <- plot +
          geom_col(
            fill = adl_palettes$primary,
            width = 0.8
          ) +
          geom_text(
            aes(label = pct_lab, y = ({{ y }} + distance_from_bar)),
            family = "L",
            size = col_text_size,
            color = "#2c2e35",
            vjust = 0
          ) +
          theme_v_bar(...)

      }
    } else if (freq_plot == FALSE) {
      if (orientation == "horizontal") {
        if (tropes == FALSE) {
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
              aes(label = mean, x = distance_from_bar),
              family = "L",
              size = col_text_size,
              hjust = 0,
              fill = "white",
              color = "#2c2e35",
              label.padding = unit(2.5, "pt")
            ) +
            theme_h_bar(...)

        } else {
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
              aes(label = mean_tr, x = distance_from_bar),
              position = position_dodge(width = dodge_width),
              hjust = 0,
              family = "L",
              size = col_text_size,
              fill = "white",
              color = "#2c2e35",
              label.padding = unit(2.5, "pt")
            ) +
            geom_label(
              aes(label = mean_lab, x = distance_from_bar),
              position = position_dodge(width = dodge_width),
              hjust = 0,
              family = "L",
              size = col_text_size,
              fill = "white",
              color = "#2c2e35",
              label.padding = unit(2.5, "pt")
            ) +
            theme_h_bar(...)
        }
      } else if (orientation == "vertical") {
        if (tropes == FALSE) {
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
              aes(label = mean, y = distance_from_bar),
              family = "L",
              size = col_text_size,
              vjust = 0,
              fill = "white",
              color = "#2c2e35",
              label.padding = unit(2.5, "pt")
            ) +
            theme_v_bar(...)

        } else {
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
              aes(label = mean_tr, y = distance_from_bar),
              vjust = 0,
              family = "L",
              size = col_text_size,
              fill = "white",
              color = "#2c2e35",
              label.padding = unit(2.5, "pt")
            ) +
            geom_label(
              aes(label = mean_lab, y = distance_from_bar),
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
  } else if (position == "dodge") {
    if (freq_plot == TRUE) {
      if (orientation == "horizontal") {
        # normal horizontal freq plot
        plot <- plot +
          geom_col(
            aes(fill = {{ fill }}, color = {{ color }}),
            position = position_dodge2(width = dodge_width, reverse = dodge_reverse),
            width = 0.8
          ) +
          geom_text(
            aes(label = pct_lab, x = ({{ x }} + distance_from_bar)),
            position = position_dodge2(width = dodge_width, reverse = dodge_reverse),
            family = "L",
            size = col_text_size,
            color = "#2c2e35",
            hjust = 0
          ) +
          theme_h_bar(...)
      }
      else if (orientation == "vertical") {
        # normal vertical freak plot
        plot <- plot +
          geom_col(
            aes(fill = {{ fill }}, color = {{ color}}),
            position = position_dodge2(width = dodge_width, reverse = dodge_reverse),
            width = 0.8
          ) +
          geom_text(
            aes(label = pct_lab, y = ({{ y }} + distance_from_bar)),
            position = position_dodge2(width = dodge_width, reverse = dodge_reverse),
            family = "L",
            size = col_text_size,
            color = "#2c2e35",
            vjust = 0
          ) +
          theme_v_bar(...)

      }
    } else if (freq_plot == FALSE) {
      if (orientation == "horizontal") {
        # horizontal dodged mean plot
        plot <- plot +
          geom_col(
            aes(fill = {{ fill }}, color = {{ color }}),
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
            aes(label = mean_tr, x = distance_from_bar),
            position = position_dodge(width = dodge_width),
            hjust = 0,
            family = "L",
            size = col_text_size,
            fill = "white",
            color = "#2c2e35",
            label.padding = unit(2.5, "pt")
          ) +
          geom_label(
            aes(label = mean_lab, x = distance_from_bar),
            position = position_dodge(width = dodge_width),
            hjust = 0,
            family = "L",
            size = col_text_size,
            fill = "white",
            color = "#2c2e35",
            label.padding = unit(2.5, "pt")
          ) +
          theme_h_bar(...)

      } else if (orientation == "vertical") {
        # vertical dodged mean plot
        plot <- plot +
          geom_col(
            aes(fill = {{ fill }}, color = {{ color }}),
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
            aes(label = mean_tr, y = distance_from_bar),
            position = position_dodge(width = dodge_width),
            vjust = 0,
            family = "L",
            size = col_text_size,
            fill = "white",
            color = "#2c2e35",
            label.padding = unit(2.5, "pt")
          ) +
          geom_label(
            aes(label = mean_lab, y = distance_from_bar),
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

}
