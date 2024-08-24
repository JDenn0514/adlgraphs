#' Create coefficient plots in ADL's style
#'
#' This function allows users to create a coefficient plot with ADL's style and
#' it was designed to help reduce the time it takes to make publication-ready
#' graphs.
#'
#' This function works best when used in conjunction with `get_coefficients()`.
#'
#' `adl_coef_plots()` is a wrapper around \code{\link[ggplot2]{geom_point}},
#' \code{\link[ggplot2]{geom_linerange}}/ and \code{\link[ggplot2]{geom_vline}}.
#' As a result, it is not possible to customize every aspect of this graph. If
#' you would like to do so, I recommend using the actual geoms from `{ggplot2}`.
#'
#' To differentiate between different variables and different models, use the
#' `facet` and `color` arguments like you would normally. However, when just
#' differentiating between variables, using the `facet` argument is recommended
#' as it makes it easier to distinguish the different variables and to compare
#' the estimates their values have.
#'
#'
#' @param data A dataframe or tibble. This can be piped in like with a normal
#'   \code{\link[ggplot2]{ggplot}} function.
#' @param x Variable that goes in the x-axis.
#' @param y Variable that goes in the y-axis.
#' @param color Set the grouping variable for which the color of the dots and
#'   the confidence intervals are colored. If NULL, the default, no grouping
#'   variable is used. Note: No need to set this if the
#'   data is not grouped at all
#' @param facet Set the variable for which you want the plot faceted. This
#'   is typically used instead of the `color` argument to distinguish the
#'   different variables and their values. Default is `var_label` but can be set
#'   to any other column in the object set in `data`. Must be set to `NULL` to
#'   remove faceting from the plot.
#' @param facet_order A character string indicating if the order of variables
#'   with which the graph is faceted should be reversed or not. There are two
#'   options:
#'   1. "original" keep the original order faceting variables
#'   2. "reverse" flips the order of faceting variables
#'   Note: this does not change the ordering of the values within each facet on
#'   the y-axis, it just reorders the different facets.
#' @param wrap_facet_labels Determine number of characters per line in the
#'   facet labels. Uses \link[scales]{label_wrap} to wrap the text across
#'   multiple lines. If left blank, defaults to 50.
#' @param wrap_y_labels Determine number of characters per line in the
#'   y-axis labels. Uses \link[scales]{label_wrap} to wrap the text across
#'   multiple lines. If left blank, defaults to 20.
#' @param x_intercept The value for which the x_intercept should be set. Default
#'   is 0 but for exponentiated models should be set to 1. If set to `NULL` then
#'   no x_intercept is shown.
#' @param point_size The size of the dots in the plot. Default is 3.5.
#' @param line_width The thickness of the lines. Default is
#' @param position A character string determining how the plot handles a grouped
#'   graph. By default it is `NULL` which assumes there is no grouping variable.
#'   If set to "dodge", it will create a dodged plot based on the variable
#'   supplied in color.
#' @param dodge_width This adjusts the width in the dodge plot. Default is 0.6.
#'   For more info check out \code{\link[ggplot2]{position_dodge}}.
#' @param dodge_reverse Logical. If set to TRUE, reverses the order of the dots
#'   and confidence intervals. Default is FALSE. For more info check out
#'   \code{\link[ggplot2]{position_dodge}}.
#' @param ... Additional arguments passed on to `theme_coef()`
#'
#' @export
#'
#'


adl_coef_plots <- function(
    data,
    x = estimate,
    y = value_label,
    color = NULL,
    facet = var_label,
    facet_order = "original",
    wrap_facet_labels = 50,
    wrap_y_labels = 20,
    x_intercept = 0,
    point_size = 3.5,
    line_width = 1,
    position = NULL,
    dodge_width = 0.6,
    dodge_reverse = FALSE,
    ...
) {

  plot <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = {{ x }}, y = {{ y }}, color = {{ color }}))

  if (is.null(position)) {

    plot <- plot +
      ggplot2::geom_point(
        size = point_size
      ) +
      ggplot2::geom_linerange(
        ggplot2::aes(xmin = conf.low, xmax = conf.high),
        linewidth = line_width
      ) +
      theme_coef(...)

  } else if (position == "dodge") {

    plot <- plot +
      ggplot2::geom_point(
        position = position_dodge2(width = dodge_width, reverse = dodge_reverse),
        size = point_size
      ) +
      ggplot2::geom_linerange(
        ggplot2::aes(xmin = conf.low, xmax = conf.high),
        position = position_dodge2(width = dodge_width, reverse = dodge_reverse),
        linewidth = line_width
      ) +
      theme_coef(...)
  }

  if (!is.null(rlang::enexpr(facet))) {
    # If facet is not NULL make a faceted plot.
    # Use rlang::ensym to capture the expression the user supplied in "facet"
    # as either a string or a symbol. Without this, is.null(facet) will return
    # an error since facet defaults to the symbol var_label.

    # enable "facet" to be either a string or symbol
    facet <- accept_string_or_sym({{ facet }})

    if (facet_order == "original") {
      # if facet_order is set to "original", the default don't reverse the facet

      plot <- plot +
        facet_col(
          ggplot2::vars(.data[[facet]]),
          scales = "free_y",
          space = "free",
          labeller = ggplot2::label_wrap_gen(wrap_facet_labels)
        )

    } else if (facet_order == "reverse") {
      # if facet_order is "reverse", reverse the order of the facets

      plot <- plot +
        facet_col(
          ggplot2::vars(forcats::fct_rev(.data[[facet]])),
          scales = "free_y",
          space = "free",
          labeller = ggplot2::label_wrap_gen(wrap_facet_labels)
        )

    } else {

      cli::cli_abort('`facet_order` must be either "original" or "reverse"')

    }

  }

  if (is.numeric(x_intercept)) {
    # if x_intercept is not NULL set the xintercept to whatever is specified

    plot <- plot +
      ggplot2::geom_vline(xintercept = x_intercept)

  } else if (is.null(x_intercept)) {
    # if x_intercept is NULL don't add it

    plot <- plot

  } else {
    # if x_intercept is not numeric

    cli::cli_abort(
      c(
        '`x_intercept` must be either `NULL` or a {.cls numeric} value',
        x = "You've supplied`x_intercept` as a {.cls {class(x_intercept)}} value"
      )
    )

  }

  #
  plot <- plot + ggplot2::scale_y_discrete(labels = label_wrap(wrap_y_labels))

  return(plot)

}






