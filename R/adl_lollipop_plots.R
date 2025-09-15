#' Lollipop plots with optional dodging and auto label offsets
#'
#' @description
#' Create vertical or horizontal lollipop plots for frequency-style data
#' (bars replaced with stems and points). Supports grouped/dodged layouts,
#' automatic label offsets based on axis span, and optional mapping of label
#' color to the plotted color or group.
#'
#' @details
#' Lollipop stems are drawn with `ggplot2::geom_linerange()` to
#' ensure proper behavior under `position = "dodge"`. Labels can be offset by a
#' fixed amount or automatically by a proportion of the rendered axis span
#' (`auto_offset_prop`), computed after scales are applied.
#'
#' When both `color` and `group` are `NULL`, stems and points are drawn with
#' `adl_palettes$primary`, and labels default to `#2c2e35` unless
#' `text_color = TRUE`, in which case labels inherit the plotted color (or the
#' primary if unmapped).
#' 
#' For more information on how the arguments point_size and linewidth check out
#' vignette from the ggplot2 website: 
#'
#' @param data A data frame or tibble.
#' @param x Variable that goes in the x-axis. This is required.
#' @param y Variable that goes in the y-axis. This is required.
#' @param col_label <data-masking> Variable used for text labels (e.g., the
#'   frequency value).
#' @param group <data-masking> Optional grouping variable (used for dodging and
#'   color mapping when `color = NULL`).
#' @param color <data-masking> Optional variable to map color of stems/points
#'   (and labels when `text_color = TRUE`).
#' @param direction A character string indicating the direction of the lines.
#'   There are two options:
#'   1. "vertical", the default, the bars are vertical
#'   2. "horizontal" the bars are horizontal
#'   This must be set explicitly as it affects the location of the text, labels,
#'   and lines
#' @param col_text_size Numeric, text size for labels (default `3.25`).
#' @param distance_from_col How far the labels are from the points in the 
#'   plot. There are two options:
#'   1. "auto", this checks the limits of the scales to determine how much 
#'      space to add between the point and the text label (this is the default)
#'   2. Numeric, a number referring to the size based on the scale
#' @param auto_offset_prop Numeric scalar (>= 0). Proportion of axis span used
#'   for the automatic label offset when `distance_from_col = "auto"`. Default
#'   is `0.03` (i.e., 3% of span).
#' @param position Either `NULL` (no dodging) or `"dodge"` for grouped layouts.
#'   `"stack"` is not supported.
#' @param dodge_width Numeric width passed to `position_dodge2()` (default `0.8`).
#' @param dodge_reverse Logical, whether to reverse the dodging order (default 
#'   `TRUE`).
#' @param wrap_facet_labels Integer passed to your faceting helpers if you
#'   facet elsewhere (kept for interface compatibility).
#' @param point_size Numeric, size of the lollipop head (default `3.5`).
#' @param linewidth Numeric, thickness of the lollipop stem (default `0.9`).
#' @param text_color Logical; if `TRUE`, `geom_text()` maps its color aesthetic
#'   to `color` (or `group` if `color` is `NULL`). If both are `NULL`, text
#'   uses "#14A2FCFF". If `FALSE` (default), text is drawn with a
#'   fixed color (`#2c2e35`).
#' @param ... Additional arguments passed to `theme_default()`.
#'
#' @return A `ggplot` object.
#'
#' @section Auto label offset:
#' - For `direction = "horizontal"`, the label offset is computed from the
#'   x-axis span.
#' - For `direction = "vertical"`, the label offset is computed from the
#'   y-axis span.
#' - If the span cannot be determined at build time, the offset falls back to
#'   `0.25`.
#'
#' @examples
#' \dontrun{
#' # Minimal vertical lollipop with auto label offset
#' adl_lollipop_plots(
#'   data = df,
#'   x = category,
#'   y = count,
#'   col_label = count,
#'   direction = "vertical",
#'   distance_from_col = "auto",
#'   auto_offset_prop = 0.03
#' )
#'
#' # Horizontal grouped with dodging; labels inherit color mapping
#' adl_lollipop_plots(
#'   data = df,
#'   x = count,
#'   y = category,
#'   col_label = count,
#'   group = grp,
#'   color = grp,
#'   direction = "horizontal",
#'   position = "dodge",
#'   distance_from_col = "auto",
#'   auto_offset_prop = 0.02,
#'   text_color = TRUE
#' )
#' }
#'
#' @seealso
#' - [ggplot2::geom_linerange()], [ggplot2::geom_point()], [ggplot2::geom_text()]
#' - [ggplot2::position_dodge2()]
#'
#' @export

adl_lollipop_plots <- function(
  data,
  x,
  y,
  col_label,
  group = NULL,
  color = NULL,             # aesthetic mapping
  direction = "vertical",
  col_text_size = 3.25,
  distance_from_col = 0.25, # numeric or "auto"
  auto_offset_prop = 0.03,  # fraction of axis span for "auto"
  position = NULL,          # NULL or "dodge"
  dodge_width = 0.8,
  dodge_reverse = TRUE,
  wrap_facet_labels = 100,
  point_size = 3.5,
  line_size = 0.9,
  text_color = FALSE,       # NEW: map text color to color/group when TRUE
  ...
) {

  # Argument checks
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

  if (!is.null(position) && identical(position, "stack")) {
    cli::cli_abort(c(
      "{.code position = \"stack\"} is not supported in {.fn adl_lollipop_plots}.",
      "i" = "Use {.code position = NULL} or {.code position = \"dodge\"}."
    ))
  }

  if (!is.numeric(auto_offset_prop) || length(auto_offset_prop) != 1 || auto_offset_prop < 0) {
    cli::cli_abort(c(
      "{.var auto_offset_prop} must be a single non-negative number.",
      "i" = "Example: {.code auto_offset_prop = 0.02} for 2% of the axis span."
    ))
  }

  # Determine if color/group are provided
  color_q <- rlang::enquo(color)
  group_q <- rlang::enquo(group)
  color_expr <- rlang::get_expr(color_q)
  group_expr <- rlang::get_expr(group_q)
  color_provided <- !is.null(color_expr)
  group_provided <- !is.null(group_expr)

  # Defaults
  default_primary <- adl_palettes$primary
  default_text_col <- "#2c2e35"

  # Base plot
  plot <- data %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = {{ x }},
        y = {{ y }},
        group = {{ group }}
      )
    )

  # Positioners
  if (is.null(position)) {
    pos_range <- "identity"
    pos_points <- "identity"
    pos_text <- "identity"
  } else if (identical(position, "dodge")) {
    pos_range  <- ggplot2::position_dodge2(width = dodge_width, reverse = dodge_reverse)
    pos_points <- ggplot2::position_dodge2(width = dodge_width, reverse = dodge_reverse)
    pos_text   <- ggplot2::position_dodge2(width = dodge_width, reverse = dodge_reverse)
  } else {
    cli::cli_abort(c(
      "Unsupported {.var position} value.",
      "i" = "Use {.code NULL} or {.code \"dodge\"}."
    ))
  }

  # Build geoms
  if (direction == "horizontal") {
    # Horizontal lollipop: stems along x, categories on y (linerange with orientation = "y")
    if (!color_provided && !group_provided) {
      plot <- plot +
        ggplot2::geom_linerange(
          ggplot2::aes(y = {{ y }}, xmin = 0, xmax = {{ x }}),
          linewidth = line_size,
          color = default_primary,
          position = pos_range,
          orientation = "y"
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = {{ x }}, y = {{ y }}),
          size = point_size,
          shape = 16,
          stroke = 1,
          color = default_primary,
          fill  = default_primary,
          position = pos_points
        )
    } else if (color_provided) {
      plot <- plot +
        ggplot2::geom_linerange(
          ggplot2::aes(y = {{ y }}, xmin = 0, xmax = {{ x }}, color = {{ color }}),
          linewidth = line_size,
          position = pos_range,
          orientation = "y"
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = {{ x }}, y = {{ y }}, color = {{ color }}),
          size = point_size,
          shape = 16,
          stroke = 1,
          position = pos_points
        )
    } else {
      plot <- plot +
        ggplot2::geom_linerange(
          ggplot2::aes(y = {{ y }}, xmin = 0, xmax = {{ x }}, color = {{ group }}),
          linewidth = line_size,
          position = pos_range,
          orientation = "y"
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = {{ x }}, y = {{ y }}, color = {{ group }}),
          size = point_size,
          shape = 16,
          stroke = 1,
          position = pos_points
        )
    }

    # Labels + auto offset
    tmp_offset <- if (is.character(distance_from_col) && distance_from_col == "auto") 0 else distance_from_col

    if (isTRUE(text_color)) {
      # Map label color to color/group if available; otherwise primary
      if (color_provided) {
        plot <- plot +
          ggplot2::geom_text(
            ggplot2::aes(label = {{ col_label }}, x = {{ x }} + tmp_offset, color = {{ color }}),
            family = adlgraphs_global$font$regular$family,
            size = col_text_size,
            hjust = 0,
            position = pos_text
          )
      } else if (group_provided) {
        plot <- plot +
          ggplot2::geom_text(
            ggplot2::aes(label = {{ col_label }}, x = {{ x }} + tmp_offset, color = {{ group }}),
            family = adlgraphs_global$font$regular$family,
            size = col_text_size,
            hjust = 0,
            position = pos_text
          )
      } else {
        plot <- plot +
          ggplot2::geom_text(
            ggplot2::aes(label = {{ col_label }}, x = {{ x }} + tmp_offset),
            family = adlgraphs_global$font$regular$family,
            size = col_text_size,
            color = default_primary,
            hjust = 0,
            position = pos_text
          )
      }
    } else {
      plot <- plot +
        ggplot2::geom_text(
          ggplot2::aes(label = {{ col_label }}, x = {{ x }} + tmp_offset),
          family = adlgraphs_global$font$regular$family,
          size = col_text_size,
          color = default_text_col,
          hjust = 0,
          position = pos_text
        )
    }

    plot <- plot + theme_h_bar(...)

    if (is.character(distance_from_col) && distance_from_col == "auto") {
      built <- ggplot2::ggplot_build(plot)
      rng <- tryCatch({
        pp <- built$layout$panel_params[[1]]
        if (!is.null(pp$x.range)) range(pp$x.range, na.rm = TRUE) else range(pp$x$range, na.rm = TRUE)
      }, error = function(e) NULL)

      auto_off <- if (!is.null(rng) && all(is.finite(rng))) auto_offset_prop * diff(rng) else 0.25

      # Replace the last text layer with auto offset
      plot$layers <- plot$layers[-length(plot$layers)]

      if (isTRUE(text_color)) {
        if (color_provided) {
          plot <- plot +
            ggplot2::geom_text(
              ggplot2::aes(label = {{ col_label }}, x = {{ x }} + auto_off, color = {{ color }}),
              family = adlgraphs_global$font$regular$family,
              size = col_text_size,
              hjust = 0,
              position = pos_text
            )
        } else if (group_provided) {
          plot <- plot +
            ggplot2::geom_text(
              ggplot2::aes(label = {{ col_label }}, x = {{ x }} + auto_off, color = {{ group }}),
              family = adlgraphs_global$font$regular$family,
              size = col_text_size,
              hjust = 0,
              position = pos_text
            )
        } else {
          plot <- plot +
            ggplot2::geom_text(
              ggplot2::aes(label = {{ col_label }}, x = {{ x }} + auto_off),
              family = adlgraphs_global$font$regular$family,
              size = col_text_size,
              color = default_primary,
              hjust = 0,
              position = pos_text
            )
        }
      } else {
        plot <- plot +
          ggplot2::geom_text(
            ggplot2::aes(label = {{ col_label }}, x = {{ x }} + auto_off),
            family = adlgraphs_global$font$regular$family,
            size = col_text_size,
            color = default_text_col,
            hjust = 0,
            position = pos_text
          )
      }
    }

  } else if (direction == "vertical") {
    # Vertical lollipop: stems along y, categories on x
    if (!color_provided && !group_provided) {
      plot <- plot +
        ggplot2::geom_linerange(
          ggplot2::aes(x = {{ x }}, ymin = 0, ymax = {{ y }}),
          linewidth = line_size,
          color = default_primary,
          position = pos_range
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = {{ x }}, y = {{ y }}),
          size = point_size,
          shape = 16,
          stroke = 1,
          color = default_primary,
          fill  = default_primary,
          position = pos_points
        )
    } else if (color_provided) {
      plot <- plot +
        ggplot2::geom_linerange(
          ggplot2::aes(x = {{ x }}, ymin = 0, ymax = {{ y }}, color = {{ color }}),
          linewidth = line_size,
          position = pos_range
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = {{ x }}, y = {{ y }}, color = {{ color }}),
          size = point_size,
          shape = 16,
          stroke = 1,
          position = pos_points
        )
    } else {
      plot <- plot +
        ggplot2::geom_linerange(
          ggplot2::aes(x = {{ x }}, ymin = 0, ymax = {{ y }}, color = {{ group }}),
          linewidth = line_size,
          position = pos_range
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = {{ x }}, y = {{ y }}, color = {{ group }}),
          size = point_size,
          shape = 16,
          stroke = 1,
          position = pos_points
        )
    }

    # Labels + auto offset
    tmp_offset <- if (is.character(distance_from_col) && distance_from_col == "auto") 0 else distance_from_col

    if (isTRUE(text_color)) {
      if (color_provided) {
        plot <- plot +
          ggplot2::geom_text(
            ggplot2::aes(label = {{ col_label }}, y = {{ y }} + tmp_offset, color = {{ color }}),
            family = adlgraphs_global$font$regular$family,
            size = col_text_size,
            vjust = 0,
            position = pos_text
          )
      } else if (group_provided) {
        plot <- plot +
          ggplot2::geom_text(
            ggplot2::aes(label = {{ col_label }}, y = {{ y }} + tmp_offset, color = {{ group }}),
            family = adlgraphs_global$font$regular$family,
            size = col_text_size,
            vjust = 0,
            position = pos_text
          )
      } else {
        plot <- plot +
          ggplot2::geom_text(
            ggplot2::aes(label = {{ col_label }}, y = {{ y }} + tmp_offset),
            family = adlgraphs_global$font$regular$family,
            size = col_text_size,
            color = default_primary,
            vjust = 0,
            position = pos_text
          )
      }
    } else {
      plot <- plot +
        ggplot2::geom_text(
          ggplot2::aes(label = {{ col_label }}, y = {{ y }} + tmp_offset),
          family = adlgraphs_global$font$regular$family,
          size = col_text_size,
          color = default_text_col,
          vjust = 0,
          position = pos_text
        )
    }

    plot <- plot + theme_v_bar(...)

    if (is.character(distance_from_col) && distance_from_col == "auto") {
      built <- ggplot2::ggplot_build(plot)
      rng <- tryCatch({
        pp <- built$layout$panel_params[[1]]
        if (!is.null(pp$y.range)) range(pp$y.range, na.rm = TRUE) else range(pp$y$range, na.rm = TRUE)
      }, error = function(e) NULL)

      auto_off <- if (!is.null(rng) && all(is.finite(rng))) auto_offset_prop * diff(rng) else 0.25

      # Replace the last text layer with auto offset
      plot$layers <- plot$layers[-length(plot$layers)]

      if (isTRUE(text_color)) {
        if (color_provided) {
          plot <- plot +
            ggplot2::geom_text(
              ggplot2::aes(label = {{ col_label }}, y = {{ y }} + auto_off, color = {{ color }}),
              family = adlgraphs_global$font$regular$family,
              size = col_text_size,
              vjust = 0,
              position = pos_text
            )
        } else if (group_provided) {
          plot <- plot +
            ggplot2::geom_text(
              ggplot2::aes(label = {{ col_label }}, y = {{ y }} + auto_off, color = {{ group }}),
              family = adlgraphs_global$font$regular$family,
              size = col_text_size,
              vjust = 0,
              position = pos_text
            )
        } else {
          plot <- plot +
            ggplot2::geom_text(
              ggplot2::aes(label = {{ col_label }}, y = {{ y }} + auto_off),
              family = adlgraphs_global$font$regular$family,
              size = col_text_size,
              color = default_primary,
              vjust = 0,
              position = pos_text
            )
        }
      } else {
        plot <- plot +
          ggplot2::geom_text(
            ggplot2::aes(label = {{ col_label }}, y = {{ y }} + auto_off),
            family = adlgraphs_global$font$regular$family,
            size = col_text_size,
            color = default_text_col,
            vjust = 0,
            position = pos_text
          )
      }
    }

  } else {
    cli::cli_abort(c(
      "Unsupported {.var direction} value.",
      "i" = "Use {.code \"vertical\"} or {.code \"horizontal\"}."
    ))
  }

  return(plot)
}
