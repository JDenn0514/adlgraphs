#' The default theme for hc charts
#'
#' This function creates the default theme that all ADL highcharter themes
#' are built off of. It's ggplot2 equivalent is `theme_default`.
#'
#' Due to certain limitations of Highcharter, this is not quite as extensive as
#' `theme_default`. For example, there are no minor grid lines right now. If I
#' am able to figure it out then I might add them in. In addition, the faceting
#' arguments from `theme_default` are not present here
#'
#' @param hc A highcharter object
#' @param base_size Base font size, given in px Also controls the spacing in
#'   the graph.
#' @param legend_position The position of the legend. Options are: "right",
#'   "top", "bottom", or "none". "none" removes the legend. "right" is the
#'   default.
#' @param axis_text Logical. Determines if BOTH axes should have labels. If
#'   `TRUE`, the default, both axes are labelled. If `FALSE`, neither axis is
#'   labelled. Note, this controls both axes. If you want to remove only one
#'   axis, use the `axis_text_x` or `axis_text_y`.
#' @param axis_text_x Logical. Determines if the x-axis has labels. If `TRUE`,
#'   the default, the x-axis labels are shown. If `FALSE`, the x-axis labels are
#'   removed from the plot.
#' @param axis_text_y Logical. Determines if the y-axis has labels. If `TRUE`,
#'   the default, the y-axis labels are shown. If `FALSE`, the y-axis labels are
#'   removed from the plot.
#' @param grid Logical. Determines if ALL grid lines should appear. If `TRUE`,
#'   the default, all grid lines appear. If `FALSE`, all grid lines disappear,
#'   regardless of what `grid_x_only` or `grid_y_only` are set to.
#' @param grid_x_only Logical. Determines if only x-axis grid lines (vertical
#'   lines) should appear. If `FALSE`, the default, all grid lines appear. If
#'   `TRUE`, only the x-axis grid lines appear and the y-axis grid lines will
#'   disappear.
#' @param grid_y_only Logical. Determines if only y-axis grid lines (horizontal
#'   lines) should appear. If `FALSE`, the default, all grid lines appear. If
#'   `TRUE`, only the y-axis grid lines appear and the  x-axis grid lines will
#'   disappear.
#' @param ... Additional arguments passed


hc_theme_default <- function(
    hc,
    # set the base font size
    base_size = 16,
    # set the legend positions
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
    ...
) {

  if (legend_position == "top") {
    # if legend_position = "top" then put the legend on top of the plot
    legend_values <- list(
      # adjust the vertical alignment of the legend
      verticalAlign = "top",
      align = "center",
      layout = "horizontal"
    )
  }
  else if (legend_position == "bottom") {
    legend_values <- list(
      # adjust the vertical alignment of the legend
      verticalAlign = "bottom",
      align = "center",
      layout = "horizontal"
    )
  }
  else if (legend_position == "right") {
    legend_values <- list(
      # adjust the vertical alignment of the legend
      verticalAlign = "middle",
      align = "right",
      layout = "vertical"
    )
  } else if (legend_position == "none") {
    legend_values <- list(
      enabled = FALSE
    )
  }

  # now do axes text
  if (isFALSE(axis_text)) {
    # remove both axis texts

    axis_text_x <- FALSE
    axis_text_y <- FALSE

  }


  # now do the grid lines
  if (isFALSE(grid)) {
    # if grid is set to FALSE remove the grid lines

    x_gridLineWidth <- 0
    y_gridLineWidth <- 0

  } else if (isTRUE(grid) && isTRUE(grid_x_only)) {
    # if grid is set to TRUE and grid_x_only is true then only keep x-axis grid lines

    x_gridLineWidth <- 2
    y_gridLineWidth <- 0

  } else if (isTRUE(grid) && isTRUE(grid_y_only)) {
    # if grid is set to TRUE and grid_y_only is true then only keep y-axis grid lines

    x_gridLineWidth <- 0
    y_gridLineWidth <- 2

  } else if (isTRUE(grid) && isFALSE(grid_x_only) && isFALSE(grid_y_only)) {

    x_gridLineWidth <- 2
    y_gridLineWidth <- 2

  }


  theme <-
    list(
      colors = c(
        "#14A2FCFF", "#B0B1B3FF", "#E84C4CFF", "#0A1A50FF",
        "#FFE500FF", "#69DA78FF", "#60269EFF", "#FFA828FF"
      ),
      chart = list(
        backgroundColor = "white",
        style = list(
          fontFamily = "Roboto",
          color = "#000000",
          fontSize = base_size
        )
      ),
      title = list(
        align = "center",
        style = list(
          fontWeight = "bold",
          fontSize = (base_size * 1.2)
        )
      ),
      subtitle = list(
        align = "left"
      ),
      yAxis = list(
        gridLineWidth = y_gridLineWidth,
        gridLineColor = "#BFBFBF",
        labels = list(
          style = list(
            fontSize = (base_size * 0.8)
          )
        ),
        lineColor = "#BFBFBF",
        # just remove minor ticks and gride lines
        minorTicks = FALSE,
        tickColor = "#BFBFBF",
        tickWidth = 0,
        visible = axis_text_y
      ),
      xAxis = list(
        gridLineWidth = x_gridLineWidth,
        gridLineColor = "#BFBFBF",
        labels = list(
          style = list(
            fontSize = (base_size * 0.8)
          )
        ),
        # just remove minor ticks and gride lines
        minorTicks = FALSE,
        lineColor = "#BFBFBF",
        tickWidth = 0,
        visible = axis_text_x
      ),
      tooltip = list(
        backgroundColor = "#FFFFFF",
        style = list(
          color = "#000000"
        )
      ),
      legend = c(
        list(
          itemStyle = list(
            color = "#3C3C3C",
            fontSize = base_size
          ),
          itemHiddenStyle = list(
            color = "#E5E5E5"
          ),
          itemMarginTop = 10,
          itemMarginBottom = 10
        ),
        legend_values
      ),
      credits = list(
        style = list(
          color = "#666"
        )
      ),
      labels = list(
        style = list(
          color = "#D7D7D8"
        )
      ),
      drilldown = list(
        activeAxisLabelStyle = list(
          color = "#F0F0F3"
        ),
        activeDataLabelStyle = list(
          color = "#F0F0F3"
        )
      ),
      navigation = list(
        buttonOptions = list(
          symbolStroke = "#DDDDDD",
          theme = list(
            fill = "#505053"
          )
        )
      ),
      legendBackgroundColor = "rgba(0, 0, 0, 0.5)",
      background2 = "#505053",
      dataLabelsColor = "#B0B0B3",
      textColor = "#C0C0C0",
      contrastTextColor = "#F0F0F3",
      maskColor = "rgba(255,255,255,0.3)"
    )

  theme <- structure(theme, class = "hc_theme")

  if (length(list(...)) > 0) {
    theme <- highcharter::hc_theme_merge(
      theme,
      highcharter::hc_theme(...)
    )
  }

  # add the theme elements to the hc object
  hc$x$theme <- theme

  # return the hc object
  hc
}
