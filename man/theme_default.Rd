% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_default.R, R/themes.R
\name{theme_default}
\alias{theme_default}
\title{Default theme}
\usage{
theme_default(
  base_size = 12,
  base_line_size = base_size/24,
  base_rect_size = base_size/24,
  base_lineheight = 1.1,
  legend_position = "right",
  axis_text = TRUE,
  axis_text_x = TRUE,
  axis_text_y = TRUE,
  grid = TRUE,
  grid_x_only = FALSE,
  grid_y_only = FALSE,
  grid_major = TRUE,
  grid_minor = TRUE,
  grid_major_x = TRUE,
  grid_major_y = TRUE,
  grid_minor_x = TRUE,
  grid_minor_y = TRUE,
  facet_title_bold = FALSE,
  facet_title_size = base_size * 0.8,
  facet_title_margin_top = 0.8 * half_line,
  facet_title_margin_bottom = 0.8 * half_line,
  facet_title_margin_right = 0.8 * half_line,
  facet_title_margin_left = 0.8 * half_line,
  panel_spacing_x = 0,
  panel_spacing_y = 0
)

theme_default(
  base_size = 12,
  base_line_size = base_size/24,
  base_rect_size = base_size/24,
  base_lineheight = 1.1,
  legend_position = "right",
  axis_text = TRUE,
  axis_text_x = TRUE,
  axis_text_y = TRUE,
  grid = TRUE,
  grid_x_only = FALSE,
  grid_y_only = FALSE,
  grid_major = TRUE,
  grid_minor = TRUE,
  grid_major_x = TRUE,
  grid_major_y = TRUE,
  grid_minor_x = TRUE,
  grid_minor_y = TRUE,
  facet_title_bold = FALSE,
  facet_title_size = base_size * 0.8,
  facet_title_margin_top = 0.8 * half_line,
  facet_title_margin_bottom = 0.8 * half_line,
  facet_title_margin_right = 0.8 * half_line,
  facet_title_margin_left = 0.8 * half_line,
  panel_spacing_x = 0,
  panel_spacing_y = 0
)
}
\arguments{
\item{base_size}{Base font size, given in pts. Also controls the spacing in
the graph.}

\item{base_line_size}{Base size for line elements.}

\item{base_rect_size}{Base size for rect elements.}

\item{base_lineheight}{Base line height for all text}

\item{legend_position}{The position of the legend. Options are: "left",
"right", "top", "bottom", or "none". "none" removes the legend. "right"
is the default.}

\item{axis_text}{Logical. Determines if BOTH axes have labels. If \code{TRUE}, the
default, both axes are labelled. If \code{FALSE}, neither axis is labelled.
Note, this controls both axes. If you want to remove only one axis, use the
\code{axis_text_x} or \code{axis_text_y}.}

\item{axis_text_x}{Logical. Determines if the x-axis has labels. If \code{TRUE},
the default, the x-axis labels are shown. If \code{FALSE}, the x-axis labels are
removed from the plot.}

\item{axis_text_y}{Logical. Determines if the y-axis has labels. If \code{TRUE},
the default, the y-axis labels are shown. If \code{FALSE}, the y-axis labels are
removed from the plot.}

\item{grid}{Logical. Determines if ALL grid lines should appear. If \code{TRUE},
the default, all grid lines appear. If \code{FALSE}, all grid lines disappear.}

\item{grid_x_only}{Logical. Determines if only x-axis grid lines (vertical
lines) should appear. If \code{FALSE}, the default, all grid lines appear. If
\code{TRUE}, only the x-axis grid lines appear and the y-axis grid lines will
disappear.}

\item{grid_y_only}{Logical. Determines if only y-axis grid lines (horizontal
lines) should appear. If \code{FALSE}, the default, all grid lines appear. If
\code{TRUE}, only the y-axis grid lines appear and the  x-axis grid lines will
disappear.}

\item{grid_major}{Logical. Determines if the major grid lines should appear.
If \code{TRUE}, the default, the major grid lines will appear. If \code{FALSE}, the
major grid lines will disappear.}

\item{grid_minor}{Logical. Determines if the minor grid lines should appear.
If \code{TRUE}, the default, the minor grid lines will appear. If \code{FALSE}, the
minor grid lines will disappear.}

\item{grid_major_x}{Logical. Determines if the major x-axis grid lines will
appear. If \code{TRUE}, the default, the major x-axis grid lines will appear.
If \code{FALSE}, the major x-axis grid lines will disappear.}

\item{grid_major_y}{Logical. Determines if the major y-axis grid lines will
appear. If \code{TRUE}, the default, the major y-axis grid lines will appear.
If \code{FALSE}, the major y-axis grid lines will disappear.}

\item{grid_minor_x}{Logical. Determines if the minor x-axis grid lines will
appear. If \code{TRUE}, the default, the minor x-axis grid lines will appear.
If \code{FALSE}, the minor x-axis grid lines will disappear.}

\item{grid_minor_y}{Logical. Determines if the minor y-axis grid lines will
appear. If \code{TRUE}, the default, the minor y-axis grid lines will appear.
If \code{FALSE}, the minor y-axis grid lines will disappear.}

\item{facet_title_bold}{Logical. Determines if the facet labels should be
bold or not. Default is \code{FALSE}.}

\item{facet_title_size}{Size of the facet titles, specified in pts. Default
is \code{base_size * 0.8}.}

\item{facet_title_margin_top}{The margin above the facet title, specified in
pts. Default is \code{0.8 * half_line}.}

\item{facet_title_margin_bottom}{The margin beneath the facet title,
specified in pts. Default is \code{0.8 * half_line}.}

\item{facet_title_margin_right}{The margin to the right of the facet title,
specified in pts. Default is \code{0.8 * half_line}.}

\item{facet_title_margin_left}{The margin to the left of the facet title,
specified in pts. Default is \code{0.8 * half_line}.}

\item{panel_spacing_x}{Horizontal spacing between the different panels when
faceting a graph, given in pts. Default is 0.}

\item{panel_spacing_y}{Vertical spacing between the different panels when
faceting a graph, given in pts. Default is 0.}
}
\description{
This function creates the default theme that all ADL theme functions are
built off of. It functions similarly to \code{\link[ggplot2]{theme_gray}},
in that all of the default ggplot2 themes are built off
\code{\link[ggplot2]{theme_gray}}. It should be noted that this works best
when dpi is set to 400 either in a Quarto or Rmarkdown doc or in
\code{\link[ggplot2]{ggsave}}.

This function creates the default theme that all ADL theme functions are
built off of. It functions similarly to \code{\link[ggplot2]{theme_gray}},
in that all of the default ggplot2 themes are built off
\code{\link[ggplot2]{theme_gray}}. It should be noted that this works best
when dpi is set to 400 either in a Quarto or Rmarkdown doc or in
\code{\link[ggplot2]{ggsave}}.
}
