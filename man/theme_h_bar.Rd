% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/themes.R
\name{theme_h_bar}
\alias{theme_h_bar}
\title{Theme for non-stacked horizontal bar plots}
\usage{
theme_h_bar(
  base_size = 12,
  legend_position = "none",
  axis_text_x = FALSE,
  grid = FALSE,
  ...
)
}
\arguments{
\item{base_size}{Base font size, given in pts. Also controls the spacing in
the graph.}

\item{legend_position}{The position of the legend. Options are: "left",
"right", "top", "bottom", or "none". "none" removes the legend. "none"
is the default.}

\item{axis_text_x}{Logical. Determines if the x-axis has labels. If \code{FALSE},
the default, the x-axis labels are removed. If \code{TRUE}, the x-axis labels
are shown.}

\item{grid}{Logical. Determines if ALL grid lines should appear. If \code{FALSE},
the default, all grid lines disappear. If \code{TRUE}, all grid lines appear.}

\item{...}{Other arguments passed onto \code{theme_default()}.}
}
\description{
This function creates a theme for non-stacked horizontal bar plots.
}
