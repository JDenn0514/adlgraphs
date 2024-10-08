% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_df_oxy.R
\name{make_df_oxy}
\alias{make_df_oxy}
\title{Automate dataset documentation}
\format{
A data frame with {nrow(df)} rows and {ncol(df)} variables:
\\describe{
}
}
\usage{
make_df_oxy(df, title, description, print = FALSE)
}
\arguments{
\item{df}{Name of data.frame or tibble object}

\item{title}{The text you want in the \verb{@title} part of the documentation}

\item{description}{The text you want in the \verb{@description} part of the
dataset documentation}

\item{print}{Logical. Should the output print to console. Default is FALSE
which writes a new R script beginning with the value of \code{df}}
}
\description{
{dataset_description},
}
\details{
Creates a new R script that comes pre-filled with roxygen2
comments. The new file is named "df_documentation" and is located in the
"R/" folder in your package.

This function is based off of \link[sinew]{makeOxygen} but is far more
limited. Unlike \link[sinew]{makeOxygen} this only works on objects that are
data.frames or tibbles. This is because the purpose of this function is to
automate the process of documenting the variables in a data frame by
leveraging the underlying variable labels.

Another important difference between this function and \link[sinew]{makeOxygen}
is that this function allows you to either print the results to the home
console or as a new script.

It should be noted that if a variable does not have a variable label, then
it will not show up in the new R script.
}
\examples{
# Add variable labels to iris dataset
library(labelled)
library(dplyr)

iris_labelled <- iris \%>\%
  labelled::set_variable_labels(
    Sepal.Length = "Length of the flower sepal, measured in millimeters",
    Sepal.Width =  "Width of the flower sepal, measured in millimeters",
    Petal.Length = "Length of the flower petal, measured in millimeters",
    Petal.Width =  "Width of the flower petal, measured in millimeters",
    Species = "The species of flower"
  )

# if you want to print to the console instead of creating a new script
# just add print = TRUE to the function
make_df_oxy(iris_labelled, print = TRUE)
}
