% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flatten_labelled_vec.R
\name{flatten_labelled_vec}
\alias{flatten_labelled_vec}
\title{Flatten a labelled vector}
\usage{
flatten_labelled_vec(x, data)
}
\arguments{
\item{x}{A vector object, the name of a column in a \code{data.frame}, or an
an actual \code{data.frame} object.}

\item{data}{A \code{data.frame} or \code{tibble} object. This should only be specified
when \code{x} is only the name of a column in a \code{data.frame}.}
}
\value{
A string if \code{x} is a vector or column, a named vector if \code{x} is a
data.frame.
}
\description{
This function "flattens" a labelled vector into a string, where each
label is equal to the value. For example if a vector has values 1 and 0
where 1 is "Yes" and 0 is "No", it will output a string that reads:
\verb{1 = "Yes", 0 = "No"}. This can be done on an individual vector
or on an entire data frame. If the vector is not labelled then it will
simply return \code{NULL}.
}
\examples{
library(adlgraphs)
# run on a single vector
flatten_labelled_vec(test_data$inferior)
# run on a data frame
flatten_labelled_vec(test_data)


}
