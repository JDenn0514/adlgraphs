% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bogus_data.R
\name{export_bogus}
\alias{export_bogus}
\title{Export data frame with only bots and duplicates}
\usage{
export_bogus(
  data,
  filename,
  export_raw_data = TRUE,
  id = NULL,
  duration,
  cut_off = 0.3
)
}
\arguments{
\item{data}{A data.frame object you want to operate on}

\item{filename}{The name of the file you want to create}

\item{export_raw_data}{Logical. If TRUE, the default, exports the raw data. If FALSE,
exports only three columns: the ID, duplicate, and bot}

\item{id}{The unique ID variable. Only relevant when \code{export_raw_data} is FALSE.}

\item{duration}{The name of the time duration variable. Must be a string.}

\item{cut_off}{Specify what percentage of the median time should be used
to remove speedsters. Default is 0.3, which means people who's time to
complete is 0.3 that of the median completion time are removed.}
}
\description{
This function creates a file containing all responses suspected of being bots,
duplicates, or speedsters. Can create a .xlsx, .sav, or .csv file. It uses
\code{get_bogus()} under the hood.
}
