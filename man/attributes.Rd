% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attributes.R
\name{attributes}
\alias{attr_var_label}
\alias{attr_val_labels}
\alias{attr_levels}
\alias{attr_transformation}
\alias{attr_note}
\alias{attr_question_preface}
\alias{attr_survey_flow}
\title{Functions to access metadata}
\usage{
attr_var_label(x, df)

attr_val_labels(x, df)

attr_levels(x, df)

attr_transformation(x, df)

attr_note(x, df)

attr_question_preface(x, df)

attr_survey_flow(x, df)
}
\arguments{
\item{x}{A vector object or the name of a column in a data.frame}

\item{df}{A \code{data.frame} or \code{tibble} object. This should be specified when \code{x}
is only the name of a column.}
}
\description{
These are a set of functions that make it easy to access attributes/metadata
from a vector or column in a \code{data.frame}. Note: these functions do not allow
you to change, set, or remove attributes.
}
\details{
\describe{

\item{\code{attr_var_label()}}{This function gets the the variable label.}

\item{\code{attr_val_labels()}}{This function gets the value labels.}

\item{\code{attr_levels()}}{This function gets the factor levels.}

\item{\code{attr_transformation()}}{This function gets the "transformation"
attribute which tells you what data transformation the variable underwent
when it was created.}

\item{\code{attr_question_preface()}}{This function gets the "question_preface"
attribute. Some questions in surveys enable respondents to select multiple
responses. Each response gets it's own variable in the data, which is labeled
"label. The text stored in this attribute contains the text that respondents
saw before the response options.}

\item{\code{attr_survey_flow()}}{This function gets teh the "survey_flow"
attribute. This is used to indicate if there was an experiment or some sort
of branching involved in the survey flow}

\item{\code{attr_note()}}{This function gets the  "note" attribute which
contains miscellaneous information about the variable.}

}
}
\examples{
library(adlgraphs)

# get the variable labels
attr_var_label(x = test_data$edu)
attr_var_label(x = "edu", df =  test_data)

# get the value labels
attr_val_labels(x = test_data$edu)
attr_val_labels(x = "edu", df = test_data)

# get the levels for edu_f
attr_levels(x = test_data$edu_f)
attr_levels(x = "edu_f", df = test_data)

# get the transformation
attr_transformation(x = test_data$edu_f)
attr_transformation(x = "edu_f", df = test_data)


}
