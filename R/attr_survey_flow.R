#' Get the survey_flow attribute
#'
#' This function makes it easy to get the survey_flow from either an
#' individual vector or a data frame. NOTE: it is not possible to set or modify
#' the question preface attribute with this function.
#'
#'
#' @param x A vector object, the name of a column in a `data.frame`, or an
#'   an actual `data.frame` object.
#' @param data A `data.frame` or `tibble` object. This should only be specified
#'   when `x` is only the name of a column in a `data.frame`.
#'
#' @returns If `x` is a variable or vector, a string containing the "survey_flow" 
#'   attribute, if one is present, is returned. If `x` is a `data.frame` then a
#'   named vector with the "survey_flow" attribute from each variable is returned.
#' 
#' @export
attr_survey_flow <- function(x, data) {
  UseMethod("attr_survey_flow")
}

#' @export
attr_survey_flow.default <- function(x, data) {
  if (missing(data)) {
    attr(x, "survey_flow", exact = TRUE)
  } else {
    attr(data[[x]], "survey_flow", exact = TRUE)
  }
}


# Create a vector containing character strings comprised of all the variable
# labels for each column in a data.frame or tibble.
# write a function that will get the variable label for each column in the data
#' @export
attr_survey_flow.data.frame <- function(x, data = NULL) {
  # get a list of columns
  cols <- names(x)

  # write up a function that makes the string in the format we want
  string_fun <- function(var) {
    string <- attr(x[[var]], "survey_flow", exact = TRUE)
  }

  # iterate string_fun over each of the columns laid out earlier
  lapply(cols, string_fun) %>%
    # set the names of the objects in the list
    stats::setNames(cols)

}

