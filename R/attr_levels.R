#' Get variable label
#'
#' This function makes it easy to get the variable labels from either an
#' individual vector or a data frame. NOTE: it is not possible to set or modify
#' the variable labels with this function.
#'
#'
#' @param x A vector object, the name of a column in a `data.frame`, or an
#'   an actual `data.frame` object.
#' @param data A `data.frame` or `tibble` object. This should only be specified
#'   when `x` is only the name of a column in a `data.frame`.
#'
#' @export
attr_levels <- function(x, data) {
  UseMethod("attr_levels")
}

#' @export
attr_levels.default <- function(x, data) {
  if (missing(data)) {
    attr(x, "levels", exact = TRUE)
  } else {
    attr(data[[x]], "levels", exact = TRUE)
  }
}


# Create a vector containing character strings comprised of all the variable
# labels for each column in a data.frame or tibble.
# write a function that will get the variable label for each column in the data
#' @export
attr_levels.data.frame <- function(x, data = NULL) {
  # get a list of columns
  cols <- names(x)

  # write up a function that makes the string in the format we want
  string_fun <- function(var) {
    string <- attr(x[[var]], "levels", exact = TRUE)
  }

  # iterate string_fun over each of the columns laid out earlier
  lapply(cols, string_fun) %>%
    # set the names of the objects in the list
    stats::setNames(cols)


}






