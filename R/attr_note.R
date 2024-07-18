#' Get the note attribute
#'
#' This function makes it easy to get the note attribute from either an
#' individual vector or a data frame. NOTE: it is not possible to set or modify
#' the note attribute with this function.
#'
#'
#' @param x A vector object, the name of a column in a `data.frame`, or an
#'   an actual `data.frame` object.
#' @param df A `data.frame` or `tibble` object. This should only be specified
#'   when `x` is only the name of a column in a `data.frame`.
#'
#' @export
attr_note <- function(x, df) {
  UseMethod("attr_note")
}

#' @export
attr_note.default <- function(x, df) {
  if (missing(df)) {
    attr(x, "note", exact = TRUE)
  } else {
    attr(df[[x]], "note", exact = TRUE)
  }
}

# write a function that will get the note attribute for each column in the df
#' @export
attr_note.data.frame <- function(x, df = NULL) {
  # get a list of columns
  cols <- names(x)

  # write up a function that makes the string in the format we want
  string_fun <- function(var) {
    string <- attr(x[[var]], "note", exact = TRUE)
  }

  # iterate string_fun over each of the columns laid out earlier
  lapply(cols, string_fun) %>%
    # set the names of the objects in the list
    setNames(cols)

}

