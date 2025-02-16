#' Get variable label
#'
#' This function makes it easy to get the variable labels from either an
#' individual vector or a data frame. NOTE: it is not possible to set or modify
#' the variable labels with this function.
#'
#' @param x A vector object, the name of a column in a `data.frame`, or an
#'   an actual `data.frame` object.
#' @param data A `data.frame` or `tibble` object. This should only be specified
#'   when `x` is only the name of a column in a `data.frame`.
#' @param unlist Logical. If `TRUE`, the default, returns a named vector. If
#'   `FALSE`, returns a list. This only works when `x` is a `data.frame`
#' @param if_null String. This determines what to return should there be no 
#'   variable label. There are three options: 
#' 
#'     - `NULL` - This is the default. Will return `NULL`
#' 
#'     - "name" - This returns the variable name
#' 
#'     - "NA" - This returns an `NA` value
#'
#' @returns If `x` is a variable or vector, a string containing the "label" 
#'   attribute, if one is present, is returned. If `x` is a `data.frame` then a
#'   named vector or list with the "label" attribute from each variable is 
#'   returned.
#' 
#' @examples 
#' # load dplyr so we can see how it might work in a typical workflow
#' library(dplyr)
#' # check for an individual vector
#' attr_var_label(test_data$top)
#' # get the variable label for the entire data set
#' attr_var_label(test_data)
#' # same, but as a list
#' attr_var_label(test_data, unlist = FALSE)
#' 
#' # now let's do it on a variable without a label
#' top <- sample(c(1:3), 10, replace = TRUE)
#' # if no label is present and use_name is TRUE, it will use the variable name
#' attr_var_label(top, use_name = TRUE)
#' # if it's set to false it will give NA
#' attr_var_label(top, use_name = FALSE)
#' 

#' 
#' @export
attr_var_label <- function(x, data, unlist, if_null = NULL) {
  UseMethod("attr_var_label")
}

#' @export
attr_var_label.default <- function(x, data, unlist = NULL, if_null = NULL) {
  x_name <- rlang::quo_get_expr(rlang::expr({{ x }}))

  if (missing(data)) {
    x <- attr(x, "label", exact = TRUE)
  } else {
    x <- attr(data[[x]], "label", exact = TRUE)
  }

  if (is.null(x) && !is.null(if_null)) {
    if (if_null == "name") {
      x <- x_name
    } else if (if_null == "NA" && !is.null(if_null)) {
      x <- NA
    }
  }
  x
}


# Create a vector containing character strings comprised of all the variable
# labels for each column in a data.frame or tibble.
# write a function that will get the variable label for each column in the data
#' @export
attr_var_label.data.frame <- function(x, data = NULL, unlist = TRUE, if_null = NULL) {
  # get a list of columns
  cols <- names(x)

  # iterate string_fun over each of the columns laid out earlier
  var_labels <- purrr::map(
    cols, 
    ~ attr_var_label.default({{ .x }}, data = x, unlist = unlist, if_null = if_null)
  ) %>%
    # set the names of the objects in the list
    setNames(cols)

  if (isTRUE(unlist)) {
    # map string_fun over each of the columns laid out earlier
    var_labels %>% unlist()
  } else {
    var_labels
  }

}







