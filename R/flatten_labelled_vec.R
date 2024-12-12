#' Flatten a labelled vector
#' 
#' This function "flattens" a labelled vector into a string, where each
#' label is equal to the value. For example if a vector has values 1 and 0
#' where 1 is "Yes" and 0 is "No", it will output a string that reads:
#' `1 = "Yes", 0 = "No"`. This can be done on an individual vector
#' or on an entire data frame. If the vector is not labelled then it will
#' simply return `NULL`.
#' 
#' @param x A vector object, the name of a column in a `data.frame`, or an
#'   an actual `data.frame` object. 
#' @param data A `data.frame` or `tibble` object. This should only be specified
#'   when `x` is only the name of a column in a `data.frame`.
#' 
#' 
#' @returns A string if `x` is a vector or column, a named vector if `x` is a 
#'   data.frame.
#' 
#' @examples
#' library(adlgraphs)
#' # run on a single vector
#' flatten_labelled_vec(test_data$inferior)
#' # run on a data frame
#' flatten_labelled_vec(test_data)
#' 

#' 
#' @export
flatten_labelled_vec <- function(x, data) {
  UseMethod("flatten_labelled_vec")
}

#' @export
flatten_labelled_vec.default <- function(x, data) {

  values <- attr_val_labels(x, data)
  names <- attr_val_labels(x, data) %>% setNames(names(.), .)

  leng <- length(values)

  internal_fun <- function(values, names, n) {
    value <- values[n]
    name <- names[n]
    if (is.null(name)) {
      return(NULL)
    } else {
      return(glue::glue("{value} = '{name}'"))
    }
  }

  string <- purrr::pmap_chr(list(values, names, seq_along(leng)), internal_fun)

  paste(string, collapse = ", ")

}

#' @export
flatten_labelled_vec.data.frame <- function(x, data) {
  
  cols <- names(x)

  string_fun <- function(var) {
    string <- flatten_labelled_vec.default(x[[var]])
  }

  lapply(cols, string_fun) %>% 
    setNames(cols)

}
