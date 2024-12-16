#' Get the column names 
#' 
#' This function is essentially a wrapper around `dplyr::select()` and `colnames()`.
#' `get_col_names(data, cols)` is the the same thing as 
#' `colnames(dplyr::select(data, cols))` just with slightly less typing.
#'
#' @param data a `data.frame` or `tibble`
#' @param cols <[`tidy-select`][dplyr_tidy_select]> One or more unquoted 
#'   expressions separated by commas. Variable names can be used as if they 
#'   were positions in the data frame, so expressions like x:y can be used
#'   to select a range of variables. 
#' 
#' @returns a vector of columns in data
#' 
#' @examples
#' library(adlgraphs)
#' # let's get all of the column names between "top" and "run"
#' get_col_names(test_data, c(top:run))
#' # can also use tidyselect syntax
#' get_col_names(test_data, tidyselect::starts_with("pol_part"))
#' 
#' @export
get_col_names <- function(data, cols) {
  colnames(dplyr::select(data, {{ cols }}))
} 



