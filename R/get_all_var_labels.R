#' Get all variable labels
#'
#' Create a vector containing character strings comprised of all the variable
#' labels for each column in a data.frame or tibble
#'
#' @param df a data.frame or tibble object
#'
#' @export

# write a function that will get the variable label for each column in the df
get_all_var_labels <- function(df) {
  # get a list of columns
  cols <- names(df)

  # write up a function that makes the string in the format we want
  string_fun <- function(x) {
    string <- labelled::var_label(df[[x]])
  }

  # map string_fun over each of the columns laid out earlier
  purrr::map(cols, string_fun) %>%
    setNames(cols) %>%
    # undo the list and convert to a vector
    unlist()
}



