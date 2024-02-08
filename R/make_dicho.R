#' Make dichotomous factors
#'
#' Create a vector of a dichotomous factor. Currently works for variables with
#' "Mostly true" through "Mostly false" and "Strongly Agree" through "Strongly
#' disagree". Use inside of dplyr::mutate() or dplyr::across() to create a new
#' column of the vectors
#'
#' @param df The name of the data frame. Usually you'll be piping in the data
#'   so you should put `.`. However, if used inside of dplyr::across() then you
#'   need to use an anonymous function like this `\(var) make_dicho(., var)`.
#' @param var A vector or variable you're transforming.
#'
#'
#'

make_dicho <- function(df = data, var) {
  var <- rlang::enexpr(var)
  if (!is.character(var)) {
    # convert to a sym() object and then use as_name to make it a string
    var <- rlang::as_name(rlang::ensym(var))
  }

  # conver the vector to a factor
  haven::as_factor(df[[var]]) %>%
    # remove the first part of the factor
    str_extract("(?<=\\s).+") %>%
    # make the first letter uppercase
    str_to_sentence()
}




