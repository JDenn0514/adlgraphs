#' Create quartiles
#'
#' This is a simple function that takes a numeric vector as the input and returns
#' a factor vector with labels indicating which quartile it is in.
#'
#' @param x A numeric vector.
#' @export

make_quarts <- function(x) {

  # get the variable name
  x_lab <- deparse(substitute(x))

  x <- split_quantile(x, 4)
  x <- case_match_fct(
    x,
    "1" ~ "Lowest 25%",
    "2" ~ "Second Lowest 25%",
    "3" ~ "Second Highest 25%",
    "4" ~ "Highest 25%"
  ) %>%
    structure(
      transformation = glue::glue("Converted {x_lab} into a factor variable with four levels based on the quartiles")
    )
}





