#' Create quartiles
#'
#' This is a simple function that takes a numeric vector as the input and returns
#' a factor vector with labels indicating which quartile it is in.
#'
#' @param x A numeric vector.

make_quarts <- function(x) {
  x <- fabricatr::split_quantile(x, 4)
  x <- case_match_fct(
    x,
    "1" ~ "Lowest 25%",
    "2" ~ "Second Lowest 25%",
    "3" ~ "Second Highest 25%",
    "4" ~ "Highest 25%"
  )
}




