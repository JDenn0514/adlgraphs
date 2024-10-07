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

  x <- quantcut(x, 4)
  
  levs <- levels(x)

  x <- case_match_fct(
    x,
    levs[1] ~ "Lowest 25%",
    levs[2] ~ "Second Lowest 25%",
    levs[3] ~ "Second Highest 25%",
    levs[4] ~ "Highest 25%"
  ) %>%
    structure(
      transformation = glue::glue("Converted {x_lab} into a factor variable with four levels based on the quartiles: {stringr::str_flatten(levs, ', ')}")
    )
  # set value labels so we know what values are in each quartile
  attr(x, "labels") <- levs
  return(x)
}






