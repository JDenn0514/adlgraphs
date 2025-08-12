#' Create quartiles
#'
#' This is a simple function that takes a numeric vector as the input and returns
#' a factor vector with labels indicating which quartile it is in.
#'
#' @param x A numeric vector.
#' 
#' @returns A factor vector with four levels the same length as `x`.
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
      transformation = glue::glue("Converted {x_lab} into a factor variable with four levels based on the quartiles: {paste0(levs, collapse = ', ')}")
    )

  return(x)
}






