#' Convert a numeric vector to a percent
#'
#' @param x A numeric vector you want to convert to percentages
#' @param decimals How many decimals should the value be rounded to. Default is
#'   2 which means it will show two decimal places, or the hundredth decimal.
#' @param scale A scaling factor: `x` will be multiplied by `scale` before
#'   formatting. This is useful if the underlying data is very small or very
#'   large. Default is 100.
#' 
#' @returns A character vector of the same length as `x`.
#'
#' @examples
#' # here's the default scale of 100
#' x <- c(0.0163, 0.95, 0.0008, 0.002)
#' make_percent(x)
#'
#' # if the values have already been multiplied by 100 and you don't need to
#' # transform them, then make the scale 1
#' x <- c(1.63, 95, 0.08, 0.2)
#' make_percent(x, scale = 1)
#'
#' # And if we want to round to the closest whole number set the digits to 0
#' x <- c(0.0163, 0.95, 0.0008, 0.002)
#' make_percent(x, decimals = 0)
#'
#'
#' @export
make_percent <- function(x, decimals = 2, scale = 100) {

  x_name <- deparse(substitute(x))

  if (!is.numeric(x)) {
    cli::cli_abort(
      c(
        "`{x_name}` must be a vector of class {.cls numeric}",
        x = "You've supplied a {.cls {class(x)}} vector"

      )
    )
  }

  new_x <- x * scale
  new_x <- round(new_x, decimals)
  new_x <- paste0(new_x, "%")


  if (!is.null(attributes(x))) {
    attributes(new_x) <- utils::modifyList(as.list(attributes(new_x)), attributes(x))
  }
  
  attr(new_x, "transformation") <- paste0("Added a `%` symbol to `", x_name, "`")

  return(new_x)
}




