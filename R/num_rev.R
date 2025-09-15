#' Reverse a numeric function
#'
#' Reverse a numeric variable while maintaining variable and value labels
#' if available. Also adds an attribute describing the transformation the
#' original variable underwent. Please check the vignette to have a better
#' understanding of exactly what this function does.
#'
#' @param x A vector of class `haven_labelled` or `numeric`
#'
#' @returns A numeric vector of the same length as `x`
#' 
#' @examples
#'
#' library(dplyr)
#'
#' test_data %>%
#'   # reverse the variable accept_isr
#'   mutate(accept_isr_rev = num_rev(accept_isr)) %>%
#'   select(starts_with("accept"))
#'
#' @export
num_rev <- function(x) {

  # get the object's name
  x_name <- deparse(substitute(x))

  if (!is.numeric(x)) {
    cli::cli_abort(c(
      "{.arg x} must be of class `numeric`",
      "i" = "`{x_name}` is of class {class(x)}"
    ))
  }

  ## reverse the values of x in a new vector "rev_x" ---------
  # get the highest value in x and add 1 to it
  max_x <- max(stats::na.omit(x)) + 1
  # reverse the value of x by subtracting it from name_vec_max
  # four variables with four response options what was a 1 is equal to a 4 
  # and what was a 2 is equal to 3, etc.
  rev_x <- max_x - x

  name_vec <- attr_val_labels(x)
  variable_label <- attr_var_label(x)
  
  if (is.null(name_vec)) {
    
    x <- rev_x
    attr(x, "label") <- variable_label
    attr(x, "transformation") <- paste0("Reversing '", x_name, "'")

  } else {

    # check to make sure there are value labels for every value
    # Get sorted labels and unique values
    labs <- sort(as.numeric(name_vec))
    vals <- sort(unique(as.numeric(x)))

    # If the values don't match the labels, throw an error
    if (!all(vals %in% labs)) {
      stop("Each value in `x` must have value labels")
    }
  
    ## create a new reversed named vector  "rev_name_vec" ---------
    # get the value labels from the named vector and reverse the order of the vector
    labels <- stats::setNames(names(name_vec), name_vec) %>% rev()
    # create a new named vector with the flipped names so that when we reverse
    # the order of the values, the values will have the appropriate labels
    rev_name_vec <- stats::setNames(name_vec, labels)

    x <- rev_x
    attr(x, "labels") <- rev_name_vec
    attr(x, "label") <- variable_label
    attr(x, "transformation") <- paste0("Reversing '", x_name, "' while maintaining correct value labels")

  }

  if (is.null(attr_var_label(x))) attr(x, "label") <- x_name

  x
}









