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
  x_lab <- deparse(substitute(x))


  if (haven::is.labelled(x) || (is.numeric(x) && !is.null(attr_val_labels(x)))) {
    # if x is haven_labelled or numeric with value labels

    ## reverse the values of x in a new vector "rev_x" ---------
    # get the highest value in x and add 1 to it
    max_x <- max(na.omit(x)) + 1
    # reverse the value of x by subtracting it from name_vec_max
    # four variables with four response options what was a 1 is equal to a 4 
    # and what was a 2 is equal to 3, etc.
    rev_x <- max_x - x


    ## create a new reversed named vector  "rev_name_vec" ---------
    # get the named vector using base functions
    name_vec <- attr_val_labels(x)
    # get the value labels from the named vector and reverse the order of the vector
    labels <- stats::setNames(names(name_vec), name_vec) %>% rev()
    # create a new named vector with the flipped names so that when we reverse
    # the order of the values, the values will have the appropriate labels
    rev_name_vec <- stats::setNames(name_vec, labels)


    ## Create the new labelled vector using haven::labelled() -------------
    # create the labelled vector by adding the reversed labels (rev_name_vec)
    # to the reversed vector ()


    if (!is.null(attr_var_label(x))) {
      # if x has a variable label

      # add variable and value labels
      haven::labelled(
        x = rev_x,
        labels = rev_name_vec,
        label = attr_var_label(x)
      ) %>%
        # add the transformation annotation
        structure(transformation = glue::glue("Reversing '{x_lab}' while maintaining correct value labels"))

    } else {
      # if x does not have a variable label

      # add value labels
      haven::labelled(
        x = rev_x,
        labels = rev_name_vec
      ) %>%
        # add the transformation annotation
        structure(transformation = glue::glue("Reversing '{x_lab}' while maintaining correct value labels"))
    }

  } else if (is.numeric(x) && is.null(attr(x, "labels"))) {
    # if x is numeric and does not have value labels

    # find the max value in x and add 1
    max_x <- max(na.omit(x)) + 1
    # reverse the value of x by subtracting it from name_vec_max
    # now what was a 1 is equal to a 4 and what was a 2 is equal to 3, etc.
    rev_x <- max_x - x

    if (!is.null(attr_var_label(x))) {
      # if x has a variable label

      # add variable label
      haven::labelled(
        x = rev_x,
        label = attr_var_label(x)
      ) %>%
        # add the transformation annotation
        structure(transformation = glue::glue("Reversing '{x_lab}'"))

    } else {
      # if x does not have a variable label

      rev_x %>%
        # add the transformation annotation
        structure(transformation = glue::glue("Reversing '{x_lab}'"))

    }

  }

}









