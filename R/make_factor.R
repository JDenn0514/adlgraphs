#' Convert a labelled vector into a factor
#' 
#' @description
#'
#' `make_factor()` takes a labelled vector and converts it to a factor variable
#' using the value labels. This works with numeric, character, and factor vectors.
#'
#' This function is very similar to [`haven::as_factor()`](https://haven.tidyverse.org/reference/as_factor.html)
#' and
#' [`sjlabelled::as_label()`](https://strengejacke.github.io/sjlabelled/reference/as_label.html)
#' and is heavily based on both. However, it has some key differences. The main
#' difference compared to both functions  is that `make_factor()` adds a
#' "transformation" attribute to the new variable  indicating how it was
#' created. You can see this in the examples.
#'
#' Compared to [`sjlabelled::as_label()`](https://strengejacke.github.io/sjlabelled/reference/as_label.html)
#' it is not as extensive. For example, while [`sjlabelled::as_label()`](https://strengejacke.github.io/sjlabelled/reference/as_label.html)
#' works with data.frames and vectors, `make_factor()` only works with vectors.
#' In addition, [`sjlabelled::as_label()`](https://strengejacke.github.io/sjlabelled/reference/as_label.html)
#' has many different arguments that enable you to control the appearance of the
#' labels, NAs, and other things. `make_factor()` on the other hand is much
#' simpler. Similarly,
#' [`haven::as_factor()`](https://haven.tidyverse.org/reference/as_factor.html)
#' also enables more customization over the output of the labels. Another key
#' difference between this function and those is that if there are values without
#' labels, this function returns an error.
#' 
#'
#' @param x A vector with value labels. Can be numeric, character, or a factor
#' @param ordered Logical. Determines if the factor be ordered. Defaults to `TRUE.`
#' @param drop_levels Logical. Determines if unused factor levels should be dropped.
#'   Defaults to `TRUE.`
#' @param force Logical. Determines if `x` should be forced to a vector even
#'   if there are no value labels. Defaults to `TRUE.`
#' @param na.rm Logical. Determines if tags should be removed from NAs. Defaults
#'   to `FALSE`.
#'
#' @returns A factor vector of same length as `x`.
#' 
#' @examples
#'
#' library(adlgraphs)
#' library(dplyr)
#'
#' # let's make a new variable and data set
#' new_df <- test_data %>%
#'   # convert top into a factor
#'   mutate(top_f = make_factor(top))
#'
#' # compare the "top_f" to "top"
#' new_df %>% select(top, top_f)
#'
#' # check the attributes to see the label and transformation
#' attributes(new_df$top_f)
#'
#' @export
make_factor <- function(x, levels = NULL, ordered = FALSE, drop_levels = TRUE, force = TRUE, na.rm = FALSE) {
  # don't use rlang as that won't work when x is "data[[x]]"
  x_name <- deparse(substitute(x))

  # Get the variable label (assumes attr_var_label function exists)
  variable_label <- attr_var_label(x)

  # Get the value labels (assumes attr_val_labels function exists)
  value_labels <- attr_val_labels(x)

  if (is.null(value_labels)) {
    # if there aren't any value labels:

    if (is.factor(x)) {
      # if it's a factor

      # if not, just return x
      if (is.null(variable_label)) attr(x, "label") <- x_name
      return(x)
 
    } else if (is.character(x)) {
        # if its a character without levels, just convert to a factor

      # convert to a factor
      x <- factor(x)
      # add transformation attribute
      attr(x, "transformation") <- paste0("Updated '", x_name, "' from a character vector to a factor")
      
      # set the label attribute
      if (is.null(variable_label)) {
        # if variabel label is null use the x_name
        attr(x, "label") <- x_name
      } else {
        # otherwise set the label with variable_label
        attr(x, "label") <- variable_label
      }
      
      return(x)
      
    } else if (is.numeric(x) && isTRUE(force)) {
      # if x is numeric and force = TRUE, force to a factor and return warning
      warning("`x` has no value labels so forcing to a factor with `as.factor()`")
      # force to a factor
      x <- as.factor(x)
      
      attr(x, "transformation") <- paste0("Converted '", x_name, "' from a numeric vector to a factor")

      # set the label attribute
      if (is.null(variable_label)) {
        # if variabel label is null use the x_name
        attr(x, "label") <- x_name
      } else {
        # otherwise set the label with variable_label
        attr(x, "label") <- variable_label
      }
      return(x)

    } else if (is.numeric(x) && isFALSE(force)) {
      # if x is numeric and force = false, return an error 
      cli::cli_abort(
        "The vector provided in `x` does not have value labels.",
        "i" = "If you want to force it to a factor, set `force = TRUE`."
      )
    }
  }

  # Ensure all values have associated labels
  if (is.numeric(x)) {
    # Get sorted labels and unique values
    labs <- sort(as.numeric(value_labels))
    vals <- sort(unique(as.numeric(x)))

    # If the values don't match the labels, throw an error
    if (!all(vals %in% labs)) {
      stop("Each value in `x` must have value labels")
    }
  } else {
    # Get sorted labels and unique values
    labs <- sort(as.character(value_labels))
    vals <- sort(unique(as.character(x)))

    # If the values don't match the labels, throw an error
    if (!all(vals %in% labs)) {
      stop("Each value in `x` must have value labels")
    }
  }

  # if na.rm is TRUE remove NAs
  if (na.rm) x[is.na(x)] <- NA

  # Get the names of the value labels
  names <- names(value_labels)
  values <- unname(value_labels)

  
  # Replace the values with the names of the value labels
  x <- replace_with(x, values, names)

  # get the levels that 
  x_levels <- unique(names)

  # if keep_all_levels = FALSE, keep only the levels that appear in the data
  if (drop_levels) x_levels <- x_levels[x_levels %in% unique(x)]

  # Convert to factor with the specified order
  x <- factor(x, levels = x_levels, ordered = ordered)


  
  if (!is.null(variable_label)) {
    # If a variable label exists, preserve it in the factor
    attr(x, "label") <- variable_label
    # add the transformation attribute
    attr(x, "transformation") <- paste("Converted '", x_name, "' into a factor based on its value labels", sep = "")
  } else {
    # add the transformation attribute
    attr(x, "transformation") <- paste("Converted '", x_name, "' into a factor based on its value labels", sep = "")
    attr(x, "label") <- x_name
  }

  return(x)
}




