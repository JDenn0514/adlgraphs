#' Convert a labelled vector into a factor
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
#' @param x A vector with value labels. Can be numeric, character, or a factor
#' @param ordered Logical. Determines if the factor be ordered. Defaults to TRUE.
#' @param keep_all_levels Logical. Determines if all original levels should be kept
#'   (TRUE) or if only the ones that appear in the data should appear (FALSE). FALSE
#'   is the default. 
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

make_factor <- function(x, ordered = FALSE, keep_all_levels = FALSE) {
  # get the variable's name as a string
  x_lab <- deparse(substitute(x))

  # Get the variable label (assumes attr_var_label function exists)
  variable_label <- attr_var_label(x)

  # Get the value labels (assumes attr_val_labels function exists)
  value_labels <- attr_val_labels(x)

  # If no value labels, handle accordingly
  if (is.null(value_labels)) {
    if (is.factor(x)) {
      return(x)  # Return the factor if it's already a factor
    } else if (is.character(x)) {
      return(factor(x))  # Convert character to factor
    } else if (is.numeric(x)) {
      stop("The vector provided in `x` does not have value labels")
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

  # Get the names of the value labels
  names <- names(value_labels)
  values <- unname(value_labels)


  # # Decide which levels to keep (either all levels or only levels in the data)
  # if (keep_all_levels) {
  #   # Keep all levels from the value labels
  #   levels <- unique(names)
  # } else {
  #   # Only keep levels that appear in the data
  #   levels <- unique(names[values %in% unique(x)])
  # }

  
  # Replace the values with the names of the value labels
  x <- replace_with(x, values, names)

  x_levels <- unique(names)

  if (isFALSE(keep_all_levels)) {
    x_levels <- x_levels[x_levels %in% unique(x)]
  }


  # Convert to factor with the specified order
  x <- factor(x, levels = x_levels, ordered = ordered)

  # If a variable label exists, preserve it in the factor
  if (!is.null(variable_label)) {
    attr(x, "label") <- variable_label
    attr(x, "transformation") <- paste("Converted '", x_lab, "' into a factor based on its value labels", sep = "")
  } else {
    attr(x, "transformation") <- paste("Converted '", x_lab, "' into a factor based on its value labels", sep = "")
  }

  return(x)
}


replace_with <- function(x, from, to) {
  stopifnot(length(from) == length(to))

  if (is.numeric(x)) {
    x <- as.numeric(x)
  } else if (is.character(x)) {
    x <- as.character(x)
  }

  out <- x
  # First replace regular values
  matches <- match(x, from, incomparables = NA)
  if (anyNA(matches)) {
    out[!is.na(matches)] <- to[matches[!is.na(matches)]]
  } else {
    out <- to[matches]
  }

  # Then tagged missing values
  tagged <- haven::is_tagged_na(x)
  if (!any(tagged)) {
    return(out)
  }

  matches <- match(haven::na_tag(x), haven::na_tag(from), incomparables = NA)

  # Could possibly be faster to use anyNA(matches)
  out[!is.na(matches)] <- to[matches[!is.na(matches)]]
  out
}
