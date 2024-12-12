#' Convert a labelled vector into a factor
#' 
#' #' @description
#' `r lifecycle::badge("experimental")`
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
#' #' @section Lifecycle:
#' The `levels` argument in `make_factor()` has not been fully developed yet. 
#' Currently, it only allows you to specify the levels on a character or 
#' factor vector without value labels. If you try using it for anything else
#' it will not work. At some point, I would like to fully implement it though 
#' so that you can specify the levels yourself, kind of like with `factor()`.
#' 
#'
#' @param x A vector with value labels. Can be numeric, character, or a factor
#' @param levels You can supply levels to be used if the vector is a character or 
#'   factor. It is not clear how this will fit in with the rest of the function 
#'   so you might get weird results at first.
#' @param ordered Logical. Determines if the factor be ordered. Defaults to `TRUE.`
#' @param drop_levels Logical. Determines if unused factor levels should be dropped.
#'   Defaults to `TRUE.`
#' @param force Logical. Determines if `x` should be forced to a vector even
#'   if there are no value labels. Defaults to `TRUE.`
#' @param na.rm Logical. Determines if tags should be removed from NAs. Defaults
#'   to `FALSE`.
#'
#' @return A factor vector of same length as `x`.
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
  # get the variable's name as a string
  x_name <- deparse(substitute(x))

  # Get the variable label (assumes attr_var_label function exists)
  variable_label <- attr_var_label(x)

  # Get the value labels (assumes attr_val_labels function exists)
  value_labels <- attr_val_labels(x)

  if (is.null(value_labels)) {
    # if there aren't any value labels:

    if (is.factor(x)) {

      # if it is a character vector
      if (!missing(levels) && !is.null(levels)) {
        x <- factor(x, levels = levels)
        lvls_names <- paste(levels, collapse = ", ")
        attr(x, "transformation") <- paste("Updated '", x_name, "' to have levels: '", lvls_names, "'", sep = "")
        lvls_names <- paste(levels, collapse = ", ")
        return(x)
      } else {
        return(x)
      } 
 
    } else if (is.character(x)) {

      # if it is a character vector
      if (!missing(levels) && !is.null(levels)) {
        x <- factor(x, levels = levels)
        lvls_names <- paste(levels, collapse = ", ")
        attr(x, "transformation") <- paste("Converted '", x_name, "' into a factor with levels: '", lvls_names, "'", sep = "")
        return(x)
      } else {
        # if levels is missing
        return(factor(x))
      }
      
    } else if (is.numeric(x) && isTRUE(force)) {
      # if x is numeric and force = TRUE, force to a factor and return warning
      warning("`x` has no value labels so forcing to a factor with `as.factor()`")
      return(as.factor(x))
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
