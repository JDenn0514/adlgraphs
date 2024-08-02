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



make_factor <- function(x, ordered = FALSE) {
  # get the object's name
  x_lab <- deparse(substitute(x))

  # get the variable lable
  variable_label <- attr_var_label(x)
  # get the value labels
  value_labels <- attr_val_labels(x)

  if (is.null(value_labels)) {
    # if there aren't any value labels then do the following

    if (is.factor(x)) {
      # if it is already a factor, no need to do anything
      return(x)

    } else if (is.character(x)) {
      # if it is a character vector, convert it to a factor
      x <- as.factor(x)
      return(x)

    } else if (is.numeric(x)) {
      # if it is  there aren't any value_labels then return an error
      stop("The vector provided in `x` does not have value labels")
    }
  }


  # ensure that all values have associated labels
  if (is.numeric(x)) {
    # get the values that have labels
    # get the value labels and their values
    labs <- attr_val_labels(x) %>%
      # convert it to numeric, this removes the labels
      as.numeric() %>%
      # sort it from smallest to biggest
      sort()

    # get the unique values in
    vals <- as.numeric(x) %>%
      unique() %>%
      sort()

    # if vals are not found in labs return a error
    if (!grepl(paste(vals, collapse=".*"), paste(labs, collapse=""))) {
      stop("Each value in `x` must have value labels")
      # cli::cli_abort(c(
      #   "Each value in {.var x} must have value labels",
      #   "i" = "Check the values to ensure that each one has a label associated with it"
      # ))
    }
  } else {

    # get the values that have labels
    # get the value labels and their values
    labs <- attr_val_labels(x) %>%
      # convert it to numeric, this removes the labels
      as.character() %>%
      # sort it from smallest to biggest
      sort()

    # get the unique values in
    vals <- as.character(x) %>%
      unique() %>%
      sort()

    # if vals are not found in labs return an error
    if (!grepl(paste(vals, collapse=".*"), paste(labs, collapse=""))) {
      stop("Each value in `x` must have value labels")
      # cli::cli_abort(c(
      #   "no Each value in {.var x} must have value labels",
      #   "i" = "Check the values to ensure that each one has a label associated with it"
      # ))
    }

  }


  # get the values
  values <- unname(value_labels)
  # return(values)
  names <- names(value_labels)

  # replace the values with the names of the value labels
  x <- replace_with(vctrs::vec_data(x), values, names)
  # make it a factor with the levels equal to the names and determine if its ordered
  x <- factor(x, levels = unique(names), ordered = ordered)

  if (!is.null(variable_label)) {
    structure(x,
              label = variable_label,
              transformation = glue::glue("Converted '{x_lab}' into a factor based on its value labels"))

  } else {
    structure(x,
              transformation = glue::glue("Converted '{x_lab}' into a factor based on its value labels"))

  }
}


replace_with <- function(x, from, to) {
  stopifnot(length(from) == length(to))

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
