#' Make dichotomous factors
#'
#' Convert a vector of class `factor` or `numeric` with value labels (e.g.,
#' \href{https://haven.tidyverse.org/reference/labelled.html}{`haven_labelled`})
#' to a dichotomous factor vector. A dichotomous factor vector is a vector of
#' class `factor` with only two bipolar levels (e.g., "Agree" and "Disagree")
#'
#' `make_dicho` was designed to work on any vector that is of class `factor`,
#' \href{https://haven.tidyverse.org/reference/labelled.html}{`haven_labelled`},
#' or `numeric` with value labels. If the vector is  numeric with no value
#' labels, the function will return an error. This is because the function first
#' converts the vector to a factor using `make_factor()`. Then, it removes the
#' first word if there are multiple words in the factor level.
#'
#' The resulting factor levels default to alphabetical but if you want to
#' reverse them, just set `flip_levels = TRUE` in the function.
#'
#' In addition, this function adds two new attributes. The first attribute,
#' `transformation`, indicates the data transformation that the original vector
#' underwent to create this new vector. The second attribute, `label`, contains
#' the variable label that was found in the original variable. However, if the
#' original vector did not have a variable label, then this attribute will not
#' show up.
#'
#' @param x A labelled vector or `factor`.
#'
#' @param flip_levels Logical. If `FALSE`, the default, the factor levels are
#'   kept the same. If `TRUE`, the factor levels are flipped. Only specify this
#'   if you want to change the order of the factor level.
#'
#' @returns A factor vector with two levels of same length as `x`.
#'
#' @examples
#' # load the libraries
#' library(dplyr)
#' library(adlgraphs)
#'
#' # Check a labelled variable -------------------------------------------------
#'
#' # convert "top" into a dichotomous factor
#' new_df <- test_data %>%
#'   mutate(top_f2 = make_dicho(top)) %>%
#'   # keep only two relevant variables
#'   select(top, top_f2)
#'
#' # compare "top" to "top_f2"
#' # we can see all response with "agree" in the name are now "agree" and
#' # all those with "disagree" are now "disagree"
#' new_df
#'
#' # show the attributes
#' attributes(new_df$top_f2)
#'
#' # Check a factor variable ---------------------------------------------------
#' new_df <- test_data %>%
#'   mutate(
#'     # convert it to a factor
#'     top_f = make_factor(top),
#'     # convert the dichotomous factor
#'     top_f2 = make_dicho(top_f)
#'   ) %>%
#'   # select the three variables to compare them
#'   select(top, top_f, top_f2)
#'
#' # compare the three variables
#' new_df
#'
#' # show the attributes for the new variable
#' attributes(new_df$top_f2)
#'
#' # Show it with flipped levels -----------------------------------------------
#'
#' # let's do the same thing but let's flip the levels now
#' new_df <- test_data %>%
#'   mutate(
#'     # show it without flipping the levels
#'     top_f2 = make_dicho(top),
#'     # show it with the levels being flipped
#'     top_f2_flip = make_dicho(top, flip_levels = TRUE)
#'   ) %>%
#'   # keep only relevant variables
#'   select(top, top_f2, top_f2_flip)
#'
#' # compare them
#' new_df
#'
#' # They look the same but if we check the levels of the factor we can see
#' # that they are in different orders
#' attributes(new_df$top_f2)
#' attributes(new_df$top_f2_flip)
#'
#' # ----------------------------------------------------------------------------
#' # function also works inside dplyr::across()
#'
#' # Create new columns using `across()`
#' new_df <- test_data %>%
#'   dplyr::mutate(
#'     # use this example if you don't want to flip the factor levels
#'     dplyr::across(
#'       c(top:deserving),
#'       make_dicho,
#'       .names = "{col}_f2"
#'     ),
#'     # if you want to flip the factor levels, follow this example
#'     dplyr::across(
#'       c(top:deserving),
#'       ~make_dicho(., flip_levels = TRUE),
#'       .names = "{col}_f2_flip"
#'     )
#'   ) %>%
#'   # select the variables with "f2" in the name
#'   select(contains("f2"))
#'
#' # show that the function worked properly by creating two new sets of variables
#' new_df
#'
#' # show the underlying structure of the entire df
#' str(new_df)
#'
#' # show how the levels are flipped when "flip_levels = TRUE"
#' levels(new_df$top_f2)
#' levels(new_df$top_f2_flip)
#'
#' @export
make_dicho <- function(x, flip_levels = FALSE) {

  # get the object's name
  x_name <- deparse(substitute(x))

  # get the variable lable
  variable_label <- attr_var_label(x)

  if (is.numeric(x) && !is.null(attr_val_labels(x))) {

    # if x is class haven_labelled convert to a factor using haven::as_factor
    x <- make_factor(x)

  } else if (is.character(x)) {

    # if x is of class character, make it a factor
    # message("`x` is a character vector which may cause ")
    x <- make_factor(x)

  } else if (is.factor(x)) {
    # if its a factor just return x
    x
  } else {
    # if x is any other class return this error
    cli::cli_abort(
      c(
        "`{x_name}` must be a vector of class {.cls factor}, {.cls character}, {.cls haven_labelled}, or {.cls numeric} with value labels",
        x = "You've supplied a {.cls {class(x)}} vector without value labels."
      )
    )
  }

  # get the original levels of the variable (this prevents the order from flipping)
  lvl_x <- levels(x)

  lvl_x_f2 <- ifelse(
    grepl("\\s", lvl_x),
    sub( "\\w+\\s", "", lvl_x),
    lvl_x
  ) 
  
  lvl_x_f2 <- unique(capitalize(lvl_x_f2))

  # remove the first word if there are multiple words (using base to )
  x <- ifelse(
    grepl("\\s", x),
    sub( "\\w+\\s", "", x),
    x
  ) 
  x <- capitalize(x)

  if (flip_levels == TRUE) {

    # Get the second level of the new vector
    lab <- unique(lvl_x_f2)[2]

    # change the factor levels
    factor(x, levels = rev(lvl_x_f2)) %>% 
      # add new attributes
      structure(
        # set the transformation attribute
        transformation = paste0("Converting '", x_name, "' to a dichotomous factor and reordering the factor levels so that '", lab, "' is the reference level"),
        # add the original variable label
        label = variable_label
      )

  } else {

    # Get the first levels of the new vector
    lab <- unique(lvl_x_f2)[1]

    # add new attributes
    factor(x, levels = lvl_x_f2) %>% 
      structure(
        # indicate that the original variable was converted to a dichotomous factor
        transformation = paste0("Converting '", x_name, "' to a dichotomous factor with '", lab, "' as the reference level"),
        # add the original variable label
        label = variable_label
      )

  }
}

capitalize <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

