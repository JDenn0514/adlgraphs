#' Make dichotomous factors
#'
#' Convert a vector of class `factor` or
#' \href{https://haven.tidyverse.org/reference/labelled.html}{`haven_labelled`}
#' to a "dichotomous factor vector". When I refer to a "dichotomous factor
#' vector", I am referring to a vector of class `factor` with only two levels
#' that are opposites (e.g., "Agree" and "Disagree")
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
#' show up. This is only useful if you care about
#'
#' @param x A labelled vector or `factor`.
#'
#' @param flip_levels Logical. If `FALSE`, the default, the factor levels are
#'   kept the same. If `TRUE`, the factor levels are flipped. Only specify this
#'   if you want to change the order of the factor level.
#'
#' @export
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' library(labelled)
#' library(haven)
#'
#' # create fake data
#' df <- tibble::tribble(
#'   ~x, ~y, ~z,
#'   3, 2, 3,
#'   4, 4, 2,
#'   2, 6, 1,
#'   1, 1, 4,
#'   5, 4, 3,
#'   6, 5, 6
#' ) %>%
#' # add value labels
#' labelled::set_value_labels(
#'   x = c(`Strongly agree` = 1,
#'         `Agree` = 2,
#'         `Somewhat agree` = 3,
#'         `Somewhat disagree` = 4,
#'         `Disagree` = 5,
#'         `Strongly disagree` = 6),
#'   y = c(`Strongly agree` = 1,
#'         `Agree` = 2,
#'         `Somewhat agree` = 3,
#'         `Somewhat disagree` = 4,
#'         `Disagree` = 5,
#'         `Strongly disagree` = 6),
#'   z = c(`Strongly agree` = 1,
#'         `Agree` = 2,
#'         `Somewhat agree` = 3,
#'         `Somewhat disagree` = 4,
#'         `Disagree` = 5,
#'         `Strongly disagree` = 6)
#' ) %>%
#' # add variable labels
#' labelled::set_variable_labels(
#'   x = "This is the variable label for x",
#'   y = "This is the variable label for y",
#'   z = "This is the variable label for z"
#' )
#'
#' # show the data transformation with a haven_labelled vector
#' dicho_df <- df %>% dplyr::mutate(dicho_x = make_dicho(x))
#' # check the updated dataset
#' dicho_df
#'
#' # Check the attributes
#' attributes(dicho_df$dicho_x)
#' # another way of checking the attributes
#' str(dicho_df$dicho_x)
#'
#' # check the factor levels
#' unique(dicho_df$dicho_x)
#'
#' # ----------------------------------------------------------------------------
#'
#' # function also works with factors
#' dicho_df <- df %>%
#'   dplyr::mutate(
#'     # convert variable to a factor
#'     factor_x = make_factor(x),
#'     # convert the factor to a dichotomous factor
#'     dicho_x = make_dicho(factor_x)
#'   )
#'
#' # check the updated dataset
#' dicho_df
#'
#' # Check the attributes
#' attributes(dicho_df$dicho_x)
#' # another way of checking the attributes
#' str(dicho_df$dicho_x)
#'
#' # check the factor levels
#' unique(dicho_df$dicho_x)
#'
#' # ----------------------------------------------------------------------------
#' # function also works inside dplyr::across()
#'
#' # Create new columns using `across()`
#' dicho_df <- df %>%
#'   dplyr::mutate(
#'     # use this example if you don't want to flip the factor levels
#'     dplyr::across(
#'       x:z,
#'       make_dicho,
#'       .names = "dicho_{col}"
#'     ),
#'     # if you want to flip the factor levels, follow this example
#'     dplyr::across(
#'       x:z,
#'       ~make_dicho(., flip_levels = TRUE),
#'       .names = "dicho_flipped_{col}"
#'     )
#'   )
#' # show that the function worked properly by creating two new sets of variables
#' dicho_df
#'
#' # show the underlying structure of the entire df
#' str(dicho_df)
#'
#' # show how the levels are flipped when "flip_levels = TRUE"
#' levels(dicho_df$dicho_x)
#' levels(dicho_df$dicho_flipped_x)
#'
#'

make_dicho <- function(x, flip_levels = FALSE) {

  # get the object's name
  x_lab <- deparse(substitute(x))

  # get the variable lable
  variable_label <- attr_var_label(x)

  if (is.numeric(x) && !is.null(attr_val_labels(x))) {

    # if x is class haven_labelled convert to a factor using haven::as_factor
    x <- make_factor(x)

  } else if (is.character(x) || is.factor(x)) {

    # if x is of class character or factor return x
    x

  } else {
    # if x is any other class return this error
    cli::cli_abort(
      c(
        "`{x_lab}` must be a vector of class {.cls factor}, {.cls character}, {.cls haven_labelled}, or {.cls numeric} with value labels",
        x = "You've supplied a {.cls {class(x)}} vector without value labels."
      )
    )
  }

  # get the original levels of the variable (this prevents the order from flipping)
  lvl_x <- levels(x)

  # get the binary levels
  lvl_x_f2 <- dplyr::if_else(
    stringr::str_detect(lvl_x, "\\s"),
    stringr::str_replace(lvl_x, "\\w+\\s", ""),
    lvl_x
  ) %>%
    stringr::str_to_title() %>%
    unique()

  # remove the first word if there are multiple words (using base to )
  x <- dplyr::if_else(
    stringr::str_detect(x, "\\s"),
    stringr::str_replace(x, "\\w+\\s", ""),
    x
  ) |>
    stringr::str_to_title()

  if (flip_levels == TRUE) {

    # Get the second level of the new vector
    lab <- unique(lvl_x_f2)[2]

    # change the factor levels
    forcats::fct_relevel(x, lab) %>%
      # add new attributes
      structure(
        # indicate that the original variable was converted to a dichotomous factor
        transformation = glue::glue("Converting '{x_lab}' to a dichotomous factor and reordering the factor levels so that '{lab}' is the reference level"),
        # add the original variable label
        label = variable_label
      )

  } else {

    # Get the first levels of the new vector
    lab <- unique(lvl_x_f2)[1]

    # add new attributes
    forcats::fct_relevel(x, lab) %>%
      structure(
        # indicate that the original variable was converted to a dichotomous factor
        transformation = glue::glue("Converting '{x_lab}' to a dichotomous factor with '{lab}' as the reference level"),
        # add the original variable label
        label = variable_label
      )

  }
}



