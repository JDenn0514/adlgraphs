#' Make dichotomous factors
#'
#' Convert a `factor` or `haven_labelled` vector to a dichotomous factor vector.
#' When I refer to a dichotomous factor vector, I am eferring to a vector of
#' class `factor` with only two levels.
#'
#' `make_dicho` was designed to work on any vector that is of class `factor` or
#' `haven_labelled`. If the vector is  numeric with no value labels, the
#' function will return an error. This is because the function first converts
#' the vector to a factor using [as_factor()] [haven::as_factor()] from the
#' `haven` package. Then, it removes the first word if there are multiple
#' words in the factor level.
#'
#' This function also
#'
#' In addition, this function adds two new attributes. The first attribute,
#' `transformation`, indicates the data transformation that the original vector
#' underwent to create this new vector. The second attribute, `label`, contains
#' the variable label that was found in the original variable. However, if the
#' original vector did not have a variable label, then this attribute will not
#' show up.
#'
#' @param x A vector of type `haven_labelled` or `factor`.
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
#' ----------------------------------------------------------------------------
#' \dontrun{
#' # function also works with factors
#' dicho_df <- df %>%
#'   dplyr::mutate(
#'     # convert variable to a factor
#'     factor_x = haven::as_factor(x),
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
#' }
#' ----------------------------------------------------------------------------
#' # function also works inside dplyr::across()
#' \dontrun{
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
#' }
#'

make_dicho <- function(x, flip_levels = FALSE) {

  # get the object's name
  x_lab <- deparse(substitute(x))

  # get the variable lable
  variable_label <- labelled::var_label(x)

  # convert the vector to a factor
  x <- haven::as_factor(x)

  # remove the first word if there are multiple words
  x <- dplyr::if_else(
    stringr::str_detect(x, "\\s"),
    stringr::str_replace(x, "\\w+\\s", ""),
    x
  ) |>
    stringr::str_to_title()

  if (flip_levels == TRUE) {

    # Get the second level of the new vector
    lab <- unique(x)[2]

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
    lab <- unique(x)[1]

    # add new attributes
    forcats::as_factor(x) %>% structure(
      # indicate that the original variable was converted to a dichotomous factor
      transformation = glue::glue("Converting '{x_lab}' to a dichotomous factor with '{lab}' as the reference level"),
      # add the original variable label
      label = variable_label
    )

  }
}




