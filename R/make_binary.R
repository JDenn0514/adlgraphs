#' Make binary variables
#'
#' Convert a vector of class `factor` or
#' \href{https://haven.tidyverse.org/reference/labelled.html}{`haven_labelled`}
#' to a "binary vector". When I refer to a "binary vector", I am referring to a
#' vector of class `numeric` with two values: 0 or 1. Another way of thinking
#' about this is by turning a variable into a dummy variable.
#'
#' `make_binary()` builds off [make_dicho()] and therefore was designed to work
#' on any vector that is of class `factor`,
#' \href{https://haven.tidyverse.org/reference/labelled.html}{`haven_labelled`},
#' or `numeric` with value labels. Because this was built off of [make_dicho],
#' if the vector is  numeric with no value labels, the function will return an
#' error.
#'
#' Similar to how [make_dicho()] provides the opportunity to flip the factor
#' levels, `make_binary()` allows you to flip which values should be recoded as
#' 0 and which should be recoded as 1. To do so, just set `flip_values = TRUE`.
#'
#' In addition, this function adds three new attributes. The first attribute,
#' `transformation`, indicates the data transformation that the original vector
#' underwent to create this new vector. The second attribute, `label`, contains
#' the variable label that was found in the original variable. However, if the
#' original vector did not have a variable label, then this attribute will not
#' show up. The third attribute, `labels`, adds value labels so you can see
#' what the 1 and 0 mean.
#'
#' @param x A vector of class `haven_labelled` or `factor`.
#'
#' @param flip_values Logical. If `FALSE`, the default, the values are kept the
#'   same. If `TRUE`, the values associated with 1 and 0 are flipped. See third
#'   example for more information.
#' 
#' @returns A numeric vector of same length as `x`.
#'
#'
#' @examples
#' library(dplyr)
#' 
#' 
#' df <- tibble::tribble(
#'   ~x, ~y, ~z,
#'   3, 2, 3,
#'   4, 4, 2,
#'   2, 6, 1,
#'   1, 1, 4,
#'   5, 4, 3,
#'   6, 5, 6
#' ) 
#' 
#' labs <- c(
#'    "Strongly agree" = 1,
#'    "Agree" = 2,
#'    "Somewhat agree" = 3,
#'    "Somewhat disagree" = 4,
#'    "Disagree" = 5,
#'    "Strongly disagree" = 6
#'  )
#' 
#' attr(df$x, "labels") <- labs
#' attr(df$z, "labels") <- labs
#' attr(df$z, "labels") <- labs
#' 
#' # show the data transformation with a haven_labelled vector
#' binary_df <- test_data %>% dplyr::mutate(binary_x = make_binary(x))
#' # check the updated dataset
#' binary_df
#'
#' # Check the attributes
#' attributes(binary_df$binary_x)
#' # another way of checking the attributes
#' str(binary_df$binary_x)
#'
#' # check the factor levels
#' unique(binary_df$binary_x)
#'
#' # ----------------------------------------------------------------------------
#'
#' # function also works with factors
#' binary_df <- df %>%
#'   dplyr::mutate(
#'     # convert variable to a factor
#'     factor_x = make_factor(x),
#'     # convert the factor to a binary variable
#'     binary_x = make_binary(factor_x)
#'   )
#'
#' # check the updated dataset
#' binary_df
#'
#' # Check the attributes
#' attributes(binary_df$binary_x)
#' # another way of checking the attributes
#' str(binary_df$binary_x)
#'
#' # check the factor levels
#' unique(binary_df$binary_x)
#'
#' # ----------------------------------------------------------------------------
#'
#' # function also works inside dplyr::across()
#'
#' # Create new columns using `across()`
#' binary_df <- df %>%
#'   dplyr::mutate(
#'     # use this example if you don't want to flip the factor levels
#'     dplyr::across(
#'       x:z,
#'       make_binary,
#'       .names = "binary_{col}"
#'     ),
#'     # if you want to flip the factor levels, follow this example
#'     dplyr::across(
#'       x:z,
#'       ~make_binary(., flip_values = TRUE),
#'       .names = "binary_flipped_{col}"
#'     )
#'   )
#' # show that the function worked properly by creating two new sets of variables
#' binary_df
#'
#' # show the underlying structure of the entire df
#' str(binary_df)
#'
#' # show how the levels are flipped when "flip_levels = TRUE"
#' levels(binary_df$binary_x)
#' levels(binary_df$binary_flipped_x)
#'
#' @export
make_binary <- function(x, flip_values = FALSE) {

  # get the object's name
  x_name <- rlang::enexpr(x)
  # make a new object containing the variable label
  variable_label <- attr_var_label(x)

  # convert the vector to a dichotomous factor
  x <- make_dicho(x)

  # get the first factor level
  first_level <- levels(x)[1]
  # get the second factor level
  second_level <- levels(x)[2]


  if (flip_values == FALSE) {

    # create a named vector using the factor labels
    values <- stats::setNames(c(1, 0), c(first_level, second_level))

    # make the variable a binary vector using case_match
    out <- dplyr::case_match(
      x,
      first_level ~ 1,
      second_level ~ 0
    ) %>%
      structure(
        transformation = paste0("Converting '", x_name, "' to a binary variable with '", first_level, "' = 1 and '", second_level, "' = 0."),
        label = variable_label,
        labels = values
      )


  } else if (flip_values == TRUE) {

    # create a named vector using the factor labels
    values <- stats::setNames(c(1, 0), c(second_level, first_level))

    out <- dplyr::case_match(
      x,
      second_level ~ 1,
      first_level ~ 0
    ) %>%
      structure(
        transformation = paste0("Converting '", x_name, "' to a binary variable with '", second_level, "' = 1 and '", first_level, "' = 0."),
        label = variable_label,
        labels = values
      )
  }

  if (is.null(attr_var_label(out))) attr(out, "label") <- x_name
}


