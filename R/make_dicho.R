#' Make dichotomous factors
#'
#' Create a vector of a dichotomous factor. Was designed to work on any vector
#' that is of class "factor" or "haven_labelled". However, if the vector is
#' numeric with no value labels, the function will return an error. In addition,
#' this function works by removing the first from the factors, unless it is only
#' one word. To illustrate what I mean, if a factor vector contains "Strongly
#' agree", the "Strongly" gets removed so it is just "Agree". If your vector is
#' not set up this way, then this function will not work properly.
#'
#' In addition, this function adds two new attributes. The first attribute,
#' `transformation`, indicates the data transformation that the original vector
#' underwent to create this new vector. The second attribute, `label`, contains
#' the variable label that was found in the original variable. However, if the
#' original vector did not have a variable label, then this attribute will not
#' show up.
#'
#' @param x A vector of type "haven_labelled" or "factor"
#'
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' library(labelled)
#' library(haven)
#'
#' # create fake data
#' data <- tibble::tribble(
#'   ~x, ~y, ~z,
#'   3, 2, 3,
#'   4, 4, 2,
#'   2, 6, 1,
#'   1, 1, 4,
#'   5, 4, 3,
#'   6, 5, 6
#'   ) %>%
#'   # add value labels
#'   labelled::set_value_labels(
#'     x = c(`Strongly agree` = 1,
#'           `Agree` = 2,
#'           `Somewhat agree` = 3,
#'           `Somewhat disagree` = 4,
#'           `Disagree` = 5,
#'           `Strongly disagree` = 6),
#'     y = c(`Strongly agree` = 1,
#'           `Agree` = 2,
#'           `Somewhat agree` = 3,
#'           `Somewhat disagree` = 4,
#'           `Disagree` = 5,
#'           `Strongly disagree` = 6),
#'     z = c(`Strongly agree` = 1,
#'           `Agree` = 2,
#'           `Somewhat agree` = 3,
#'           `Somewhat disagree` = 4,
#'           `Disagree` = 5,
#'           `Strongly disagree` = 6)
#'   )
#'
#'  # show the data transformation with a haven_labelled vector
#'  new_data <- data %>% dplyr::mutate(new_x = make_dicho(x))
#'  # check the updated dataset
#'  new_data
#'
#'  # Check the attributes
#'  attributes(new_data$new_x)
#'  # another way of checking the attributes
#'  str(new_data$new_x)
#'
#'  # check the factor levels
#'  unique(new_data$new_x)
#'
#'  ---------------------------------------------------------------------------
#'  # when done on a factor vector, the function still works
#'  new_data <- data %>%
#'    dplyr::mutate(
#'      # convert x to a factor and name it "factor_x"
#'      factor_x = haven::as_factor(x),
#'      # convert the x_factor variable to a dichotomous factor
#'      new_x = make_dicho(x_factor)
#'    )
#'  # check the updated dataset
#'  new_data
#'
#'  # Check the attributes
#'  attributes(new_data$new_x)
#'  # another way of checking the attributes
#'  str(new_data$new_x)
#'
#'  # check the factor levels
#'  unique(new_data$new_x)
#'
#' ----------------------------------------------------------------------------
#' # function also works inside dplyr::across()
#'  new_data <- data %>%
#'    dplyr::mutate(
#'      dplyr::across(x:z, make_dicho, .names = "new{col}")
#'    )
#'  # check the updated dataset
#'  new_data
#'
#'  # check the attributes for the new dataset
#'  str(new_data)
#'

make_dicho <- function(x) {

  # get the object's name
  x_lab <- deparse(substitute(x))

  # convert the vector to a factor
  x <- haven::as_factor(x)

  # remove the first
  x <- dplyr::if_else(
    stringr::str_detect(x, "\\s"),
    stringr::str_replace(x, "\\w+\\s", ""),
    x
  )  %>%
    # convert to a title case
    stringr::str_to_title() %>%
    # add new attributes
    structure(
      # indicate that the original variable was converted to a dichotomous factor
      transformation = glue("Converting '{x_lab}' to a dichotomous factor"),
      # add the original variable label
      label = labelled::var_label(x)
    )
}


