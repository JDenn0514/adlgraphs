#' Calculate row sums
#' 
#' This function makes it easy to calculate row sums for multiple variables.
#' It uses <[`tidy-select`][dplyr_tidy_select]> syntax to determine which 
#' variables to include in the operation. 
#' 
#' This function also has the option of adding a new variable label attribute.
#' Furthermore, it automatically adds two more attributes: `transformation` 
#' and `variables`. The `trasnformation` attribute basically explains how the 
#' variable was created by saying "Took the average of..." and then lists 
#' the variables included. `variables` just lists the variables included
#' in the operation.
#' 
#' @param cols <[`tidy-select`][dplyr_tidy_select]> The variables you want 
#'   to use when calculating row sums
#' @param label A string specifying the variable label. If not specified, 
#'   defaults to NULL
#' @param na.rm Determines if NAs should be removed. Default is TRUE.
#' 
#' @examples
#' # load the dplyr package
#' library(dplyr)
#' # make a new df with the new column
#' new <- test_data %>% 
#'   mutate(
#'     sdo_sum_new = row_sums(
#'       # specify the variables involved in the row means
#'       cols = c(top_rev:deserving_flip),
#'       # specify the variable label
#'       label = "Social Dominance Orientation Sum",
#'       # remove NAs
#'       na.rm = TRUE
#'     )
#'   )
#' 
#' # Show that the attributes
#' attributes(new$sdo_sum_new)
#' 
#' # show the output
#' new$sdo_sum_new
#' 
#' @export
row_sums <- function(cols, label = NULL, na.rm = TRUE) {

  # get the dataframe with only relevant columns
  data <- dplyr::pick({{ cols }})
  # get the column names
  vars <- names(data)
  # combine them into a single string
  trans_vars <- paste(vars, collapse = ", ")

  transformation <- paste("Took the sum of", trans_vars)

  # calculate the rowmeans
  rowSums(data, na.rm = na.rm) %>% 
    # add label and variables attributes
    structure(
      label = label,
      transformation = transformation,
      variables = vars
    )
}
