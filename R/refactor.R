#' Assign or reorder factor levels manually
#'
#' This is a low level function that allows you to convert a character vector
#' into a factor and manually assign the levels, or to manually reassign the
#' levels of a factor vector. While very similar to `factor()` a key difference
#' is that this keeps the original attributes of `x`.
#'
#' @param x A factor or character vector
#' @param new_levels A list of the new levels of the factor
#' @param ordered Logical. Specifies if the factor is ordered. Default is NA,
#'   which checks to see if the factor is ordered and then uses that to determine
#'   if it should be ordered
#'
#' @returns A factor variable of the same length as `x`
#' 
#' @examples
#' # load the dplyr library so we can use `mutate()`
#' library(dplyr)
#'
#' # let's manually reorder the factor levels of `edu_f` from the `test_data`
#' # data set so it's in a random order that I specify
#' test_data <- test_data %>%
#'   mutate(
#'     # make the new reordered variable
#'     edu_f_reordered = refactor(
#'       # specify we are reordering the `edu_f` variable
#'       f = edu_f,
#'       new_levels = c(
#'         "Bachelor's Degree",
#'         "Graduate Degree",
#'         "High School or Less",
#'         "Some College"
#'       )
#'     )
#'   )
#'
#' # let's check the new levels
#' levels(test_data$edu_f_reordered)
#' # and compare them to the original levels
#' levels(test_data$edu_f)
#' @export

refactor <- function(x, new_levels, ordered = NA) {

  # get the argument name
  x_name <- deparse(substitute(x))

  # convert character vector to
  if (is.character(x)) {
    # if f is a character vector make it a factor
    x <- factor(x)
  } else if (is.factor(x)) {
    # if it is factor just leave it
    x
  } else {
    # if it is not character vector or factor return an error
    cli::cli_abort(
      "{.arg {x_name}} must be a factor or character vector, not {.obj_type_friendly {x}}.",
    )
  }

  if (is.na(ordered)) {
    # if ordered = NA, make it ordered or not based on the original factor
    ordered <- is.ordered(x)
  }

  # make the new factor
  new_x <- factor(x, levels = new_levels, exclude = NULL, ordered = ordered)

  # update the attributes in f
  # this function uses the attributes from new_f and keeps the ones in f that
  # are not in new_f
  attributes(new_x) <- utils::modifyList(as.list(attributes(x)), attributes(new_x))
  # return the vector
  new_x

}


