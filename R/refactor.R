#' Assign or reorder factor levels manually
#'
#' This is a low level function that allows you to convert a character vector
#' into a factor and manuall assign the levels, or to manually reassign the
#' levels of a factor vector
#'
#' @param f A factor or character vector
#' @param new_levels A list of the new levels of the factor
#' @param ordered Logical. Specifies if the factor is ordered. Default is NA,
#'   which checks to see if the factor is ordered and then uses that to determine
#'   if it should be ordered
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

refactor <- function(f, new_levels, ordered = NA) {

  # get the argument name
  f_lab <- deparse(substitute(f))

  # convert character vector to
  if (is.character(f)) {
    # if f is a character vector make it a factor
    f <- factor(f)
  } else if (is.factor(f)) {
    # if it is factor just leave it
    f
  } else {
    # if it is not character vector or factor return an error
    cli::cli_abort(
      "{.arg {f_lab}} must be a factor or character vector, not {.obj_type_friendly {f}}.",
    )
  }

  if (is.na(ordered)) {
    # if ordered = NA, make it ordered or not based on the original factor
    ordered <- is.ordered(f)
  }

  # make the new factor
  new_f <- factor(f, levels = new_levels, exclude = NULL, ordered = ordered)

  # update the attributes in f
  # this function uses the attributes from new_f and keeps the ones in f that
  # are not in new_f
  attributes(new_f) <- utils::modifyList(as.list(attributes(f)), attributes(new_f))
  # return the vector
  new_f

}


