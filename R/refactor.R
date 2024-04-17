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
  if (is.na(ordered)) {
    # if ordered = NA

    # make
    ordered <- is.ordered(f)
  }

  new_f <- factor(f, levels = new_levels, exclude = NULL, ordered = ordered)
  attributes(new_f) <- utils::modifyList(attributes(f), attributes(new_f))
  new_f
}


