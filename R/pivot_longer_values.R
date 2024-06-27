#' Pivot data from wide to long with value labels
#'
#' This function is a wrapper around \code{\link[tidyr]{pivot_longer}}. This
#' function operates in pretty much the exact same way but uses the variable
#' labels from the variables specified in `cols` to make new value labels in the
#' new variable created in the `names_to` variable.
#'
#' @inheritParams tidyr::pivot_longer
#' @param name_label Add a variable label to the new column with the names of
#'   the columns
#'
#' @export

pivot_longer_values <- function(data, cols, names_to, values_to, name_label = NA) {
  # create the long data frame
  long <- data %>%
    tidyr::pivot_longer(
      cols = {{ cols }},
      names_to = names_to,
      values_to = values_to
    )


  # get the newly created vector of names
  names <- long[[{{ names_to }}]]
  # get the newly created vector of values
  values <- long[[{{ values_to }}]]
  # get the variable labels to go into names as value labels
  var_labs <- data %>%
    select({{ cols }}) %>%
    get_all_var_labels()

  # flip the names and values of the vector
  var_labs <- setNames(names(var_labs), var_labs)

  names <- structure(
    names,
    labels = var_labs,
    label = name_label
  )

  long[[{{ names_to }}]] <- names
  return(long)



}
