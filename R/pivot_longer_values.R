#' Pivot data from wide to long with value labels
#'
#' This function is a wrapper around \code{\link[tidyr]{pivot_longer}}. This
#' function operates in pretty much the exact same way but uses the variable
#' labels from the variables specified in `cols` to make new value labels in the
#' new variable created in the `names_to` variable.
#'
#' @inheritParams tidyr::pivot_longer
#'
#' @export

pivot_longer_values <- function(data, cols, names_to, values_to) {
  # create the long data frame
  long <- data %>%
    tidyr::pivot_longer(
      cols = {{ cols }},
      names_to = names_to,
      values_to = values_to
    )

  # create a vector containing the variable labels
  var_labs <- labelled::var_label(x = data %>% select( {{ cols }})) %>%
    unlist()

  # flip the names and values of the vector
  var_labs <- setNames(names(var_labs), var_labs)

  # add the vector of labels as value labels to the new column of names
  labelled::val_labels(long[{{names_to}}]) <- var_labs

  return(long)

}
