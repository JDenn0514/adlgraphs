#' Calculate weighted frequencies
#' 
#' @description
#' 
#' `r lifecycle::badge("experimental")`
#'
#' Use this function to calculate simple weighted frequencies.
#' You can also specify a grouping variable by which you want to calculate the
#' frequencies.
#'
#' The `x`, `group`, and `wt` arguments can either be strings or symbols
#' (meaning they can have quotes or no quotes). The benefit of this is that it
#' makes it really easy to iterate this function over a list or vector of
#' variables with other functions like [map()] [purrr::map()] or [walk()]
#' [purrr::walk()] that are found in the `purrr` package.
#'
#' @param data An object of type data.frame or tibble. If piping the data into the
#'   function, this is not required.
#' @param x Either a character string or symbol. The variable with which want
#'   to get the frequencies.
#' @param group <[`tidy-select`][dplyr_tidy_select]> A selection of columns to 
#'   group the data by in addition to `treats`. This operates very similarly
#'   to `.by` from dplyr (for more info on that see [?dplyr_by][dplyr_by]). 
#' @param wt Weights. Add if you have a weighting variable and want to get
#'   weighted frequencies.
#' @param drop_zero Logical. Determines if rows with 0 should be removed 
#'   Default is `FALSE`.
#' @param decimals Number of decimals each number should be rounded to. Default
#'   is 3.
#' @param na.rm Logical. Determines if NAs should be kept or removed Default is
#'   `TRUE`.
#' 
#' @export
funky_freqs <- function(
  data, 
  x, 
  group, 
  wt, 
  drop_zero = FALSE,
  decimals = 3,
  na.rm = TRUE
) {
  

  # get the object's name
  x_name <- rlang::enexpr(x)

  # ensure that string or symbol are accepted in x
  x <- rlang::as_name(rlang::ensym(x))
  
  # get the variable label in x
  x_label <- attr_var_label(data[[x]])

  # Prepare group variables
  # if the data is grouped, use dplyr::group_vars to get them, else set to NULL
  group_names <- if(inherits(data, "grouped_df")) dplyr::group_vars(data) else NULL
  # if group arg is missing set to NULL, else use as.character(substitute()) to capture it
  group_vars <- if (missing(group)) NULL else select_groups({{ group }}, data)
  # remove the "c" from the group_vars vector if it is there
  group_vars <- group_vars[group_vars != "c"]
  # combine group_names and group_vars for the final vector of group names
  # use unique to make sure there aren't any duplicates
  group_names <- unique(c(group_names, group_vars))

  # Prepare weights
  if (missing(wt)) {
    wt <- "wts"
    data[[wt]] <- rep(1, length(data[[x]]))  
  } else {
    # ensure that string or symbol are accepted in wt
    wt <- rlang::as_name(rlang::ensym(wt))
    data[[wt]][is.na(data[[wt]])] <- 0
  }
  # subset the data with only relevant variables
  # this is so that when we remove NAs we are only doing it over the right variables
  data <- data[c(x, group_names, wt)]

  # if na.rm is TRUE remove NAs from all columns in data
  if (na.rm) data <- data[stats::complete.cases(data),]

  # Get the value labels (assumes attr_val_labels function exists)
  value_labels <- attr_val_labels(data[[x]])

  # Get sorted labels and unique values
  if (is.numeric(data[[x]])) {
    labs <- sort(as.numeric(value_labels))
    vals <- sort(unique(as.numeric(data[[x]])))
  } else {
    labs <- sort(as.character(value_labels))
    vals <- sort(unique(as.character(x)))
  }

  # If the values don't match the labels, don't make into a factor
  if (!all(vals %in% labs)) {
    # convert the group_names to factors before the analysis to preserve NA tags
    data[,c(group_names)] <- lapply(
      data[,c(group_names)], 
      \(y) make_factor(y, drop_levels = TRUE, force = TRUE, na.rm = na.rm)
    )
  } else {
    # convert the x and group_names to factors before the analysis to preserve NA tags
    data[,c(x, group_names)] <- lapply(
      data[,c(x, group_names)], 
      \(y) make_factor(y, drop_levels = TRUE, force = TRUE, na.rm = na.rm)
    )
  }

  if (!missing(group) && !is.null(group_names)) {
    # if the group arg is not missing, apply grouping based on group_names
    data <- data %>% 
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_names)))
  } 

  out <- data %>% 
    # calculate the frequencies
    dplyr::count(.data[[x]], wt = .data[[wt]]) %>% 
    # clean up the data
    dplyr::mutate(
      # use prop.table() to calculate percentage and round()
      # add 2 to decimals so that when converted to percentage 
      # it has the correct number of decimals
      pct = round(prop.table(n), decimals + 2),
      # round the n to the number of decimals
      n = round(n, decimals)
    ) 
  
  if (!is.null(group_names)) {
    # if there are groups add the value labels

    # get the variable labels as a named list
    group_labels <- attr_var_label(data[,group_names])
    # for each value in names(group_labels) add the variable label from group_labels
    for (y in names(group_labels)) attr(out[[y]], "label") <- group_labels[[y]]

  }

  

  # if drop_zero is TRUE, remove any rows with 0
  if (drop_zero) out <- out[out$n != 0,]

  if (!is.null(x_label)) {
    # if there is a variable label in the x variable

    # add the variable label to x
    attr(out[[x]], "label") <- x_label
    # add the variable label of x as an attribute called 
    # variable_label to the output dataframe
    attr(out, "variable_label") <- x_label
    # add the variable name of x as an attribute called
    # variable_name to the output dataframe
    attr(out, "variable_name") <- x_name

  } else {
    # if x does not have a variable label

    # add the variable name of x as an attribute called
    # variable_label to the output dataframe
    attr(out, "variable_label") <- x_name
    # add the variable name of x as an attribute called
    # variable_name to the output dataframe    
    attr(out, "variable_name") <- x_name

  }

  # add an attribute containing the names of the grouping variables
  attr(out, "group_names") <- group_names

  attr(out, "group_labels") <- attr_var_label(data[group_names], unlist = FALSE)

  # add a variable for the n variable
  attr(out$n, "label") <- "N"
  # add a variable label for the pct variable
  attr(out$pct, "label") <- "Percent"

  attr(out, "dataset") <- data

  # get the classes of the data.frame
  class_names <- class(out)
  # add adlgraphs_freqs to the classes
  attr(out, "class") <- c("adlgraphs_freqs", class_names)

  out
  
}
