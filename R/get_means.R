#' Calculate means with confidence intervals
#'
#' @description
#' Use this function to calculate simple weighted means with 95% confidence
#' intervals or weighted grouped means.
#'
#' The `x`, `group`, and `wt` arguments can either be strings or symbols
#' (meaning they can have quotes or no quotes). The benefit of this is that it
#' makes it easy to iterate this function over a list or vector of
#' variables with other functions like [map()] [purrr::map()] or [walk()]
#' [purrr::walk()] that are found in the `purrr` package.
#'
#' @param data An object of type data.frame or tibble. If piping the data into the
#'   function, this is not required.
#' @param x Either a character string or symbol. The variable with which you want
#'   to get the mean.
#' @param group <[`tidy-select`][dplyr_tidy_select]> A selection of columns to 
#'   group the data by in addition to `treats`. This operates very similarly
#'   to `.by` from dplyr (for more info on that see [?dplyr_by][dplyr_by]). It can
#'   also be a character vector, but it can't be an external vector.
#' @param wt Weights. Add if you have a weighting variable and want to get
#'   weighted means.
#' @param decimals Number of decimals to round the results to. Default is 3.
#' @param na.rm Logical. Determines if NAs should be removed from the grouping
#'   variables prior to analysis. Default is TRUE.
#' @param conf_level What should the confidence level be when calculating
#'   confidence intervals. Defaults to 0.95 
#'
#' @returns A tibble with one row if no `group` is provided and `data` 
#'   is not of class `"grouped_df"`. If data is of class `"grouped_df"` or `group`
#'   is provided, it will return a row for each unique observation or combination 
#'   of observations.
#' 
#' 
#' 
#' @examples
#' # load the package
#' library(dplyr)
#'
#' # Let's calculate the overall average score for trad_n
#' get_means(test_data, trad_n)
#'
#' # it also works if x is a string
#' get_means(test_data, "trad_n")
#'
#' # Let's do that again but add weights
#' get_means(test_data, trad_n, wt = wts)
#'
#' # the wt argument can also be in quotes like this
#' get_means(test_data, "trad_n", wt = "wts")
#'
#' # Now let's do the average score for different education levels
#' get_means(test_data, trad_n, edu_f, wts)
#'
#' # it also works with quotes
#' get_means(test_data, "trad_n", "edu_f", "wts")
#'
#' # you can also pipe in the `data` argument if you want to do some data
#' # transformations before you calculate the means. For example, say you want
#' # to compare the means of `trad_n` among people who agreed vs disagreed with
#' # the variable `top`:
#' test_data %>%
#'   mutate(top_f2 = make_dicho(top)) %>%
#'   get_means(trad_n, top_f2, wts)
#'
#' @export
get_means <- function(
  data, 
  x, 
  group = NULL, 
  wt = NULL,
  decimals = 3, 
  na.rm = TRUE,
  conf_level = 0.95
) {
  
  x_name <- rlang::enexpr(x)

  # Ensure x is a string
  x <- rlang::as_name(rlang::ensym(x))

  # Check if x is numeric
  if (!is.numeric(data[[x]])) {
    cli::cli_abort(c(
      "`{x}` must be a numeric variable.",
      x = "Supplied variable is {class(data[[x]])}."
    ))
  }

  # Prepare group variables
  # if the data is grouped, use dplyr::group_vars to get them, else set to NULL
  group_names <- if(inherits(data, "grouped_df")) dplyr::group_vars(data) else NULL
  # if group arg is missing set to NULL, else use select_groups to capture it
  group_vars <- if (missing(group)) NULL else select_groups({{ group }}, data)
  # group_vars <- if (missing(group)) NULL else eval_select_by(rlang::enexpr(group), data)
  # remove the "c" from the group_vars vector if it is there
  group_vars <- group_vars[group_vars != "c"]
  # get the groups
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

  # get the data
  data <- data[c(x, group_names, wt)]
  
  # if na.rm is TRUE remove NAs
  if (na.rm) data <- data[stats::complete.cases(data),]  

  if (!is.null(group_names)) {
    # if the group arg is not missing, apply grouping based on group_names
    data <- data %>% 
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_names)))
  } 
  
  # Summarize data
  out <- data %>%
    dplyr::summarise(
      # calculate the weighted n
      n = sum(.data[[wt]], na.rm = TRUE), 
      # calculate the mean (weighted sum / n)
      mean = sum(.data[[x]] * .data[[wt]], na.rm = TRUE) / n,
      # calculate the weighted sd
      sd = sqrt(sum(.data[[wt]] * (.data[[x]] - mean)^2, na.rm = TRUE) / n),
      # remove the groups
      .groups = "drop"
    ) %>% 
    dplyr::mutate(
      # calculate std.error
      std.error = sd / sqrt(n), 
      # calculate the confidence invtervals
      conf.low = mean - qt(1 - ((1 - conf_level) / 2), n - 1) * std.error,
      conf.high = mean + qt(1 - ((1 - conf_level) / 2), n - 1) * std.error,
      # convert all group variables to a factor
      dplyr::across(
        # run the function over the variables in group_names
        tidyselect::all_of(group_names),
        # convert to a factor, removing levels, forcing to factor, and keeping NA as NA
        ~ make_factor(.x, drop_levels = TRUE, force = TRUE, na.rm = TRUE)
      ),
      # round all numeric columns 
      dplyr::across(
        dplyr::where(is.numeric),
        ~round(.x, decimals)
      )
    )
  
  out <- out[c(group_names, "mean", "sd", "n", "conf.low", "conf.high")]
      
  if (!is.null(group_names)) {
    # if there are groups add the value labels

    # get the variable labels as a named list
    group_labels <- attr_var_label(data[,group_names])
    # for each value in names(group_labels) add the variable label from group_labels
    for (y in names(group_labels)) attr(out[[y]], "label") <- group_labels[[y]]

  }

  if (!is.null(attr_var_label(data[[x]]))) {
    # if there is a variable label in the x variable

    # add the variable label of x as an attribute called 
    # variable_label to the output dataframe
    attr(out, "variable_label") <- attr_var_label(data[[x]])
    # add the variable name of x as an attribute called
    # variable_name to the output dataframe
    attr(out, "variable_name") <- x

  } else {
    # if x does not have a variable label

    # add the variable name of x as an attribute called
    # variable_label to the output dataframe
    attr(out, "variable_label") <- x
    # add the variable name of x as an attribute called
    # variable_name to the output dataframe    
    attr(out, "variable_name") <- x

  }

  # add an attribute containing the names of the grouping variables
  attr(out, "group_names") <- group_names

  # add a variable for the n variable
  attr(out$n, "label") <- "N"
  # add a variable label for the mean variable
  attr(out$mean, "label") <- "Mean"
  # add a variable label for the mean variable
  attr(out$sd, "label") <- "SD"
  # add a variable label for the mean variable
  attr(out$conf.low, "label") <- "Low CI"
  # add a variable label for the mean variable
  attr(out$conf.high, "label") <- "High CI"

  # get the classes of the data.frame
  class_names <- class(out)
  # add adlgraphs_freqs to the classes
  attr(out, "class") <- c("adlgraphs_means", class_names)
  
  out
}

