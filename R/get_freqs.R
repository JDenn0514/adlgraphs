#' Calculate weighted frequencies
#' 
#' @description
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
#' @examples
#' # load the package
#' library(dplyr)
#'
#' # Let's calculate the overall frequency for big_events
#' get_freqs(test_data, big_events)
#'
#' # Let's do that again but add weights
#' get_freqs(test_data, big_events, wt = wts)
#' 
#' # Can also a grouping variable by specifying the group arg
#' get_freqs(test_data, big_events, group = pid_f3, wt = wts)
#' 
#' # You can also group the data and do it
#' test_data %>% 
#'   group_by(pid_f3) %>% 
#'   get_freqs(big_events, wt = wts)
#' 
#' # you can also group by two or more variables
#' get_freqs(test_data, big_events, group = c(pid_f3, edu_f2), wt = wts)
#' 
#' # also works when the arguments are strings
#' get_freqs(test_data, "big_events", group = c("pid_f3", "edu_f2"), wt = "wts")
#'
#' 
#' @export
get_freqs <- function(
  data, 
  x, 
  group, 
  wt, 
  drop_zero = FALSE,
  decimals = 3,
  na.rm = TRUE
) {
  UseMethod("get_freqs")
}

#' @export
get_freqs.default <- function(
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

    if (!is.numeric(data[[wt]])) {
      # if it is not numeric then return an error
      cli::cli_abort(c(
        "`{wt}` must be a numeric variable.",
        x = "Supplied variable is {class(data[[wt]])}."
      ))

    } else {
      # if it is numeric, replace NAs with 0
      data[[wt]][is.na(data[[wt]])] <- 0
    }

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
      \(y) make_factor(y, drop_levels = drop_zero, force = TRUE, na.rm = na.rm)
    )
  } else {
    # convert the x and group_names to factors before the analysis to preserve NA tags
    data[,c(x, group_names)] <- lapply(
      data[,c(x, group_names)], 
      \(y) make_factor(y, drop_levels = drop_zero, force = TRUE, na.rm = na.rm)
    )
  }

  if (!missing(group) && !is.null(group_names)) {
    # if the group arg is not missing, apply grouping based on group_names
    data <- data %>% 
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_names)))
  } 

  out <- data %>% 
    # calculate the frequencies
    dplyr::count(.data[[x]], wt = .data[[wt]], .drop = drop_zero) %>% 
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
  if (!is.null(group_names)) {
    attr(out, "group_names") <- group_names
    attr(out, "group_labels") <- group_labels
  }

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


#' @export
get_freqs.survey.design <- function(
  data,
  x,
  group,
  wt, # ignored for survey data
  drop_zero = FALSE,
  decimals = 3,
  na.rm = TRUE
) {
  
  # Get the object's name
  x_name <- rlang::enexpr(x)
  
  # Ensure that string or symbol are accepted in x
  x <- rlang::as_name(rlang::ensym(x))
  
  # Get the data frame
  survey_data <- data$variables
  
  # Get the variable label in x BEFORE any modifications
  x_label <- attr_var_label(survey_data[[x]])
  
  # Prepare group variables
  group_names <- if (inherits(survey_data, "grouped_df")) dplyr::group_vars(survey_data) else NULL
  group_vars <- if (missing(group)) NULL else select_groups({{ group }}, survey_data)
  group_vars <- group_vars[group_vars != "c"]
  group_names <- unique(c(group_names, group_vars))
  
  # EXTRACT GROUP LABELS BEFORE ANY MODIFICATIONS
  group_labels <- list()
  if (!is.null(group_names)) {
    for (group_var in group_names) {
      group_labels[[group_var]] <- attr(survey_data[[group_var]], "label")
    }
  }
  
  # Handle missing data
  if (na.rm) {
    vars_to_check <- c(x, group_names)
    complete_cases <- stats::complete.cases(survey_data[vars_to_check])
    data <- data[complete_cases, ]
    survey_data <- data$variables
  }
  
  # Convert variables to factors
  value_labels <- attr_val_labels(survey_data[[x]])
  
  # Get sorted labels and unique values
  if (is.numeric(survey_data[[x]])) {
    labs <- sort(as.numeric(value_labels))
    vals <- sort(unique(as.numeric(survey_data[[x]])))
  } else {
    labs <- sort(as.character(value_labels))
    vals <- sort(unique(as.character(survey_data[[x]])))
  }
  
  # Convert to factors
  if (!all(vals %in% labs)) {
    # Only convert group variables to factors
    if (!is.null(group_names)) {
      for (group_var in group_names) {
        survey_data[[group_var]] <- make_factor(
          survey_data[[group_var]], 
          drop_levels = drop_zero, 
          force = TRUE, 
          na.rm = na.rm
        )
      }
    }
  } else {
    # Convert both x and group variables to factors
    survey_data[[x]] <- make_factor(
      survey_data[[x]], 
      drop_levels = drop_zero, 
      force = TRUE, 
      na.rm = na.rm
    )
    if (!is.null(group_names)) {
      for (group_var in group_names) {
        survey_data[[group_var]] <- make_factor(
          survey_data[[group_var]], 
          drop_levels = drop_zero, 
          force = TRUE, 
          na.rm = na.rm
        )
      }
    }
  }
  
  # Update survey design with modified data
  data$variables <- survey_data
  
  if (!is.null(group_names)) {
    # Manual approach for grouped analysis (more reliable)
    group_levels <- unique(survey_data[[group_names[1]]])
    x_levels <- if (is.factor(survey_data[[x]])) levels(survey_data[[x]]) else unique(survey_data[[x]])
    
    out_list <- list()
    
    for (group_val in group_levels) {
      # Subset survey design for this group
      subset_design <- data[survey_data[[group_names[1]]] == group_val, ]
      
      if (nrow(subset_design$variables) > 0) {
        # Get frequency table for this group
        freq_table <- survey::svytable(
          formula = stats::reformulate(x),
          design = subset_design
        )
        
        # Convert to data frame
        group_df <- data.frame(
          group_val = rep(group_val, length(freq_table)),
          x_val = names(freq_table),
          n = round(as.numeric(freq_table), decimals),
          stringsAsFactors = FALSE
        )
        
        # Set proper column names
        names(group_df)[1] <- group_names[1]
        names(group_df)[2] <- x
        
        # Calculate percentages within this group
        group_df$pct <- round(group_df$n / sum(group_df$n), decimals + 2)
        
        # Convert x back to proper type if needed
        if (is.factor(survey_data[[x]])) {
          group_df[[x]] <- factor(group_df[[x]], levels = levels(survey_data[[x]]))
        } else if (is.numeric(survey_data[[x]])) {
          group_df[[x]] <- as.numeric(group_df[[x]])
        }
        
        out_list[[as.character(group_val)]] <- group_df
      }
    }
    
    # Combine all groups
    out <- do.call(rbind, out_list)
    
    # Remove zero frequencies if requested
    if (drop_zero) {
      out <- out[out$n > 0, ]
    }
    
  } else {
    # No grouping - simple frequency table
    freq_results <- survey::svytable(
      formula = stats::reformulate(x),
      design = data
    )
    
    # Convert to data frame
    out <- data.frame(
      x_val = names(freq_results),
      n = round(as.numeric(freq_results), decimals),
      stringsAsFactors = FALSE
    )
    
    # Set proper column name
    names(out)[1] <- x
    
    # Calculate percentages
    out$pct <- round(out$n / sum(out$n), decimals + 2)
    
    # Remove zero frequencies if requested
    if (drop_zero) {
      out <- out[out$n > 0, ]
    }
    
    # Convert x back to proper type if needed
    if (is.factor(survey_data[[x]])) {
      out[[x]] <- factor(out[[x]], levels = levels(survey_data[[x]]))
    } else if (is.numeric(survey_data[[x]])) {
      out[[x]] <- as.numeric(out[[x]])
    }
  }
  
  # Add group labels using the labels we extracted earlier
  if (!is.null(group_names)) {
    for (y in names(group_labels)) {
      if (!is.null(group_labels[[y]])) {
        attr(out[[y]], "label") <- group_labels[[y]]
      }
    }
  }
  
  # Add variable labels and attributes
  if (!is.null(x_label)) {
    attr(out[[x]], "label") <- x_label
    attr(out, "variable_label") <- x_label
    attr(out, "variable_name") <- x_name
  } else {
    attr(out, "variable_label") <- x_name
    attr(out, "variable_name") <- x_name
  }
  
  # Add group attributes
  if (!is.null(group_names)) {
    attr(out, "group_names") <- group_names
    attr(out, "group_labels") <- group_labels
  }
  
  # Add variable labels for n and pct
  attr(out$n, "label") <- "N"
  attr(out$pct, "label") <- "Percent"
  
  # Add dataset attribute (survey design)
  attr(out, "dataset") <- data
  
  # Add class
  structure(out, class = c("adlgraphs_freqs", "tbl_df", "tbl", class(out)))
}


