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
  UseMethod("get_means")
}


#' @export
get_means.default <- function(
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
        tidyselect::where(is.numeric),
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

#' @export
get_means.survey.design <- function(
  data,
  x,
  group = NULL,
  wt = NULL, # ignored for survey data
  decimals = 3,
  na.rm = TRUE,
  conf_level = 0.95
) {
  
  # Capture the expression of x for later use in attributes
  x_name <- rlang::enexpr(x)
  
  # Convert x to a string name for consistent handling
  x <- rlang::as_name(rlang::ensym(x))
  
  # Extract the data frame from the survey design object
  survey_data <- data$variables
  
  # Validate that x is numeric (required for mean calculations)
  if (!is.numeric(survey_data[[x]])) {
    cli::cli_abort(c(
      "`{x}` must be a numeric variable.",
      x = "Supplied variable is {class(survey_data[[x]])}."
    ))
  }
  
  # Prepare group variables by combining existing groups and new group argument
  # Check if data is already grouped using dplyr
  group_names <- if (inherits(survey_data, "grouped_df")) dplyr::group_vars(survey_data) else NULL
  # Extract group variables from the group argument (custom function)
  group_vars <- if (missing(group)) NULL else select_groups({{ group }}, survey_data)
  # Remove "c" if it appears in group_vars (artifact from select_groups)
  group_vars <- group_vars[group_vars != "c"]
  # Combine and deduplicate group variables
  group_names <- unique(c(group_names, group_vars))
  
  # EXTRACT VARIABLE LABELS BEFORE ANY DATA MODIFICATIONS
  # This prevents labels from being lost during type conversions
  # Get label for the outcome variable (custom function)
  x_label <- attr_var_label(survey_data[[x]])
  # Initialize list to store group variable labels
  group_labels <- list()
  if (!is.null(group_names)) {
    # Extract label for each group variable using base R attr()
    for (group_var in group_names) {
      group_labels[[group_var]] <- attr(survey_data[[group_var]], "label")
    }
  }
  
  # CONVERT HAVEN LABELLED VARIABLES TO PLAIN R TYPES
  # This prevents vec_arith errors when survey functions do calculations
  # Convert outcome variable from haven_labelled to plain numeric
  survey_data[[x]] <- as.numeric(survey_data[[x]])
  
  # Convert group variables from haven_labelled to character before factor conversion
  if (!is.null(group_names)) {
    for (group_var in group_names) {
      # Check if variable is haven_labelled and convert to character
      if (inherits(survey_data[[group_var]], "haven_labelled")) {
        survey_data[[group_var]] <- as.character(survey_data[[group_var]])
      }
    }
  }
  
  # Handle missing data removal if requested
  if (na.rm) {
    # Identify variables to check for completeness
    vars_to_check <- c(x, group_names)
    # Find rows with complete cases across all relevant variables
    complete_cases <- stats::complete.cases(survey_data[vars_to_check])
    # Subset the survey design to complete cases only
    data <- data[complete_cases, ]
    # Update survey_data reference after subsetting
    survey_data <- data$variables
  }
  
  # Convert group variables to factors for proper grouping
  if (!is.null(group_names)) {
    for (group_var in group_names) {
      # Use custom make_factor function to convert to factor with proper handling
      survey_data[[group_var]] <- make_factor(
        survey_data[[group_var]],
        drop_levels = TRUE,    # Remove unused factor levels
        force = TRUE,          # Force conversion to factor
        na.rm = TRUE          # Handle NAs appropriately
      )
    }
  }
  
  # Update the survey design object with the modified data
  data$variables <- survey_data
  
  # Create formula for survey calculations (outcome ~ 1 for means)
  outcome_formula <- stats::reformulate("1", x)
  
  # GROUPED ANALYSIS: Calculate means separately for each group combination
  if (!is.null(group_names)) {
  
    # CREATE GROUPING FORMULA FOR SURVEY CALCULATIONS
    # The survey package needs a formula to specify which variables to group by
    if (length(group_names) == 1) {
      # Single grouping variable: create formula like ~gender
      by_formula <- stats::reformulate(group_names[1])
    } else {
      # Multiple grouping variables: create formula like ~gender + education
      by_formula <- stats::reformulate(group_names)
    }
    
    # CALCULATE SURVEY-WEIGHTED MEANS FOR ALL GROUPS AT ONCE
    # svyby() is the survey package's optimized function for grouped calculations
    # It automatically handles survey weights, design effects, and creates all group combinations
    mean_results <- survey::svyby(
      formula = outcome_formula,        # What to calculate (e.g., ~income)
      by = by_formula,                  # How to group (e.g., ~gender + education)
      design = data,                    # Survey design object with weights/strata
      FUN = survey::svymean,            # Function to apply (survey-weighted mean)
      na.rm = na.rm                     # How to handle missing values
    )
    
    # EXTRACT MEANS AND STANDARD ERRORS FROM SURVEY RESULTS
    # svyby() returns a data frame with group columns plus statistical results
    means <- mean_results[[x]]         # Extract the mean values (column named after our variable)
    ses <- sqrt(mean_results[["se"]]^2) # Extract standard errors (always in "se" column)
    
    # PREPARE FOR MANUAL STANDARD DEVIATION CALCULATIONS
    # Unfortunately, svyby() doesn't calculate weighted SDs, so we need to do this manually
    # But we can still optimize by reducing the number of iterations
    
    # Extract just the grouping variables from the survey data
    group_data <- data$variables[group_names]
    
    # Create a single interaction variable that combines all group levels
    # This converts multiple grouping variables into one factor with combined levels
    # Example: gender="Male" + education="College" becomes "Male_College"
    group_interaction <- interaction(group_data, drop = TRUE, sep = "_")
    
    # Get all unique combinations that actually exist in the data
    # This is more efficient than nested loops because we only iterate over existing combinations
    unique_combinations <- levels(group_interaction)
    
    # PRE-ALLOCATE VECTORS FOR STANDARD DEVIATIONS AND SAMPLE SIZES
    # Pre-allocation is faster than growing vectors in a loop
    sds <- numeric(length(unique_combinations))  # Will store weighted standard deviations
    ns <- numeric(length(unique_combinations))   # Will store effective sample sizes
    
    # CALCULATE WEIGHTED STANDARD DEVIATIONS FOR EACH GROUP COMBINATION
    # This is the only part that still requires a loop, but it's much more efficient
    # because we're iterating over unique combinations rather than nested group levels
    for (i in seq_along(unique_combinations)) {
      
      # Create logical vector identifying rows belonging to this group combination
      # This is vectorized and much faster than multiple nested comparisons
      subset_logical <- group_interaction == unique_combinations[i]
      
      # Handle missing values in the logical vector
      subset_logical[is.na(subset_logical)] <- FALSE
      
      # Only proceed if this group combination has any observations
      if (any(subset_logical)) {
        
        # Subset the survey design to just this group combination
        subset_design <- data[subset_logical, ]
        
        # Extract the raw data values for our outcome variable
        subset_data <- subset_design$variables[[x]]
        
        # Extract the survey weights for these observations
        subset_weights <- stats::weights(subset_design, "sampling")
        
        # CALCULATE WEIGHTED STANDARD DEVIATION MANUALLY
        # Step 1: Calculate weighted mean (for variance calculation)
        weighted_mean <- sum(subset_data * subset_weights) / sum(subset_weights)
        
        # Step 2: Calculate weighted variance
        # Formula: Σ(weight * (value - weighted_mean)²) / Σ(weights)
        weighted_var <- sum(subset_weights * (subset_data - weighted_mean)^2) / sum(subset_weights)
        
        # Step 3: Standard deviation is square root of variance
        sds[i] <- sqrt(weighted_var)
        
        # Step 4: Effective sample size is sum of weights
        ns[i] <- sum(subset_weights)
      }
    }
    
    # CALCULATE CONFIDENCE INTERVALS USING SURVEY-APPROPRIATE METHODS
    # Survey data requires special handling for confidence intervals due to design effects
    
    # Get design-based degrees of freedom (accounts for survey design complexity)
    df <- survey::degf(data)
    
    # Calculate t-distribution critical value based on confidence level
    # Uses t-distribution rather than normal because of finite sample sizes
    t_val <- stats::qt(1 - ((1 - conf_level) / 2), df)
    
    # Calculate confidence interval bounds
    # CI = mean ± (t-value × standard error)
    conf_low <- means - t_val * ses
    conf_high <- means + t_val * ses
    
    # CREATE FINAL OUTPUT DATA FRAME
    # Combine all results into a clean, formatted data frame
    out <- data.frame(
      # Include all grouping variable columns from svyby results
      # This preserves the original group level names and types
      mean_results[group_names],
      
      # Add calculated statistics, rounded to specified decimal places
      mean = round(means, decimals),           # Survey-weighted means
      sd = round(sds, decimals),               # Weighted standard deviations
      n = round(ns, decimals),                 # Effective sample sizes
      conf.low = round(conf_low, decimals),    # Lower confidence interval bounds
      conf.high = round(conf_high, decimals),  # Upper confidence interval bounds
      
      # Prevent automatic conversion of character variables to factors
      stringsAsFactors = FALSE
    )
    
  } else {
    # UNGROUPED ANALYSIS: Calculate overall means
    
    # Calculate survey-weighted mean and standard error
    mean_result <- survey::svymean(outcome_formula, design = data, na.rm = na.rm)
    mean_val <- as.numeric(mean_result)
    mean_se <- sqrt(as.numeric(attr(mean_result, "var")))
    
    # Calculate weighted standard deviation manually
    raw_data <- data$variables[[x]]
    raw_weights <- stats::weights(data, "sampling")
    weighted_mean <- sum(raw_data * raw_weights) / sum(raw_weights)
    weighted_var <- sum(raw_weights * (raw_data - weighted_mean)^2) / sum(raw_weights)
    sd_val <- sqrt(weighted_var)
    
    # Calculate effective sample size
    n_val <- sum(raw_weights)
    
    ### Calculate CIs
    # get survey design based degrees of freedom
    df <- survey::degf(data)
    # calculate the t-distribution using the quantile function
    t_val <- stats::qt(1 - ((1 - conf_level) / 2), df)
    # determine low CI
    conf_low <- mean_val - t_val * mean_se
    # calculate low CIs
    conf_high <- mean_val + t_val * mean_se
    
    # Create results data frame
    out <- data.frame(
      mean = round(mean_val, decimals),
      sd = round(sd_val, decimals),
      n = round(n_val, decimals),
      conf.low = round(conf_low, decimals),
      conf.high = round(conf_high, decimals),
      stringsAsFactors = FALSE
    )
  }
  
  # Reorder columns to match original function's output format
  col_order <- c(group_names, "mean", "sd", "n", "conf.low", "conf.high")
  # Only include columns that actually exist in the output
  col_order <- col_order[col_order %in% names(out)]
  out <- out[col_order]
  
  # Add group variable labels using the labels extracted earlier
  if (!is.null(group_names)) {
    for (y in names(group_labels)) {
      # Only add label if it exists and is not null
      if (!is.null(group_labels[[y]])) {
        attr(out[[y]], "label") <- group_labels[[y]]
      }
    }
  }
  
  # Add variable labels and attributes for the outcome variable
  if (!is.null(x_label)) {
    # Use the extracted variable label
    attr(out, "variable_label") <- x_label
    attr(out, "variable_name") <- x_name
  } else {
    # Fallback to variable name if no label exists
    attr(out, "variable_label") <- x_name
    attr(out, "variable_name") <- x_name
  }
  
  # Add group names as an attribute for downstream functions
  if (!is.null(group_names)) {
    attr(out, "group_names") <- group_names
  }
  
  ### Add descriptive labels for each output column
  attr(out$n, "label") <- "N"                    # Sample size
  attr(out$mean, "label") <- "Mean"              # Mean value
  attr(out$sd, "label") <- "SD"                  # Standard deviation
  attr(out$conf.low, "label") <- "Low CI"        # Lower confidence interval
  attr(out$conf.high, "label") <- "High CI"      # Upper confidence interval
  
  # Add custom class for method dispatch and formatting
  structure(out, class = c("adlgraphs_means", "tbl_df", "tbl", class(out)))
}
