#' Calculate difference in means
#' 
#' @description
#' This function calculates the difference in means using a 
#' bivariate regression, as well the p-value indicating how
#' significant each difference is. The main function doing the
#' calculations `lm()`.
#' 
#' NOTE: This function does not perform an actual Dunnet Test as it 
#' does not calculate the quantile of the multivariate t-distribution
#' when determining the confidence intervals and p-values. If you need
#' to perform an actual Dunnett Test use the `dunnett()` function 
#' instead. Please be aware that that function is far slower when 
#' there are many comparison groups due to the nature of 
#' `mvtnorm::qmvt()` and high dimensional data.
#' 
#' @param data A data frame or tibble.
#' @param x A numeric vector that will be used to calculate the means.
#'   This can be a string or symbol.
#' @param treats A variable whose values are used to determine if the means
#'   are statistically significantly different from each other. Should be 
#'   a factor or character vector. This can be a string or symbol.
#' @param group <[`tidy-select`][dplyr_tidy_select]> A selection of columns to 
#'   group the data by in addition to `treats`. This operates very similarly
#'   to `.by` from dplyr (for more info on that see [?dplyr_by][dplyr_by]). 
#'   See examples to see how it operates.
#' @param wt Weights. Add if you have a weighting variable and want to perform
#'   Dunnett's test with weighted means.
#' @param show_means Logical. Default is `FALSE` which does not show the mean
#'   values for the levels. If `TRUE`, will add a column called `mean` that
#'   contains the means.
#' @param show_pct_change Logical. Default is `FALSE` which does not show the
#'   percent change from the reference category to the other categories. If 
#'   `TRUE`, will show the percent change.
#' @param ref_level A string that specifies the level of the reference group
#'   through which the others will be tested.
#' @param conf.level A number between 0 and 1 that signifies the width of the
#'   desired confidence interval. Default is 0.95, which corresponds to a 95%
#'   confidence interval.
#' @param decimals Number of decimals each number should be rounded to. Default
#'   is 3.
#' @param na.rm Logical. Default is `TRUE` which removes NAs prior to 
#'   calculation.
#' 
#' @returns A tibble with one row if no `group` is provided and `data` 
#'   is not of class `"grouped_df"`. If data is of class `"grouped_df"` or 
#'   `group` is provided, it will return one row for each unique observation 
#'   if one group is provides and one row per unique combination of observations
#'   if multiple groups are used.
#' 
#' @examples
#' # load dplyr for the pipe: %>% 
#' library(dplyr)
#' library(adlgraphs)
#' 
#' # Check to see if any of the partisan groups are significantly different
#' # from the control group (in this case "Democrat") for conspiracy
#' # theory belief
#' get_diffs(test_data, "acts_avg", "pid_f3")
#'
#' # now do the same as above but make "Independent" the control group
#' get_diffs(test_data, "acts_avg", "pid_f3", ref_level = "Independent")
#'
#' # now let's add in education (`edu_f2`) as the `group` variable. This let's us
#' # compare education levels within each level of `edu_f2`. Note how the arguments
#' # don't have to be strings
#' get_diffs(test_data, acts_avg, pid_f3, edu_f2)
#' 
#' # we can also group by multiple variables. Due to a small n, I'm going to use 
#' # `edu_f2` instead of `edu_f`. 
#' test_data %>% 
#'   dplyr::mutate(values_f2 = make_dicho(values)) %>% 
#'   get_diffs(acts_avg, treats = pid_f3, group = c(edu_f2, values_f2))
#' 
#' # now let's do those previous two calculations but using `dplyr::group_by()`
#' test_data %>% 
#'   dplyr::group_by(pid_f3) %>% 
#'   get_diffs(acts_avg, edu_f)
#' 
#' # we can also group by multiple variables
#' test_data %>% 
#'   dplyr::mutate(values_f2 = make_dicho(values)) %>% 
#'   dplyr::group_by(pid_f3, values_f2) %>% 
#'   get_diffs(acts_avg, edu_f2)
#' 
#' @export
get_diffs <- function(
  data, 
  x, 
  treats, 
  group = NULL, 
  wt = NULL, 
  ref_level,
  show_means = FALSE,
  show_pct_change = FALSE,
  decimals = 3, 
  conf.level = 0.95,
  na.rm = TRUE
) {
  UseMethod("get_diffs")
}

#' @export
get_diffs.default <- function(
  data, 
  x, 
  treats, 
  group = NULL, 
  wt = NULL, 
  ref_level,
  show_means = FALSE,
  show_pct_change = FALSE,
  decimals = 3, 
  conf.level = 0.95,
  na.rm = TRUE
) {

  # Ensure inputs are symbols or strings
  x_name <- rlang::enexpr(x)
  x <- rlang::as_name(rlang::ensym(x))
  treats <- rlang::as_name(rlang::ensym(treats))
  
  ## Prepare group variables
  group_vars <- if (missing(group)) NULL else select_groups({{ group }}, data)
  group_vars <- group_vars[group_vars != "c"]
  group_names <- if (inherits(data, "grouped_df")) dplyr::group_vars(data) else NULL
  group_names <- unique(c(group_names, group_vars))
  # combine to the grouping variables and the treatments
  group_cols <- c(group_names, treats)
  
  # Check for numeric x and reference level presence
  if (!is.numeric(data[[x]])) {
    cli::cli_abort(c(
      "{.arg x} must be of class `numeric`",
      "i" = "`{x_name}` is of class {class(data[[x]])}"
    ))
  }

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

  # force the treats variable to a factor
  data[[treats]] <- make_factor(data[[treats]], drop_levels = TRUE, force = TRUE)

  # if rev_level is missing, set it to the first level in the treats variable
  if (missing(ref_level)) ref_level <- levels(data[[treats]])[1]

  data <- data[c(x, treats, group_names, wt)]
  if (na.rm) data <- data[stats::complete.cases(data), ]

  if (!is.null(group_names)) {

    nest_data <- make_nested(data, {{ group_names }})
    
    # iterate over each nest tibble from 
    out <- purrr::map(
      nest_data$data,
      ~bivariate_reg(
        .x,
        x = {{ x }}, 
        treats = {{ treats }},
        wt = {{ wt }}, 
        show_means = show_means,
        show_pct_change = show_pct_change,
        ref_level = ref_level, 
        conf.level = conf.level
      )
    ) %>% 
      stats::setNames(nest_data$name) 

    # combine the lists
    out <- vctrs::vec_rbind(!!!out, .names_to = "groups") %>% 
      tidyr::separate_wider_delim(cols = "groups", delim = "_", names = group_names)

  } else {
    out <- bivariate_reg(
      data = data,
      x = {{ x }}, 
      treats = {{ treats }},
      wt = {{ wt }}, 
      show_means = show_means,
      show_pct_change = show_pct_change,
      ref_level = ref_level, 
      conf.level = conf.level
    )
  }

  # clean up the treats variable by removing it from the string in the term col
  out[[treats]] <- gsub(pattern = treats, replacement = "", x = out$term, fixed = TRUE)
  
  # keep only the relevant columns and reorder them
  out <- out[c(group_names, treats, "Estimate", "pct_change", "mean",  "n", "conf.low", "conf.high", "Pr(>|t|)", "stars")]
  # rename the columns
  colnames(out) <- c(group_names, treats, "diffs", "pct_change", "mean", "n", "conf.low", "conf.high", "p_value", "stars")

  if (!show_means) {
    # if show_means is false, remove it
    out <- out[, !names(out) == "mean"]
  } else {
    # otherwise, round it
    out$mean <- round(out$mean, decimals)
    # also add label
    attr(out$mean, "label") <- "Mean"
  }

  if (!show_pct_change) {
    # if show_pct_change is false, remove it
    out <- out[, !names(out) == "pct_change"]
  } else {
    # otherwise, round it
    out$pct_change <- round(out$pct_change, decimals + 2)
    # also add label
    attr(out$pct_change, "label") <- paste("Percent change from", ref_level)
  }


  # set the grouping columns to factors using levels from original data set
  out[group_cols] <- purrr::map(
    group_cols %>% setNames(nm = .),
    ~ factor(out[[.x]], levels = levels(data[[.x]]))
  )  
  
  # reorder the columns in teh grouping variables
  out <- dplyr::arrange(out, dplyr::across(tidyselect::all_of(group_cols)))

  # round the numeric columns to decimals places
  out$diffs <- round(out$diffs, decimals)
  out$conf.low <- round(out$conf.low, decimals)
  out$conf.high <- round(out$conf.high, decimals)
  out$p_value <- round(out$p_value, decimals)

  if (!is.null(group_names)) {
    # if there are groups add the value labels

    # get the variable labels as a named list
    group_labels <- attr_var_label(data[,group_names])
    # for each value in names(group_labels) add the variable label from group_labels
    for (y in names(group_labels)) attr(out[[y]], "label") <- group_labels[[y]]

  }
 
  # Add labels
  attr(out[[treats]], "label") <- attr(data[[treats]], "label")
  attr(out$diffs, "label") <- paste("Difference in means relative to", ref_level)
  attr(out$n, "label") <- "N"
  attr(out$conf.low, "label") <- "Low CI"
  attr(out$conf.high, "label") <- "High CI"
  attr(out$p_value, "label") <- "P-Value"
  attr(out$stars, "label") <- ""

  if (!is.null(attr_var_label(data[[x]]))) {
    # if there is a variable label in the x variable

    # add the variable label of x as an attribute called 
    # variable_label to the output dataframe
    attr(out, "variable_label") <- attr_var_label(data[[x]])
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
  
  # Return results and set the class
  structure(out, class = c("adlgraphs_mean_diffs", class(out)))
}


#' @export
get_diffs.survey.design <- function(
  data,
  x,
  treats,
  group = NULL,
  wt = NULL, # ignored for survey data
  ref_level,
  show_means = FALSE,
  show_pct_change = FALSE,
  decimals = 3,
  conf.level = 0.95,
  na.rm = TRUE
) {
  
  # Ensure inputs are symbols or strings
  x_name <- rlang::enexpr(x)
  x <- rlang::as_name(rlang::ensym(x))
  treats <- rlang::as_name(rlang::ensym(treats))
  
  # Get the data frame
  survey_data <- data$variables
  
  ## Prepare group variables
  group_vars <- if (missing(group)) NULL else select_groups({{ group }}, survey_data)
  group_vars <- group_vars[group_vars != "c"]
  group_names <- if (inherits(survey_data, "grouped_df")) dplyr::group_vars(survey_data) else NULL
  group_names <- unique(c(group_names, group_vars))
  
  # Check for numeric x
  if (!is.numeric(survey_data[[x]])) {
    cli::cli_abort(c(
      "{.arg x} must be of class `numeric`",
      "i" = "`{x_name}` is of class {class(survey_data[[x]])}"
    ))
  }
  
  # Force treats to factor and set reference level
  survey_data[[treats]] <- make_factor(survey_data[[treats]], drop_levels = TRUE, force = TRUE)
  if (missing(ref_level)) ref_level <- levels(survey_data[[treats]])[1]
  if (ref_level != levels(survey_data[[treats]])[1]) {
    survey_data[[treats]] <- stats::relevel(survey_data[[treats]], ref_level)
  }
  
  # Update survey design with modified data
  data$variables <- survey_data
  
  # Handle missing data
  if (na.rm) {
    complete_cases <- stats::complete.cases(survey_data[c(x, treats, group_names)])
    data <- data[complete_cases, ]
    survey_data <- data$variables
  }
  
  # Create formulas
  outcome_formula <- stats::reformulate("1", x)
  treatment_formula <- stats::reformulate(treats, x)
  
  if (!is.null(group_names)) {
    
    # Manual splitting approach (statistically sound and reliable)
    group_levels <- unique(data$variables[[group_names]])
    out_list <- list()
    
    for (group_val in group_levels) {
      # Subset the survey design for this group
      subset_design <- data[data$variables[[group_names]] == group_val, ]
      
      # Run regression on this subset
      model <- survey::svyglm(treatment_formula, design = subset_design)
      coefs <- summary(model)$coefficients
      
      # Get non-intercept terms (treatment effects)
      non_intercept <- !grepl("(Intercept)", rownames(coefs))
      
      if (any(non_intercept)) {
        result <- coefs[non_intercept, , drop = FALSE]
        
        # Calculate confidence intervals
        z <- stats::qnorm(1 - ((1 - conf.level) / 2))
        conf_low <- result[, "Estimate"] - z * result[, "Std. Error"]
        conf_high <- result[, "Estimate"] + z * result[, "Std. Error"]
        
        # Calculate sample sizes
        design_matrix <- stats::model.matrix(model)
        weights_vec <- stats::weights(subset_design, "sampling")
        n_counts <- colSums(design_matrix * weights_vec)
        
        # Create data frame for this group
        group_df <- data.frame(
          group_val = rep(group_val, nrow(result)),
          term = rownames(result),
          estimate = result[, "Estimate"],
          std_error = result[, "Std. Error"],
          p_value = result[, "Pr(>|t|)"],
          conf_low = conf_low,
          conf_high = conf_high,
          n = n_counts[-1], # exclude intercept
          stringsAsFactors = FALSE
        )
        
        # Set proper column name for group
        names(group_df)[1] <- group_names
        
        # Clean up treatment names
        group_df[[treats]] <- gsub(pattern = treats, replacement = "", x = group_df$term, fixed = TRUE)
        
        out_list[[as.character(group_val)]] <- group_df
      }
    }
    
    # Combine all groups
    out <- do.call(rbind, out_list)
    
    # Get means if requested
    if (show_means) {
      means_results <- survey::svyby(
        formula = outcome_formula,
        by = stats::reformulate(c(group_names, treats)),
        design = data,
        FUN = survey::svymean,
        na.rm = na.rm,
        deff = FALSE
      )
      
      means_df <- as.data.frame(means_results)
      mean_col_idx <- which(!names(means_df) %in% c(group_names, treats))
      if (length(mean_col_idx) > 0) {
        names(means_df)[mean_col_idx[1]] <- "mean"
      }
      
      out <- merge(out, means_df, by = c(group_names, treats), all.x = TRUE)
    }
    
  } else {
    # No grouping - single regression
    model <- survey::svyglm(treatment_formula, design = data)
    coefs <- summary(model)$coefficients
    non_intercept <- !grepl("(Intercept)", rownames(coefs))
    
    if (any(non_intercept)) {
      result <- coefs[non_intercept, , drop = FALSE]
      
      z <- stats::qnorm(1 - ((1 - conf.level) / 2))
      conf_low <- result[, "Estimate"] - z * result[, "Std. Error"]
      conf_high <- result[, "Estimate"] + z * result[, "Std. Error"]
      
      design_matrix <- stats::model.matrix(model)
      weights_vec <- stats::weights(data, "sampling")
      n_counts <- colSums(design_matrix * weights_vec)
      
      out <- data.frame(
        term = rownames(result),
        estimate = result[, "Estimate"],
        std_error = result[, "Std. Error"],
        p_value = result[, "Pr(>|t|)"],
        conf_low = conf_low,
        conf_high = conf_high,
        n = n_counts[-1],
        stringsAsFactors = FALSE
      )
      
      out[[treats]] <- gsub(pattern = treats, replacement = "", x = out$term, fixed = TRUE)
      
      if (show_means) {
        means_by_treatment <- survey::svyby(
          formula = outcome_formula,
          by = stats::reformulate(treats),
          design = data,
          FUN = survey::svymean,
          na.rm = na.rm,
          deff = FALSE
        )
        
        means_df <- as.data.frame(means_by_treatment)
        mean_col_idx <- which(!names(means_df) %in% treats)
        if (length(mean_col_idx) > 0) {
          names(means_df)[mean_col_idx[1]] <- "mean"
        }
        
        out <- merge(out, means_df, by = treats, all.x = TRUE)
      }
    }
  }
  
  # Post-process results
  out$stars <- stars_pval(out$p_value)
  
  # Calculate percent change if requested
  if (show_pct_change) {
    if (show_means) {
      if (!is.null(group_names)) {
        ref_means <- survey::svyby(
          formula = outcome_formula,
          by = stats::reformulate(group_names),
          design = data[data$variables[[treats]] == ref_level, ],
          FUN = survey::svymean,
          na.rm = na.rm,
          deff = FALSE
        )
        
        ref_means_df <- as.data.frame(ref_means)
        mean_col_idx <- which(!names(ref_means_df) %in% group_names)
        if (length(mean_col_idx) > 0) {
          names(ref_means_df)[mean_col_idx[1]] <- "ref_mean"
        }
        
        out <- merge(out, ref_means_df, by = group_names, all.x = TRUE)
        out$pct_change <- out$estimate / out$ref_mean
        out$ref_mean <- NULL
      } else {
        ref_mean_val <- survey::svymean(
          outcome_formula,
          design = data[data$variables[[treats]] == ref_level, ],
          na.rm = na.rm
        )[[1]]
        out$pct_change <- out$estimate / ref_mean_val
      }
    } else {
      cli::cli_warn("show_pct_change requires show_means = TRUE for survey data")
      out$pct_change <- NA
    }
  }
  
  # Reorder and rename columns
  col_order <- c(group_names, treats, "estimate", "pct_change", "mean", "n", "conf_low", "conf_high", "p_value", "stars")
  col_order <- col_order[col_order %in% names(out)]
  out <- out[col_order]
  
  names(out)[names(out) == "estimate"] <- "diffs"
  
  if (!show_means && "mean" %in% names(out)) {
    out <- out[, !names(out) == "mean"]
  }
  
  if (!show_pct_change && "pct_change" %in% names(out)) {
    out <- out[, !names(out) == "pct_change"]
  }
  
  # Round numeric columns
  out$diffs <- round(out$diffs, decimals)
  out$conf_low <- round(out$conf_low, decimals)
  out$conf_high <- round(out$conf_high, decimals)
  out$p_value <- round(out$p_value, decimals)
  
  if ("mean" %in% names(out)) {
    out$mean <- round(out$mean, decimals)
  }
  
  if ("pct_change" %in% names(out)) {
    out$pct_change <- round(out$pct_change, decimals + 2)
  }
  
  # Add attributes
  attr(out[[treats]], "label") <- attr(survey_data[[treats]], "label")
  attr(out$diffs, "label") <- paste("Difference in means relative to", ref_level)
  attr(out$n, "label") <- "N"
  attr(out$conf_low, "label") <- "Low CI"
  attr(out$conf_high, "label") <- "High CI"
  attr(out$p_value, "label") <- "P-Value"
  attr(out$stars, "label") <- ""
  
  if (!is.null(attr_var_label(survey_data[[x]]))) {
    attr(out, "variable_label") <- attr_var_label(survey_data[[x]])
    attr(out, "variable_name") <- x_name
  } else {
    attr(out, "variable_label") <- x_name
    attr(out, "variable_name") <- x_name
  }
  
  attr(out, "group_names") <- group_names
  
  structure(out, class = c("adlgraphs_mean_diffs", "tbl_df", "tbl", class(out)))
}


bivariate_reg <- function(
  data, 
  x, 
  treats, 
  wt, 
  show_means = FALSE,
  show_pct_change = FALSE,
  ref_level, 
  conf.level = 0.95, 
  decimals = 3
) {
  
  data[[treats]] <- make_factor(data[[treats]], drop_levels = TRUE, force = TRUE)

  if (!missing(ref_level) && ref_level != levels(data[[treats]])[1]) {
    data[[treats]] <- stats::relevel(data[[treats]], ref_level)
  } 
  
  
  # create the model
  model <- stats::lm(
    # use reformulate
    stats::reformulate(treats, x),
    data = data,
    weights = data[[wt]]
  )

  # calculate the number of observations per level
  n <- colSums(stats::model.matrix(model))
  
  # get the coefficients
  coefs <- summary(model)$coefficients %>%
    # make the rownames a column called "term"
    tibble::as_tibble(rownames = "term")
  
  # calculate z
  z <- stats::qt(1 - ((1 - conf.level) / 2), df = model$df.residual)
  # calculate the margin of error
  coefs$moe <- z * coefs$`Std. Error`
  coefs$conf.low <- coefs$Estimate - coefs$moe
  coefs$conf.high <- coefs$Estimate + coefs$moe
  
  # get the reference stats
  # use grepl() to return only rows with "(Intercept)" in term col
  ref <- coefs[grepl("(Intercept)", coefs$term, fixed = TRUE),]
  # Get non-reference stats 
  # Use !grepl() to return any rows without "(Intercept)" in term col
  non_ref <- coefs[!grepl("(Intercept)", coefs$term, fixed = TRUE),] 


  if (show_pct_change) {
    # if show_pct_change = TRUE, calculate percent change

    ref$pct_change <- NA
    # add the percent change from the ref group to the non-ref groups
    non_ref$pct_change <- non_ref$Estimate / ref$Estimate
    
  } else {
    ref$pct_change <- NA
    non_ref$pct_change <- NA
  }
  # return(non_ref)

  if (show_means) {
    # if show_means = TRUE, calculate the means

    # clean up the ref stats row
    # create a new column called "mean" with the value of the estimate
    ref$term <- ref_level
    ref$mean <- ref$Estimate
    # convert the estimate to 0
    ref$Estimate <- 0
    # convert the SE to NA
    ref$`Std. Error` <- NA
    # convert the t-value to NA
    ref$`t value` <- NA
    # convert the p-value to NA
    ref$`Pr(>|t|)` <- NA
    # add NA to the margin of error
    ref$moe <- NA
    # add NA to the low CI
    ref$conf.low <- NA
    # add NA to the high CI
    ref$conf.high <- NA
    # add the number of observations
    # n[1] is the total number of observations in the data
    # n[-1] is the number of observations in each level
    # subtract the sum of n[-1] from n[1] to get the number  
    #   of observations in the reference group
    ref$n <- n[1] - sum(n[-1])


    # clean up the non-reference stats

    # add a new column called "mean" by adding the value in the reference
    #   mean (ref$mean) to the each value in the non-reference estimate (non-ref$Estimate) 
    non_ref$mean <- ref$mean + non_ref$Estimate
    # add the number of observations
    non_ref$n <- n[-1]

    ### combine reference and non-reference stats
    # combine the two objects in a list
    list_out <- list(ref, non_ref)
    # use vec_rbind to combine the list a df by splicing (!!!)
    #   each element of list_out
    out <- vctrs::vec_rbind(!!!list_out)

    
  } else {
    non_ref$mean <- NA
    out <- non_ref
    out$n <- n[-1]

  }
  out$stars <- stars_pval(out$`Pr(>|t|)`)
  out

}




