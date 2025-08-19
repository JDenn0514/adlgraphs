#' Pivot data from wide to long with value labels
#'
#' This function is a wrapper around \code{\link[tidyr]{pivot_longer}}. This
#' function operates in pretty much the exact same way but uses the variable
#' labels from the variables specified in `cols` to make new value labels in the
#' new variable created in the `names_to` variable. 
#' 
#' An additional note is that this function also works with survey objects 
#' created with either `survey::svydesign()` or `sryvr::as_survey_design()`.
#' The function first pivots the data, then re-creates the survey object 
#' using the same variables used
#'
#' @inheritParams tidyr::pivot_longer
#' @param name_label Add a variable label to the new column with the names of
#'   the columns
#' @param ... Additional arguments passed to \code{\link[tidyr]{pivot_longer}}.
#'
#' @returns A "long" data.frame.
#' 
#' @export
pivot_longer_values <- function(
  data, 
  cols, 
  names_to = "names", 
  values_to = "values", 
  name_label,
  ...
) {
  UseMethod("pivot_longer_values")
}

#' @export
pivot_longer_values.default <- function(
  data, 
  cols, 
  names_to = "names", 
  values_to = "values", 
  name_label,
  ...
) {

  # convert names_to and values_to to strings
  names_to <- rlang::as_name(rlang::ensym(names_to))
  values_to <- rlang::as_name(rlang::ensym(values_to))
  
  # get the columns that are getting pivoted
  cols <- get_col_names(data, {{ cols }})
  
  # create the long data frame
  long <- data %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(cols),
      names_to = names_to,
      values_to = values_to,
      ...
    )
  
  # get the newly created vector of names and values
  names <- long[[names_to]]
  values <- long[[values_to]]
  
  # get the variable labels to go into names as value labels
  var_labs <- attr_var_label(data[cols])
  
  # flip the names and values of the vector
  var_labs <- setNames(names(var_labs), var_labs)
  
  # if the 
  if (missing(name_label)) {
    name_label <- attr_question_preface(data[[cols[1]]])
  }

  attr(long[[names_to]], "labels") <- var_labs
  attr(long[[names_to]], "label") <- name_label

  return(long)

}

#' @export
pivot_longer_values.survey.design <- function(
  data,
  cols,
  names_to = "names",
  values_to = "values",
  name_label,
  ...
) {
  
  # convert names_to and values_to to strings
  names_to <- rlang::as_name(rlang::ensym(names_to))
  values_to <- rlang::as_name(rlang::ensym(values_to))
  
  # Extract the data frame from the srvyr object
  df <- data$variables
  design_vars <- attr(data, "survey_vars")
  
  cols <- get_col_names(df, {{ cols }})
  
  # Get variable labels before pivoting
  var_labs <- attr_var_label(df[cols])
  var_labs <- setNames(names(var_labs), var_labs)
  
  if (missing(name_label)) {
    name_label <- attr_question_preface(df[[cols[1]]])
  }
  
  # Perform the pivot on the data
  long_data <- df %>%
    tidyr::pivot_longer(
      cols = all_of(cols),
      names_to = names_to,
      values_to = values_to,
      ...
    )
  
  # Update the names column with proper labels
  names_col <- long_data[[names_to]]
  names_col <- structure(
    names_col,
    labels = var_labs,
    label = name_label
  )
  long_data[[names_to]] <- names_col
  
  # Build the survey design arguments dynamically
  survey_args <- list(.data = long_data)
  
  # PROCESS IDS
  if (!is.null(design_vars$ids)) {
    ids <- rlang::as_name(design_vars$ids[[1]])
    if (ids == "1") {
      survey_args$ids <- 1
    } else if (grepl("\\+", ids)) {
      ids_vars <- strsplit(ids, split = " + ", fixed = TRUE)[[1]]
      survey_args$ids <- tidyselect::all_of(ids_vars)
    } else {
      survey_args$ids <- tidyselect::all_of(ids)
    }
  }
  
  # PROCESS STRATA
  if (!is.null(design_vars$strata)) {
    strata <- as.character(design_vars$strata[[1]])
    if (grepl("\\+", strata)) {
      strata_vars <- strsplit(strata, split = " + ", fixed = TRUE)[[1]]
      survey_args$strata <- tidyselect::all_of(strata_vars)
    } else {
      survey_args$strata <- tidyselect::all_of(strata)
    }
  }
  
  # PROCESS WEIGHTS
  if (!is.null(design_vars$weights)) {
    weights <- rlang::as_name(design_vars$weights[[1]])
    if (grepl("\\+", weights)) {
      weights_vars <- strsplit(weights, split = " + ", fixed = TRUE)[[1]]
      survey_args$weights <- tidyselect::all_of(weights_vars)
    } else {
      survey_args$weights <- tidyselect::all_of(weights)
    }
  }
  
  # PROCESS FPC
  if (!is.null(design_vars$fpc)) {
    fpc <- rlang::as_name(design_vars$fpc[[1]])
    if (grepl("\\+", fpc)) {
      fpc_vars <- strsplit(fpc, split = " + ", fixed = TRUE)[[1]]
      survey_args$fpc <- tidyselect::all_of(fpc_vars)
    } else {
      survey_args$fpc <- tidyselect::all_of(fpc)
    }
  }
  
  # PROCESS NEST
  if (!is.null(design_vars$nest)) {
    survey_args$nest <- design_vars$nest
  }
  
  # Create the survey design using do.call to only pass non-NULL arguments
  new_survey <- do.call(srvyr::as_survey_design, survey_args)
  
  return(new_survey)
}
