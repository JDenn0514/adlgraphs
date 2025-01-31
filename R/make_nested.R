#' Create a nested data frame
#' 
#' This function is very similar to `tidyr::nest()`in that it creates a data 
#' frame where one of the columns is comprised of a list of data frames. 
#' 
#' @details
#' While similar to `tidyr::nest()` there are a few key differences. 
#' 
#' The first is that this function uses the vctrs package under the hood, 
#' which makes it about twice as fast as `tidyr::nest()`. 
#' 
#' Second, it is more limited in its scope and functionality. Where 
#' `tidyr::nest()` allows you to determine which columns are in the inner 
#' data frames (the ones in the list column), this function does not. 
#' Instead, you are only able to specify the variables that remain in the 
#' outer data frame. 
#' 
#' Third,  this function creates an extra column called "name" that 
#' concatenates the values from the columns in the outer rows together
#' using `paste()`.
#' 
#' Fourth, unlike in `tidyr::nest()` where variables supplied to the `.by` 
#' argument supersede any grouping variables specified through 
#' `dplyr::group_by()`, `nest_data()` combines the two to nest the data. 
#' 
#' @param data A data frame
#' @param group Columns to nest by; these will remain in the outer data 
#'   frame. If `data` is not a grouped data frame, then this must be supplied.
#'   If supplying multiple must be in a vector. Can be either a string or 
#'   symbol.
#' @param na.rm Determines if rows with NA should be kept or dropped. Defaults
#'   to TRUE.
#' @param sep A character string to separate the values. This is the `sep` 
#'   argument in `paste()`. Check examples to to see it in action.
#' 
#' @examples
#' 
#' library(dplyr)
#' # nest by one variable
#' make_nested(test_data, pid_f3)
#' 
#' # nest by multiple variables
#' make_nested(test_data, c(pid_f3, edu_f2))
#' 
#' # group data to create nested data frame
#' test_data %>% 
#'   dplyr::group_by(pid_f3, edu_f2) %>% 
#'   make_nested()
#' 
#' # use group_by and nest_data 
#' test_data %>% 
#'   dplyr::group_by(pid_f3) %>% 
#'   make_nested(edu_f2)
#' 
#' # use different sep argument
#' make_nested(test_data, c(pid_f3, edu_f2), sep = ":")
#'   
#' 
#' 
#' @export
make_nested <- function(data, group, na.rm = TRUE, sep = "_") {
  
  ## Prepare group variables
  group_vars <- if (missing(group)) NULL else select_groups({{ group }}, data)
  group_vars <- group_vars[group_vars != "c"]
  group_names <- if (inherits(data, "grouped_df")) dplyr::group_vars(data) else NULL
  group_names <- unique(c(group_names, group_vars))

  if (is.null(group_names)) stop("No grouping variables were provided. Must supply at least one.")

  # split up the data frame using vec_split 
  res <- vctrs::vec_split(
    # the data frame to split, use setdiff to get the columns not in group_names
    x = data[!names(data) %in% c(group_names)],
    # split the data by the grouping variables
    by = data[group_names]
  )
  # create a nested data frame based on the split data from res
  nest_data <- vctrs::vec_cbind(
    # this creates columns with the levels from the variables used to split the data
    res$key, 
    # this creates a new tibble from each combination of levels used to split the data
    tibble::new_tibble(list(data = res$val))
  )
  # get the columns in group_names as a list and unname it
  cols <- unname(as.list(nest_data[group_names]))
  # using the list of columns, paste them together using do.call and paste
  nest_data$name <- do.call(paste, c(cols, sep = sep))

  if (na.rm) nest_data <- na.omit(nest_data)
  
  sort_by(nest_data, nest_data[group_names])

}